// Autopilot: periodically discover tokens, research the most promising few,
// and buy the ones that pass verification AND every strategy filter — within
// all risk limits, a daily buy cap and per-token cooldowns. Exits are handled
// by the engine's monitor (take-profit ladder, optional stop-loss/time exit).
//
// In paper mode it simulates fills. In live mode it only runs with the bot
// wallet (a browser wallet cannot sign unattended) and only after an explicit
// start with a typed confirmation.
import type { Alerts } from '../alerts.js';
import type { AuditLog } from '../audit.js';
import type { Scanner } from '../research/scanner.js';
import type { State, Store } from '../store.js';
import type { BotWallet } from '../wallet/botWallet.js';
import type { AutopilotLogEntry } from '../../shared/types.js';
import { big } from './accounting.js';
import { TradeRejected, type Engine } from './engine.js';
import { autopilotBuysToday, describeDropped, evaluateCandidate, prefilterWithStats, recentlyTraded } from './strategy.js';

const SOL = 1e9;
/** Violations that block every buy, not just this token: stop the cycle instead of trying the next one. */
const GLOBAL_BLOCKERS = /Exposure|max open positions|Daily loss|daily loss budget|Insufficient SOL|paused|not enabled|locked|autopilot is off/i;

export const solString = (sol: number): string => sol.toFixed(9).replace(/0+$/, '').replace(/\.$/, '');

export class Autopilot {
  private timer: NodeJS.Timeout | null = null;
  private running = false;

  constructor(
    private readonly store: Store,
    private readonly audit: AuditLog,
    private readonly alerts: Alerts,
    private readonly engine: Engine,
    private readonly scanner: Scanner,
    private readonly botWallet: BotWallet,
  ) {}

  get s(): State {
    return this.store.state;
  }

  private log(e: Omit<AutopilotLogEntry, 'at'>): void {
    this.store.update((s) => s.autopilot.log.push({ at: new Date().toISOString(), ...e }));
  }

  /** Log a status line unless it repeats the previous one. */
  private note(detail: string): void {
    const last = this.s.autopilot.log.at(-1);
    if (last?.action === 'info' && last.detail === detail) return;
    this.log({ action: 'info', detail });
  }

  start(): void {
    const loop = async () => {
      await this.cycle().catch((e) => this.log({ action: 'error', detail: `cycle failed: ${(e as Error).message}` }));
      this.timer = setTimeout(loop, Math.max(30, this.s.settings.autopilot.scanIntervalSec) * 1000);
    };
    this.timer = setTimeout(loop, 5_000);
  }

  stop(): void {
    if (this.timer) clearTimeout(this.timer);
  }

  /** Problems that prevent starting the autopilot right now. */
  startProblems(phrase?: string): string[] {
    const s = this.s;
    const ap = s.settings.autopilot;
    const L = s.settings.limits;
    const problems: string[] = [];
    if (s.paused) problems.push('Resume from the emergency pause first.');
    if (s.mode === 'live') {
      if (s.live?.signer !== 'bot') problems.push('Live autopilot needs live mode with the bot wallet — a browser wallet cannot sign unattended.');
      if (!this.botWallet.isUnlocked()) problems.push('Unlock the bot wallet first.');
      if (phrase !== 'START AUTOPILOT') problems.push('Type START AUTOPILOT to confirm.');
      if (!s.settings.stopLoss.enabled && !ap.acknowledgeNoStopLoss)
        problems.push('Decide on a stop-loss first: enable one in Limits & rules, or tick "run without a stop-loss" in the autopilot strategy.');
    }
    if (ap.buySizeSol > L.maxBuySol) problems.push(`Buy size ${ap.buySizeSol} SOL is above your max buy (${L.maxBuySol} SOL).`);
    if (ap.slippageBps > L.maxSlippageBps) problems.push(`Autopilot slippage ${ap.slippageBps} bps is above your limit (${L.maxSlippageBps} bps).`);
    if (!ap.sources.newPools && !ap.sources.trendingPools && ap.watchlist.length === 0) problems.push('Turn on a discovery source or add tokens to the watchlist.');
    if (!s.settings.takeProfit.enabled && !s.settings.stopLoss.enabled && !s.settings.timeExit.enabled)
      problems.push('Turn on at least one exit rule (take-profit ladder, stop-loss or time exit); otherwise the autopilot would only ever buy.');
    return problems;
  }

  enable(phrase?: string): void {
    const problems = this.startProblems(phrase);
    if (problems.length) throw new TradeRejected('Autopilot not started.', problems);
    this.store.update((s) => {
      s.settings.autopilot.enabled = true;
      s.autopilot.consecutiveErrors = 0;
      s.autopilot.lastError = null;
    });
    const mode = this.s.mode;
    this.audit.append('autopilot.started', { mode, autopilot: this.s.settings.autopilot, takeProfit: this.s.settings.takeProfit, stopLoss: this.s.settings.stopLoss, limits: this.s.settings.limits });
    this.log({ action: 'info', detail: `Autopilot started (${mode}).` });
    this.alerts.raise(mode === 'live' ? 'warn' : 'info', mode === 'live' ? 'LIVE autopilot started: the bot wallet will buy and sell automatically within your limits.' : 'Paper autopilot started.');
    setTimeout(() => void this.cycle(), 500);
  }

  disable(reason: string): void {
    if (!this.s.settings.autopilot.enabled) return;
    this.store.update((s) => (s.settings.autopilot.enabled = false));
    this.audit.append('autopilot.stopped', { reason });
    this.log({ action: 'info', detail: `Autopilot stopped: ${reason}` });
  }

  /** One scan → research → (maybe) buy round. */
  async cycle(now = new Date()): Promise<void> {
    if (this.running) return;
    this.running = true;
    try {
      const ap = this.s.settings.autopilot;
      if (!ap.enabled || this.s.paused) return;
      if (!this.engine.autopilotActive()) {
        this.note('Waiting: live autopilot needs the bot wallet unlocked and live mode using the bot wallet.');
        return;
      }
      const mode = this.s.mode;
      let buys = autopilotBuysToday(this.s.positions, mode, now);
      if (buys >= ap.maxBuysPerDay) {
        this.note(`Daily buy limit reached (${ap.maxBuysPerDay}); exits keep running.`);
        return;
      }
      const blockers = await this.engine.buyBlockers(big(Math.round(ap.buySizeSol * SOL)));
      if (blockers.length) {
        this.note(`Not buying: ${blockers.join('; ')}. Exits keep running.`);
        return;
      }

      const scan = await this.scanner.scan(ap, now);
      this.store.update((s) => {
        s.autopilot.lastScanAt = now.toISOString();
        s.autopilot.lastScanCandidates = scan.candidates.length;
      });
      if (scan.errors.length) {
        this.log({ action: 'error', detail: `Discovery problems: ${scan.errors.join('; ')}` });
        if (scan.candidates.length === 0) {
          const n = this.store.update((s) => {
            s.autopilot.lastError = scan.errors.join('; ');
            return ++s.autopilot.consecutiveErrors;
          });
          if (n >= 3) this.engine.pause('autopilot: token discovery failed 3 times in a row', 'system');
          return;
        }
      }
      this.store.update((s) => (s.autopilot.consecutiveErrors = 0));

      const rejected = this.s.autopilot.rejected;
      const { kept, dropped } = prefilterWithStats(scan.candidates, ap);
      const cands = kept.filter((c) => {
        if (recentlyTraded(this.s.positions, c.mint, mode, now, ap.cooldownMinutes)) {
          dropped['already held / bought recently'] = (dropped['already held / bought recently'] ?? 0) + 1;
          return false;
        }
        if (rejected[c.mint] && new Date(rejected[c.mint]!).getTime() > now.getTime()) {
          dropped[`rejected recently (re-checked after ${ap.recheckAfterMinutes} min)`] = (dropped[`rejected recently (re-checked after ${ap.recheckAfterMinutes} min)`] ?? 0) + 1;
          return false;
        }
        return true;
      });
      if (!cands.length) {
        this.note(`Scanned ${scan.candidates.length} pools, none to research. Filtered out: ${describeDropped(dropped) || 'nothing'}. Next scan in ${ap.scanIntervalSec} s.`);
        return;
      }
      this.note(`Scanned ${scan.candidates.length} pools: researching up to ${Math.min(cands.length, ap.maxResearchPerScan)} of ${cands.length}. Filtered out: ${describeDropped(dropped) || 'nothing'}.`);

      let researched = 0;
      for (const c of cands) {
        if (researched >= ap.maxResearchPerScan || buys >= ap.maxBuysPerDay) break;
        if (!this.engine.autopilotActive()) break; // paused or stopped mid-cycle
        researched++;
        const base = { mint: c.mint, symbol: c.symbol, source: c.source };
        const recheckAt = new Date(now.getTime() + ap.recheckAfterMinutes * 60_000).toISOString();
        let report;
        try {
          report = await this.engine.research(c.mint);
        } catch (e) {
          this.log({ ...base, action: 'error', detail: `research failed: ${(e as Error).message}` });
          this.store.update((s) => (s.autopilot.rejected[c.mint] = recheckAt));
          continue;
        }
        const d = evaluateCandidate(report, ap);
        const symbol = report.token.symbol.value ?? c.symbol;
        if (!d.ok) {
          this.log({ ...base, symbol, action: 'skip', detail: `Skipped: ${d.failures[0]}`, reasons: d.failures });
          this.store.update((s) => (s.autopilot.rejected[c.mint] = recheckAt));
          continue;
        }
        try {
          const intent = await this.engine.prepare({ side: 'buy', mint: c.mint, amountSol: solString(ap.buySizeSol), slippageBps: ap.slippageBps, reason: 'autopilot' }, { automated: true });
          if (mode === 'paper') this.engine.confirmPaper(intent.id);
          else await this.engine.executeWithBot(intent.id);
          buys++;
          this.log({ ...base, symbol, action: 'buy', detail: `Bought ${solString(ap.buySizeSol)} SOL of ${symbol ?? c.mint}.`, reasons: d.passes });
          this.audit.append('autopilot.buy', { mint: c.mint, symbol, intentId: intent.id, checks: d.passes, source: c.source });
        } catch (e) {
          const v = e instanceof TradeRejected ? e.violations : [];
          this.log({ ...base, symbol, action: 'skip', detail: `Buy not done: ${v[0] ? `${(e as Error).message} ${v[0]}` : (e as Error).message}`, reasons: v });
          if (v.some((x) => GLOBAL_BLOCKERS.test(x))) break;
          this.store.update((s) => (s.autopilot.rejected[c.mint] = recheckAt));
        }
      }
    } finally {
      this.running = false;
    }
  }
}
