// Trading engine: prepare → (paper fill | wallet signature → submit →
// confirm) → accounting, plus the monitor loop that evaluates exit rules.
// On any uncertainty (unknown transaction outcome, repeated data failures)
// it pauses trading and raises an alert rather than guessing.
import { createHash, randomUUID } from 'node:crypto';
import bs58 from 'bs58';
import { getAssociatedTokenAddressSync } from '@solana/spl-token';
import { PublicKey, SendTransactionError, VersionedTransaction, type Connection } from '@solana/web3.js';
import type { Alerts } from '../alerts.js';
import type { AuditLog } from '../audit.js';
import { env } from '../env.js';
import { NoRouteError, type Quote, type SwapProvider } from '../market/types.js';
import type { Researcher } from '../research/index.js';
import type { State, Store } from '../store.js';
import type { Fill, IntentSummary, Mode, Position, PrepareRequest, Proposal, ResearchReport, TradeIntent, TradeReason } from '../../shared/types.js';
import { WSOL_MINT } from '../../shared/types.js';
import { applySell, big, openPosition, realizedOnUtcDay, remainingTakeProfitRaw, unrealizedLamports } from './accounting.js';
import { checkTrade } from './risk.js';
import { evaluateRules, multipleOfEntry, takeProfitTriggered, type SellQuote } from './rules.js';
import { fetchLookupTables, simulateSwap } from './simulate.js';
import { verifyTransaction } from './txverify.js';

export class TradeRejected extends Error {
  constructor(
    message: string,
    readonly violations: string[] = [],
  ) {
    super(message);
  }
}

const BASE_FEE = 5_000n;
const ATA_RENT = 2_039_280n;

export function parseSol(s: string): bigint {
  if (!/^\d+(\.\d{1,9})?$/.test(s.trim())) throw new TradeRejected('Amount must be a decimal SOL amount with at most 9 decimals.');
  const [w, f = ''] = s.trim().split('.');
  return BigInt(w!) * 1_000_000_000n + BigInt(f.padEnd(9, '0'));
}

export class Engine {
  private monitorFailures = 0;
  private monitorTimer: NodeJS.Timeout | null = null;
  private monitoring = false;

  constructor(
    private readonly store: Store,
    private readonly audit: AuditLog,
    private readonly alerts: Alerts,
    readonly provider: SwapProvider,
    private readonly researcher: Researcher,
    private readonly conn: Connection,
  ) {}

  get s(): State {
    return this.store.state;
  }

  // ------------------------------------------------------------ pause

  pause(reason: string, source: 'user' | 'system'): void {
    const cancelled: string[] = [];
    this.store.update((s) => {
      s.paused = true;
      s.pauseReason = reason;
      s.pausedAt = new Date().toISOString();
      for (const i of s.intents) {
        // Unsigned / unsent intents can be cancelled; submitted ones are already on their way to the network.
        if (i.status === 'pending') {
          i.status = 'cancelled';
          i.error = 'cancelled by emergency pause';
          cancelled.push(i.id);
        }
      }
    });
    this.audit.append('pause', { reason, source, cancelledIntents: cancelled });
    this.alerts.raise(source === 'system' ? 'error' : 'warn', `Trading paused: ${reason}${cancelled.length ? ` (${cancelled.length} pending trade(s) cancelled)` : ''}`);
  }

  resume(): void {
    this.store.update((s) => {
      s.paused = false;
      s.pauseReason = null;
      s.pausedAt = null;
    });
    this.monitorFailures = 0;
    this.audit.append('resume', {});
  }

  // ------------------------------------------------------------ research

  async research(mint: string): Promise<ResearchReport> {
    const r = await this.researcher.research(mint.trim());
    this.store.update((s) => (s.research[r.mint] = r));
    this.audit.append('research', {
      mint: r.mint,
      verdict: r.verdict,
      reasons: r.verdictReasons,
      flags: r.flags.map((f) => ({ code: f.code, severity: f.severity, kind: f.kind })),
      sources: r.sources,
    });
    return r;
  }

  private async freshResearch(mint: string): Promise<ResearchReport> {
    const cached = this.s.research[mint];
    const maxAge = this.s.settings.verification.maxDataAgeSec * 1000;
    if (cached && Date.now() - new Date(cached.generatedAt).getTime() < maxAge * 0.8) return cached;
    return this.research(mint);
  }

  // ------------------------------------------------------------ helpers

  private openPositions(mode: Mode): Position[] {
    return this.s.positions.filter((p) => p.status === 'open' && p.mode === mode);
  }

  private realizedToday(mode: Mode, now: Date): bigint {
    const key = now.toISOString().slice(0, 10);
    const events = this.s.lossEvents.filter((e) => e.mode === mode && e.at.slice(0, 10) === key).reduce((a, e) => a - big(e.lamports), 0n);
    return realizedOnUtcDay(this.s.positions.filter((p) => p.mode === mode), now) + events;
  }

  private unrealizedLoss(mode: Mode): bigint {
    let loss = 0n;
    for (const p of this.openPositions(mode)) {
      const m = this.s.marks[p.id];
      if (!m || m.stale) continue;
      const u = unrealizedLamports(p, big(m.netValueLamports));
      if (u < 0n) loss += u;
    }
    return loss;
  }

  private estFees(mode: Mode): { network: bigint; priority: bigint } {
    return mode === 'paper' ? { network: BASE_FEE, priority: BigInt(this.s.settings.paper.priorityFeeLamports) } : { network: BASE_FEE, priority: BigInt(this.s.settings.priorityFeeMaxLamports) };
  }

  private async balance(mode: Mode, wallet: string | null): Promise<bigint | null> {
    if (mode === 'paper') return big(this.s.paperBalanceLamports);
    if (!wallet) return null;
    try {
      return BigInt(await this.conn.getBalance(new PublicKey(wallet), 'confirmed'));
    } catch {
      return null;
    }
  }

  // ------------------------------------------------------------ prepare

  async prepare(req: PrepareRequest, opts: { automated?: boolean; ruleAppliesAfterTakeProfit?: boolean } = {}): Promise<TradeIntent> {
    const mode = this.s.mode;
    const now = new Date();
    if (this.s.paused) throw new TradeRejected('Trading is paused.', ['Trading is paused (emergency pause is on).']);
    const wallet = mode === 'live' ? (req.wallet ?? null) : null;
    const reason: TradeReason = req.reason ?? 'manual';

    let position: Position | null = null;
    let research: ResearchReport | null = null;
    let mint: string;
    let amountRaw: bigint;
    let decimals: number;
    let symbol: string;
    let tokenProgram: string;

    if (req.side === 'buy') {
      if (!req.mint || !req.amountSol) throw new TradeRejected('Buy needs a mint and an amount.');
      mint = req.mint.trim();
      amountRaw = parseSol(req.amountSol);
      research = await this.freshResearch(mint);
      if (research.token.decimals.value === null || !research.token.tokenProgram.value) throw new TradeRejected('Token decimals / program unknown; cannot trade.', ['Key token data missing.']);
      decimals = research.token.decimals.value;
      tokenProgram = research.token.tokenProgram.value;
      symbol = research.token.symbol.value ?? mint.slice(0, 6);
    } else {
      position = this.s.positions.find((p) => p.id === req.positionId) ?? null;
      if (!position) throw new TradeRejected('Unknown position.');
      mint = position.mint;
      decimals = position.decimals;
      symbol = position.symbol;
      const held = big(position.heldTokenRaw);
      amountRaw = req.amountRaw ? big(req.amountRaw) : req.sellPct ? (held * BigInt(Math.round(req.sellPct * 100))) / 10_000n : 0n;
      research = this.s.research[mint] ?? null;
      tokenProgram = research?.token.tokenProgram.value ?? '';
      if (mode === 'live' && !tokenProgram) tokenProgram = (await this.freshResearch(mint)).token.tokenProgram.value ?? '';
    }

    const inputMint = req.side === 'buy' ? WSOL_MINT : mint;
    const outputMint = req.side === 'buy' ? mint : WSOL_MINT;
    let quote: Quote | null = null;
    let quoteErr: string | null = null;
    if (amountRaw > 0n && req.slippageBps > 0 && req.slippageBps <= this.s.settings.limits.maxSlippageBps) {
      try {
        quote = await this.provider.quote({ inputMint, outputMint, amountRaw, slippageBps: req.slippageBps });
      } catch (e) {
        quoteErr = e instanceof NoRouteError ? `No route: ${e.message}` : `Quote failed: ${(e as Error).message}`;
      }
    }

    const fees = this.estFees(mode);
    const needsAta = req.side === 'buy' && !this.openPositions(mode).some((p) => p.mint === mint);
    const rent = needsAta ? ATA_RENT : 0n;
    const risk = checkTrade({
      settings: this.s.settings,
      mode,
      paused: this.s.paused,
      liveArmed: !!this.s.live,
      side: req.side,
      amountRaw,
      requestedSlippageBps: req.slippageBps,
      quote: quote ? { priceImpactPct: quote.summary.priceImpactPct, fetchedAt: quote.fetchedAt } : null,
      now,
      openPositions: this.openPositions(mode),
      realizedTodayLamports: this.realizedToday(mode, now),
      unrealizedLossLamports: this.unrealizedLoss(mode),
      balanceLamports: req.side === 'buy' ? await this.balance(mode, wallet) : null,
      estCostsLamports: fees.network + fees.priority + rent,
      research,
      overrideUnverified: !!req.overrideUnverified,
      position,
      acknowledgeLocked: !!req.acknowledgeLocked,
      automated: !!opts.automated,
      ruleAppliesAfterTakeProfit: !!opts.ruleAppliesAfterTakeProfit,
      wallet,
    });
    if (req.side === 'sell') risk.violations = risk.violations.filter((v) => !v.startsWith('Wallet balance unknown'));
    if (quoteErr) risk.violations.push(quoteErr);
    if (risk.violations.length) {
      this.audit.append('trade.rejected', { mode, req, violations: risk.violations });
      throw new TradeRejected('Trade blocked by risk checks.', risk.violations);
    }
    const q = quote!;

    const est = big(q.summary.outAmount);
    const estPrice = req.side === 'buy' ? Number(amountRaw) / 1e9 / (Number(est) / 10 ** decimals) : Number(est) / 1e9 / (Number(amountRaw) / 10 ** decimals);
    const summary: IntentSummary = {
      side: req.side,
      mint,
      symbol,
      decimals,
      inputMint,
      outputMint,
      inAmountRaw: amountRaw.toString(),
      estOutRaw: q.summary.outAmount,
      minOutRaw: q.summary.minOutAmount,
      estPriceSolPerToken: estPrice,
      slippageBps: req.slippageBps,
      priceImpactPct: q.summary.priceImpactPct,
      route: q.summary.routeLabels,
      fees: { networkLamports: fees.network.toString(), priorityMaxLamports: fees.priority.toString(), platformRaw: q.summary.platformFeeRaw, rentDepositLamports: rent.toString() },
      riskFlags: research?.flags ?? [],
      verdict: research?.verdict ?? 'n/a',
      warnings: risk.warnings,
      permissions: mode === 'paper' ? ['Paper trade: nothing is signed or sent to the network.'] : [],
      instructions: [],
      simulation: null,
    };

    const intent: TradeIntent = {
      id: randomUUID(),
      mode,
      createdAt: now.toISOString(),
      expiresAt: new Date(q.fetchedAt.getTime() + this.s.settings.quoteMaxAgeSec * 1000).toISOString(),
      status: 'pending',
      reason,
      positionId: position?.id ?? null,
      wallet,
      summary,
      request: { ...req, mint },
    };

    if (mode === 'live') {
      if (!wallet) throw new TradeRejected('Connect your wallet.');
      const built = await this.provider.buildSwap(q, wallet, this.s.settings.priorityFeeMaxLamports);
      const tx = VersionedTransaction.deserialize(Buffer.from(built.txBase64, 'base64'));
      const alts = await fetchLookupTables(this.conn, tx);
      const v = verifyTransaction(tx, alts, { user: wallet, side: req.side, tokenMint: mint, tokenProgram });
      summary.instructions = v.instructions;
      summary.permissions = v.permissions;
      summary.fees.priorityMaxLamports = String(v.priorityFeeMaxLamports);
      if (!v.ok) {
        this.audit.append('trade.verify_failed', { mint, problems: v.problems });
        this.alerts.raise('error', `Refused to sign a ${req.side} of ${symbol}: transaction failed verification (${v.problems.join('; ')}).`);
        throw new TradeRejected('Transaction failed verification.', v.problems);
      }
      const feeBudget = BASE_FEE + BigInt(v.priorityFeeMaxLamports) + BigInt(v.ataCreations) * ATA_RENT;
      const sim = await simulateSwap(this.conn, tx, { user: wallet, side: req.side, tokenMint: mint, tokenProgram, inAmountRaw: amountRaw, minOutRaw: big(q.summary.minOutAmount), feeBudgetLamports: feeBudget });
      summary.simulation = { ok: sim.ok, detail: sim.detail, ...(sim.logs ? { logs: sim.logs } : {}) };
      if (!sim.ok) {
        this.audit.append('trade.simulation_failed', { mint, detail: sim.detail });
        throw new TradeRejected('Simulation failed.', [sim.detail]);
      }
      intent.txBase64 = built.txBase64;
      intent.messageHashHex = createHash('sha256').update(tx.message.serialize()).digest('hex');
      intent.lastValidBlockHeight = built.lastValidBlockHeight;
    }

    this.store.update((s) => {
      s.intents.push(intent);
      if (req.proposalId) {
        const p = s.proposals.find((x) => x.id === req.proposalId);
        if (p) p.status = 'done';
      }
    });
    this.audit.append('trade.prepared', { intentId: intent.id, mode, reason, wallet, summary: { ...summary, instructions: summary.instructions.map((i) => i.name) }, quote: q.summary, researchVerdict: research?.verdict, researchReasons: research?.verdictReasons });
    return intent;
  }

  private assertConfirmable(intent: TradeIntent | undefined, mode: Mode): TradeIntent {
    if (!intent) throw new TradeRejected('Unknown trade.');
    if (intent.status !== 'pending') throw new TradeRejected(`Trade is ${intent.status}.`);
    if (intent.mode !== mode || this.s.mode !== mode) throw new TradeRejected('Trading mode changed since this trade was prepared.');
    if (this.s.paused) throw new TradeRejected('Trading is paused.');
    if (Date.now() > new Date(intent.expiresAt).getTime()) {
      this.store.update(() => (intent.status = 'expired'));
      throw new TradeRejected('Quote expired. Prepare the trade again to get a fresh price.');
    }
    // Limits may have changed since the intent was prepared (another trade filled, settings edited).
    const r = checkTrade({
      settings: this.s.settings,
      mode,
      paused: this.s.paused,
      liveArmed: !!this.s.live,
      side: intent.summary.side,
      amountRaw: big(intent.summary.inAmountRaw),
      requestedSlippageBps: intent.summary.slippageBps,
      quote: { priceImpactPct: intent.summary.priceImpactPct, fetchedAt: new Date(intent.createdAt) },
      now: new Date(),
      openPositions: this.openPositions(mode),
      realizedTodayLamports: this.realizedToday(mode, new Date()),
      unrealizedLossLamports: this.unrealizedLoss(mode),
      // Live balances were checked at prepare time and are enforced again by simulation and by the chain itself.
      balanceLamports: intent.summary.side === 'buy' ? (mode === 'paper' ? big(this.s.paperBalanceLamports) : 10n ** 18n) : null,
      estCostsLamports: big(intent.summary.fees.networkLamports) + big(intent.summary.fees.priorityMaxLamports) + big(intent.summary.fees.rentDepositLamports),
      research: intent.summary.side === 'buy' ? (this.s.research[intent.summary.mint] ?? null) : null,
      overrideUnverified: !!intent.request.overrideUnverified,
      position: intent.positionId ? (this.s.positions.find((p) => p.id === intent.positionId) ?? null) : null,
      acknowledgeLocked: !!intent.request.acknowledgeLocked || intent.reason !== 'manual',
      automated: false,
      wallet: intent.wallet,
    });
    const violations = r.violations.filter((v) => !v.startsWith('Wallet balance unknown') && !v.startsWith('Research report is stale'));
    if (violations.length) throw new TradeRejected('Trade no longer passes risk checks.', violations);
    return intent;
  }

  // ------------------------------------------------------------ paper

  confirmPaper(id: string): Position {
    const intent = this.assertConfirmable(this.s.intents.find((i) => i.id === id), 'paper');
    const S = intent.summary;
    const est = big(S.estOutRaw);
    const out = (est * BigInt(10_000 - this.s.settings.paper.extraSlippageBps)) / 10_000n;
    const fees = this.estFees('paper');
    const at = new Date().toISOString();
    if (out < big(S.minOutRaw)) {
      // A real swap would revert here and still cost the network fee.
      this.store.update((s) => {
        intent.status = 'failed';
        intent.error = 'simulated slippage exceeded tolerance; swap reverted';
        s.paperBalanceLamports = (big(s.paperBalanceLamports) - fees.network - fees.priority).toString();
        s.lossEvents.push({ at, lamports: (fees.network + fees.priority).toString(), mode: 'paper', reason: 'failed swap fee' });
      });
      this.audit.append('trade.failed', { intentId: id, mode: 'paper', error: intent.error });
      throw new TradeRejected('Simulated slippage exceeded your tolerance; the swap would revert (fee still charged).');
    }
    const fill: Fill = {
      id: randomUUID(),
      side: S.side,
      at,
      mode: 'paper',
      reason: intent.reason,
      inputRaw: S.inAmountRaw,
      outputRaw: out.toString(),
      quotedOutRaw: S.estOutRaw,
      requestedInRaw: S.inAmountRaw,
      networkFeeLamports: fees.network.toString(),
      priorityFeeLamports: fees.priority.toString(),
      platformFeeLamports: S.side === 'sell' ? S.fees.platformRaw : '0',
      rentLamports: S.fees.rentDepositLamports,
      intentId: id,
    };
    return this.recordFill(intent, fill);
  }

  private recordFill(intent: TradeIntent, fill: Fill): Position {
    const S = intent.summary;
    const tp = this.s.settings.takeProfit;
    let result!: Position;
    this.store.update((s) => {
      if (S.side === 'buy') {
        result = openPosition({ id: randomUUID(), mint: S.mint, symbol: S.symbol, decimals: S.decimals, wallet: intent.wallet }, fill, tp);
        s.positions.push(result);
        if (intent.mode === 'paper') s.paperBalanceLamports = (big(s.paperBalanceLamports) - big(fill.inputRaw) - big(fill.networkFeeLamports) - big(fill.priorityFeeLamports) - big(fill.rentLamports)).toString();
      } else {
        const idx = s.positions.findIndex((p) => p.id === intent.positionId);
        const { position, fill: f } = applySell(s.positions[idx]!, fill);
        s.positions[idx] = position;
        result = position;
        fill.realizedPnlLamports = f.realizedPnlLamports;
        if (intent.mode === 'paper') s.paperBalanceLamports = (big(s.paperBalanceLamports) + big(fill.outputRaw) - big(fill.networkFeeLamports) - big(fill.priorityFeeLamports) - big(fill.platformFeeLamports)).toString();
      }
      intent.status = 'confirmed';
    });
    this.audit.append('trade.filled', { intentId: intent.id, positionId: result.id, mode: intent.mode, fill, takeProfit: result.takeProfit, locked: result.locked });
    if (result.takeProfit.state === 'done' && fill.reason === 'take_profit')
      this.alerts.raise('info', `Take-profit completed for ${result.symbol}: sold ${result.takeProfit.soldRaw} raw units. Position is now locked — no further automatic trades.`);
    return result;
  }

  cancel(id: string): void {
    this.store.update(() => {
      const i = this.s.intents.find((x) => x.id === id);
      if (i && i.status === 'pending') i.status = 'cancelled';
    });
    this.audit.append('trade.cancelled', { intentId: id });
  }

  // ------------------------------------------------------------ live

  async submitSigned(id: string, signedTxBase64: string): Promise<{ intent: TradeIntent; position?: Position }> {
    const intent = this.assertConfirmable(this.s.intents.find((i) => i.id === id), 'live');
    const tx = VersionedTransaction.deserialize(Buffer.from(signedTxBase64, 'base64'));
    const hash = createHash('sha256').update(tx.message.serialize()).digest('hex');
    if (hash !== intent.messageHashHex) {
      this.store.update(() => {
        intent.status = 'failed';
        intent.error = 'signed transaction differs from the reviewed transaction';
      });
      this.audit.append('trade.tampered', { intentId: id });
      this.alerts.raise('error', 'The signed transaction differs from the one you reviewed (your wallet or an extension modified it). It was NOT sent.');
      throw new TradeRejected('Signed transaction does not match the reviewed transaction; not sent.');
    }
    const sigBytes = tx.signatures[0];
    if (!sigBytes || sigBytes.every((b) => b === 0)) throw new TradeRejected('Transaction is not signed.');
    const signature = bs58.encode(sigBytes);

    this.store.update(() => {
      intent.status = 'submitted';
      intent.signature = signature;
    });
    this.audit.append('trade.submitted', { intentId: id, signature });

    try {
      await this.conn.sendRawTransaction(tx.serialize(), { skipPreflight: false, preflightCommitment: 'confirmed', maxRetries: 3 });
    } catch (e) {
      if (e instanceof SendTransactionError) {
        this.store.update(() => {
          intent.status = 'failed';
          intent.error = `rejected in preflight: ${e.message}`;
        });
        this.audit.append('trade.failed', { intentId: id, signature, error: e.message });
        this.alerts.raise('warn', `Trade rejected by the network before execution (no funds moved): ${e.message.slice(0, 200)}`);
        throw new TradeRejected('The network rejected the transaction in preflight; nothing was executed.', [e.message]);
      }
      return this.markUnknown(intent, `send failed with an uncertain outcome: ${(e as Error).message}`);
    }

    try {
      const conf = await this.conn.confirmTransaction({ signature, blockhash: tx.message.recentBlockhash, lastValidBlockHeight: intent.lastValidBlockHeight! }, 'confirmed');
      if (conf.value.err) return this.finalizeFailed(intent, JSON.stringify(conf.value.err));
    } catch (e) {
      const st = await this.conn.getSignatureStatus(signature, { searchTransactionHistory: true }).catch(() => null);
      if (st && st.value === null && /block ?height exceeded|expired/i.test((e as Error).message)) {
        this.store.update(() => {
          intent.status = 'expired';
          intent.error = 'transaction expired without landing';
        });
        this.audit.append('trade.expired', { intentId: id, signature });
        this.alerts.raise('warn', 'Transaction expired without landing on-chain; nothing was executed.');
        return { intent };
      }
      return this.markUnknown(intent, `confirmation uncertain: ${(e as Error).message}`);
    }
    return this.finalizeConfirmed(intent);
  }

  private markUnknown(intent: TradeIntent, why: string): { intent: TradeIntent } {
    this.store.update(() => {
      intent.status = 'unknown';
      intent.error = why;
    });
    this.audit.append('trade.unknown', { intentId: intent.id, signature: intent.signature, why });
    this.pause(`outcome of transaction ${intent.signature} is unknown — check it on an explorer and use Reconcile before resuming`, 'system');
    return { intent };
  }

  private async finalizeFailed(intent: TradeIntent, err: string): Promise<{ intent: TradeIntent }> {
    const tx = await this.conn.getTransaction(intent.signature!, { maxSupportedTransactionVersion: 0, commitment: 'confirmed' }).catch(() => null);
    const fee = tx?.meta?.fee ?? 0;
    this.store.update((s) => {
      intent.status = 'failed';
      intent.error = `failed on-chain: ${err}`;
      if (fee) s.lossEvents.push({ at: new Date().toISOString(), lamports: String(fee), mode: 'live', reason: 'failed transaction fee' });
    });
    this.audit.append('trade.failed', { intentId: intent.id, signature: intent.signature, error: err, feeLamports: fee });
    this.alerts.raise('warn', `Swap failed on-chain (${err}). Only the network fee (${fee} lamports) was spent.`);
    return { intent };
  }

  private async finalizeConfirmed(intent: TradeIntent): Promise<{ intent: TradeIntent; position?: Position }> {
    let tx = null;
    for (let i = 0; i < 5 && !tx; i++) {
      tx = await this.conn.getTransaction(intent.signature!, { maxSupportedTransactionVersion: 0, commitment: 'confirmed' }).catch(() => null);
      if (!tx) await new Promise((r) => setTimeout(r, 1500));
    }
    if (!tx?.meta) return this.markUnknown(intent, 'confirmed, but transaction details could not be read');
    if (tx.meta.err) return this.finalizeFailed(intent, JSON.stringify(tx.meta.err));

    const S = intent.summary;
    const user = intent.wallet!;
    const fee = BigInt(tx.meta.fee);
    const sigCount = BigInt(tx.transaction.signatures.length);
    const network = BASE_FEE * sigCount;
    const solDelta = BigInt(tx.meta.postBalances[0]!) - BigInt(tx.meta.preBalances[0]!);
    const bal = (arr: typeof tx.meta.preTokenBalances) => (arr ?? []).filter((b) => b.mint === S.mint && b.owner === user).reduce((a, b) => a + BigInt(b.uiTokenAmount.amount), 0n);
    const hadAccount = (tx.meta.preTokenBalances ?? []).some((b) => b.mint === S.mint && b.owner === user);
    const tokDelta = bal(tx.meta.postTokenBalances) - bal(tx.meta.preTokenBalances);

    const rent = S.side === 'buy' && !hadAccount ? ATA_RENT : 0n;
    const fill: Fill = {
      id: randomUUID(),
      side: S.side,
      at: new Date((tx.blockTime ?? Date.now() / 1000) * 1000).toISOString(),
      mode: 'live',
      reason: intent.reason,
      inputRaw: S.side === 'buy' ? (-solDelta - fee - rent).toString() : (-tokDelta).toString(),
      outputRaw: S.side === 'buy' ? tokDelta.toString() : (solDelta + fee).toString(),
      quotedOutRaw: S.estOutRaw,
      requestedInRaw: S.inAmountRaw,
      networkFeeLamports: network.toString(),
      priorityFeeLamports: (fee - network).toString(),
      platformFeeLamports: '0',
      rentLamports: rent.toString(),
      signature: intent.signature!,
      intentId: intent.id,
    };
    if (big(fill.inputRaw) <= 0n || big(fill.outputRaw) <= 0n) return this.markUnknown(intent, `confirmed, but balance changes were unexpected (in ${fill.inputRaw}, out ${fill.outputRaw})`);
    if (big(fill.outputRaw) < big(S.minOutRaw)) this.alerts.raise('warn', 'Received less than the quoted minimum; check the transaction.');
    const position = this.recordFill(intent, fill);
    return { intent, position };
  }

  async reconcile(id: string): Promise<TradeIntent> {
    const intent = this.s.intents.find((i) => i.id === id);
    if (!intent?.signature) throw new TradeRejected('Nothing to reconcile.');
    if (intent.status !== 'unknown' && intent.status !== 'submitted') throw new TradeRejected(`Trade is ${intent.status}.`);
    const st = await this.conn.getSignatureStatus(intent.signature, { searchTransactionHistory: true });
    if (!st.value) {
      const height = await this.conn.getBlockHeight('confirmed');
      if (intent.lastValidBlockHeight && height > intent.lastValidBlockHeight) {
        this.store.update(() => {
          intent.status = 'expired';
          intent.error = 'never landed (blockhash expired)';
        });
        this.audit.append('trade.reconciled', { intentId: id, result: 'expired' });
      }
      return intent;
    }
    if (st.value.err) await this.finalizeFailed(intent, JSON.stringify(st.value.err));
    else await this.finalizeConfirmed(intent);
    this.audit.append('trade.reconciled', { intentId: id, result: intent.status });
    return intent;
  }

  // ------------------------------------------------------------ monitor

  start(): void {
    const loop = async () => {
      await this.monitorTick().catch((e) => this.alerts.raise('error', `Monitor error: ${(e as Error).message}`));
      this.monitorTimer = setTimeout(loop, this.s.settings.monitorIntervalSec * 1000);
    };
    this.monitorTimer = setTimeout(loop, 2_000);
  }

  stop(): void {
    if (this.monitorTimer) clearTimeout(this.monitorTimer);
  }

  private async sellQuote(mint: string, amount: bigint): Promise<SellQuote & { summary: Quote['summary'] }> {
    const q = await this.provider.quote({ inputMint: mint, outputMint: WSOL_MINT, amountRaw: amount, slippageBps: this.s.settings.limits.maxSlippageBps });
    return { inRaw: amount, outLamports: big(q.summary.outAmount), priceImpactPct: q.summary.priceImpactPct, fetchedAt: q.fetchedAt, summary: q.summary };
  }

  async monitorTick(): Promise<void> {
    if (this.monitoring) return;
    this.monitoring = true;
    try {
      const open = this.s.positions.filter((p) => p.status === 'open');
      if (!open.length) return;
      let failures = 0;
      const estFee = BASE_FEE + this.estFees(this.s.mode).priority;
      for (const p of open) {
        let full: (SellQuote & { summary: Quote['summary'] }) | null = null;
        try {
          full = await this.sellQuote(p.mint, big(p.heldTokenRaw));
        } catch (e) {
          failures++;
          this.store.update((s) => (s.marks[p.id] = { ...(s.marks[p.id] ?? { netValueLamports: '0', priceImpactPct: 0, multipleOfEntry: null }), at: new Date().toISOString(), stale: true, error: (e as Error).message }));
          continue;
        }
        const ctxBase = { settings: this.s.settings, estSellFeeLamports: estFee };
        this.store.update((s) => {
          s.marks[p.id] = {
            at: full!.fetchedAt.toISOString(),
            netValueLamports: (full!.outLamports - estFee).toString(),
            priceImpactPct: full!.priceImpactPct,
            multipleOfEntry: multipleOfEntry(p, full!, ctxBase),
            stale: false,
          };
        });
        if (p.mode !== this.s.mode || this.s.paused) continue;
        if (this.s.intents.some((i) => i.positionId === p.id && (i.status === 'pending' || i.status === 'submitted' || i.status === 'unknown'))) continue;
        if (this.s.proposals.some((x) => x.positionId === p.id && x.status === 'open')) continue;

        const rem = remainingTakeProfitRaw(p);
        let tpQuote: SellQuote | null = null;
        if (rem > 0n && !p.locked) {
          try {
            tpQuote = rem === full.inRaw ? full : await this.sellQuote(p.mint, rem);
          } catch {
            failures++;
          }
        }
        const d = evaluateRules(p, { now: new Date(), mode: p.mode, settings: this.s.settings, estSellFeeLamports: estFee, takeProfitQuote: tpQuote, fullQuote: full });
        if (d.action !== 'sell') continue;
        await this.actOnRule(p, d.rule, d.amountRaw, d.requiresApproval, d.detail, estFee);
      }
      // Market data outages: stop trading after repeated failures.
      if (failures > 0 && failures >= open.length) this.monitorFailures++;
      else this.monitorFailures = 0;
      if (this.monitorFailures >= 3 && !this.s.paused) this.pause('market data unavailable for 3 consecutive monitor cycles', 'system');
    } finally {
      this.monitoring = false;
    }
  }

  /** Respect the price-impact limit by selling in smaller chunks (a partial fill of the rule's target). */
  private async actOnRule(p: Position, rule: Exclude<TradeReason, 'manual'>, amount: bigint, requiresApproval: boolean, detail: string, estFee: bigint): Promise<void> {
    const maxImpact = this.s.settings.limits.maxPriceImpactPct;
    let chunk = amount;
    let q = await this.sellQuote(p.mint, chunk);
    for (let i = 0; i < 4 && q.priceImpactPct > maxImpact; i++) {
      chunk = chunk / 2n;
      if (chunk === 0n) break;
      q = await this.sellQuote(p.mint, chunk);
    }
    if (chunk === 0n || q.priceImpactPct > maxImpact) {
      this.alerts.raise('warn', `${rule} triggered for ${p.symbol} but even a 1/16 chunk exceeds the ${maxImpact}% price-impact limit. Not trading.`);
      return;
    }
    if (rule === 'take_profit' && !takeProfitTriggered(p, q, { settings: this.s.settings, estSellFeeLamports: estFee })) return;
    const partial = chunk < amount ? ` Selling a ${chunk}/${amount} chunk to stay within the price-impact limit; the rest is retried next cycle.` : '';
    const ruleCfg = rule === 'stop_loss' ? this.s.settings.stopLoss : rule === 'time_exit' ? this.s.settings.timeExit : null;

    if (!requiresApproval && p.mode === 'paper') {
      try {
        const intent = await this.prepare(
          { side: 'sell', positionId: p.id, amountRaw: chunk.toString(), slippageBps: this.s.settings.limits.maxSlippageBps, reason: rule },
          { automated: true, ruleAppliesAfterTakeProfit: ruleCfg?.applyAfterTakeProfit ?? false },
        );
        this.confirmPaper(intent.id);
        this.audit.append('rule.executed', { positionId: p.id, rule, amountRaw: chunk.toString(), detail: detail + partial });
      } catch (e) {
        const v = e instanceof TradeRejected ? e.violations.join('; ') : '';
        this.alerts.raise('warn', `${rule} for ${p.symbol} could not execute: ${(e as Error).message} ${v}`);
      }
      return;
    }
    const proposal: Proposal = {
      id: randomUUID(),
      createdAt: new Date().toISOString(),
      positionId: p.id,
      rule,
      amountRaw: chunk.toString(),
      detail: detail + partial,
      autoExecute: !requiresApproval,
      status: 'open',
    };
    this.store.update((s) => s.proposals.push(proposal));
    this.audit.append('rule.proposed', proposal);
    this.alerts.raise('info', `${rule.replace('_', '-')} triggered for ${p.symbol}: ${proposal.detail} ${proposal.autoExecute ? 'Your wallet will ask you to sign when the dashboard is open.' : 'Review and approve it in the dashboard.'}`);
  }

  dismissProposal(id: string): void {
    this.store.update((s) => {
      const p = s.proposals.find((x) => x.id === id);
      if (p) p.status = 'dismissed';
    });
    this.audit.append('rule.dismissed', { proposalId: id });
  }

  // ------------------------------------------------------------ live mode

  bindWallet(wallet: string | null): void {
    if (this.s.mode === 'live') throw new TradeRejected('Disable live mode before changing the bound wallet.');
    if (wallet) new PublicKey(wallet);
    this.store.update((s) => (s.settings.boundWallet = wallet));
    this.audit.append('wallet.bound', { wallet });
  }

  async armLive(wallet: string, phrase: string): Promise<{ warnings: string[] }> {
    const problems: string[] = [];
    if (!env.liveTradingAllowed) problems.push('LIVE_TRADING_ALLOWED is not set to true in the server environment.');
    if (this.provider.mock) problems.push('MARKET_DATA=mock cannot be used for live trading.');
    if (this.s.paused) problems.push('Resume from the emergency pause first.');
    if (!this.s.settings.boundWallet) problems.push('Bind a wallet (ideally a low-balance test wallet) first.');
    else if (this.s.settings.boundWallet !== wallet) problems.push('Connected wallet is not the bound wallet.');
    if (phrase !== 'ENABLE LIVE TRADING') problems.push('Confirmation phrase does not match.');
    if (problems.length) throw new TradeRejected('Live mode not enabled.', problems);
    const warnings: string[] = [];
    const bal = await this.balance('live', wallet);
    if (bal === null) throw new TradeRejected('Could not read wallet balance (RPC problem). Live mode not enabled.');
    if (bal > BigInt(Math.round(this.s.settings.testWalletMaxBalanceSol * 1e9)))
      warnings.push(`This wallet holds ${(Number(bal) / 1e9).toFixed(3)} SOL, above your ${this.s.settings.testWalletMaxBalanceSol} SOL test-wallet threshold. Consider a separate low-balance wallet.`);
    if (/api\.mainnet-beta\.solana\.com/.test(env.rpcUrl)) warnings.push('Using the public RPC endpoint, which is rate-limited; a dedicated RPC provider is recommended.');
    this.store.update((s) => {
      s.mode = 'live';
      s.live = { armedAt: new Date().toISOString(), wallet };
    });
    this.audit.append('live.armed', { wallet, settings: this.s.settings, warnings });
    this.alerts.raise('warn', `LIVE trading enabled for wallet ${wallet}. Every trade still requires your wallet's signature.`);
    return { warnings };
  }

  disarmLive(): void {
    this.store.update((s) => {
      s.mode = 'paper';
      s.live = null;
      for (const i of s.intents) if (i.mode === 'live' && i.status === 'pending') i.status = 'cancelled';
    });
    this.audit.append('live.disarmed', {});
  }
}
