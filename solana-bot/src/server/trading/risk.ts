// Pure pre-trade limit checks. Run before preparing every trade and again
// before confirming/submitting it.
import type { Mode, Position, ResearchReport, Settings, Side } from '../../shared/types.js';
import { big } from './accounting.js';

const SOL = 1_000_000_000;
const toLamports = (sol: number): bigint => BigInt(Math.round(sol * SOL));

export interface RiskInput {
  settings: Settings;
  mode: Mode;
  paused: boolean;
  liveArmed: boolean;
  side: Side;
  /** Buy: lamports in. Sell: token raw in. */
  amountRaw: bigint;
  requestedSlippageBps: number;
  quote: { priceImpactPct: number; fetchedAt: Date } | null;
  now: Date;
  /** Open positions in the same mode. */
  openPositions: Position[];
  /** Realized P&L today (UTC) in lamports, including failed-transaction fees (negative = loss). */
  realizedTodayLamports: bigint;
  /** Sum of unrealized losses on open positions from the latest marks (<= 0). */
  unrealizedLossLamports: bigint;
  /** Wallet (or paper) SOL balance, if known. */
  balanceLamports: bigint | null;
  /** Estimated fees + rent for this trade. */
  estCostsLamports: bigint;
  /** Buys: research report for the token. */
  research?: ResearchReport | null;
  /** User explicitly acknowledged trading a token that is not verified. */
  overrideUnverified?: boolean;
  /** Sells: the position, if any. */
  position?: Position | null;
  /** User explicitly approved trading a position that is locked after take-profit. */
  acknowledgeLocked?: boolean;
  /** Trade originates from an automatic rule rather than a click. */
  automated: boolean;
  /** Automated sells: the firing rule is explicitly allowed to act after take-profit. */
  ruleAppliesAfterTakeProfit?: boolean;
  wallet: string | null;
}

export interface RiskResult {
  ok: boolean;
  violations: string[];
  warnings: string[];
}

export function checkTrade(i: RiskInput): RiskResult {
  const v: string[] = [];
  const w: string[] = [];
  const L = i.settings.limits;

  if (i.paused) v.push('Trading is paused (emergency pause is on).');
  if (i.mode === 'live') {
    if (!i.liveArmed) v.push('Live trading has not been enabled.');
    if (!i.wallet) v.push('No wallet connected.');
    else if (i.settings.boundWallet && i.wallet !== i.settings.boundWallet) v.push('Connected wallet is not the wallet bound for live trading.');
    else if (!i.settings.boundWallet) v.push('No wallet is bound for live trading.');
  }

  if (!Number.isInteger(i.requestedSlippageBps) || i.requestedSlippageBps <= 0) v.push('Slippage must be a positive whole number of bps.');
  if (i.requestedSlippageBps > L.maxSlippageBps) v.push(`Slippage ${i.requestedSlippageBps} bps exceeds limit ${L.maxSlippageBps} bps.`);

  if (!i.quote) v.push('No quote available.');
  else {
    const ageSec = (i.now.getTime() - i.quote.fetchedAt.getTime()) / 1000;
    if (ageSec > i.settings.quoteMaxAgeSec) v.push(`Quote is stale (${ageSec.toFixed(0)}s old, limit ${i.settings.quoteMaxAgeSec}s).`);
    if (!Number.isFinite(i.quote.priceImpactPct)) v.push('Quote has no price impact estimate.');
    else if (i.quote.priceImpactPct > L.maxPriceImpactPct) v.push(`Price impact ${i.quote.priceImpactPct.toFixed(2)}% exceeds limit ${L.maxPriceImpactPct}%.`);
  }

  if (i.amountRaw <= 0n) v.push('Amount must be greater than zero.');

  if (i.side === 'buy') checkBuy(i, v, w);
  else checkSell(i, v, w);

  return { ok: v.length === 0, violations: v, warnings: w };
}

function checkBuy(i: RiskInput, v: string[], w: string[]): void {
  const L = i.settings.limits;
  const amount = i.amountRaw;

  // Research gate.
  const r = i.research;
  if (!r) v.push('No research report for this token.');
  else {
    const ageSec = (i.now.getTime() - new Date(r.generatedAt).getTime()) / 1000;
    if (ageSec > i.settings.verification.maxDataAgeSec) v.push('Research report is stale; refresh it before trading.');
    const critical = r.flags.filter((f) => f.severity === 'critical');
    if (critical.length) v.push(`Critical risk flags: ${critical.map((f) => f.code).join(', ')}.`);
    if (r.verdict !== 'verified') {
      if (i.automated) v.push('Token is not verified; automatic buys are never allowed for unverified tokens.');
      else if (!i.overrideUnverified) v.push('Token is not verified. Review the report and explicitly acknowledge the risk to continue.');
      else w.push('Proceeding on an unverified token by explicit user override.');
    }
    if (r.mock && i.mode === 'live') v.push('Mock data can never be used for live trades.');
  }
  // Automatic buys are not part of this bot: buys are always user-initiated.
  if (i.automated) v.push('Automatic buys are disabled.');

  if (amount > toLamports(L.maxBuySol)) v.push(`Buy of ${fmt(amount)} SOL exceeds max buy ${L.maxBuySol} SOL.`);

  const exposure = i.openPositions.reduce((s, p) => s + big(p.costBasisLamports), 0n);
  if (exposure + amount > toLamports(L.maxTotalExposureSol))
    v.push(`Exposure would be ${fmt(exposure + amount)} SOL, above limit ${L.maxTotalExposureSol} SOL.`);

  if (i.openPositions.length >= L.maxOpenPositions) v.push(`Already at max open positions (${L.maxOpenPositions}).`);

  const lossToday = lossOf(i.realizedTodayLamports) + lossOf(i.unrealizedLossLamports);
  const limit = toLamports(L.dailyLossLimitSol);
  if (lossToday >= limit) v.push(`Daily loss limit reached (${fmt(lossToday)} of ${L.dailyLossLimitSol} SOL).`);
  else if (L.capBuyByRemainingDailyLoss && amount > limit - lossToday)
    v.push(`Buy exceeds remaining daily loss budget (${fmt(limit - lossToday)} SOL).`);

  if (i.balanceLamports === null) v.push('Wallet balance unknown.');
  else {
    const needed = amount + i.estCostsLamports + toLamports(L.minSolReserve);
    if (i.balanceLamports < needed)
      v.push(`Insufficient SOL: need ${fmt(needed)} (amount + fees + ${L.minSolReserve} SOL reserve), have ${fmt(i.balanceLamports)}. Borrowed funds are never used.`);
  }

  const locked = i.openPositions.find((p) => r && p.mint === r.mint && p.locked);
  if (locked && !i.acknowledgeLocked) v.push('A position in this token is locked after take-profit; approve explicitly to buy more.');
}

function checkSell(i: RiskInput, v: string[], w: string[]): void {
  const p = i.position;
  if (!p) {
    v.push('Sells must reference an open position.');
    return;
  }
  if (p.status !== 'open') v.push('Position is closed.');
  if (p.mode !== i.mode) v.push(`Position belongs to ${p.mode} mode.`);
  if (i.amountRaw > big(p.heldTokenRaw)) v.push('Sell amount exceeds tokens held.');
  if (p.locked) {
    if (i.automated && i.ruleAppliesAfterTakeProfit) w.push('Locked position: rule is configured to apply after take-profit.');
    else if (i.automated) v.push('Position is locked after take-profit; automatic sells need a rule that explicitly applies after take-profit.');
    else if (!i.acknowledgeLocked) v.push('Position is locked after take-profit; approve this trade explicitly.');
    else w.push('Trading a locked position by explicit user approval.');
  }
  if (i.mode === 'live' && p.wallet && i.wallet !== p.wallet) v.push('Position was opened from a different wallet.');
}

const lossOf = (pnl: bigint): bigint => (pnl < 0n ? -pnl : 0n);
const fmt = (l: bigint): string => (Number(l) / SOL).toFixed(4);
