// Pure position accounting. All amounts are integers (lamports / token base
// units) handled as bigint; nothing here does I/O.
import type { Fill, Position, TakeProfitLevelState, TakeProfitRule, TakeProfitState } from '../../shared/types.js';

const BPS = 10_000n;

export const big = (s: string | number | bigint): bigint => BigInt(s);
const min = (a: bigint, b: bigint): bigint => (a < b ? a : b);
const max0 = (a: bigint): bigint => (a > 0n ? a : 0n);

export function pctToBps(pct: number): bigint {
  if (!Number.isFinite(pct) || pct < 0 || pct > 100) throw new Error(`invalid percent ${pct}`);
  return BigInt(Math.round(pct * 100));
}

export function multipleToBps(multiple: number): bigint {
  if (!Number.isFinite(multiple) || multiple <= 0) throw new Error(`invalid multiple ${multiple}`);
  return BigInt(Math.round(multiple * 10_000));
}

export function fillFeesLamports(f: Pick<Fill, 'networkFeeLamports' | 'priorityFeeLamports' | 'platformFeeLamports'>): bigint {
  return big(f.networkFeeLamports) + big(f.priorityFeeLamports) + big(f.platformFeeLamports);
}

export interface OpenPositionMeta {
  id: string;
  mint: string;
  symbol: string;
  decimals: number;
  wallet: string | null;
}

/** pending: nothing sold · partial: some levels finished · done: every level finished or skipped. */
export function ladderState(levels: TakeProfitLevelState[]): TakeProfitState['state'] {
  if (levels.length === 0) return 'pending';
  if (levels.every((l) => l.state === 'done' || l.state === 'skipped')) return 'done';
  if (levels.some((l) => l.state !== 'pending')) return 'partial';
  return 'pending';
}

/** Create a position (lot) from a confirmed buy fill, snapshotting the take-profit ladder. */
export function openPosition(meta: OpenPositionMeta, buy: Fill, tp: Pick<TakeProfitRule, 'enabled' | 'levels' | 'moonBagPct'>): Position {
  if (buy.side !== 'buy') throw new Error('openPosition requires a buy fill');
  const received = big(buy.outputRaw);
  const swapped = big(buy.inputRaw);
  if (received <= 0n) throw new Error('buy fill received no tokens');
  if (swapped <= 0n) throw new Error('buy fill spent nothing');
  const fees = fillFeesLamports(buy);
  const cost = swapped + fees;
  const levels: TakeProfitLevelState[] = (tp.enabled ? [...tp.levels] : [])
    .sort((a, b) => a.multiple - b.multiple)
    .map((l) => ({
      multiple: l.multiple,
      sell: { ...l.sell },
      state: 'pending',
      targetRaw: l.sell.kind === 'percent_of_original' ? ((received * pctToBps(l.sell.pct)) / BPS).toString() : null,
      soldRaw: '0',
    }));
  return {
    id: meta.id,
    mint: meta.mint,
    symbol: meta.symbol,
    decimals: meta.decimals,
    mode: buy.mode,
    wallet: meta.wallet,
    openedAt: buy.at,
    status: 'open',
    originalTokenRaw: received.toString(),
    heldTokenRaw: received.toString(),
    entrySwapLamports: swapped.toString(),
    entryCostLamports: cost.toString(),
    costBasisLamports: cost.toString(),
    realizedPnlLamports: '0',
    feesPaidLamports: fees.toString(),
    rentDepositLamports: big(buy.rentLamports).toString(),
    takeProfit: {
      state: 'pending',
      levels,
      moonBagRaw: tp.enabled ? ((received * pctToBps(tp.moonBagPct)) / BPS).toString() : '0',
      soldRaw: '0',
    },
    locked: false,
    fills: [buy],
  };
}

/** Tokens automatic rules may sell: everything held except the moon bag. */
export function sellableRaw(position: Position): bigint {
  return max0(big(position.heldTokenRaw) - big(position.takeProfit.moonBagRaw));
}

/** SOL returned so far by sells of this position, net of their fees. */
export function proceedsLamports(position: Position): bigint {
  return position.fills.filter((f) => f.side === 'sell').reduce((a, f) => a + big(f.outputRaw) - fillFeesLamports(f), 0n);
}

/** SOL still needed to have recovered everything put into the position (entry incl. fees). */
export function costStillToRecover(position: Position): bigint {
  return max0(big(position.entryCostLamports) - proceedsLamports(position));
}

/** Index of the next ladder level to work on, or -1. */
export function nextLevelIndex(position: Position): number {
  return position.takeProfit.levels.findIndex((l) => l.state === 'pending' || l.state === 'partial');
}

export interface TakeProfitPlan {
  index: number;
  /**
   * Tokens to sell for this level. null = depends on the price (a recover-cost
   * level that has not triggered yet; see recoverCostAmount). 0 = nothing to
   * sell, the level should be skipped.
   */
  amountRaw: bigint | null;
}

export function planTakeProfit(position: Position): TakeProfitPlan | null {
  if (position.locked || position.status !== 'open') return null;
  const index = nextLevelIndex(position);
  if (index < 0) return null;
  const lvl = position.takeProfit.levels[index]!;
  const sellable = sellableRaw(position);
  if (sellable === 0n) return { index, amountRaw: 0n };
  if (lvl.targetRaw !== null) return { index, amountRaw: min(max0(big(lvl.targetRaw) - big(lvl.soldRaw)), sellable) };
  if (lvl.sell.kind === 'all_except_moon_bag') return { index, amountRaw: sellable };
  // recover_cost before its first sell
  return { index, amountRaw: costStillToRecover(position) === 0n ? 0n : null };
}

/**
 * Tokens to sell so that proceeds (after the estimated fee) cover the cost
 * still to recover even if the fill comes in at the worst price the slippage
 * tolerance allows. Priced from a quote for a LARGER amount (the whole
 * holding); smaller sells have less price impact, so this errs on the side of
 * recovering at least the cost. Capped at what may be sold.
 *   q × out × (1 − slip) / in − fee ≥ needed   ⇒   q = ceil((needed + fee) × in × 10⁴ / (out × (10⁴ − slipBps)))
 */
export function recoverCostAmount(position: Position, q: { inRaw: bigint; outLamports: bigint }, estFeeLamports: bigint, slippageBps = 0): bigint {
  const needed = costStillToRecover(position);
  if (needed === 0n || q.outLamports <= 0n || q.inRaw <= 0n) return 0n;
  const den = q.outLamports * (BPS - BigInt(slippageBps));
  if (den <= 0n) return 0n;
  const num = (needed + estFeeLamports) * q.inRaw * BPS;
  const amount = (num + den - 1n) / den;
  return min(amount, sellableRaw(position));
}

/** Mark levels that can no longer sell anything as skipped/done and lock the position when the ladder is finished. */
function settleLadder(position: Position, at: string): Position {
  const tp = position.takeProfit;
  const levels = tp.levels.map((l) => ({ ...l }));
  if (sellableRaw(position) === 0n) {
    for (const l of levels) {
      if (l.state === 'pending' || l.state === 'partial') {
        l.state = big(l.soldRaw) > 0n ? 'done' : 'skipped';
        l.completedAt = at;
      }
    }
  }
  const state = ladderState(levels);
  return { ...position, takeProfit: { ...tp, levels, state }, locked: position.locked || (levels.length > 0 && state === 'done') };
}

/** Skip a ladder level that has nothing to sell (e.g. cost already recovered). */
export function skipLevel(position: Position, index: number, at: string): Position {
  const levels = position.takeProfit.levels.map((l, i) => (i === index && (l.state === 'pending' || l.state === 'partial') ? { ...l, state: big(l.soldRaw) > 0n ? ('done' as const) : ('skipped' as const), completedAt: at } : l));
  return settleLadder({ ...position, takeProfit: { ...position.takeProfit, levels } }, at);
}

/**
 * Apply a confirmed sell fill. Cost basis is allocated pro-rata to the tokens
 * sold (average cost); selling everything releases all remaining basis so no
 * rounding dust is left behind. Returns a new Position and the fill with its
 * realized P&L filled in.
 */
export function applySell(position: Position, sell: Fill): { position: Position; fill: Fill } {
  if (sell.side !== 'sell') throw new Error('applySell requires a sell fill');
  if (position.status !== 'open') throw new Error('position is closed');
  const held = big(position.heldTokenRaw);
  const sold = big(sell.inputRaw);
  if (sold <= 0n) throw new Error('sell fill sold nothing');
  if (sold > held) throw new Error(`sell of ${sold} exceeds held ${held}`);

  const basis = big(position.costBasisLamports);
  const allocated = sold === held ? basis : (basis * sold) / held;
  const fees = fillFeesLamports(sell);
  const proceeds = big(sell.outputRaw) - fees;
  const realized = proceeds - allocated;

  const fill: Fill = { ...sell, realizedPnlLamports: realized.toString() };
  let next: Position = {
    ...position,
    heldTokenRaw: (held - sold).toString(),
    costBasisLamports: (basis - allocated).toString(),
    realizedPnlLamports: (big(position.realizedPnlLamports) + realized).toString(),
    feesPaidLamports: (big(position.feesPaidLamports) + fees).toString(),
    rentDepositLamports: (big(position.rentDepositLamports) + big(sell.rentLamports)).toString(),
    takeProfit: { ...position.takeProfit, levels: position.takeProfit.levels.map((l) => ({ ...l })) },
    fills: [...position.fills, fill],
  };

  if (sell.reason === 'take_profit' && next.takeProfit.levels.length) {
    const idx = sell.tpLevel !== undefined && next.takeProfit.levels[sell.tpLevel] ? sell.tpLevel : nextLevelIndex(position);
    const lvl = next.takeProfit.levels[idx];
    if (lvl) {
      if (lvl.targetRaw === null) lvl.targetRaw = sell.tpTargetRaw ?? sold.toString();
      lvl.soldRaw = (big(lvl.soldRaw) + sold).toString();
      lvl.triggeredAt ??= sell.at;
      if (big(lvl.soldRaw) >= big(lvl.targetRaw)) {
        lvl.state = 'done';
        lvl.completedAt = sell.at;
      } else lvl.state = 'partial';
    }
    next.takeProfit.soldRaw = (big(next.takeProfit.soldRaw) + sold).toString();
  }
  next = settleLadder(next, sell.at);

  if (held - sold === 0n) {
    next.status = 'closed';
    next.closedAt = sell.at;
  }
  return { position: next, fill };
}

/** Entry cost in lamports for the chosen basis. */
export function entryLamports(position: Position, basis: TakeProfitRule['entryPriceBasis']): bigint {
  return basis === 'fill_price' ? big(position.entrySwapLamports) : big(position.entryCostLamports);
}

/** SOL per whole token, for display only (floating point). */
export function priceSolPerToken(lamports: bigint, tokenRaw: bigint, decimals: number): number {
  if (tokenRaw === 0n) return 0;
  return Number(lamports) / 1e9 / (Number(tokenRaw) / 10 ** decimals);
}

export function entryPrice(position: Position, basis: TakeProfitRule['entryPriceBasis']): number {
  return priceSolPerToken(entryLamports(position, basis), big(position.originalTokenRaw), position.decimals);
}

/** Unrealized P&L given the net value (after estimated sell fees) of the held tokens. */
export function unrealizedLamports(position: Position, netValueLamports: bigint): bigint {
  if (position.status !== 'open') return 0n;
  return netValueLamports - big(position.costBasisLamports);
}

/** How much worse than quoted a fill was, in bps (positive = worse). */
export function slippageBps(fill: Pick<Fill, 'quotedOutRaw' | 'outputRaw'>): number {
  const q = big(fill.quotedOutRaw);
  if (q === 0n) return 0;
  return Number(((q - big(fill.outputRaw)) * BPS) / q);
}

/** Fraction of the requested input that was actually swapped (1 = complete). */
export function fillRatio(fill: Pick<Fill, 'inputRaw' | 'requestedInRaw'>): number {
  const req = big(fill.requestedInRaw);
  if (req === 0n) return 1;
  return Number((big(fill.inputRaw) * BPS) / req) / 10_000;
}

/** Realized P&L across all positions for fills whose timestamp falls on the given UTC day. */
export function realizedOnUtcDay(positions: Position[], day: Date): bigint {
  const key = day.toISOString().slice(0, 10);
  let total = 0n;
  for (const p of positions) {
    for (const f of p.fills) {
      if (f.realizedPnlLamports && f.at.slice(0, 10) === key) total += big(f.realizedPnlLamports);
    }
  }
  return total;
}

/**
 * Convert a position saved by the single-rule version (one target, no ladder)
 * to the ladder format.
 */
export function migratePosition(p: Position, legacyMultiple: number): Position {
  const tp = p.takeProfit as unknown as Partial<TakeProfitState> & { targetRaw?: string; soldRaw?: string; state?: TakeProfitState['state']; triggeredAt?: string; completedAt?: string };
  if (Array.isArray(tp?.levels)) return p;
  const original = big(p.originalTokenRaw);
  const target = big(tp?.targetRaw ?? '0');
  const pct = original > 0n ? Number((target * 10_000n) / original) / 100 : 50;
  const level: TakeProfitLevelState = {
    multiple: legacyMultiple,
    sell: { kind: 'percent_of_original', pct },
    state: tp?.state ?? 'pending',
    targetRaw: target.toString(),
    soldRaw: tp?.soldRaw ?? '0',
    ...(tp?.triggeredAt ? { triggeredAt: tp.triggeredAt } : {}),
    ...(tp?.completedAt ? { completedAt: tp.completedAt } : {}),
  };
  return { ...p, takeProfit: { state: tp?.state ?? 'pending', levels: [level], moonBagRaw: '0', soldRaw: tp?.soldRaw ?? '0' } };
}
