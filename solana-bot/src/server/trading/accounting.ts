// Pure position accounting. All amounts are integers (lamports / token base
// units) handled as bigint; nothing here does I/O.
import type { Fill, Position, TakeProfitRule } from '../../shared/types.js';

const BPS = 10_000n;

export const big = (s: string | number | bigint): bigint => BigInt(s);

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

/** Create a position (lot) from a confirmed buy fill. */
export function openPosition(meta: OpenPositionMeta, buy: Fill, tp: Pick<TakeProfitRule, 'sellFractionPct'>): Position {
  if (buy.side !== 'buy') throw new Error('openPosition requires a buy fill');
  const received = big(buy.outputRaw);
  const swapped = big(buy.inputRaw);
  if (received <= 0n) throw new Error('buy fill received no tokens');
  if (swapped <= 0n) throw new Error('buy fill spent nothing');
  const fees = fillFeesLamports(buy);
  const cost = swapped + fees;
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
      targetRaw: ((received * pctToBps(tp.sellFractionPct)) / BPS).toString(),
      soldRaw: '0',
    },
    locked: false,
    fills: [buy],
  };
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
  const next: Position = {
    ...position,
    heldTokenRaw: (held - sold).toString(),
    costBasisLamports: (basis - allocated).toString(),
    realizedPnlLamports: (big(position.realizedPnlLamports) + realized).toString(),
    feesPaidLamports: (big(position.feesPaidLamports) + fees).toString(),
    rentDepositLamports: (big(position.rentDepositLamports) + big(sell.rentLamports)).toString(),
    takeProfit: { ...position.takeProfit },
    fills: [...position.fills, fill],
  };

  if (sell.reason === 'take_profit') {
    const tpSold = big(position.takeProfit.soldRaw) + sold;
    const target = big(position.takeProfit.targetRaw);
    next.takeProfit.soldRaw = tpSold.toString();
    next.takeProfit.triggeredAt ??= sell.at;
    // Done when the target is reached, or when nothing is left to sell.
    if (tpSold >= target || held - sold === 0n) {
      next.takeProfit.state = 'done';
      next.takeProfit.completedAt = sell.at;
      next.locked = true;
    } else {
      next.takeProfit.state = 'partial';
    }
  }

  if (held - sold === 0n) {
    next.status = 'closed';
    next.closedAt = sell.at;
  }
  return { position: next, fill };
}

/** Remaining token amount the take-profit rule may still sell for this position. */
export function remainingTakeProfitRaw(position: Position): bigint {
  if (position.takeProfit.state === 'done') return 0n;
  const remaining = big(position.takeProfit.targetRaw) - big(position.takeProfit.soldRaw);
  const held = big(position.heldTokenRaw);
  const r = remaining < held ? remaining : held;
  return r > 0n ? r : 0n;
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
