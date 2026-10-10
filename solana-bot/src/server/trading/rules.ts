// Pure exit-rule evaluation: take-profit (default 5× → sell 50% of the
// original amount), plus optional stop-loss and time exit that stay off until
// configured.
import type { Mode, Position, Settings, TradeReason } from '../../shared/types.js';
import { big, entryLamports, multipleToBps, pctToBps, remainingTakeProfitRaw } from './accounting.js';

/** An executable sell quote for `inRaw` tokens of the position's mint. */
export interface SellQuote {
  inRaw: bigint;
  outLamports: bigint;
  priceImpactPct: number;
  fetchedAt: Date;
}

export interface RuleContext {
  now: Date;
  mode: Mode;
  settings: Settings;
  /** Estimated network + priority fee of a sell, deducted from quote proceeds. */
  estSellFeeLamports: bigint;
  /** Quote for the remaining take-profit amount (null if not available). */
  takeProfitQuote: SellQuote | null;
  /** Quote for the full held amount (used by stop-loss). */
  fullQuote: SellQuote | null;
}

export type RuleDecision =
  | { action: 'none'; notes: string[] }
  | {
      action: 'sell';
      rule: Exclude<TradeReason, 'manual'>;
      amountRaw: bigint;
      /** False → execute without dashboard approval (paper, or an explicitly enabled live automation). */
      requiresApproval: boolean;
      detail: string;
      notes: string[];
    };

const BPS = 10_000n;

function isStale(q: SellQuote, ctx: RuleContext): boolean {
  return ctx.now.getTime() - q.fetchedAt.getTime() > ctx.settings.quoteMaxAgeSec * 1000;
}

/**
 * True when the quote's net proceeds per token are at least `multiple` × the
 * recorded entry price. Exact integer comparison:
 *   (out - fees) / q  >=  m × entryCost / original
 *   (out - fees) × original × 10000  >=  mBps × entryCost × q
 */
export function takeProfitTriggered(position: Position, q: SellQuote, ctx: Pick<RuleContext, 'settings' | 'estSellFeeLamports'>): boolean {
  if (q.inRaw <= 0n) return false;
  const net = q.outLamports - ctx.estSellFeeLamports;
  if (net <= 0n) return false;
  const entry = entryLamports(position, ctx.settings.takeProfit.entryPriceBasis);
  const mBps = multipleToBps(ctx.settings.takeProfit.multiple);
  return net * big(position.originalTokenRaw) * BPS >= mBps * entry * q.inRaw;
}

/** Current net executable price as a multiple of entry (display only). */
export function multipleOfEntry(position: Position, q: SellQuote, ctx: Pick<RuleContext, 'settings' | 'estSellFeeLamports'>): number | null {
  if (q.inRaw <= 0n) return null;
  const entry = entryLamports(position, ctx.settings.takeProfit.entryPriceBasis);
  if (entry === 0n) return null;
  const net = q.outLamports - ctx.estSellFeeLamports;
  // scale to 1e6 precision before converting to float
  const scaled = (net * big(position.originalTokenRaw) * 1_000_000n) / (entry * q.inRaw);
  return Number(scaled) / 1_000_000;
}

function stopLossTriggered(position: Position, q: SellQuote, ctx: RuleContext): boolean {
  const dropPct = ctx.settings.stopLoss.dropPct;
  if (dropPct === null || q.inRaw <= 0n) return false;
  const net = q.outLamports - ctx.estSellFeeLamports;
  // value per token <= (1 - drop) × entry per token
  const entry = entryLamports(position, ctx.settings.takeProfit.entryPriceBasis);
  const keepBps = BPS - pctToBps(dropPct);
  return net * big(position.originalTokenRaw) * BPS <= keepBps * entry * q.inRaw;
}

function approvalNeeded(mode: Mode, autoExecuteLive: boolean): boolean {
  return mode === 'live' ? !autoExecuteLive : false;
}

export function evaluateRules(position: Position, ctx: RuleContext): RuleDecision {
  const notes: string[] = [];
  if (position.status !== 'open' || big(position.heldTokenRaw) === 0n) return { action: 'none', notes: ['position closed'] };
  const { takeProfit: tp, stopLoss: sl, timeExit: te } = ctx.settings;
  const held = big(position.heldTokenRaw);

  // 1. Take-profit (only while not yet completed).
  if (tp.enabled && !position.locked) {
    const remaining = remainingTakeProfitRaw(position);
    const q = ctx.takeProfitQuote;
    if (remaining > 0n) {
      if (!q) notes.push('take-profit: no quote available, not evaluated');
      else if (isStale(q, ctx)) notes.push('take-profit: quote is stale, not evaluated');
      else if (q.inRaw !== remaining) notes.push('take-profit: quote amount does not match remaining target, not evaluated');
      else if (takeProfitTriggered(position, q, ctx)) {
        const m = multipleOfEntry(position, q, ctx);
        return {
          action: 'sell',
          rule: 'take_profit',
          amountRaw: remaining,
          requiresApproval: approvalNeeded(ctx.mode, tp.autoExecuteLive),
          detail: `Net executable price is ${m?.toFixed(2)}× entry (rule: ${tp.multiple}×). Selling ${remaining} raw units = remaining part of ${tp.sellFractionPct}% of the original amount.`,
          notes,
        };
      }
    }
  }

  // After take-profit completes the position is locked: only rules the user
  // explicitly allowed to act after take-profit may run.
  const locked = position.locked;

  // 2. Stop-loss (disabled by default).
  if (sl.enabled && sl.dropPct !== null && (!locked || sl.applyAfterTakeProfit)) {
    const q = ctx.fullQuote;
    if (!q) notes.push('stop-loss: no quote available, not evaluated');
    else if (isStale(q, ctx)) notes.push('stop-loss: quote is stale, not evaluated');
    else if (q.inRaw !== held) notes.push('stop-loss: quote amount does not match holding, not evaluated');
    else if (stopLossTriggered(position, q, ctx)) {
      const amount = (held * pctToBps(sl.sellFractionPct)) / BPS;
      if (amount > 0n) {
        return {
          action: 'sell',
          rule: 'stop_loss',
          amountRaw: amount,
          requiresApproval: approvalNeeded(ctx.mode, sl.autoExecuteLive),
          detail: `Net value is at least ${sl.dropPct}% below entry. Selling ${sl.sellFractionPct}% of the current holding.`,
          notes,
        };
      }
    }
  }

  // 3. Time exit (disabled by default).
  if (te.enabled && te.maxHoldMinutes !== null && (!locked || te.applyAfterTakeProfit)) {
    const heldMin = (ctx.now.getTime() - new Date(position.openedAt).getTime()) / 60_000;
    if (heldMin >= te.maxHoldMinutes) {
      const amount = (held * pctToBps(te.sellFractionPct)) / BPS;
      if (amount > 0n) {
        return {
          action: 'sell',
          rule: 'time_exit',
          amountRaw: amount,
          requiresApproval: approvalNeeded(ctx.mode, te.autoExecuteLive),
          detail: `Held for ${heldMin.toFixed(0)} min (limit ${te.maxHoldMinutes}). Selling ${te.sellFractionPct}% of the current holding.`,
          notes,
        };
      }
    }
  }

  if (locked) notes.push('position locked after take-profit: no automatic trades');
  return { action: 'none', notes };
}
