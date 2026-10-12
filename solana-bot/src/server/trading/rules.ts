// Pure exit-rule evaluation: the take-profit ladder (default 3× → recover the
// initial investment, 5× → sell 50% of the original amount, keep a moon bag),
// plus optional stop-loss and time exit that stay off until configured.
import type { ExitReason, Mode, Position, Settings, TakeProfitLevel } from '../../shared/types.js';
import { big, entryLamports, multipleToBps, pctToBps, planTakeProfit, recoverCostAmount, sellableRaw } from './accounting.js';

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
  /** Quote for the planned take-profit amount when it is known (see planTakeProfit). */
  takeProfitQuote: SellQuote | null;
  /** Quote for the full held amount (stop-loss, and sizing a recover-cost level). */
  fullQuote: SellQuote | null;
  /** Autopilot is running: exits execute without dashboard approval in live mode too. */
  autopilotActive?: boolean;
}

export type RuleDecision =
  | { action: 'none'; notes: string[] }
  | {
      action: 'sell';
      rule: ExitReason;
      amountRaw: bigint;
      /** False → execute without dashboard approval (paper, autopilot, or an explicitly enabled live automation). */
      requiresApproval: boolean;
      detail: string;
      notes: string[];
      /** Take-profit only: ladder level and its full target amount. */
      tpLevel?: number;
      tpTargetRaw?: bigint;
    };

const BPS = 10_000n;

function isStale(q: SellQuote, ctx: RuleContext): boolean {
  return ctx.now.getTime() - q.fetchedAt.getTime() > ctx.settings.quoteMaxAgeSec * 1000;
}

export function levelLabel(l: TakeProfitLevel): string {
  switch (l.sell.kind) {
    case 'recover_cost':
      return `${l.multiple}× → recover initial investment`;
    case 'percent_of_original':
      return `${l.multiple}× → sell ${l.sell.pct}% of original`;
    case 'all_except_moon_bag':
      return `${l.multiple}× → sell all but the moon bag`;
  }
}

/**
 * True when the quote's net proceeds per token are at least `multiple` × the
 * recorded entry price. Exact integer comparison:
 *   (out - fees) / q  >=  m × entryCost / original
 *   (out - fees) × original × 10000  >=  mBps × entryCost × q
 */
export function takeProfitTriggered(position: Position, q: SellQuote, ctx: Pick<RuleContext, 'settings' | 'estSellFeeLamports'>, multiple: number): boolean {
  if (q.inRaw <= 0n) return false;
  const net = q.outLamports - ctx.estSellFeeLamports;
  if (net <= 0n) return false;
  const entry = entryLamports(position, ctx.settings.takeProfit.entryPriceBasis);
  return net * big(position.originalTokenRaw) * BPS >= multipleToBps(multiple) * entry * q.inRaw;
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

function approvalNeeded(ctx: RuleContext, autoExecuteLive: boolean): boolean {
  return ctx.mode === 'live' ? !(autoExecuteLive || ctx.autopilotActive) : false;
}

/** Amount an exit rule sells: a share of the holding, never the moon bag unless allowed. */
function exitAmount(position: Position, pct: number, includeMoonBag: boolean): bigint {
  const held = big(position.heldTokenRaw);
  const amount = (held * pctToBps(pct)) / BPS;
  const cap = includeMoonBag ? held : sellableRaw(position);
  return amount < cap ? amount : cap;
}

export function evaluateRules(position: Position, ctx: RuleContext): RuleDecision {
  const notes: string[] = [];
  if (position.status !== 'open' || big(position.heldTokenRaw) === 0n) return { action: 'none', notes: ['position closed'] };
  const { takeProfit: tp, stopLoss: sl, timeExit: te } = ctx.settings;
  const held = big(position.heldTokenRaw);

  // 1. Take-profit ladder (levels in ascending order, one per evaluation).
  if (tp.enabled && !position.locked) {
    const plan = planTakeProfit(position);
    if (plan && plan.amountRaw === 0n) notes.push(`take-profit level ${plan.index + 1} has nothing to sell`);
    else if (plan) {
      const lvl = position.takeProfit.levels[plan.index]!;
      const label = levelLabel(lvl);
      if (plan.amountRaw === null) {
        // Recover-cost level: size it from the price of the whole holding.
        const q = ctx.fullQuote;
        if (!q) notes.push('take-profit: no quote available, not evaluated');
        else if (isStale(q, ctx)) notes.push('take-profit: quote is stale, not evaluated');
        else if (q.inRaw !== held) notes.push('take-profit: quote amount does not match holding, not evaluated');
        else if (takeProfitTriggered(position, q, ctx, lvl.multiple)) {
          const amount = recoverCostAmount(position, q, ctx.estSellFeeLamports, ctx.settings.limits.maxSlippageBps);
          if (amount > 0n) {
            const m = multipleOfEntry(position, q, ctx);
            return {
              action: 'sell',
              rule: 'take_profit',
              amountRaw: amount,
              tpLevel: plan.index,
              tpTargetRaw: amount,
              requiresApproval: approvalNeeded(ctx, tp.autoExecuteLive),
              detail: `Level ${plan.index + 1} (${label}): net price is ${m?.toFixed(2)}× entry. Selling ${amount} raw units to take the initial investment back out.`,
              notes,
            };
          }
        }
      } else {
        const q = ctx.takeProfitQuote;
        if (!q) notes.push('take-profit: no quote available, not evaluated');
        else if (isStale(q, ctx)) notes.push('take-profit: quote is stale, not evaluated');
        else if (q.inRaw !== plan.amountRaw) notes.push('take-profit: quote amount does not match the planned amount, not evaluated');
        else if (takeProfitTriggered(position, q, ctx, lvl.multiple)) {
          const m = multipleOfEntry(position, q, ctx);
          return {
            action: 'sell',
            rule: 'take_profit',
            amountRaw: plan.amountRaw,
            tpLevel: plan.index,
            tpTargetRaw: lvl.targetRaw !== null ? big(lvl.targetRaw) : plan.amountRaw,
            requiresApproval: approvalNeeded(ctx, tp.autoExecuteLive),
            detail: `Level ${plan.index + 1} (${label}): net price is ${m?.toFixed(2)}× entry. Selling ${plan.amountRaw} raw units.`,
            notes,
          };
        }
      }
    }
  }

  // After the ladder completes the position is locked: only rules the user
  // explicitly allowed to act after take-profit may run.
  const locked = position.locked;

  // 2. Stop-loss (disabled by default).
  if (sl.enabled && sl.dropPct !== null && (!locked || sl.applyAfterTakeProfit)) {
    const q = ctx.fullQuote;
    if (!q) notes.push('stop-loss: no quote available, not evaluated');
    else if (isStale(q, ctx)) notes.push('stop-loss: quote is stale, not evaluated');
    else if (q.inRaw !== held) notes.push('stop-loss: quote amount does not match holding, not evaluated');
    else if (stopLossTriggered(position, q, ctx)) {
      const amount = exitAmount(position, sl.sellFractionPct, sl.includeMoonBag);
      if (amount > 0n) {
        return {
          action: 'sell',
          rule: 'stop_loss',
          amountRaw: amount,
          requiresApproval: approvalNeeded(ctx, sl.autoExecuteLive),
          detail: `Net value is at least ${sl.dropPct}% below entry. Selling ${sl.sellFractionPct}% of the current holding${sl.includeMoonBag ? '' : ' (moon bag kept)'}.`,
          notes,
        };
      }
      notes.push('stop-loss triggered but only the moon bag is left');
    }
  }

  // 3. Time exit (disabled by default).
  if (te.enabled && te.maxHoldMinutes !== null && (!locked || te.applyAfterTakeProfit)) {
    const heldMin = (ctx.now.getTime() - new Date(position.openedAt).getTime()) / 60_000;
    if (heldMin >= te.maxHoldMinutes) {
      const amount = exitAmount(position, te.sellFractionPct, te.includeMoonBag);
      if (amount > 0n) {
        return {
          action: 'sell',
          rule: 'time_exit',
          amountRaw: amount,
          requiresApproval: approvalNeeded(ctx, te.autoExecuteLive),
          detail: `Held for ${heldMin.toFixed(0)} min (limit ${te.maxHoldMinutes}). Selling ${te.sellFractionPct}% of the current holding${te.includeMoonBag ? '' : ' (moon bag kept)'}.`,
          notes,
        };
      }
    }
  }

  if (locked) notes.push('take-profit ladder finished: position locked, no automatic trades');
  return { action: 'none', notes };
}
