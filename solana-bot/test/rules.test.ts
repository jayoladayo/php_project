import { describe, expect, it } from 'vitest';
import { applySell, openPosition } from '../src/server/trading/accounting.js';
import { evaluateRules, multipleOfEntry, takeProfitTriggered, type RuleContext, type SellQuote } from '../src/server/trading/rules.js';
import type { Settings, TakeProfitRule } from '../src/shared/types.js';
import { META, fill, settings } from './helpers.js';

const NOW = new Date('2026-10-10T12:10:00.000Z');
// Entry: 0.1 SOL swapped + 20,000 lamports fees → 1e9 raw tokens. Effective cost 100,020,000.
const buy = fill({ side: 'buy', inputRaw: '100000000', outputRaw: '1000000000', networkFeeLamports: '5000', priorityFeeLamports: '15000' });
const FEE = 10_000n;
const ENTRY = 100_020_000n;
const HELD = 1_000_000_000n;

const quote = (inRaw: bigint, outLamports: bigint, fetchedAt = NOW, priceImpactPct = 0.5): SellQuote => ({ inRaw, outLamports, fetchedAt, priceImpactPct });
const ctx = (p: Partial<RuleContext> = {}): RuleContext => ({
  now: NOW,
  mode: 'paper',
  settings: settings(),
  estSellFeeLamports: FEE,
  takeProfitQuote: null,
  fullQuote: null,
  ...p,
});
/** Gross quote output for `inRaw` tokens at exactly `m`× the effective entry price, net of the fee. */
const atMultiple = (m: bigint, inRaw: bigint) => (m * ENTRY * inRaw) / HELD + FEE;

const withTp = (tp: Partial<TakeProfitRule>): Settings => {
  const s = settings();
  s.takeProfit = { ...s.takeProfit, ...tp };
  return s;
};

describe('ladder level 1: 3× → recover the initial investment', () => {
  const pos = openPosition(META, buy, settings().takeProfit);

  it('does not trigger just below 3× (net of fees)', () => {
    expect(evaluateRules(pos, ctx({ fullQuote: quote(HELD, atMultiple(3n, HELD) - 1n) })).action).toBe('none');
  });

  it('triggers at exactly 3× and sells about a third — enough to recover cost even at the worst allowed slippage', () => {
    const out = atMultiple(3n, HELD);
    const d = evaluateRules(pos, ctx({ fullQuote: quote(HELD, out) }));
    expect(d.action).toBe('sell');
    if (d.action !== 'sell') return;
    expect(d.rule).toBe('take_profit');
    expect(d.tpLevel).toBe(0);
    expect(d.tpTargetRaw).toBe(d.amountRaw);
    // ≈ 1/3 ÷ (1 − 1% slippage tolerance)
    expect(d.amountRaw).toBeGreaterThan(336_000_000n);
    expect(d.amountRaw).toBeLessThan(337_500_000n);
    const worstFill = (d.amountRaw * out * 9_900n) / (HELD * 10_000n);
    expect(worstFill - FEE).toBeGreaterThanOrEqual(ENTRY);
    expect(d.requiresApproval).toBe(false); // paper executes automatically
  });

  it('sells fewer tokens when the price is already higher (6×)', () => {
    const d = evaluateRules(pos, ctx({ fullQuote: quote(HELD, atMultiple(6n, HELD)) }));
    expect(d.action === 'sell' && d.amountRaw < 170_000_000n && d.amountRaw > 167_000_000n).toBe(true);
  });

  it('needs a fresh quote for the whole holding', () => {
    expect(evaluateRules(pos, ctx({ fullQuote: quote(HELD, atMultiple(9n, HELD), new Date(NOW.getTime() - 60_000)) })).notes.join()).toMatch(/stale/);
    expect(evaluateRules(pos, ctx({ fullQuote: quote(HELD - 1n, atMultiple(9n, HELD)) })).action).toBe('none');
    expect(evaluateRules(pos, ctx()).notes.join()).toMatch(/no quote/);
  });
});

describe('ladder level 2: 5× → sell 50% of the original, keep the moon bag', () => {
  const pos = openPosition(META, buy, settings().takeProfit);
  const afterL1 = applySell(pos, fill({ side: 'sell', inputRaw: '333400000', outputRaw: '100100000', networkFeeLamports: '10000', reason: 'take_profit', tpLevel: 0, tpTargetRaw: '333400000' })).position;
  const L2 = 500_000_000n;

  it('is not evaluated before level 1 finished', () => {
    // A 5× price fires level 1 first (one level per evaluation).
    const d = evaluateRules(pos, ctx({ fullQuote: quote(HELD, atMultiple(5n, HELD)), takeProfitQuote: quote(L2, atMultiple(5n, L2)) }));
    expect(d.action === 'sell' && d.tpLevel).toBe(0);
  });

  it('triggers at exactly 5× for exactly 50% of the original amount', () => {
    expect(evaluateRules(afterL1, ctx({ takeProfitQuote: quote(L2, atMultiple(5n, L2) - 1n) })).action).toBe('none');
    const d = evaluateRules(afterL1, ctx({ takeProfitQuote: quote(L2, atMultiple(5n, L2)) }));
    expect(d.action === 'sell' && [d.tpLevel, d.amountRaw, d.tpTargetRaw]).toEqual([1, L2, L2]);
  });

  it('after the last level the position is locked and only the moon bag rides', () => {
    const done = applySell(afterL1, fill({ side: 'sell', inputRaw: L2.toString(), outputRaw: '250100000', reason: 'take_profit', tpLevel: 1 })).position;
    expect(done.locked).toBe(true);
    const s = settings();
    s.stopLoss = { enabled: true, dropPct: 50, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: false, includeMoonBag: true };
    s.timeExit = { enabled: true, maxHoldMinutes: 1, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: false, includeMoonBag: true };
    const held = BigInt(done.heldTokenRaw);
    const d = evaluateRules(done, ctx({ settings: s, fullQuote: quote(held, 1n), takeProfitQuote: quote(held, 10n ** 12n) }));
    expect(d.action).toBe('none');
    expect(d.notes.join()).toMatch(/locked/);
  });
});

describe('ladder options', () => {
  it('a single 5× / 50% level behaves like the original rule', () => {
    const s = withTp({ levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }], moonBagPct: 0 });
    const pos = openPosition(META, buy, s.takeProfit);
    const half = 500_000_000n;
    expect(takeProfitTriggered(pos, quote(half, atMultiple(5n, half)), ctx({ settings: s }), 5)).toBe(true);
    expect(takeProfitTriggered(pos, quote(half, atMultiple(5n, half) - 1n), ctx({ settings: s }), 5)).toBe(false);
    // gross 5× without room for the fee is not enough
    expect(takeProfitTriggered(pos, quote(half, atMultiple(5n, half) - FEE), ctx({ settings: s }), 5)).toBe(false);
  });

  it('fill-price basis ignores entry fees', () => {
    const s = withTp({ entryPriceBasis: 'fill_price' });
    const pos = openPosition(META, buy, s.takeProfit);
    const half = 500_000_000n;
    // 5 × 100,000,000 / 2 = 250,000,000 net
    expect(takeProfitTriggered(pos, quote(half, 250_010_000n), ctx({ settings: s }), 5)).toBe(true);
    expect(takeProfitTriggered(pos, quote(half, 250_009_999n), ctx({ settings: s }), 5)).toBe(false);
  });

  it('requires dashboard approval in live mode unless automation or the autopilot is on', () => {
    const pos = openPosition(META, buy, settings().takeProfit);
    const q = quote(HELD, atMultiple(3n, HELD));
    const live = evaluateRules(pos, ctx({ mode: 'live', fullQuote: q }));
    expect(live.action === 'sell' && live.requiresApproval).toBe(true);
    const auto = evaluateRules(pos, ctx({ mode: 'live', fullQuote: q, settings: withTp({ autoExecuteLive: true }) }));
    expect(auto.action === 'sell' && auto.requiresApproval).toBe(false);
    const pilot = evaluateRules(pos, ctx({ mode: 'live', fullQuote: q, autopilotActive: true }));
    expect(pilot.action === 'sell' && pilot.requiresApproval).toBe(false);
  });

  it('does nothing when take-profit is disabled', () => {
    const pos = openPosition(META, buy, settings().takeProfit);
    expect(evaluateRules(pos, ctx({ settings: withTp({ enabled: false }), fullQuote: quote(HELD, atMultiple(50n, HELD)) })).action).toBe('none');
  });

  it('continues a partially filled level with the remaining amount, only while still triggered', () => {
    const pos = openPosition(META, buy, settings().takeProfit);
    const partial = applySell(pos, fill({ side: 'sell', inputRaw: '150000000', outputRaw: '45000000', reason: 'take_profit', tpLevel: 0, tpTargetRaw: '333400000' })).position;
    const rem = 183_400_000n;
    const hit = evaluateRules(partial, ctx({ takeProfitQuote: quote(rem, atMultiple(3n, rem)) }));
    expect(hit.action === 'sell' && [hit.amountRaw, hit.tpTargetRaw]).toEqual([rem, 333_400_000n]);
    expect(evaluateRules(partial, ctx({ takeProfitQuote: quote(rem, atMultiple(3n, rem) - 1n) })).action).toBe('none');
  });

  it('reports the current multiple of entry', () => {
    const pos = openPosition(META, buy, settings().takeProfit);
    expect(multipleOfEntry(pos, quote(HELD, atMultiple(5n, HELD)), ctx())).toBeCloseTo(5, 5);
  });
});

describe('stop-loss and time exit', () => {
  const pos = openPosition(META, buy, settings().takeProfit); // 15% moon bag = 150,000,000

  it('are off by default', () => {
    const later = new Date(NOW.getTime() + 30 * 86_400_000);
    expect(evaluateRules(pos, ctx({ now: later, fullQuote: quote(HELD, 1n, later) })).action).toBe('none');
  });

  it('stop-loss fires when configured and keeps the moon bag by default', () => {
    const s = settings();
    s.stopLoss = { enabled: true, dropPct: 50, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: false, includeMoonBag: false };
    // 50% of 100,020,000 = 50,010,000 net value
    const d = evaluateRules(pos, ctx({ settings: s, fullQuote: quote(HELD, 50_020_000n) }));
    expect(d.action === 'sell' && [d.rule, d.amountRaw]).toEqual(['stop_loss', 850_000_000n]);
    expect(evaluateRules(pos, ctx({ settings: s, fullQuote: quote(HELD, 50_020_001n) })).action).toBe('none');
    s.stopLoss.includeMoonBag = true;
    const all = evaluateRules(pos, ctx({ settings: s, fullQuote: quote(HELD, 50_020_000n) }));
    expect(all.action === 'sell' && all.amountRaw).toBe(HELD);
  });

  it('time exit fires when configured', () => {
    const s = settings();
    s.timeExit = { enabled: true, maxHoldMinutes: 10, sellFractionPct: 50, autoExecuteLive: false, applyAfterTakeProfit: false, includeMoonBag: false };
    const d = evaluateRules(pos, ctx({ settings: s }));
    expect(d.action === 'sell' && d.amountRaw).toBe(500_000_000n);
  });

  it('a locked position can be acted on only by a rule configured to apply after take-profit', () => {
    const s1 = withTp({ levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }], moonBagPct: 0 });
    const p = openPosition(META, buy, s1.takeProfit);
    const done = applySell(p, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '260000000', reason: 'take_profit', tpLevel: 0 })).position;
    s1.timeExit = { enabled: true, maxHoldMinutes: 1, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: true, includeMoonBag: false };
    const d = evaluateRules(done, ctx({ settings: s1 }));
    expect(d.action === 'sell' && d.rule).toBe('time_exit');
  });
});
