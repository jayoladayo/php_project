import { describe, expect, it } from 'vitest';
import { applySell, openPosition } from '../src/server/trading/accounting.js';
import { evaluateRules, multipleOfEntry, takeProfitTriggered, type RuleContext, type SellQuote } from '../src/server/trading/rules.js';
import { META, fill, settings } from './helpers.js';

const NOW = new Date('2026-10-10T12:10:00.000Z');
// Entry: 0.1 SOL swapped + 20,000 lamports fees → 1e9 raw tokens. Effective cost 100,020,000.
const buy = fill({ side: 'buy', inputRaw: '100000000', outputRaw: '1000000000', networkFeeLamports: '5000', priorityFeeLamports: '15000' });
const pos = openPosition(META, buy, { sellFractionPct: 50 });
const TARGET = 500_000_000n;

const quote = (inRaw: bigint, outLamports: bigint, fetchedAt = NOW, priceImpactPct = 0.5): SellQuote => ({ inRaw, outLamports, fetchedAt, priceImpactPct });
const ctx = (p: Partial<RuleContext> = {}): RuleContext => ({
  now: NOW,
  mode: 'paper',
  settings: settings(),
  estSellFeeLamports: 10_000n,
  takeProfitQuote: null,
  fullQuote: null,
  ...p,
});

// Exactly 5× effective entry for the 50% target: 5 × 100,020,000 / 2 = 250,050,000 net, + 10,000 fee.
const EXACT_5X_OUT = 250_050_000n + 10_000n;

describe('5× take-profit trigger', () => {
  it('does not trigger just below 5× net of fees', () => {
    expect(takeProfitTriggered(pos, quote(TARGET, EXACT_5X_OUT - 1n), ctx())).toBe(false);
    expect(evaluateRules(pos, ctx({ takeProfitQuote: quote(TARGET, EXACT_5X_OUT - 1n) })).action).toBe('none');
  });

  it('triggers at exactly 5× and sells 50% of the ORIGINAL amount', () => {
    const d = evaluateRules(pos, ctx({ takeProfitQuote: quote(TARGET, EXACT_5X_OUT) }));
    expect(d.action).toBe('sell');
    if (d.action !== 'sell') return;
    expect(d.rule).toBe('take_profit');
    expect(d.amountRaw).toBe(TARGET);
    expect(d.requiresApproval).toBe(false); // paper mode executes automatically
  });

  it('accounts for estimated sell fees: a gross 5× quote is not enough', () => {
    // Gross out of exactly 5× (no room for the 10,000 lamport fee)
    expect(takeProfitTriggered(pos, quote(TARGET, 250_050_000n), ctx())).toBe(false);
  });

  it('fill-price basis ignores entry fees', () => {
    const s = settings();
    s.takeProfit.entryPriceBasis = 'fill_price';
    // 5 × 100,000,000 / 2 = 250,000,000 net
    expect(takeProfitTriggered(pos, quote(TARGET, 250_010_000n), ctx({ settings: s }))).toBe(true);
    expect(takeProfitTriggered(pos, quote(TARGET, 250_009_999n), ctx({ settings: s }))).toBe(false);
  });

  it('respects a configured multiple and fraction', () => {
    const s = settings();
    s.takeProfit.multiple = 3;
    s.takeProfit.sellFractionPct = 25;
    const p = openPosition(META, buy, { sellFractionPct: 25 });
    const target = 250_000_000n;
    // 3 × 100,020,000 / 4 = 75,015,000 net
    const d = evaluateRules(p, ctx({ settings: s, takeProfitQuote: quote(target, 75_015_000n + 10_000n) }));
    expect(d.action === 'sell' && d.amountRaw).toBe(target);
  });

  it('requires dashboard approval in live mode unless automation is explicitly enabled', () => {
    const live = evaluateRules(pos, ctx({ mode: 'live', takeProfitQuote: quote(TARGET, EXACT_5X_OUT) }));
    expect(live.action === 'sell' && live.requiresApproval).toBe(true);
    const s = settings();
    s.takeProfit.autoExecuteLive = true;
    const auto = evaluateRules(pos, ctx({ mode: 'live', settings: s, takeProfitQuote: quote(TARGET, EXACT_5X_OUT) }));
    expect(auto.action === 'sell' && auto.requiresApproval).toBe(false);
  });

  it('ignores stale quotes', () => {
    const old = new Date(NOW.getTime() - 60_000);
    const d = evaluateRules(pos, ctx({ takeProfitQuote: quote(TARGET, EXACT_5X_OUT * 10n, old) }));
    expect(d.action).toBe('none');
    expect(d.notes.join()).toMatch(/stale/);
  });

  it('ignores a quote for the wrong amount', () => {
    const d = evaluateRules(pos, ctx({ takeProfitQuote: quote(TARGET + 1n, EXACT_5X_OUT * 2n) }));
    expect(d.action).toBe('none');
  });

  it('does nothing when disabled', () => {
    const s = settings();
    s.takeProfit.enabled = false;
    expect(evaluateRules(pos, ctx({ settings: s, takeProfitQuote: quote(TARGET, EXACT_5X_OUT * 2n) })).action).toBe('none');
  });

  it('after a partial fill only sells the remainder, and only while still ≥ 5×', () => {
    const { position: partial } = applySell(pos, fill({ side: 'sell', inputRaw: '200000000', outputRaw: '100100000', reason: 'take_profit' }));
    const rem = 300_000_000n;
    // 5× for 3/10 of original: 5 × 100,020,000 × 0.3 = 150,030,000 net
    const hit = evaluateRules(partial, ctx({ takeProfitQuote: quote(rem, 150_030_000n + 10_000n) }));
    expect(hit.action === 'sell' && hit.amountRaw).toBe(rem);
    const miss = evaluateRules(partial, ctx({ takeProfitQuote: quote(rem, 150_030_000n) }));
    expect(miss.action).toBe('none');
  });

  it('after the take-profit completes, the position is locked: no further automatic trades', () => {
    const { position: done } = applySell(pos, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '260000000', reason: 'take_profit' }));
    expect(done.locked).toBe(true);
    const s = settings();
    s.stopLoss = { enabled: true, dropPct: 50, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: false };
    s.timeExit = { enabled: true, maxHoldMinutes: 1, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: false };
    const d = evaluateRules(done, ctx({ settings: s, takeProfitQuote: quote(500_000_000n, 10n ** 12n), fullQuote: quote(500_000_000n, 1n) }));
    expect(d.action).toBe('none');
    expect(d.notes.join()).toMatch(/locked/);
  });

  it('a locked position can be acted on only by a rule the user configured to apply after take-profit', () => {
    const { position: done } = applySell(pos, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '260000000', reason: 'take_profit' }));
    const s = settings();
    s.timeExit = { enabled: true, maxHoldMinutes: 1, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: true };
    const d = evaluateRules(done, ctx({ settings: s }));
    expect(d.action === 'sell' && d.rule).toBe('time_exit');
  });

  it('reports the current multiple of entry', () => {
    expect(multipleOfEntry(pos, quote(TARGET, EXACT_5X_OUT), ctx())).toBeCloseTo(5, 5);
  });
});

describe('stop-loss and time exit are off by default', () => {
  it('does not fire on a crash or long hold unless configured', () => {
    const later = new Date(NOW.getTime() + 30 * 86_400_000);
    const d = evaluateRules(pos, ctx({ now: later, fullQuote: quote(1_000_000_000n, 1n, later) }));
    expect(d.action).toBe('none');
  });

  it('stop-loss fires when configured', () => {
    const s = settings();
    s.stopLoss = { enabled: true, dropPct: 50, sellFractionPct: 100, autoExecuteLive: false, applyAfterTakeProfit: false };
    // 50% of 100,020,000 = 50,010,000 net value
    expect(evaluateRules(pos, ctx({ settings: s, fullQuote: quote(1_000_000_000n, 50_020_000n) })).action).toBe('sell');
    expect(evaluateRules(pos, ctx({ settings: s, fullQuote: quote(1_000_000_000n, 50_020_001n) })).action).toBe('none');
  });

  it('time exit fires when configured', () => {
    const s = settings();
    s.timeExit = { enabled: true, maxHoldMinutes: 10, sellFractionPct: 50, autoExecuteLive: false, applyAfterTakeProfit: false };
    const d = evaluateRules(pos, ctx({ settings: s }));
    expect(d.action === 'sell' && d.amountRaw).toBe(500_000_000n);
  });
});
