import { describe, expect, it } from 'vitest';
import {
  applySell,
  costStillToRecover,
  entryPrice,
  fillRatio,
  migratePosition,
  openPosition,
  planTakeProfit,
  realizedOnUtcDay,
  recoverCostAmount,
  sellableRaw,
  skipLevel,
  slippageBps,
  unrealizedLamports,
} from '../src/server/trading/accounting.js';
import type { Position, TakeProfitRule } from '../src/shared/types.js';
import { META, fill, settings } from './helpers.js';

// Buy 0.1 SOL (+ 20,000 lamports fees) → 1e9 raw tokens (1000 tokens at 6 decimals)
const buy = fill({ side: 'buy', inputRaw: '100000000', outputRaw: '1000000000', networkFeeLamports: '5000', priorityFeeLamports: '15000', rentLamports: '2039280' });
const LADDER = settings().takeProfit; // 3× recover cost, 5× 50% of original, 15% moon bag
const SINGLE: TakeProfitRule = { ...LADDER, levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }], moonBagPct: 0 };

describe('openPosition', () => {
  it('records cost basis including fees but excluding refundable rent', () => {
    const p = openPosition(META, buy, LADDER);
    expect(p.entrySwapLamports).toBe('100000000');
    expect(p.entryCostLamports).toBe('100020000');
    expect(p.costBasisLamports).toBe('100020000');
    expect(p.feesPaidLamports).toBe('20000');
    expect(p.rentDepositLamports).toBe('2039280');
    expect(p.originalTokenRaw).toBe('1000000000');
  });

  it('snapshots the take-profit ladder and moon bag', () => {
    const p = openPosition(META, buy, LADDER);
    expect(p.takeProfit.moonBagRaw).toBe('150000000');
    expect(p.takeProfit.levels.map((l) => [l.multiple, l.sell.kind, l.targetRaw, l.state])).toEqual([
      [3, 'recover_cost', null, 'pending'],
      [5, 'percent_of_original', '500000000', 'pending'],
    ]);
    expect(sellableRaw(p)).toBe(850_000_000n);
  });

  it('sorts levels by multiple', () => {
    const p = openPosition(META, buy, { ...LADDER, levels: [LADDER.levels[1]!, LADDER.levels[0]!] });
    expect(p.takeProfit.levels.map((l) => l.multiple)).toEqual([3, 5]);
  });

  it('has no ladder and no moon bag when take-profit is disabled', () => {
    const p = openPosition(META, buy, { ...LADDER, enabled: false });
    expect(p.takeProfit.levels).toEqual([]);
    expect(p.takeProfit.moonBagRaw).toBe('0');
    expect(planTakeProfit(p)).toBeNull();
  });

  it('computes entry price on both bases', () => {
    const p = openPosition(META, buy, LADDER);
    expect(entryPrice(p, 'fill_price')).toBeCloseTo(0.0001, 10); // 0.1 SOL / 1000 tokens
    expect(entryPrice(p, 'effective_incl_fees')).toBeCloseTo(0.00010002, 10);
  });

  it('rejects empty fills', () => {
    expect(() => openPosition(META, fill({ side: 'buy', inputRaw: '1', outputRaw: '0' }), LADDER)).toThrow();
    expect(() => openPosition(META, fill({ side: 'sell', inputRaw: '1', outputRaw: '1' }), LADDER)).toThrow();
  });

  it('floors percentage targets for odd amounts', () => {
    const p = openPosition(META, fill({ side: 'buy', inputRaw: '1000', outputRaw: '7' }), SINGLE);
    expect(p.takeProfit.levels[0]!.targetRaw).toBe('3');
  });
});

describe('applySell basics', () => {
  const p0 = openPosition(META, buy, SINGLE);

  it('allocates cost basis pro-rata and books realized P&L net of fees', () => {
    const { position, fill: f } = applySell(p0, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '500000000', networkFeeLamports: '5000', reason: 'take_profit', tpLevel: 0 }));
    // allocated = 100,020,000 / 2 = 50,010,000; proceeds = 499,995,000
    expect(f.realizedPnlLamports).toBe(String(499_995_000 - 50_010_000));
    expect(position.costBasisLamports).toBe('50010000');
    expect(position.heldTokenRaw).toBe('500000000');
    expect(position.feesPaidLamports).toBe('25000');
    expect(position.takeProfit.state).toBe('done');
    expect(position.locked).toBe(true);
    expect(position.status).toBe('open');
  });

  it('closes the position and releases all basis when everything is sold', () => {
    const third = applySell(p0, fill({ side: 'sell', inputRaw: '333333333', outputRaw: '30000000' })).position;
    const { position } = applySell(third, fill({ side: 'sell', inputRaw: '666666667', outputRaw: '60000000' }));
    expect(position.status).toBe('closed');
    expect(position.costBasisLamports).toBe('0');
    expect(position.heldTokenRaw).toBe('0');
    expect(position.realizedPnlLamports).toBe(String(90_000_000 - 100_020_000)); // no dust lost
  });

  it('refuses to oversell', () => {
    expect(() => applySell(p0, fill({ side: 'sell', inputRaw: '1000000001', outputRaw: '1' }))).toThrow(/exceeds held/);
  });

  it('computes unrealized P&L from net value', () => {
    expect(unrealizedLamports(p0, 120_000_000n)).toBe(120_000_000n - 100_020_000n);
  });
});

describe('take-profit ladder', () => {
  const p0 = openPosition(META, buy, LADDER);

  it('a recover-cost level is sized from the price, so it has no fixed amount until it triggers', () => {
    expect(planTakeProfit(p0)).toEqual({ index: 0, amountRaw: null });
    expect(costStillToRecover(p0)).toBe(100_020_000n);
  });

  it('recoverCostAmount sells enough to get the whole entry cost back after fees', () => {
    // Whole holding (1e9) quotes 300,070,000 lamports (≈3×). Fee 10,000.
    const q = { inRaw: 1_000_000_000n, outLamports: 300_070_000n };
    const amount = recoverCostAmount(p0, q, 10_000n);
    expect(amount).toBeGreaterThan(333_000_000n);
    expect(amount).toBeLessThan(334_000_000n);
    // At the same per-token price, proceeds after the fee cover the entry cost.
    expect((amount * q.outLamports) / q.inRaw - 10_000n).toBeGreaterThanOrEqual(100_020_000n);
    // ...and one token less would not.
    expect(((amount - 1n) * q.outLamports) / q.inRaw - 10_000n).toBeLessThan(100_020_000n);
  });

  it('runs 3× recover → 5× 50% → moon bag, then locks', () => {
    // Level 1: recover cost
    let { position: p } = applySell(p0, fill({ side: 'sell', inputRaw: '333400000', outputRaw: '100100000', networkFeeLamports: '10000', reason: 'take_profit', tpLevel: 0, tpTargetRaw: '333400000' }));
    expect(p.takeProfit.levels[0]).toMatchObject({ state: 'done', targetRaw: '333400000', soldRaw: '333400000' });
    expect(p.takeProfit.state).toBe('partial');
    expect(p.locked).toBe(false);
    expect(costStillToRecover(p)).toBe(0n);
    // Level 2: 50% of original
    expect(planTakeProfit(p)).toEqual({ index: 1, amountRaw: 500_000_000n });
    ({ position: p } = applySell(p, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '250100000', reason: 'take_profit', tpLevel: 1 })));
    expect(p.takeProfit.levels[1]!.state).toBe('done');
    expect(p.takeProfit.state).toBe('done');
    expect(p.locked).toBe(true);
    expect(p.heldTokenRaw).toBe('166600000'); // ≥ 150,000,000 moon bag
    expect(planTakeProfit(p)).toBeNull();
  });

  it('never sells into the moon bag: targets are capped at what is sellable', () => {
    const greedy = openPosition(META, buy, { ...LADDER, levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 85 } }], moonBagPct: 30 });
    expect(planTakeProfit(greedy)).toEqual({ index: 0, amountRaw: 700_000_000n });
  });

  it('an all-except-moon-bag level sells down to exactly the moon bag', () => {
    const p = openPosition(META, buy, { ...LADDER, levels: [{ multiple: 4, sell: { kind: 'all_except_moon_bag' } }], moonBagPct: 20 });
    expect(planTakeProfit(p)).toEqual({ index: 0, amountRaw: 800_000_000n });
    const { position } = applySell(p, fill({ side: 'sell', inputRaw: '800000000', outputRaw: '1', reason: 'take_profit', tpLevel: 0, tpTargetRaw: '800000000' }));
    expect(position.heldTokenRaw).toBe('200000000');
    expect(position.takeProfit.state).toBe('done');
    expect(position.locked).toBe(true);
  });

  it('tracks partial fills per level and keeps the same target', () => {
    let { position: p } = applySell(p0, fill({ side: 'sell', inputRaw: '150000000', outputRaw: '45000000', reason: 'take_profit', tpLevel: 0, tpTargetRaw: '333400000' }));
    expect(p.takeProfit.levels[0]).toMatchObject({ state: 'partial', soldRaw: '150000000', targetRaw: '333400000' });
    expect(planTakeProfit(p)).toEqual({ index: 0, amountRaw: 183_400_000n });
    ({ position: p } = applySell(p, fill({ side: 'sell', inputRaw: '183400000', outputRaw: '55000000', reason: 'take_profit', tpLevel: 0 })));
    expect(p.takeProfit.levels[0]!.state).toBe('done');
  });

  it('manual sells do not count toward a level, but do count toward recovered cost', () => {
    const { position } = applySell(p0, fill({ side: 'sell', inputRaw: '100000000', outputRaw: '60000000', networkFeeLamports: '5000' }));
    expect(position.takeProfit.levels[0]!.soldRaw).toBe('0');
    expect(costStillToRecover(position)).toBe(100_020_000n - 59_995_000n);
  });

  it('a recover-cost level with nothing left to recover plans 0 and can be skipped', () => {
    const { position } = applySell(p0, fill({ side: 'sell', inputRaw: '100000000', outputRaw: '200000000' }));
    expect(planTakeProfit(position)).toEqual({ index: 0, amountRaw: 0n });
    const skipped = skipLevel(position, 0, '2026-10-10T13:00:00.000Z');
    expect(skipped.takeProfit.levels[0]!.state).toBe('skipped');
    expect(planTakeProfit(skipped)).toEqual({ index: 1, amountRaw: 500_000_000n });
  });

  it('when only the moon bag is left, remaining levels are skipped and the position locks', () => {
    const { position } = applySell(p0, fill({ side: 'sell', inputRaw: '850000000', outputRaw: '1' }));
    expect(position.takeProfit.levels.map((l) => l.state)).toEqual(['skipped', 'skipped']);
    expect(position.locked).toBe(true);
  });
});

describe('migratePosition', () => {
  it('converts a single-target position from the previous version into a one-level ladder', () => {
    const legacy = {
      ...openPosition(META, buy, SINGLE),
      takeProfit: { state: 'partial', targetRaw: '500000000', soldRaw: '200000000', triggeredAt: '2026-10-10T12:05:00.000Z' },
    } as unknown as Position;
    const p = migratePosition(legacy, 5);
    expect(p.takeProfit.levels).toEqual([
      { multiple: 5, sell: { kind: 'percent_of_original', pct: 50 }, state: 'partial', targetRaw: '500000000', soldRaw: '200000000', triggeredAt: '2026-10-10T12:05:00.000Z' },
    ]);
    expect(p.takeProfit.moonBagRaw).toBe('0');
    expect(planTakeProfit(p)).toEqual({ index: 0, amountRaw: 300_000_000n });
    expect(migratePosition(p, 5)).toBe(p); // idempotent
  });
});

describe('fill diagnostics', () => {
  it('measures slippage against the quote and fill ratio for partial fills', () => {
    const f = fill({ side: 'sell', inputRaw: '400', requestedInRaw: '500', outputRaw: '9900', quotedOutRaw: '10000' });
    expect(slippageBps(f)).toBe(100);
    expect(fillRatio(f)).toBe(0.8);
  });

  it('sums realized P&L for a UTC day', () => {
    const p = openPosition(META, buy, SINGLE);
    const { position } = applySell(p, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '40000000', at: '2026-10-10T23:59:00.000Z' }));
    expect(realizedOnUtcDay([position], new Date('2026-10-10T05:00:00Z'))).toBe(40_000_000n - 50_010_000n);
    expect(realizedOnUtcDay([position], new Date('2026-10-11T05:00:00Z'))).toBe(0n);
  });
});
