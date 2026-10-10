import { describe, expect, it } from 'vitest';
import {
  applySell,
  entryPrice,
  fillRatio,
  openPosition,
  realizedOnUtcDay,
  remainingTakeProfitRaw,
  slippageBps,
  unrealizedLamports,
} from '../src/server/trading/accounting.js';
import { META, fill } from './helpers.js';

// Buy 0.1 SOL → 1,000,000 tokens (6 decimals = 1.0 token... use 1e9 raw = 1000 tokens)
const buy = fill({ side: 'buy', inputRaw: '100000000', outputRaw: '1000000000', networkFeeLamports: '5000', priorityFeeLamports: '15000', rentLamports: '2039280' });

describe('openPosition', () => {
  it('records cost basis including fees but excluding refundable rent', () => {
    const p = openPosition(META, buy, { sellFractionPct: 50 });
    expect(p.entrySwapLamports).toBe('100000000');
    expect(p.entryCostLamports).toBe('100020000');
    expect(p.costBasisLamports).toBe('100020000');
    expect(p.feesPaidLamports).toBe('20000');
    expect(p.rentDepositLamports).toBe('2039280');
    expect(p.originalTokenRaw).toBe('1000000000');
    expect(p.takeProfit).toEqual({ state: 'pending', targetRaw: '500000000', soldRaw: '0' });
  });

  it('computes entry price on both bases', () => {
    const p = openPosition(META, buy, { sellFractionPct: 50 });
    expect(entryPrice(p, 'fill_price')).toBeCloseTo(0.0001, 10); // 0.1 SOL / 1000 tokens
    expect(entryPrice(p, 'effective_incl_fees')).toBeCloseTo(0.00010002, 10);
  });

  it('rejects empty fills', () => {
    expect(() => openPosition(META, fill({ side: 'buy', inputRaw: '1', outputRaw: '0' }), { sellFractionPct: 50 })).toThrow();
    expect(() => openPosition(META, fill({ side: 'sell', inputRaw: '1', outputRaw: '1' }), { sellFractionPct: 50 })).toThrow();
  });

  it('floors the take-profit target for odd amounts', () => {
    const p = openPosition(META, fill({ side: 'buy', inputRaw: '1000', outputRaw: '7' }), { sellFractionPct: 50 });
    expect(p.takeProfit.targetRaw).toBe('3');
  });
});

describe('applySell', () => {
  const p0 = openPosition(META, buy, { sellFractionPct: 50 });

  it('allocates cost basis pro-rata and books realized P&L net of fees', () => {
    // Sell half for 0.5 SOL, 5000 lamports network fee
    const { position, fill: f } = applySell(p0, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '500000000', networkFeeLamports: '5000', reason: 'take_profit' }));
    // allocated = 100,020,000 / 2 = 50,010,000; proceeds = 499,995,000
    expect(f.realizedPnlLamports).toBe(String(499_995_000 - 50_010_000));
    expect(position.costBasisLamports).toBe('50010000');
    expect(position.heldTokenRaw).toBe('500000000');
    expect(position.feesPaidLamports).toBe('25000');
    expect(position.takeProfit.state).toBe('done');
    expect(position.locked).toBe(true);
    expect(position.status).toBe('open');
  });

  it('tracks a partial take-profit fill and keeps the position unlocked', () => {
    const { position } = applySell(p0, fill({ side: 'sell', inputRaw: '200000000', outputRaw: '200000000', reason: 'take_profit' }));
    expect(position.takeProfit.state).toBe('partial');
    expect(position.takeProfit.soldRaw).toBe('200000000');
    expect(position.locked).toBe(false);
    expect(remainingTakeProfitRaw(position)).toBe(300_000_000n);
  });

  it('completes the take-profit across chunks then locks', () => {
    let { position } = applySell(p0, fill({ side: 'sell', inputRaw: '200000000', outputRaw: '200000000', reason: 'take_profit' }));
    ({ position } = applySell(position, fill({ side: 'sell', inputRaw: '300000000', outputRaw: '300000000', reason: 'take_profit' })));
    expect(position.takeProfit.state).toBe('done');
    expect(position.locked).toBe(true);
    expect(remainingTakeProfitRaw(position)).toBe(0n);
  });

  it('manual sells do not count toward the take-profit target', () => {
    const { position } = applySell(p0, fill({ side: 'sell', inputRaw: '100000000', outputRaw: '10000000' }));
    expect(position.takeProfit.soldRaw).toBe('0');
    expect(position.takeProfit.state).toBe('pending');
    expect(remainingTakeProfitRaw(position)).toBe(500_000_000n);
  });

  it('caps remaining take-profit at what is still held', () => {
    const { position } = applySell(p0, fill({ side: 'sell', inputRaw: '800000000', outputRaw: '1' }));
    expect(remainingTakeProfitRaw(position)).toBe(200_000_000n);
  });

  it('closes the position and releases all basis when everything is sold', () => {
    const third = applySell(p0, fill({ side: 'sell', inputRaw: '333333333', outputRaw: '30000000' })).position;
    const { position } = applySell(third, fill({ side: 'sell', inputRaw: '666666667', outputRaw: '60000000' }));
    expect(position.status).toBe('closed');
    expect(position.costBasisLamports).toBe('0');
    expect(position.heldTokenRaw).toBe('0');
    // total realized = 90,000,000 - 100,020,000 exactly (no dust lost)
    expect(position.realizedPnlLamports).toBe(String(90_000_000 - 100_020_000));
  });

  it('refuses to oversell', () => {
    expect(() => applySell(p0, fill({ side: 'sell', inputRaw: '1000000001', outputRaw: '1' }))).toThrow(/exceeds held/);
  });

  it('computes unrealized P&L from net value', () => {
    expect(unrealizedLamports(p0, 120_000_000n)).toBe(120_000_000n - 100_020_000n);
  });
});

describe('fill diagnostics', () => {
  it('measures slippage against the quote and fill ratio for partial fills', () => {
    const f = fill({ side: 'sell', inputRaw: '400', requestedInRaw: '500', outputRaw: '9900', quotedOutRaw: '10000' });
    expect(slippageBps(f)).toBe(100);
    expect(fillRatio(f)).toBe(0.8);
  });

  it('sums realized P&L for a UTC day', () => {
    const p = openPosition(META, buy, { sellFractionPct: 50 });
    const { position } = applySell(p, fill({ side: 'sell', inputRaw: '500000000', outputRaw: '40000000', at: '2026-10-10T23:59:00.000Z' }));
    expect(realizedOnUtcDay([position], new Date('2026-10-10T05:00:00Z'))).toBe(40_000_000n - 50_010_000n);
    expect(realizedOnUtcDay([position], new Date('2026-10-11T05:00:00Z'))).toBe(0n);
  });
});
