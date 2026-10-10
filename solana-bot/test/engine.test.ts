// End-to-end paper trading against the offline mock market.
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import type { Connection } from '@solana/web3.js';
import { beforeEach, describe, expect, it } from 'vitest';
import { Alerts } from '../src/server/alerts.js';
import { AuditLog } from '../src/server/audit.js';
import { MockProvider } from '../src/server/market/mock.js';
import { Researcher } from '../src/server/research/index.js';
import { Store } from '../src/server/store.js';
import { Engine, TradeRejected, parseSol } from '../src/server/trading/engine.js';

const GOOD = 'MockGoodToken111111111111111111111111111111';
const RUG = 'MockRugToken1111111111111111111111111111111';
const noConn = {} as Connection; // paper mode never touches the RPC

let store: Store;
let engine: Engine;
let provider: MockProvider;
let audit: AuditLog;

beforeEach(() => {
  const dir = mkdtempSync(join(tmpdir(), 'engine-'));
  store = new Store(join(dir, 'state.json'));
  audit = new AuditLog(join(dir, 'audit.jsonl'));
  provider = new MockProvider();
  engine = new Engine(store, audit, new Alerts(store), provider, new Researcher(noConn, provider, () => store.state.settings), noConn);
});

const buy = async (amountSol = '0.05', overrideUnverified = true) => {
  const i = await engine.prepare({ side: 'buy', mint: GOOD, amountSol, slippageBps: 100, overrideUnverified });
  return engine.confirmPaper(i.id);
};

describe('paper engine', () => {
  it('parses SOL amounts exactly', () => {
    expect(parseSol('0.05')).toBe(50_000_000n);
    expect(parseSol('1.000000001')).toBe(1_000_000_001n);
    expect(() => parseSol('1e-3')).toThrow();
  });

  it('blocks unverified tokens without an explicit override and honeypots even with one', async () => {
    await expect(buy('0.05', false)).rejects.toThrow(TradeRejected);
    await engine.research(RUG);
    await expect(engine.prepare({ side: 'buy', mint: RUG, amountSol: '0.01', slippageBps: 100, overrideUnverified: true })).rejects.toMatchObject({
      violations: expect.arrayContaining([expect.stringMatching(/Critical risk flags: .*freeze_authority_active/)]),
    });
  });

  it('runs the 5× rule: sells 50% of the original amount once, then locks', async () => {
    const pos = await buy();
    expect(store.state.positions).toHaveLength(1);
    await engine.monitorTick();
    expect(store.state.positions[0]!.fills).toHaveLength(1); // nothing at 1×

    provider.movePrice(GOOD, 4.5);
    await engine.monitorTick();
    expect(store.state.positions[0]!.fills).toHaveLength(1); // still below 5× net of fees

    provider.movePrice(GOOD, 1.25); // ≈5.6×
    await engine.monitorTick();
    const p = store.state.positions[0]!;
    expect(p.takeProfit.state).toBe('done');
    expect(p.locked).toBe(true);
    expect(p.takeProfit.soldRaw).toBe(pos.takeProfit.targetRaw);
    expect(BigInt(p.heldTokenRaw)).toBe(BigInt(pos.originalTokenRaw) - BigInt(pos.takeProfit.targetRaw));
    expect(BigInt(p.realizedPnlLamports)).toBeGreaterThan(0n);

    provider.movePrice(GOOD, 3);
    await engine.monitorTick();
    expect(store.state.positions[0]!.fills).toHaveLength(2); // locked: no more automatic trades
    expect(audit.verify()).toBeNull();
  });

  it('splits the take-profit into chunks when the full sale would exceed the price-impact limit (partial fill)', async () => {
    const t = provider.token(GOOD)!;
    t.solReserve = 20n * 1_000_000_000n; // thin pool
    t.tokenReserve = 2_000_000n * 1_000_000n;
    store.state.settings.limits.maxPriceImpactPct = 5;
    await buy('0.05');
    provider.movePrice(GOOD, 6);
    store.state.settings.limits.maxPriceImpactPct = 0.3;
    await engine.monitorTick();
    let p = store.state.positions[0]!;
    expect(p.takeProfit.state).toBe('partial');
    expect(p.locked).toBe(false);
    const sold1 = BigInt(p.takeProfit.soldRaw);
    expect(sold1).toBeGreaterThan(0n);
    expect(sold1).toBeLessThan(BigInt(p.takeProfit.targetRaw));

    store.state.settings.limits.maxPriceImpactPct = 5;
    await engine.monitorTick();
    p = store.state.positions[0]!;
    expect(p.takeProfit.state).toBe('done');
    expect(p.takeProfit.soldRaw).toBe(p.takeProfit.targetRaw);
  });

  it('enforces total exposure (cost basis incl. fees) across trades', async () => {
    await buy('0.05');
    await buy('0.05');
    await buy('0.05');
    // 3 × (0.05 + fees) + 0.05 > 0.2 SOL limit
    await expect(buy('0.05')).rejects.toMatchObject({ violations: expect.arrayContaining([expect.stringMatching(/Exposure/)]) });
  });

  it('emergency pause cancels pending trades and blocks new ones', async () => {
    const i = await engine.prepare({ side: 'buy', mint: GOOD, amountSol: '0.01', slippageBps: 100, overrideUnverified: true });
    engine.pause('test', 'user');
    expect(store.state.intents.find((x) => x.id === i.id)?.status).toBe('cancelled');
    expect(() => engine.confirmPaper(i.id)).toThrow(/cancelled/);
    await expect(buy('0.01')).rejects.toThrow(/paused/);
    // rules do not trade while paused
    engine.resume();
    await buy('0.01');
    engine.pause('test', 'user');
    provider.movePrice(GOOD, 10);
    await engine.monitorTick();
    expect(store.state.positions[0]!.fills).toHaveLength(1);
  });

  it('rejects expired quotes', async () => {
    const i = await engine.prepare({ side: 'buy', mint: GOOD, amountSol: '0.01', slippageBps: 100, overrideUnverified: true });
    store.state.intents.find((x) => x.id === i.id)!.expiresAt = new Date(Date.now() - 1000).toISOString();
    expect(() => engine.confirmPaper(i.id)).toThrow(/expired/);
  });

  it('live mode cannot be armed with mock data or without the server flag', async () => {
    engine.bindWallet('11111111111111111111111111111111');
    await expect(engine.armLive('11111111111111111111111111111111', 'ENABLE LIVE TRADING')).rejects.toMatchObject({
      violations: expect.arrayContaining([expect.stringMatching(/mock/), expect.stringMatching(/LIVE_TRADING_ALLOWED/)]),
    });
  });
});
