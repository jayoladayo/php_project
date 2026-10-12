// End-to-end paper trading against the offline mock market: manual trades,
// the take-profit ladder with a moon bag, partial fills, limits, pause, and
// the autopilot.
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import type { Connection } from '@solana/web3.js';
import { beforeEach, describe, expect, it } from 'vitest';
import { Alerts } from '../src/server/alerts.js';
import { AuditLog } from '../src/server/audit.js';
import { MockProvider } from '../src/server/market/mock.js';
import { Researcher } from '../src/server/research/index.js';
import { MockScanner } from '../src/server/research/scanner.js';
import { Store } from '../src/server/store.js';
import { costStillToRecover } from '../src/server/trading/accounting.js';
import { Autopilot } from '../src/server/trading/autopilot.js';
import { Engine, TradeRejected, parseSol } from '../src/server/trading/engine.js';
import { BotWallet } from '../src/server/wallet/botWallet.js';

const GOOD = 'MockGoodToken111111111111111111111111111111';
const MOON = 'MockMoonToken111111111111111111111111111111';
const FADE = 'MockFadeToken111111111111111111111111111111';
const WHALE = 'MockWhaLeToken11111111111111111111111111111';
const RUG = 'MockRugToken1111111111111111111111111111111';
const noConn = {} as Connection; // paper mode never touches the RPC

let store: Store;
let engine: Engine;
let provider: MockProvider;
let audit: AuditLog;
let autopilot: Autopilot;

beforeEach(() => {
  const dir = mkdtempSync(join(tmpdir(), 'engine-'));
  store = new Store(join(dir, 'state.json'));
  audit = new AuditLog(join(dir, 'audit.jsonl'));
  provider = new MockProvider();
  const alerts = new Alerts(store);
  const botWallet = new BotWallet(join(dir, 'bot-wallet.json'));
  engine = new Engine(store, audit, alerts, provider, new Researcher(noConn, provider, () => store.state.settings), noConn, botWallet);
  autopilot = new Autopilot(store, audit, alerts, engine, new MockScanner(provider), botWallet);
});

const buy = async (mint = GOOD, amountSol = '0.05', overrideUnverified = false) => {
  const i = await engine.prepare({ side: 'buy', mint, amountSol, slippageBps: 100, overrideUnverified });
  return engine.confirmPaper(i.id);
};
const pos = () => store.state.positions[0]!;

describe('paper engine', () => {
  it('parses SOL amounts exactly', () => {
    expect(parseSol('0.05')).toBe(50_000_000n);
    expect(parseSol('1.000000001')).toBe(1_000_000_001n);
    expect(() => parseSol('1e-3')).toThrow();
  });

  it('buys verified tokens; unverified ones need an explicit override; honeypots are always blocked', async () => {
    await buy(GOOD);
    await expect(buy(WHALE)).rejects.toThrow(TradeRejected);
    await buy(WHALE, '0.01', true);
    await engine.research(RUG);
    await expect(buy(RUG, '0.01', true)).rejects.toMatchObject({
      violations: expect.arrayContaining([expect.stringMatching(/Critical risk flags: .*freeze_authority_active/)]),
    });
  });

  it('runs the ladder: 3× recovers the investment, 5× sells half of the original, the moon bag rides', async () => {
    const opened = await buy(GOOD);
    expect(opened.takeProfit.levels.map((l) => l.state)).toEqual(['pending', 'pending']);
    await engine.monitorTick();
    expect(pos().fills).toHaveLength(1); // nothing at 1×

    provider.movePrice(GOOD, 2.9);
    await engine.monitorTick();
    expect(pos().fills).toHaveLength(1); // below 3× net of fees

    provider.movePrice(GOOD, 3.2 / 2.9);
    await engine.monitorTick();
    let p = pos();
    expect(p.fills).toHaveLength(2);
    expect(p.takeProfit.levels[0]!.state).toBe('done');
    expect(costStillToRecover(p)).toBe(0n); // the initial investment is back out
    expect(p.locked).toBe(false);
    const soldL1 = BigInt(p.takeProfit.levels[0]!.soldRaw);
    expect(soldL1 * 100n / BigInt(p.originalTokenRaw)).toBeGreaterThanOrEqual(30n); // ≈ 1/3.2 of the bag
    expect(soldL1 * 100n / BigInt(p.originalTokenRaw)).toBeLessThanOrEqual(33n);

    provider.movePrice(GOOD, 5.5 / 3.2);
    await engine.monitorTick();
    p = pos();
    expect(p.takeProfit.levels[1]).toMatchObject({ state: 'done', soldRaw: p.takeProfit.levels[1]!.targetRaw });
    expect(p.takeProfit.state).toBe('done');
    expect(p.locked).toBe(true);
    expect(BigInt(p.heldTokenRaw)).toBeGreaterThanOrEqual(BigInt(p.takeProfit.moonBagRaw));
    expect(BigInt(p.realizedPnlLamports)).toBeGreaterThan(0n);

    provider.movePrice(GOOD, 3);
    await engine.monitorTick();
    expect(pos().fills).toHaveLength(3); // locked: the moon bag is never sold automatically
    expect(store.state.alerts.map((a) => a.message).join('\n')).toMatch(/Take-profit level 2 .* moon bag/);
    expect(audit.verify()).toBeNull();
  });

  it('splits a level into chunks when the full sale would exceed the price-impact limit (partial fill)', async () => {
    const t = provider.token(GOOD)!;
    t.solReserve = 2n * 1_000_000_000n; // thin pool
    t.tokenReserve = 200_000n * 1_000_000n;
    store.state.settings.limits.maxPriceImpactPct = 5;
    store.state.settings.takeProfit.levels = [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }];
    await buy(GOOD, '0.05', true);
    provider.movePrice(GOOD, 6);
    store.state.settings.limits.maxPriceImpactPct = 1;
    await engine.monitorTick();
    let lvl = pos().takeProfit.levels[0]!;
    expect(lvl.state).toBe('partial');
    expect(BigInt(lvl.soldRaw)).toBeGreaterThan(0n);
    expect(BigInt(lvl.soldRaw)).toBeLessThan(BigInt(lvl.targetRaw!));
    expect(pos().locked).toBe(false);

    store.state.settings.limits.maxPriceImpactPct = 5;
    await engine.monitorTick();
    lvl = pos().takeProfit.levels[0]!;
    expect(lvl.state).toBe('done');
    expect(lvl.soldRaw).toBe(lvl.targetRaw);
  });

  it('enforces total exposure (cost basis incl. fees) across trades', async () => {
    await buy(GOOD);
    await buy(MOON);
    await buy(FADE);
    // 3 × (0.05 + fees) + 0.05 > 0.2 SOL limit
    await expect(buy(GOOD)).rejects.toMatchObject({ violations: expect.arrayContaining([expect.stringMatching(/Exposure/)]) });
  });

  it('emergency pause cancels pending trades, blocks new ones, and stops rules', async () => {
    const i = await engine.prepare({ side: 'buy', mint: GOOD, amountSol: '0.01', slippageBps: 100 });
    engine.pause('test', 'user');
    expect(store.state.intents.find((x) => x.id === i.id)?.status).toBe('cancelled');
    expect(() => engine.confirmPaper(i.id)).toThrow(/cancelled/);
    await expect(buy(GOOD, '0.01')).rejects.toThrow(/paused/);
    engine.resume();
    await buy(GOOD, '0.01');
    engine.pause('test', 'user');
    provider.movePrice(GOOD, 10);
    await engine.monitorTick();
    expect(pos().fills).toHaveLength(1);
  });

  it('rejects expired quotes', async () => {
    const i = await engine.prepare({ side: 'buy', mint: GOOD, amountSol: '0.01', slippageBps: 100 });
    store.state.intents.find((x) => x.id === i.id)!.expiresAt = new Date(Date.now() - 1000).toISOString();
    expect(() => engine.confirmPaper(i.id)).toThrow(/expired/);
  });

  it('live mode cannot be armed with mock data, without the server flag, or without a bot wallet', async () => {
    await expect(engine.armLive('bot', null, 'ENABLE LIVE TRADING')).rejects.toMatchObject({
      violations: expect.arrayContaining([expect.stringMatching(/mock/), expect.stringMatching(/LIVE_TRADING_ALLOWED/), expect.stringMatching(/Create the bot wallet/)]),
    });
  });
});

describe('autopilot (paper)', () => {
  it('buys only verified tokens that pass every filter, within the daily cap, then stops buying', async () => {
    autopilot.enable();
    await autopilot.cycle();
    let mints = store.state.positions.map((p) => p.mint);
    // Researched by liquidity: MGOOD (buy), MWHALE (not verified → skip), MMOON (buy). MRUG never passes the pre-filter.
    expect(mints).toEqual([GOOD, MOON]);
    const log = store.state.autopilot.log;
    expect(log.find((e) => e.mint === WHALE)).toMatchObject({ action: 'skip' });
    expect(log.find((e) => e.mint === WHALE)!.reasons!.join()).toMatch(/not verified/);
    expect(log.some((e) => e.mint === RUG)).toBe(false);
    expect(store.state.positions.every((p) => p.fills[0]!.reason === 'autopilot' && p.fills[0]!.inputRaw === '20000000')).toBe(true);

    await autopilot.cycle(); // MGOOD/MMOON held, MWHALE waits for its recheck → MFADE
    mints = store.state.positions.map((p) => p.mint);
    expect(mints).toEqual([GOOD, MOON, FADE]);

    await autopilot.cycle(); // daily cap (3) reached
    expect(store.state.positions).toHaveLength(3);
    expect(store.state.autopilot.log.at(-1)!.detail).toMatch(/Daily buy limit/);
    expect(store.state.alerts.some((a) => /Autopilot bought/.test(a.message))).toBe(true);
  });

  it('exits run automatically while the autopilot is on', async () => {
    autopilot.enable();
    await autopilot.cycle();
    provider.movePrice(MOON, 3.3);
    await engine.monitorTick();
    const moon = store.state.positions.find((p) => p.mint === MOON)!;
    expect(moon.takeProfit.levels[0]!.state).toBe('done');
    expect(costStillToRecover(moon)).toBe(0n);
  });

  it('never buys while paused, stopped, or when limits are used up', async () => {
    autopilot.enable();
    engine.pause('test', 'user');
    await autopilot.cycle();
    expect(store.state.positions).toHaveLength(0);
    engine.resume();
    autopilot.disable('test');
    await autopilot.cycle();
    expect(store.state.positions).toHaveLength(0);

    store.state.settings.limits.maxOpenPositions = 1;
    autopilot.enable();
    await autopilot.cycle();
    await autopilot.cycle();
    expect(store.state.positions).toHaveLength(1);
    expect(store.state.autopilot.log.at(-1)!.detail).toMatch(/Not buying: 1 of 1 positions open/);
  });

  it('refuses to start with impossible settings, and live autopilot needs the bot wallet', () => {
    store.state.settings.autopilot.buySizeSol = 1;
    expect(() => autopilot.enable()).toThrow(TradeRejected);
    store.state.settings.autopilot.buySizeSol = 0.02;
    store.state.mode = 'live';
    store.state.live = { armedAt: new Date().toISOString(), wallet: 'W', signer: 'browser' };
    const problems = autopilot.startProblems('START AUTOPILOT');
    expect(problems.join(' ')).toMatch(/browser wallet cannot sign unattended/);
    expect(problems.join(' ')).toMatch(/Unlock the bot wallet/);
    expect(problems.join(' ')).toMatch(/stop-loss/);
  });
});
