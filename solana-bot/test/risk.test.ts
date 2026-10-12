import { describe, expect, it } from 'vitest';
import { applySell, openPosition } from '../src/server/trading/accounting.js';
import { checkTrade, type RiskInput } from '../src/server/trading/risk.js';
import type { ResearchReport } from '../src/shared/types.js';
import { META, fill, settings } from './helpers.js';

const NOW = new Date('2026-10-10T12:00:00Z');
const SOL = 1_000_000_000n;
const report = (p: Partial<ResearchReport> = {}): ResearchReport =>
  ({ mint: META.mint, generatedAt: NOW.toISOString(), verdict: 'verified', flags: [], mock: false, ...p }) as ResearchReport;

const base = (p: Partial<RiskInput> = {}): RiskInput => ({
  settings: settings(),
  mode: 'paper',
  paused: false,
  liveArmed: false,
  side: 'buy',
  amountRaw: SOL / 50n, // 0.02
  requestedSlippageBps: 50,
  quote: { priceImpactPct: 0.3, fetchedAt: NOW },
  now: NOW,
  openPositions: [],
  realizedTodayLamports: 0n,
  unrealizedLossLamports: 0n,
  balanceLamports: SOL,
  estCostsLamports: 30_000n,
  research: report(),
  automated: false,
  wallet: null,
  ...p,
});

describe('risk limits', () => {
  it('passes a small, verified paper buy', () => {
    expect(checkTrade(base())).toEqual({ ok: true, violations: [], warnings: [] });
  });

  it.each([
    ['pause', { paused: true }, /paused/],
    ['max buy', { amountRaw: SOL / 10n }, /max buy/],
    ['slippage', { requestedSlippageBps: 500 }, /Slippage 500/],
    ['price impact', { quote: { priceImpactPct: 5, fetchedAt: NOW } }, /Price impact/],
    ['stale quote', { quote: { priceImpactPct: 0.1, fetchedAt: new Date(NOW.getTime() - 60_000) } }, /stale/],
    ['no quote', { quote: null }, /No quote/],
    ['balance', { balanceLamports: SOL / 50n }, /Insufficient SOL/],
    ['unknown balance', { balanceLamports: null }, /balance unknown/],
    ['daily loss', { realizedTodayLamports: -SOL / 10n }, /Daily loss limit/],
    ['daily loss incl. unrealized', { realizedTodayLamports: -SOL / 20n, unrealizedLossLamports: -SOL / 20n }, /Daily loss limit/],
    ['remaining budget', { realizedTodayLamports: -(SOL / 10n - SOL / 100n) }, /remaining daily loss budget/],
    ['live not armed', { mode: 'live' as const }, /Live trading has not been enabled/],
  ])('blocks on %s', (_name, patch, msg) => {
    const r = checkTrade(base(patch as Partial<RiskInput>));
    expect(r.ok).toBe(false);
    expect(r.violations.join(' ')).toMatch(msg);
  });

  it('enforces total exposure across open positions', () => {
    const p = openPosition(META, fill({ side: 'buy', inputRaw: String(SOL / 5n - 10_000n), outputRaw: '1000' }), { enabled: true, levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }], moonBagPct: 0 });
    const r = checkTrade(base({ openPositions: [p] }));
    expect(r.violations.join()).toMatch(/Exposure/);
  });

  it('blocks unverified tokens unless the user explicitly overrides, and always for automation', () => {
    const r = report({ verdict: 'not_verified' });
    expect(checkTrade(base({ research: r })).violations.join()).toMatch(/not verified/);
    const ok = checkTrade(base({ research: r, overrideUnverified: true }));
    expect(ok.ok).toBe(true);
    expect(ok.warnings.join()).toMatch(/override/);
    expect(checkTrade(base({ research: r, overrideUnverified: true, automated: true })).ok).toBe(false);
  });

  it('critical flags block even with an override', () => {
    const r = report({ verdict: 'not_verified', flags: [{ code: 'freeze_authority_active', severity: 'critical', kind: 'fact', message: '' }] });
    expect(checkTrade(base({ research: r, overrideUnverified: true })).violations.join()).toMatch(/Critical/);
  });

  it('allows automatic buys only while the autopilot is on, and never into a token already held', () => {
    expect(checkTrade(base({ automated: true })).violations.join()).toMatch(/autopilot is off/);
    expect(checkTrade(base({ automated: true, autopilotEnabled: true })).ok).toBe(true);
    const held = openPosition(META, fill({ side: 'buy', inputRaw: '1000', outputRaw: '1000' }), { enabled: true, levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }], moonBagPct: 0 });
    expect(checkTrade(base({ automated: true, autopilotEnabled: true, openPositions: [held] })).violations.join()).toMatch(/already holds/);
  });

  it('never uses mock data live', () => {
    const r = checkTrade(base({ mode: 'live', liveArmed: true, wallet: 'W', liveWallet: 'W', research: report({ mock: true }) }));
    expect(r.violations.join()).toMatch(/Mock/);
  });

  it('live trades must come from the wallet live trading was enabled for', () => {
    expect(checkTrade(base({ mode: 'live', liveArmed: true, wallet: 'OTHER', liveWallet: 'ARMED' })).violations.join()).toMatch(/not the wallet live trading was enabled for/);
    expect(checkTrade(base({ mode: 'live', liveArmed: true, wallet: 'ARMED', liveWallet: 'ARMED' })).ok).toBe(true);
    expect(checkTrade(base({ mode: 'live', liveArmed: true, wallet: 'ARMED', liveWallet: null })).ok).toBe(false);
  });

  describe('sells', () => {
    const pos = openPosition(META, fill({ side: 'buy', inputRaw: '1000', outputRaw: '1000' }), { enabled: true, levels: [{ multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } }], moonBagPct: 0 });
    const locked = applySell(pos, fill({ side: 'sell', inputRaw: '500', outputRaw: '5000', reason: 'take_profit', tpLevel: 0 })).position;
    const sell = (p: Partial<RiskInput>) => base({ side: 'sell', amountRaw: 100n, research: undefined, ...p });

    it('allows a manual sell of an open position', () => {
      expect(checkTrade(sell({ position: pos })).ok).toBe(true);
    });
    it('blocks overselling', () => {
      expect(checkTrade(sell({ position: pos, amountRaw: 1001n })).violations.join()).toMatch(/exceeds tokens held/);
    });
    it('locked positions need explicit approval; automation needs a rule that applies after take-profit', () => {
      expect(checkTrade(sell({ position: locked })).ok).toBe(false);
      expect(checkTrade(sell({ position: locked, acknowledgeLocked: true })).ok).toBe(true);
      expect(checkTrade(sell({ position: locked, automated: true })).ok).toBe(false);
      expect(checkTrade(sell({ position: locked, automated: true, ruleAppliesAfterTakeProfit: true })).ok).toBe(true);
    });
  });
});
