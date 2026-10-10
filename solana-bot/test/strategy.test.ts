import { describe, expect, it } from 'vitest';
import { parseGeckoPools } from '../src/server/research/scanner.js';
import { autopilotBuysToday, evaluateCandidate, prefilterCandidates, recentlyTraded } from '../src/server/trading/strategy.js';
import { openPosition } from '../src/server/trading/accounting.js';
import type { Candidate, ResearchReport, Sourced } from '../src/shared/types.js';
import { META, fill, settings } from './helpers.js';

const NOW = new Date('2026-10-10T12:00:00Z');
const AP = settings().autopilot;
const s = <T>(value: T | null, note?: string): Sourced<T> => ({ value, source: 't', fetchedAt: NOW.toISOString(), ...(note ? { note } : {}) });

const cand = (p: Partial<Candidate>): Candidate => ({
  mint: 'M1', symbol: 'A', source: 'new_pools', poolAddress: 'P', dex: 'raydium',
  liquidityUsd: 100_000, volume24hUsd: 200_000, ageMinutes: 600, priceChange1hPct: 5, ...p,
});

function report(patch: (r: ResearchReport) => void = () => undefined): ResearchReport {
  const r = {
    mint: 'M1', generatedAt: NOW.toISOString(), mock: false, verdict: 'verified', verdictReasons: ['ok'], summary: '', uncertainty: [], flags: [],
    token: { ageMinutes: s(600) },
    market: { liquidityUsd: s(100_000), volume24hUsd: s(200_000), txns24h: s({ buys: 400, sells: 300 }), priceChange: s({ h1: 5 }), lpLockedPct: s(100) },
    holders: { count: s(1500) },
  } as unknown as ResearchReport;
  patch(r);
  return r;
}

describe('prefilterCandidates', () => {
  it('drops pools that already fail a filter, de-duplicates, and puts the watchlist first', () => {
    const out = prefilterCandidates(
      [
        cand({ mint: 'LOWLIQ', liquidityUsd: 1000 }),
        cand({ mint: 'BABY', ageMinutes: 5 }),
        cand({ mint: 'OLD', ageMinutes: 30 * 1440 }),
        cand({ mint: 'SPIKE', priceChange1hPct: 400 }),
        cand({ mint: 'THIN', volume24hUsd: 10 }),
        cand({ mint: 'So11111111111111111111111111111111111111112' }),
        cand({ mint: 'DUP', liquidityUsd: 50_000 }),
        cand({ mint: 'DUP', liquidityUsd: 90_000, source: 'trending_pools' }),
        cand({ mint: 'BIG', liquidityUsd: 900_000 }),
        cand({ mint: 'WATCH', source: 'watchlist', liquidityUsd: null, volume24hUsd: null, ageMinutes: null, priceChange1hPct: null }),
        cand({ mint: 'UNKNOWN', liquidityUsd: null, volume24hUsd: null, ageMinutes: null, priceChange1hPct: null }),
      ],
      AP,
    );
    expect(out.map((c) => c.mint)).toEqual(['WATCH', 'BIG', 'DUP', 'UNKNOWN']);
    expect(out.find((c) => c.mint === 'DUP')?.liquidityUsd).toBe(90_000);
  });
});

describe('evaluateCandidate', () => {
  it('buys only verified tokens that pass every filter', () => {
    const d = evaluateCandidate(report(), AP);
    expect(d.failures).toEqual([]);
    expect(d.ok).toBe(true);
  });

  it.each<[string, (r: ResearchReport) => void, RegExp]>([
    ['not verified', (r) => ((r.verdict = 'not_verified'), (r.verdictReasons = ['Missing key data: liquidity.'])), /not verified: Missing key data/],
    ['serious flag', (r) => (r.flags = [{ code: 'lp_not_locked', severity: 'high', kind: 'fact', message: '' }]), /flags: lp_not_locked/],
    ['low liquidity', (r) => (r.market.liquidityUsd = s(5000)), /liquidity \$5,000/],
    ['too young', (r) => (r.token.ageMinutes = s(10)), /age 10 min < 60/],
    ['too old', (r) => (r.token.ageMinutes = s(20_000)), /> 10080/],
    ['age only a lower bound', (r) => (r.token.ageMinutes = s(9000, 'lower bound: history deeper than scan')), /9000\+ min/],
    ['low volume', (r) => (r.market.volume24hUsd = s(100)), /24h volume \$100/],
    ['few holders', (r) => (r.holders.count = s(20)), /holders 20 < 300/],
    ['holder count unknown', (r) => (r.holders.count = s<number>(null)), /holders unknown/],
    ['nobody sold', (r) => (r.market.txns24h = s({ buys: 900, sells: 3 })), /24h sells 3 < 50/],
    ['chasing a spike', (r) => (r.market.priceChange = s({ h1: 180 })), /not chasing/],
    ['LP lock unknown', (r) => (r.market.lpLockedPct = s<number>(null)), /LP lock unknown/],
    ['LP not locked', (r) => (r.market.lpLockedPct = s(40)), /LP lock 40%/],
  ])('rejects: %s', (_n, patch, msg) => {
    const d = evaluateCandidate(report(patch), AP);
    expect(d.ok).toBe(false);
    expect(d.failures.join(' | ')).toMatch(msg);
  });

  it('can relax LP and holder requirements when configured', () => {
    const ap = { ...AP, filters: { ...AP.filters, requireLpLockKnown: false, minHolders: 0 } };
    expect(evaluateCandidate(report((r) => ((r.market.lpLockedPct = s<number>(null)), (r.holders.count = s<number>(null)))), ap).ok).toBe(true);
  });
});

describe('daily caps and cooldowns', () => {
  const at = (iso: string, reason: 'autopilot' | 'manual' = 'autopilot') =>
    ({ ...openPosition({ ...META, mint: 'M1' }, fill({ side: 'buy', inputRaw: '1000', outputRaw: '1000', at: iso, reason }), settings().takeProfit), mode: 'paper' as const });

  it('counts only autopilot buys opened today (UTC)', () => {
    const ps = [at('2026-10-10T01:00:00.000Z'), at('2026-10-10T02:00:00.000Z', 'manual'), at('2026-10-09T23:59:00.000Z')];
    expect(autopilotBuysToday(ps, 'paper', NOW)).toBe(1);
    expect(autopilotBuysToday(ps, 'live', NOW)).toBe(0);
  });

  it('treats held tokens and recent buys as off-limits', () => {
    const closed = { ...at('2026-10-10T11:00:00.000Z'), status: 'closed' as const };
    expect(recentlyTraded([closed], 'M1', 'paper', NOW, 120)).toBe(true);
    expect(recentlyTraded([closed], 'M1', 'paper', NOW, 30)).toBe(false);
    expect(recentlyTraded([at('2026-09-01T00:00:00.000Z')], 'M1', 'paper', NOW, 30)).toBe(true); // still open
    expect(recentlyTraded([closed], 'OTHER', 'paper', NOW, 120)).toBe(false);
  });
});

describe('parseGeckoPools', () => {
  it('maps GeckoTerminal pool data to candidates', () => {
    const json = {
      data: [
        {
          attributes: { address: 'POOL1', name: 'WIF / SOL', pool_created_at: '2026-10-10T10:00:00Z', reserve_in_usd: '123456.7', volume_usd: { h24: '999' }, price_change_percentage: { h1: '-3.5' } },
          relationships: { base_token: { data: { id: 'solana_MintAAA' } }, dex: { data: { id: 'raydium' } } },
        },
        { attributes: {}, relationships: { base_token: { data: { id: 'eth_0xabc' } } } },
        { attributes: { reserve_in_usd: null }, relationships: { base_token: { data: { id: 'solana_MintBBB' } } } },
      ],
    };
    const out = parseGeckoPools(json, 'new_pools', NOW);
    expect(out).toEqual([
      { mint: 'MintAAA', symbol: 'WIF', source: 'new_pools', poolAddress: 'POOL1', dex: 'raydium', liquidityUsd: 123456.7, volume24hUsd: 999, ageMinutes: 120, priceChange1hPct: -3.5 },
      { mint: 'MintBBB', symbol: null, source: 'new_pools', poolAddress: null, dex: null, liquidityUsd: null, volume24hUsd: null, ageMinutes: null, priceChange1hPct: null },
    ]);
  });
});
