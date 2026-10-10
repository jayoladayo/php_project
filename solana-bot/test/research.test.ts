import { describe, expect, it } from 'vitest';
import { analyzeBundles } from '../src/server/research/bundles.js';
import { analyzeSocial, normalizeText } from '../src/server/research/social.js';
import { assessReport, type ReportDraft } from '../src/server/research/verify.js';
import { DEFAULT_SETTINGS } from '../src/shared/defaults.js';
import type { SocialPost, Sourced } from '../src/shared/types.js';

const NOW = new Date('2026-10-10T12:00:00Z');
const at = NOW.toISOString();
const s = <T>(value: T | null, note?: string): Sourced<T> => ({ value, source: 'test', fetchedAt: at, ...(note ? { note } : {}) });

function cleanDraft(): ReportDraft {
  return {
    mint: 'M',
    generatedAt: at,
    mock: false,
    token: {
      name: s('Test'), symbol: s('TST'), decimals: s(6), supplyRaw: s('1000000000000'), tokenProgram: s('Tokenkeg'),
      mintAuthority: s<string>('none'), freezeAuthority: s<string>('none'), extensions: s<string[]>([]),
      createdAt: s('2026-09-01T00:00:00Z'), ageMinutes: s(50_000), creator: s('CREATOR'),
    },
    market: {
      priceUsd: s(0.02), priceSol: s(0.0001), liquidityUsd: s(250_000), volume24hUsd: s(1e6), marketCapUsd: s(20_000),
      fdvUsd: s(20_000), txns24h: s({ buys: 10, sells: 10 }), priceChange: s({ h24: 1 }), priceHistory: s([]),
      pairAddress: s('P'), dex: s('raydium'), lpLockedPct: s(100),
    },
    holders: { count: s(5000), top: s([]), top10Pct: s(20), maxSingleWalletPct: s(4) },
    sellability: {
      buyQuote: s({ inputMint: 'SOL', outputMint: 'M', inAmount: '10000000', outAmount: '99000000', minOutAmount: '1', slippageBps: 50, priceImpactPct: 0.1, routeLabels: [], platformFeeRaw: '0', fetchedAt: at, source: 't' }),
      sellQuote: s({ inputMint: 'M', outputMint: 'SOL', inAmount: '99000000', outAmount: '9800000', minOutAmount: '1', slippageBps: 50, priceImpactPct: 0.1, routeLabels: [], platformFeeRaw: '0', fetchedAt: at, source: 't' }),
      roundTripLossPct: s(2),
    },
    bundles: s({ signals: [], earlyBuyers: 3, sharedFunders: [] }),
    social: s<never>(null, 'X_BEARER_TOKEN not set'),
    thirdPartyFlags: s([]),
    sources: [],
  };
}
const T = DEFAULT_SETTINGS.verification;

describe('assessReport', () => {
  it('verifies complete, fresh, consistent, unflagged data — and says it is not a guarantee', () => {
    const r = assessReport(cleanDraft(), T, NOW);
    expect(r.verdictReasons).toHaveLength(1);
    expect(r.verdict).toBe('verified');
    expect(r.verdictReasons[0]).toMatch(/not a guarantee/);
    // missing social data never blocks or helps
    expect(r.uncertainty.join()).toMatch(/never treated as evidence/);
  });

  it('active mint / freeze authority is critical', () => {
    const d = cleanDraft();
    d.token.mintAuthority = s('SOMEONE');
    d.token.freezeAuthority = s('SOMEONE');
    const r = assessReport(d, T, NOW);
    expect(r.verdict).toBe('not_verified');
    expect(r.flags.filter((f) => f.severity === 'critical').map((f) => f.code)).toEqual(['mint_authority_active', 'freeze_authority_active']);
  });

  it('missing key data → not verified', () => {
    const d = cleanDraft();
    d.market.liquidityUsd = s<number>(null);
    expect(assessReport(d, T, NOW).verdictReasons.join()).toMatch(/Missing key data: liquidity/);
  });

  it('stale data → not verified', () => {
    const d = cleanDraft();
    d.holders.top10Pct = { ...d.holders.top10Pct, fetchedAt: new Date(NOW.getTime() - 3_600_000).toISOString() };
    expect(assessReport(d, T, NOW).verdictReasons.join()).toMatch(/Stale data: top-10/);
  });

  it('contradictory price data → not verified', () => {
    const d = cleanDraft();
    d.market.priceSol = s(0.0005); // router says ~0.000101
    expect(assessReport(d, T, NOW).verdictReasons.join()).toMatch(/Contradictory/);
  });

  it('no sell route is critical (honeypot-like)', () => {
    const d = cleanDraft();
    d.sellability.sellQuote = s<never>(null, 'no route');
    const r = assessReport(d, T, NOW);
    expect(r.flags.find((f) => f.code === 'no_sell_route')?.severity).toBe('critical');
  });

  it('dangerous Token-2022 extensions are flagged', () => {
    const d = cleanDraft();
    d.token.extensions = s(['permanentDelegate', 'transferHook']);
    const codes = assessReport(d, T, NOW).flags.map((f) => f.code);
    expect(codes).toContain('ext_permanentDelegate');
    expect(codes).toContain('ext_transferHook');
  });

  it('high-severity signals (e.g. bundles) make a token not verified, labelled as signals', () => {
    const d = cleanDraft();
    d.bundles = s({ signals: [{ code: 'bundle_same_slot', kind: 'signal', severity: 'high', message: 'x' }], earlyBuyers: 5, sharedFunders: [] });
    const r = assessReport(d, T, NOW);
    expect(r.verdict).toBe('not_verified');
    expect(r.flags.find((f) => f.code === 'bundle_same_slot')?.kind).toBe('signal');
  });

  it('concentration, low liquidity, unlocked LP and round-trip loss flags', () => {
    const d = cleanDraft();
    d.holders.top10Pct = s(70);
    d.holders.maxSingleWalletPct = s(30);
    d.market.liquidityUsd = s(1000);
    d.market.lpLockedPct = s(0);
    d.sellability.roundTripLossPct = s(25);
    const codes = assessReport(d, T, NOW).flags.map((f) => f.code);
    expect(codes).toEqual(expect.arrayContaining(['holder_concentration', 'single_holder', 'low_liquidity', 'lp_not_locked', 'round_trip_loss']));
  });
});

describe('analyzeBundles', () => {
  const supply = 1_000_000n;
  it('flags same-slot launch buys and shared/creator funders as signals', () => {
    const r = analyzeBundles({
      creationSlot: 100,
      creator: 'C',
      totalSupplyRaw: supply,
      earlyReceipts: [
        { slot: 100, wallet: 'A', amountRaw: 100_000n, signature: 's1' },
        { slot: 100, wallet: 'B', amountRaw: 100_000n, signature: 's2' },
        { slot: 100, wallet: 'D', amountRaw: 100_000n, signature: 's3' },
        { slot: 100, wallet: 'C', amountRaw: 500_000n, signature: 's0' }, // creator excluded
        { slot: 150, wallet: 'E', amountRaw: 1n, signature: 's4' },
      ],
      funding: [
        { wallet: 'A', funder: 'F' },
        { wallet: 'B', funder: 'F' },
        { wallet: 'D', funder: 'C' },
        { wallet: 'E', funder: null },
      ],
      holdingsByWallet: new Map([['A', 100_000n], ['B', 100_000n], ['D', 100_000n]]),
    });
    const codes = r.signals.map((x) => x.code);
    expect(codes).toEqual(expect.arrayContaining(['bundle_same_slot', 'launch_window_concentration', 'shared_funder', 'creator_funded_holders']));
    expect(r.signals.every((x) => x.kind === 'signal')).toBe(true);
    expect(r.signals.find((x) => x.code === 'bundle_same_slot')?.severity).toBe('high');
    expect(r.earlyBuyers).toBe(4);
  });

  it('stays quiet for organic-looking activity', () => {
    const r = analyzeBundles({
      creationSlot: 100,
      creator: 'C',
      totalSupplyRaw: supply,
      earlyReceipts: [
        { slot: 101, wallet: 'A', amountRaw: 1000n, signature: 's1' },
        { slot: 140, wallet: 'B', amountRaw: 1000n, signature: 's2' },
      ],
      funding: [{ wallet: 'A', funder: 'X' }, { wallet: 'B', funder: 'Y' }],
      holdingsByWallet: new Map(),
    });
    expect(r.signals).toEqual([]);
  });
});

describe('analyzeSocial', () => {
  const post = (id: string, author: string, text: string, minutes: number, followers = 50, ageDays = 5): SocialPost => ({
    id, url: `https://x.com/${author}/status/${id}`, text, createdAt: new Date(NOW.getTime() - minutes * 60_000).toISOString(),
    likes: 500, reposts: 10, replies: 2,
    author: { id: author, username: author, createdAt: new Date(NOW.getTime() - ageDays * 86_400_000).toISOString(), followers, following: 10, postCount: 10, verified: false },
    signals: [],
  });

  it('detects copied text, bursts, new accounts and hype — as signals only', () => {
    const t = 'This gem is going 100x guaranteed!! ape in now';
    const posts = [post('1', 'a', t + ' https://t.co/x', 1), post('2', 'b', t, 2), post('3', 'c', t.toUpperCase(), 3), post('4', 'd', '#ad check $TST', 4), post('5', 'e', 'hmm', 5), post('6', 'f', 'later', 6)];
    const r = analyzeSocial(posts, NOW);
    const codes = r.signals.map((x) => x.code);
    expect(codes).toEqual(expect.arrayContaining(['social_copied_text', 'social_burst', 'social_low_credibility_majority', 'social_paid_promotion']));
    expect(r.posts[0]?.signals).toEqual(expect.arrayContaining(['hype language (not evidence)', 'near-identical text posted by other accounts']));
    expect(r.signals.every((x) => x.kind === 'signal')).toBe(true);
  });

  it('normalizes away links, tags and addresses', () => {
    expect(normalizeText('Buy $TST now! https://x.co/a @bob So11111111111111111111111111111111111111112')).toBe('buy now');
  });
});
