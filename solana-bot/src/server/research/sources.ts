// Off-chain market and social data sources. Each returns plain data; the
// orchestrator wraps results with source names, links and timestamps.
import { env } from '../env.js';
import { fetchJson, setHostInterval } from '../http.js';
import type { SocialPost } from '../../shared/types.js';

setHostInterval('api.geckoterminal.com', 2_100); // ~30 req/min public limit
setHostInterval('api.x.com', 1_000);

// ---------------------------------------------------------------- DexScreener

export interface DexPair {
  dexId: string;
  url: string;
  pairAddress: string;
  baseToken: { address: string; name: string; symbol: string };
  quoteToken: { address: string; symbol: string };
  priceNative?: string;
  priceUsd?: string;
  txns?: { h24?: { buys: number; sells: number } };
  volume?: { h24?: number };
  priceChange?: Record<string, number>;
  liquidity?: { usd?: number };
  fdv?: number;
  marketCap?: number;
  pairCreatedAt?: number;
}

export const dexScreenerUrl = (mint: string) => `https://api.dexscreener.com/tokens/v1/solana/${mint}`;

/** The most liquid pair where the token is the base asset. */
export async function getDexPair(mint: string): Promise<DexPair | null> {
  const pairs = await fetchJson<DexPair[]>(dexScreenerUrl(mint));
  const mine = pairs.filter((p) => p.baseToken.address === mint);
  mine.sort((a, b) => (b.liquidity?.usd ?? 0) - (a.liquidity?.usd ?? 0));
  return mine[0] ?? null;
}

// ---------------------------------------------------------------- GeckoTerminal

export const geckoOhlcvUrl = (pool: string) =>
  `https://api.geckoterminal.com/api/v2/networks/solana/pools/${pool}/ohlcv/hour?aggregate=1&limit=72&currency=usd`;

export async function getPriceHistory(pool: string): Promise<Array<{ t: string; close: number }>> {
  const r = await fetchJson<{ data?: { attributes?: { ohlcv_list?: number[][] } } }>(geckoOhlcvUrl(pool));
  return (r.data?.attributes?.ohlcv_list ?? [])
    .map((c) => ({ t: new Date((c[0] ?? 0) * 1000).toISOString(), close: c[4] ?? 0 }))
    .reverse();
}

// ---------------------------------------------------------------- RugCheck

export const rugcheckUrl = (mint: string) => `https://api.rugcheck.xyz/v1/tokens/${mint}/report`;
export const rugcheckPage = (mint: string) => `https://rugcheck.xyz/tokens/${mint}`;

export interface RugcheckFacts {
  risks: Array<{ name: string; level: string; description: string }>;
  lpLockedPct: number | null;
  totalHolders: number | null;
}

export async function getRugcheck(mint: string, pairAddress: string | null): Promise<RugcheckFacts> {
  const r = await fetchJson<{
    risks?: Array<{ name?: string; level?: string; description?: string }>;
    markets?: Array<{ pubkey?: string; lp?: { lpLockedPct?: number } }>;
    totalHolders?: number;
  }>(rugcheckUrl(mint));
  const markets = r.markets ?? [];
  const main = markets.find((m) => m.pubkey === pairAddress) ?? null;
  const lockedValues = markets.map((m) => m.lp?.lpLockedPct).filter((x): x is number => typeof x === 'number');
  return {
    risks: (r.risks ?? []).map((x) => ({ name: x.name ?? '?', level: x.level ?? 'info', description: x.description ?? '' })),
    lpLockedPct: main?.lp?.lpLockedPct ?? (lockedValues.length ? Math.max(...lockedValues) : null),
    totalHolders: typeof r.totalHolders === 'number' ? r.totalHolders : null,
  };
}

// ---------------------------------------------------------------- X / Twitter

export const xSearchUrl = (query: string) => {
  const q = new URLSearchParams({
    query,
    max_results: '50',
    'tweet.fields': 'created_at,public_metrics,author_id',
    expansions: 'author_id',
    'user.fields': 'created_at,public_metrics,verified,username',
  });
  return `https://api.x.com/2/tweets/search/recent?${q}`;
};

/** Recent posts mentioning the mint address (the address avoids ticker collisions). */
export async function getXPosts(mint: string): Promise<SocialPost[]> {
  if (!env.xBearerToken) throw new Error('X_BEARER_TOKEN not set');
  type U = { id: string; username: string; created_at?: string; verified?: boolean; public_metrics?: { followers_count: number; following_count: number; tweet_count: number } };
  const r = await fetchJson<{
    data?: Array<{ id: string; text: string; created_at: string; author_id: string; public_metrics?: { like_count: number; retweet_count: number; reply_count: number } }>;
    includes?: { users?: U[] };
  }>(xSearchUrl(`"${mint}" -is:retweet`), { headers: { authorization: `Bearer ${env.xBearerToken}` }, retries: 1 });
  const users = new Map((r.includes?.users ?? []).map((u) => [u.id, u]));
  return (r.data ?? []).map((t) => {
    const u = users.get(t.author_id);
    return {
      id: t.id,
      url: `https://x.com/${u?.username ?? 'i'}/status/${t.id}`,
      text: t.text,
      createdAt: t.created_at,
      likes: t.public_metrics?.like_count ?? 0,
      reposts: t.public_metrics?.retweet_count ?? 0,
      replies: t.public_metrics?.reply_count ?? 0,
      author: {
        id: t.author_id,
        username: u?.username ?? 'unknown',
        createdAt: u?.created_at ?? null,
        followers: u?.public_metrics?.followers_count ?? 0,
        following: u?.public_metrics?.following_count ?? 0,
        postCount: u?.public_metrics?.tweet_count ?? 0,
        verified: u?.verified ?? false,
      },
      signals: [],
    };
  });
}
