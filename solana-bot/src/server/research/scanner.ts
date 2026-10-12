// Token discovery for the autopilot. Sources only decide what gets
// researched — never what gets bought. Paid promotion feeds (DexScreener
// boosts / paid profiles) and social media are deliberately not used.
import { fetchJson } from '../http.js';
import { MOCK_SOL_USD, type MockProvider } from '../market/mock.js';
import type { AutopilotSettings, Candidate } from '../../shared/types.js';

export const geckoNewPoolsUrl = 'https://api.geckoterminal.com/api/v2/networks/solana/new_pools?page=1';
export const geckoTrendingPoolsUrl = 'https://api.geckoterminal.com/api/v2/networks/solana/trending_pools?page=1';

interface GeckoPool {
  attributes?: {
    address?: string;
    name?: string;
    pool_created_at?: string;
    reserve_in_usd?: string | number | null;
    volume_usd?: { h24?: string | number | null };
    price_change_percentage?: { h1?: string | number | null };
  };
  relationships?: {
    base_token?: { data?: { id?: string } };
    dex?: { data?: { id?: string } };
  };
}

const num = (v: unknown): number | null => {
  if (v === null || v === undefined || v === '') return null;
  const n = Number(v);
  return Number.isFinite(n) ? n : null;
};

/** Map GeckoTerminal pools to candidates (base token = the token being traded). */
export function parseGeckoPools(json: { data?: GeckoPool[] }, source: Candidate['source'], now: Date): Candidate[] {
  const out: Candidate[] = [];
  for (const p of json.data ?? []) {
    const id = p.relationships?.base_token?.data?.id ?? '';
    if (!id.startsWith('solana_')) continue;
    const mint = id.slice('solana_'.length);
    const a = p.attributes ?? {};
    const created = a.pool_created_at ? new Date(a.pool_created_at).getTime() : NaN;
    out.push({
      mint,
      symbol: a.name?.split(' / ')[0]?.trim() || null,
      source,
      poolAddress: a.address ?? null,
      dex: p.relationships?.dex?.data?.id ?? null,
      liquidityUsd: num(a.reserve_in_usd),
      volume24hUsd: num(a.volume_usd?.h24),
      ageMinutes: Number.isFinite(created) ? (now.getTime() - created) / 60_000 : null,
      priceChange1hPct: num(a.price_change_percentage?.h1),
    });
  }
  return out;
}

export interface ScanResult {
  candidates: Candidate[];
  errors: string[];
}

export interface Scanner {
  scan(ap: AutopilotSettings, now: Date): Promise<ScanResult>;
}

export class LiveScanner implements Scanner {
  async scan(ap: AutopilotSettings, now: Date): Promise<ScanResult> {
    const candidates: Candidate[] = ap.watchlist.map((mint) => ({ mint, symbol: null, source: 'watchlist', poolAddress: null, dex: null, liquidityUsd: null, volume24hUsd: null, ageMinutes: null, priceChange1hPct: null }));
    const errors: string[] = [];
    const feeds: Array<[boolean, string, Candidate['source']]> = [
      [ap.sources.newPools, geckoNewPoolsUrl, 'new_pools'],
      [ap.sources.trendingPools, geckoTrendingPoolsUrl, 'trending_pools'],
    ];
    for (const [on, url, source] of feeds) {
      if (!on) continue;
      try {
        candidates.push(...parseGeckoPools(await fetchJson<{ data?: GeckoPool[] }>(url), source, now));
      } catch (e) {
        errors.push(`${source}: ${(e as Error).message}`);
      }
    }
    return { candidates, errors };
  }
}

export class MockScanner implements Scanner {
  constructor(private readonly provider: MockProvider) {}

  async scan(ap: AutopilotSettings): Promise<ScanResult> {
    const candidates: Candidate[] = this.provider.list().map((t) => ({
      mint: t.mint,
      symbol: t.symbol,
      source: ap.watchlist.includes(t.mint) ? 'watchlist' : 'mock',
      poolAddress: 'MockPool',
      dex: 'mock',
      liquidityUsd: (Number(t.solReserve) / 1e9) * MOCK_SOL_USD * 2,
      volume24hUsd: t.volume24hUsd,
      ageMinutes: t.ageMinutes,
      priceChange1hPct: 0,
    }));
    return { candidates, errors: [] };
  }
}
