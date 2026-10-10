// Pure autopilot strategy: which discovered tokens are worth researching, and
// whether a researched token may be bought automatically.
//
// The buy decision is never based on social posts, paid promotion, bundle
// signals or a price spike: a token must pass the full verification (complete,
// fresh, consistent data with no high/critical flags) AND every strategy
// filter. Discovery sources (new/trending pools) only decide what gets
// researched.
import type { AutopilotSettings, Candidate, Mode, Position, ResearchReport } from '../../shared/types.js';

/** SOL, stablecoins and liquid-staking tokens are never autopilot targets. */
export const EXCLUDED_MINTS = new Set([
  'So11111111111111111111111111111111111111112', // wSOL
  'EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v', // USDC
  'Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB', // USDT
  'mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So', // mSOL
  'J1toso1uCk3RLmjorhTtrVwY9HJ7X8V9yYac6Y7kGCPn', // jitoSOL
  'bSo13r4TkiE4KumL71LsHTPpL2euBYLFx6h9HP3piy1', // bSOL
]);

/**
 * Drop candidates that already fail a filter on the cheap pool data (unknown
 * values pass here — full research decides), de-duplicate by mint, and order
 * watchlist first, then by liquidity.
 */
export function prefilterCandidates(cands: Candidate[], ap: AutopilotSettings): Candidate[] {
  const f = ap.filters;
  const best = new Map<string, Candidate>();
  for (const c of cands) {
    if (EXCLUDED_MINTS.has(c.mint)) continue;
    if (c.source !== 'watchlist') {
      if (c.liquidityUsd !== null && c.liquidityUsd < f.minLiquidityUsd) continue;
      if (c.volume24hUsd !== null && c.volume24hUsd < f.minVolume24hUsd) continue;
      if (c.ageMinutes !== null && c.ageMinutes < f.minAgeMinutes) continue;
      if (c.ageMinutes !== null && f.maxAgeMinutes !== null && c.ageMinutes > f.maxAgeMinutes) continue;
      if (c.priceChange1hPct !== null && f.maxPriceChange1hPct !== null && c.priceChange1hPct > f.maxPriceChange1hPct) continue;
    }
    const prev = best.get(c.mint);
    if (!prev || (prev.source !== 'watchlist' && (c.source === 'watchlist' || (c.liquidityUsd ?? 0) > (prev.liquidityUsd ?? 0)))) best.set(c.mint, c);
  }
  return [...best.values()].sort((a, b) => {
    if ((a.source === 'watchlist') !== (b.source === 'watchlist')) return a.source === 'watchlist' ? -1 : 1;
    return (b.liquidityUsd ?? 0) - (a.liquidityUsd ?? 0);
  });
}

export interface CandidateDecision {
  ok: boolean;
  /** Every failed check (empty when ok). */
  failures: string[];
  /** Every passed check, for the activity log. */
  passes: string[];
}

/** May the autopilot buy this researched token? */
export function evaluateCandidate(r: ResearchReport, ap: AutopilotSettings): CandidateDecision {
  const f = ap.filters;
  const failures: string[] = [];
  const passes: string[] = [];
  const check = (ok: boolean, pass: string, fail: string) => (ok ? passes.push(pass) : failures.push(fail));

  check(r.verdict === 'verified', 'passed verification', `not verified: ${r.verdictReasons.join(' ')}`);
  const serious = r.flags.filter((x) => x.severity === 'high' || x.severity === 'critical');
  check(serious.length === 0, 'no high/critical flags', `flags: ${serious.map((x) => x.code).join(', ')}`);

  const liq = r.market.liquidityUsd.value;
  check(liq !== null && liq >= f.minLiquidityUsd, `liquidity $${Math.round(liq ?? 0).toLocaleString()}`, `liquidity ${liq === null ? 'unknown' : `$${Math.round(liq).toLocaleString()}`} < $${f.minLiquidityUsd.toLocaleString()}`);

  const age = r.token.ageMinutes.value;
  const ageIsLowerBound = !!r.token.ageMinutes.note?.includes('lower bound');
  check(age !== null && age >= f.minAgeMinutes, `age ${age === null ? '?' : Math.round(age)} min`, `age ${age === null ? 'unknown' : `${Math.round(age)} min`} < ${f.minAgeMinutes} min`);
  if (f.maxAgeMinutes !== null)
    check(age !== null && !ageIsLowerBound && age <= f.maxAgeMinutes, `age ≤ ${f.maxAgeMinutes} min`, `age ${age === null ? 'unknown' : `${Math.round(age)}${ageIsLowerBound ? '+' : ''} min`} > ${f.maxAgeMinutes} min`);

  const vol = r.market.volume24hUsd.value;
  check(vol !== null && vol >= f.minVolume24hUsd, `24h volume $${Math.round(vol ?? 0).toLocaleString()}`, `24h volume ${vol === null ? 'unknown' : `$${Math.round(vol).toLocaleString()}`} < $${f.minVolume24hUsd.toLocaleString()}`);

  const holders = r.holders.count.value;
  if (f.minHolders > 0) check(holders !== null && holders >= f.minHolders, `${holders} holders`, `holders ${holders ?? 'unknown'} < ${f.minHolders}`);

  const sells = r.market.txns24h.value?.sells ?? null;
  if (f.minSells24h > 0) check(sells !== null && sells >= f.minSells24h, `${sells} sells in 24h (others can sell)`, `24h sells ${sells ?? 'unknown'} < ${f.minSells24h}`);

  const ch1 = r.market.priceChange.value?.h1 ?? null;
  if (f.maxPriceChange1hPct !== null)
    check(ch1 !== null && ch1 <= f.maxPriceChange1hPct, `1h change ${ch1}%`, `1h change ${ch1 === null ? 'unknown' : `${ch1}%`} — not chasing moves above ${f.maxPriceChange1hPct}%`);

  const lp = r.market.lpLockedPct.value;
  if (f.requireLpLockKnown) check(lp !== null && lp >= f.minLpLockedPct, `LP ${lp?.toFixed(0)}% locked/burned`, `LP lock ${lp === null ? 'unknown' : `${lp.toFixed(0)}%`} < ${f.minLpLockedPct}%`);
  else if (lp !== null) check(lp >= f.minLpLockedPct, `LP ${lp.toFixed(0)}% locked/burned`, `LP lock ${lp.toFixed(0)}% < ${f.minLpLockedPct}%`);

  return { ok: failures.length === 0, failures, passes };
}

export function autopilotBuysToday(positions: Position[], mode: Mode, now: Date): number {
  const day = now.toISOString().slice(0, 10);
  return positions.filter((p) => p.mode === mode && p.fills[0]?.reason === 'autopilot' && p.openedAt.slice(0, 10) === day).length;
}

/** Already held, or bought within the cooldown window. */
export function recentlyTraded(positions: Position[], mint: string, mode: Mode, now: Date, cooldownMinutes: number): boolean {
  return positions.some(
    (p) => p.mode === mode && p.mint === mint && (p.status === 'open' || now.getTime() - new Date(p.openedAt).getTime() < cooldownMinutes * 60_000),
  );
}
