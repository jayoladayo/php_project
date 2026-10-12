export const sol = (lamports: string | bigint | number | null | undefined, dp = 4): string =>
  lamports === null || lamports === undefined ? '—' : `${(Number(lamports) / 1e9).toFixed(dp)} SOL`;

export const tokens = (raw: string | bigint, decimals: number): string => {
  const n = Number(raw) / 10 ** decimals;
  return n.toLocaleString(undefined, { maximumFractionDigits: n < 1 ? 6 : 2 });
};

export const price = (p: number | null | undefined): string => {
  if (p === null || p === undefined || !Number.isFinite(p)) return '—';
  if (p === 0) return '0';
  return p < 0.001 ? p.toExponential(3) : p.toPrecision(4);
};

export const usd = (n: number | null | undefined): string =>
  n === null || n === undefined ? '—' : `$${n.toLocaleString(undefined, { maximumFractionDigits: n < 1 ? 6 : 0 })}`;

export const ago = (iso: string | null | undefined): string => {
  if (!iso) return '—';
  const s = Math.round((Date.now() - new Date(iso).getTime()) / 1000);
  if (s < 60) return `${s}s ago`;
  if (s < 3600) return `${Math.round(s / 60)}m ago`;
  if (s < 86400) return `${Math.round(s / 3600)}h ago`;
  return `${Math.round(s / 86400)}d ago`;
};

export const short = (a: string | null | undefined): string => (a ? `${a.slice(0, 4)}…${a.slice(-4)}` : '—');

export const pnlClass = (v: string | number | null | undefined): string => (v === null || v === undefined ? '' : Number(v) >= 0 ? 'pos' : 'neg');
