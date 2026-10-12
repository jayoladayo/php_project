// fetch wrapper: per-host rate limiting, timeouts, bounded retries on 429/5xx
// with Retry-After support, and redacted error messages.
import { redactString } from './audit.js';

export class HttpError extends Error {
  constructor(
    message: string,
    readonly status: number | null,
  ) {
    super(message);
  }
}

const nextSlot = new Map<string, number>();
const minIntervalMs = new Map<string, number>();

/** Minimum spacing between requests to a host (defaults to 250 ms). */
export function setHostInterval(host: string, ms: number): void {
  minIntervalMs.set(host, ms);
}

export async function waitForSlot(host: string): Promise<void> {
  const interval = minIntervalMs.get(host) ?? 250;
  const now = Date.now();
  const slot = Math.max(now, nextSlot.get(host) ?? 0);
  nextSlot.set(host, slot + interval);
  if (slot > now) await new Promise((r) => setTimeout(r, slot - now));
}

export interface FetchOpts {
  method?: 'GET' | 'POST';
  headers?: Record<string, string>;
  body?: unknown;
  timeoutMs?: number;
  retries?: number;
}

export async function fetchJson<T>(url: string, opts: FetchOpts = {}): Promise<T> {
  const host = new URL(url).host;
  const retries = opts.retries ?? 2;
  let lastErr: unknown;
  for (let attempt = 0; attempt <= retries; attempt++) {
    await waitForSlot(host);
    try {
      const res = await fetch(url, {
        method: opts.method ?? 'GET',
        headers: { accept: 'application/json', ...(opts.body ? { 'content-type': 'application/json' } : {}), ...opts.headers },
        body: opts.body ? JSON.stringify(opts.body) : undefined,
        signal: AbortSignal.timeout(opts.timeoutMs ?? 10_000),
      });
      if (res.ok) return (await res.json()) as T;
      const text = (await res.text().catch(() => '')).slice(0, 300);
      const err = new HttpError(redactString(`${res.status} from ${url}: ${text}`), res.status);
      if (res.status === 429 || res.status >= 500) {
        lastErr = err;
        const ra = Number(res.headers.get('retry-after'));
        await sleep(Number.isFinite(ra) && ra > 0 ? Math.min(ra * 1000, 10_000) : 500 * 2 ** attempt);
        continue;
      }
      throw err;
    } catch (e) {
      if (e instanceof HttpError && e.status !== null && e.status !== 429 && e.status < 500) throw e;
      lastErr = e instanceof HttpError ? e : new HttpError(redactString(`request to ${url} failed: ${(e as Error).message}`), null);
      if (attempt < retries) await sleep(500 * 2 ** attempt);
    }
  }
  throw lastErr;
}

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));
