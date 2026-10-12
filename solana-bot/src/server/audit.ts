// Append-only, hash-chained JSONL audit log. Every entry is redacted before it
// is written so secrets (API keys, anything resembling a private key or seed
// phrase) never reach disk.
import { createHash } from 'node:crypto';
import { appendFileSync, existsSync, mkdirSync, readFileSync } from 'node:fs';
import { dirname } from 'node:path';

const SECRET_KEY = /(secret|private.?key|seed|mnemonic|passphrase|password|api.?key|authorization|bearer|cookie)/i;
const URL_SECRET_PARAM = /([?&](?:api[-_]?key|apikey|key|token|access_token|auth)=)[^&#\s]+/gi;
// A base58 string of 64–90 chars is the shape of an encoded secret key.
const BASE58_SECRET = /\b[1-9A-HJ-NP-Za-km-z]{64,90}\b/g;
// A JSON-style byte array of 64 numbers is the shape of a keypair file.
const BYTE_ARRAY_KEY = /\[\s*(?:\d{1,3}\s*,\s*){63}\d{1,3}\s*\]/g;
const REDACTED = '[REDACTED]';

const registeredSecrets = new Set<string>();

/** Register runtime secret values (e.g. API keys from env) so they are scrubbed wherever they appear. */
export function registerSecret(value: string | undefined): void {
  if (value && value.length >= 8) registeredSecrets.add(value);
}

function looksLikeMnemonic(s: string): boolean {
  const words = s.trim().split(/\s+/);
  return (words.length === 12 || words.length === 24) && words.every((w) => /^[a-z]{3,8}$/.test(w));
}

export function redactString(s: string): string {
  let out = s;
  for (const secret of registeredSecrets) out = out.split(secret).join(REDACTED);
  out = out.replace(URL_SECRET_PARAM, `$1${REDACTED}`);
  // Transaction signatures are 87–88 base58 chars too; they are public and
  // essential for the audit trail, so only scrub them outside known keys.
  out = out.replace(BYTE_ARRAY_KEY, REDACTED);
  if (looksLikeMnemonic(out)) return REDACTED;
  return out;
}

const PUBLIC_LONG_FIELDS = new Set(['signature', 'signatures', 'txBase64', 'signedTx', 'messageHashHex', 'logs']);

export function redact(value: unknown, key = '', depth = 0): unknown {
  if (depth > 20) return '[TRUNCATED]';
  if (key && SECRET_KEY.test(key)) return REDACTED;
  if (typeof value === 'bigint') return value.toString();
  if (typeof value === 'string') {
    const s = redactString(value);
    return PUBLIC_LONG_FIELDS.has(key) ? s : s.replace(BASE58_SECRET, REDACTED);
  }
  if (Array.isArray(value)) {
    if (value.length === 64 && value.every((n) => Number.isInteger(n) && n >= 0 && n <= 255)) return REDACTED;
    return value.map((v) => redact(v, PUBLIC_LONG_FIELDS.has(key) ? key : '', depth + 1));
  }
  if (value && typeof value === 'object') {
    if (value instanceof Error) return { name: value.name, message: redactString(value.message) };
    if (value instanceof Uint8Array) return value.length === 64 ? REDACTED : `[${value.length} bytes]`;
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value)) out[k] = redact(v, k, depth + 1);
    return out;
  }
  return value;
}

export interface AuditEntry {
  seq: number;
  ts: string;
  event: string;
  data: unknown;
  prevHash: string;
  hash: string;
}

const hashOf = (e: Omit<AuditEntry, 'hash'>): string =>
  createHash('sha256').update(JSON.stringify([e.seq, e.ts, e.event, e.data, e.prevHash])).digest('hex');

export class AuditLog {
  private seq = 0;
  private prevHash = 'genesis';

  constructor(private readonly path: string) {
    mkdirSync(dirname(path), { recursive: true });
    if (existsSync(path)) {
      const lines = readFileSync(path, 'utf8').trim().split('\n').filter(Boolean);
      const last = lines.at(-1);
      if (last) {
        const e = JSON.parse(last) as AuditEntry;
        this.seq = e.seq;
        this.prevHash = e.hash;
      }
    }
  }

  append(event: string, data: unknown): AuditEntry {
    const base = { seq: this.seq + 1, ts: new Date().toISOString(), event, data: redact(data), prevHash: this.prevHash };
    const entry: AuditEntry = { ...base, hash: hashOf(base) };
    appendFileSync(this.path, JSON.stringify(entry) + '\n', { mode: 0o600 });
    this.seq = entry.seq;
    this.prevHash = entry.hash;
    return entry;
  }

  tail(limit: number): AuditEntry[] {
    if (!existsSync(this.path)) return [];
    const lines = readFileSync(this.path, 'utf8').trim().split('\n').filter(Boolean);
    return lines.slice(-limit).map((l) => JSON.parse(l) as AuditEntry);
  }

  /** Recompute the hash chain; returns the first broken sequence number, or null if intact. */
  verify(): number | null {
    if (!existsSync(this.path)) return null;
    let prev = 'genesis';
    for (const line of readFileSync(this.path, 'utf8').trim().split('\n').filter(Boolean)) {
      const e = JSON.parse(line) as AuditEntry;
      const { hash, ...rest } = e;
      if (e.prevHash !== prev || hashOf(rest) !== hash) return e.seq;
      prev = hash;
    }
    return null;
  }
}
