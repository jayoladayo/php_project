import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { AuditLog, redact, registerSecret } from '../src/server/audit.js';

describe('redaction', () => {
  it('removes secret-looking keys and values but keeps public signatures', () => {
    registerSecret('super-secret-helius-key');
    const sig = '5'.repeat(88);
    const out = redact({
      privateKey: 'abc',
      seedPhrase: 'x',
      apiKey: 'y',
      rpc: 'https://mainnet.helius-rpc.com/?api-key=super-secret-helius-key',
      other: 'calling https://api.example.com/x?token=abc123&q=1',
      keypair: Array.from({ length: 64 }, (_, i) => i),
      note: 'abandon ability able about above absent absorb abstract absurd abuse access accident',
      leaked: '4'.repeat(88),
      signature: sig,
      nested: { authorization: 'Bearer zzz', mint: 'So11111111111111111111111111111111111111112' },
    }) as Record<string, unknown>;
    expect(out.privateKey).toBe('[REDACTED]');
    expect(out.seedPhrase).toBe('[REDACTED]');
    expect(out.apiKey).toBe('[REDACTED]');
    expect(out.rpc).toBe('https://mainnet.helius-rpc.com/?api-key=[REDACTED]');
    expect(out.other).toBe('calling https://api.example.com/x?token=[REDACTED]&q=1');
    expect(out.keypair).toBe('[REDACTED]');
    expect(out.note).toBe('[REDACTED]');
    expect(out.leaked).toBe('[REDACTED]');
    expect(out.signature).toBe(sig);
    expect(out.nested).toEqual({ authorization: '[REDACTED]', mint: 'So11111111111111111111111111111111111111112' });
  });
});

describe('AuditLog', () => {
  it('appends a hash chain and detects tampering', () => {
    const path = join(mkdtempSync(join(tmpdir(), 'audit-')), 'audit.jsonl');
    const log = new AuditLog(path);
    log.append('a', { x: 1 });
    log.append('b', { secretKey: 'nope' });
    expect(log.verify()).toBeNull();
    expect(readFileSync(path, 'utf8')).not.toContain('nope');
    // Reopen continues the chain.
    new AuditLog(path).append('c', {});
    expect(new AuditLog(path).verify()).toBeNull();
    const lines = readFileSync(path, 'utf8').trim().split('\n');
    lines[1] = lines[1]!.replace('"b"', '"B"');
    writeFileSync(path, lines.join('\n') + '\n');
    expect(new AuditLog(path).verify()).toBe(2);
  });
});
