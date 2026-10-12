import { mkdtempSync, readFileSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import bs58 from 'bs58';
import { Keypair, SystemProgram, TransactionMessage, VersionedTransaction } from '@solana/web3.js';
import { describe, expect, it } from 'vitest';
import { BotWallet, WrongPassphraseError } from '../src/server/wallet/botWallet.js';

const PASS = 'correct horse battery staple';
const fresh = () => new BotWallet(join(mkdtempSync(join(tmpdir(), 'botw-')), 'bot-wallet.json'));

describe('BotWallet', () => {
  it('generates a key, stores it encrypted, and never in plain text', () => {
    const w = fresh();
    expect(w.exists()).toBe(false);
    const pub = w.create(PASS);
    expect(w.exists()).toBe(true);
    expect(w.isUnlocked()).toBe(true);
    expect(w.publicKey()).toBe(pub);
    const path = (w as unknown as { path: string }).path;
    const raw = readFileSync(path, 'utf8');
    const kp = w.decrypt(PASS);
    expect(raw).not.toContain(bs58.encode(kp.secretKey));
    expect(raw).not.toContain(Buffer.from(kp.secretKey).toString('base64'));
    if (process.platform !== 'win32') expect(statSync(path).mode & 0o777).toBe(0o600);
  });

  it('rejects short passphrases and never overwrites an existing wallet', () => {
    const w = fresh();
    expect(() => w.create('short')).toThrow(/at least/);
    w.create(PASS);
    expect(() => w.create(PASS)).toThrow(/already exists/);
  });

  it('unlocks with the right passphrase only', () => {
    const w = fresh();
    const pub = w.create(PASS);
    w.lock();
    expect(w.isUnlocked()).toBe(false);
    expect(() => w.unlock('wrong passphrase!!')).toThrow(WrongPassphraseError);
    expect(w.isUnlocked()).toBe(false);
    w.unlock(PASS);
    expect(w.isUnlocked()).toBe(true);
    expect(w.decrypt(PASS).publicKey.toBase58()).toBe(pub);
  });

  it('signs only transactions it pays for, and only while unlocked', () => {
    const w = fresh();
    const pub = w.decrypt.bind(w);
    w.create(PASS);
    const me = pub(PASS).publicKey;
    const build = (payer = me) =>
      new VersionedTransaction(
        new TransactionMessage({ payerKey: payer, recentBlockhash: Keypair.generate().publicKey.toBase58(), instructions: [SystemProgram.transfer({ fromPubkey: payer, toPubkey: Keypair.generate().publicKey, lamports: 1 })] }).compileToV0Message(),
      );
    const signed = w.sign(build());
    expect(signed.signatures[0]!.some((b) => b !== 0)).toBe(true);
    expect(() => w.sign(build(Keypair.generate().publicKey))).toThrow(/not the fee payer/);
    w.lock();
    expect(() => w.sign(build())).toThrow(/locked/);
  });
});
