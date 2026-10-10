// The bot's own hot wallet, for unattended trading.
//
// The key is generated here (the user never pastes a seed phrase or private
// key), encrypted at rest with a passphrase (scrypt → AES-256-GCM), and only
// held in memory while unlocked. It is used to sign exactly two things: swap
// transactions that already passed the allowlist check and simulation, and
// SOL withdrawals to the owner's wallet. There is no API to sign anything else
// and no API that returns the key; exporting it is a local command-line step.
import { createCipheriv, createDecipheriv, randomBytes, scryptSync, timingSafeEqual } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';
import { Keypair, type VersionedTransaction } from '@solana/web3.js';

export const MIN_PASSPHRASE_LENGTH = 12;

interface WalletFile {
  version: 1;
  publicKey: string;
  createdAt: string;
  kdf: { name: 'scrypt'; N: number; r: number; p: number; salt: string };
  cipher: { name: 'aes-256-gcm'; iv: string; tag: string; ciphertext: string };
}

// N=2^15 keeps unlock under a second on a laptop while making guessing costly.
const KDF = { N: 1 << 15, r: 8, p: 1 };

function deriveKey(passphrase: string, salt: Buffer, params: { N: number; r: number; p: number }): Buffer {
  return scryptSync(passphrase.normalize('NFKC'), salt, 32, { ...params, maxmem: 128 * params.N * params.r * 2 });
}

export class WrongPassphraseError extends Error {
  constructor() {
    super('Wrong passphrase.');
  }
}

export class BotWallet {
  private keypair: Keypair | null = null;

  constructor(private readonly path: string) {}

  exists(): boolean {
    return existsSync(this.path);
  }

  private read(): WalletFile {
    const f = JSON.parse(readFileSync(this.path, 'utf8')) as WalletFile;
    if (f.version !== 1 || f.kdf?.name !== 'scrypt' || f.cipher?.name !== 'aes-256-gcm') throw new Error('Unsupported bot wallet file.');
    return f;
  }

  publicKey(): string | null {
    return this.exists() ? this.read().publicKey : null;
  }

  isUnlocked(): boolean {
    return this.keypair !== null;
  }

  /** Generate a new wallet. Refuses to overwrite an existing one (it may hold funds). */
  create(passphrase: string): string {
    if (this.exists()) throw new Error('A bot wallet already exists. It is never overwritten, because it may hold funds.');
    if (passphrase.length < MIN_PASSPHRASE_LENGTH) throw new Error(`Passphrase must be at least ${MIN_PASSPHRASE_LENGTH} characters.`);
    const kp = Keypair.generate();
    const salt = randomBytes(16);
    const iv = randomBytes(12);
    const key = deriveKey(passphrase, salt, KDF);
    const c = createCipheriv('aes-256-gcm', key, iv);
    const ciphertext = Buffer.concat([c.update(Buffer.from(kp.secretKey)), c.final()]);
    const file: WalletFile = {
      version: 1,
      publicKey: kp.publicKey.toBase58(),
      createdAt: new Date().toISOString(),
      kdf: { name: 'scrypt', ...KDF, salt: salt.toString('base64') },
      cipher: { name: 'aes-256-gcm', iv: iv.toString('base64'), tag: c.getAuthTag().toString('base64'), ciphertext: ciphertext.toString('base64') },
    };
    mkdirSync(dirname(this.path), { recursive: true });
    writeFileSync(this.path, JSON.stringify(file, null, 2), { mode: 0o600, flag: 'wx' });
    key.fill(0);
    this.keypair = kp;
    return file.publicKey;
  }

  /** Decrypt the key without keeping it (used to authorise withdrawals and exports). */
  decrypt(passphrase: string): Keypair {
    const f = this.read();
    const key = deriveKey(passphrase, Buffer.from(f.kdf.salt, 'base64'), f.kdf);
    try {
      const d = createDecipheriv('aes-256-gcm', key, Buffer.from(f.cipher.iv, 'base64'));
      d.setAuthTag(Buffer.from(f.cipher.tag, 'base64'));
      const secret = Buffer.concat([d.update(Buffer.from(f.cipher.ciphertext, 'base64')), d.final()]);
      const kp = Keypair.fromSecretKey(new Uint8Array(secret));
      secret.fill(0);
      const a = Buffer.from(kp.publicKey.toBase58());
      const b = Buffer.from(f.publicKey);
      if (a.length !== b.length || !timingSafeEqual(a, b)) throw new Error('Bot wallet file is corrupted (public key mismatch).');
      return kp;
    } catch (e) {
      if (e instanceof Error && e.message.includes('corrupted')) throw e;
      throw new WrongPassphraseError();
    } finally {
      key.fill(0);
    }
  }

  unlock(passphrase: string): void {
    this.keypair = this.decrypt(passphrase);
  }

  lock(): void {
    this.keypair = null;
  }

  /** Sign a transaction whose fee payer is this wallet. Callers must have verified it first. */
  sign(tx: VersionedTransaction): VersionedTransaction {
    if (!this.keypair) throw new Error('Bot wallet is locked.');
    const payer = tx.message.staticAccountKeys[0]?.toBase58();
    if (payer !== this.keypair.publicKey.toBase58()) throw new Error('Refusing to sign: the bot wallet is not the fee payer.');
    tx.sign([this.keypair]);
    return tx;
  }
}
