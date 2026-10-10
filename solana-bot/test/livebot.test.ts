// The live, unattended path with the bot wallet — the one that moves real
// money — exercised end to end against a fake RPC node and a fake swap
// router that build real Solana transactions: prepare → allowlist check →
// simulation → bot signature (verified with ed25519) → send → confirm →
// accounting; plus autopilot, a tampered route, locking, and withdrawals.
import { createPublicKey, verify } from 'node:crypto';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import bs58 from 'bs58';
import {
  createApproveInstruction,
  createAssociatedTokenAccountIdempotentInstruction,
  createCloseAccountInstruction,
  createSyncNativeInstruction,
  getAssociatedTokenAddressSync,
} from '@solana/spl-token';
import { ComputeBudgetProgram, Keypair, PublicKey, SystemProgram, TransactionInstruction, TransactionMessage, VersionedTransaction, type Connection } from '@solana/web3.js';
import { beforeEach, describe, expect, it } from 'vitest';
import { Alerts } from '../src/server/alerts.js';
import { AuditLog } from '../src/server/audit.js';
import { env } from '../src/server/env.js';
import type { BuiltSwap, Quote, QuoteParams, SwapProvider } from '../src/server/market/types.js';
import { Researcher } from '../src/server/research/index.js';
import type { Scanner } from '../src/server/research/scanner.js';
import { assessReport, type ReportDraft } from '../src/server/research/verify.js';
import { Store } from '../src/server/store.js';
import { Autopilot } from '../src/server/trading/autopilot.js';
import { Engine, TradeRejected } from '../src/server/trading/engine.js';
import { PROGRAMS } from '../src/server/trading/txverify.js';
import { BotWallet, WrongPassphraseError } from '../src/server/wallet/botWallet.js';
import { DEFAULT_SETTINGS } from '../src/shared/defaults.js';
import { WSOL_MINT, type ResearchReport, type Sourced } from '../src/shared/types.js';

const PASS = 'a long test passphrase';
const SOL = 1_000_000_000n;
const OUT_TOKENS = 1_000_000_000n; // 1000 tokens at 6 decimals
const PRIORITY = 10_000n; // 50,000 µL × 200,000 CU
const RENT = 2_039_280n;
const wsol = new PublicKey(WSOL_MINT);

/** What the fake router built last, so the fake node can play it back. */
interface World {
  balance: bigint;
  last: { user: PublicKey; mint: PublicKey; inLamports: bigint } | null;
  sent: VersionedTransaction[];
  injectApprove: boolean;
}

class FakeRouter implements SwapProvider {
  readonly name = 'FakeRouter';
  readonly mock = false;
  constructor(private readonly w: World) {}
  async quote(p: QuoteParams): Promise<Quote> {
    const out = p.inputMint === WSOL_MINT ? OUT_TOKENS : 0n;
    const fetchedAt = new Date();
    return {
      raw: p,
      fetchedAt,
      summary: { inputMint: p.inputMint, outputMint: p.outputMint, inAmount: p.amountRaw.toString(), outAmount: out.toString(), minOutAmount: ((out * 99n) / 100n).toString(), slippageBps: p.slippageBps, priceImpactPct: 0.1, routeLabels: ['Fake (100%)'], platformFeeRaw: '0', fetchedAt: fetchedAt.toISOString(), source: 'fake' },
    };
  }
  async buildSwap(q: Quote, userPublicKey: string): Promise<BuiltSwap> {
    const user = new PublicKey(userPublicKey);
    const mint = new PublicKey(q.summary.outputMint);
    const inLamports = BigInt(q.summary.inAmount);
    const wsolAta = getAssociatedTokenAddressSync(wsol, user);
    const tokenAta = getAssociatedTokenAddressSync(mint, user);
    const ixs = [
      ComputeBudgetProgram.setComputeUnitLimit({ units: 200_000 }),
      ComputeBudgetProgram.setComputeUnitPrice({ microLamports: 50_000 }),
      createAssociatedTokenAccountIdempotentInstruction(user, wsolAta, user, wsol),
      SystemProgram.transfer({ fromPubkey: user, toPubkey: wsolAta, lamports: inLamports }),
      createSyncNativeInstruction(wsolAta),
      createAssociatedTokenAccountIdempotentInstruction(user, tokenAta, user, mint),
      new TransactionInstruction({ programId: new PublicKey(PROGRAMS.jupiterV6), keys: [user, wsolAta, tokenAta, mint, wsol].map((pubkey) => ({ pubkey, isSigner: false, isWritable: true })), data: Buffer.from([9]) }),
      createCloseAccountInstruction(wsolAta, user, user),
      ...(this.w.injectApprove ? [createApproveInstruction(tokenAta, Keypair.generate().publicKey, user, 1n << 60n)] : []),
    ];
    const msg = new TransactionMessage({ payerKey: user, recentBlockhash: Keypair.generate().publicKey.toBase58(), instructions: ixs }).compileToV0Message();
    this.w.last = { user, mint, inLamports };
    return { txBase64: Buffer.from(new VersionedTransaction(msg).serialize()).toString('base64'), lastValidBlockHeight: 1000, prioritizationFeeLamports: Number(PRIORITY) };
  }
}

function tokenAccountData(amount: bigint): string {
  const b = Buffer.alloc(165);
  b.writeBigUInt64LE(amount, 64);
  return b.toString('base64');
}

function fakeConnection(w: World): Connection {
  const spent = () => w.last!.inLamports + 5_000n + PRIORITY + RENT;
  return {
    getBalance: async () => Number(w.balance),
    getMultipleAccountsInfo: async () => [{ lamports: Number(w.balance), data: Buffer.alloc(0), owner: SystemProgram.programId, executable: false, rentEpoch: 0 }, null],
    simulateTransaction: async () => ({
      context: { slot: 1 },
      value: {
        err: null,
        logs: ['Program log: fake swap'],
        unitsConsumed: 90_000,
        accounts: [
          { lamports: Number(w.balance - spent()), data: [Buffer.alloc(0).toString('base64'), 'base64'], owner: SystemProgram.programId.toBase58(), executable: false, rentEpoch: 0 },
          { lamports: Number(RENT), data: [tokenAccountData(OUT_TOKENS), 'base64'], owner: PROGRAMS.token, executable: false, rentEpoch: 0 },
        ],
      },
    }),
    sendRawTransaction: async (bytes: Uint8Array) => {
      const tx = VersionedTransaction.deserialize(bytes);
      w.sent.push(tx);
      return bs58.encode(tx.signatures[0]!);
    },
    confirmTransaction: async () => ({ context: { slot: 2 }, value: { err: null } }),
    getTransaction: async (sig: string) => ({
      slot: 2,
      blockTime: Math.floor(Date.now() / 1000),
      transaction: { signatures: [sig] },
      meta: {
        err: null,
        fee: Number(5_000n + PRIORITY),
        preBalances: [Number(w.balance)],
        postBalances: [Number(w.balance - spent())],
        preTokenBalances: [],
        postTokenBalances: [{ accountIndex: 1, mint: w.last!.mint.toBase58(), owner: w.last!.user.toBase58(), uiTokenAmount: { amount: OUT_TOKENS.toString(), decimals: 6, uiAmount: 1000, uiAmountString: '1000' } }],
      },
    }),
    getSignatureStatus: async () => ({ context: { slot: 2 }, value: { err: null, confirmations: 1, slot: 2, confirmationStatus: 'confirmed' } }),
    getBlockHeight: async () => 100,
    getLatestBlockhash: async () => ({ blockhash: Keypair.generate().publicKey.toBase58(), lastValidBlockHeight: 1000 }),
  } as unknown as Connection;
}

/** A report that passes verification and the default autopilot filters. */
function verifiedReport(mint: string): ResearchReport {
  const at = new Date().toISOString();
  const s = <T>(value: T | null): Sourced<T> => ({ value, source: 'test', fetchedAt: at });
  const quote = (inputMint: string, outputMint: string, inAmount: string, outAmount: string) => ({ inputMint, outputMint, inAmount, outAmount, minOutAmount: '1', slippageBps: 50, priceImpactPct: 0.1, routeLabels: [], platformFeeRaw: '0', fetchedAt: at, source: 't' });
  const draft: ReportDraft = {
    mint,
    generatedAt: at,
    mock: false,
    token: {
      name: s('Live Test'), symbol: s('LIVE'), decimals: s(6), supplyRaw: s('1000000000000000'), tokenProgram: s(PROGRAMS.token),
      mintAuthority: s<string>('none'), freezeAuthority: s<string>('none'), extensions: s<string[]>([]),
      createdAt: s('2026-10-01T00:00:00Z'), ageMinutes: s(3000), creator: s('C'),
    },
    market: {
      priceUsd: s(0.003), priceSol: s(0.00002), liquidityUsd: s(250_000), volume24hUsd: s(1e6), marketCapUsd: s(3e6), fdvUsd: s(3e6),
      txns24h: s({ buys: 600, sells: 500 }), priceChange: s({ h1: 2 }), priceHistory: s([]), pairAddress: s('P'), dex: s('raydium'), lpLockedPct: s(100),
    },
    holders: { count: s(5000), top: s([]), top10Pct: s(18), maxSingleWalletPct: s(3) },
    sellability: {
      buyQuote: s(quote(WSOL_MINT, mint, '10000000', '500000000')),
      sellQuote: s(quote(mint, WSOL_MINT, '500000000', '9800000')),
      roundTripLossPct: s(2),
    },
    bundles: s({ signals: [], earlyBuyers: 3, sharedFunders: [] }),
    social: s<never>(null),
    thirdPartyFlags: s([]),
    sources: [],
  };
  const r = assessReport(draft, DEFAULT_SETTINGS.verification, new Date());
  expect(r.verdict).toBe('verified');
  return r;
}

let w: World;
let store: Store;
let engine: Engine;
let bot: BotWallet;
let autopilot: Autopilot;
let owner: string;
let scannerMints: string[];

beforeEach(() => {
  env.liveTradingAllowed = true;
  const dir = mkdtempSync(join(tmpdir(), 'livebot-'));
  w = { balance: SOL, last: null, sent: [], injectApprove: false };
  store = new Store(join(dir, 'state.json'));
  const audit = new AuditLog(join(dir, 'audit.jsonl'));
  const alerts = new Alerts(store);
  const conn = fakeConnection(w);
  const router = new FakeRouter(w);
  bot = new BotWallet(join(dir, 'bot-wallet.json'));
  bot.create(PASS);
  // Research is stubbed (the real collectors are covered elsewhere and need the network): it
  // returns a verified report for any mint, so this file exercises only the live trading path.
  const researcher = { research: async (mint: string) => verifiedReport(mint) } as unknown as Researcher;
  engine = new Engine(store, audit, alerts, router, researcher, conn, bot);
  scannerMints = [];
  const scanner: Scanner = {
    scan: async () => ({ candidates: scannerMints.map((mint) => ({ mint, symbol: 'LIVE', source: 'new_pools' as const, poolAddress: 'P', dex: 'raydium', liquidityUsd: 250_000, volume24hUsd: 1e6, ageMinutes: 3000, priceChange1hPct: 2 })), errors: [] }),
  };
  autopilot = new Autopilot(store, audit, alerts, engine, scanner, bot);
  owner = Keypair.generate().publicKey.toBase58();
});

const newVerifiedMint = () => {
  const mint = Keypair.generate().publicKey.toBase58();
  store.state.research[mint] = verifiedReport(mint);
  return mint;
};

const armBot = async () => {
  engine.bindWallet(owner, PASS);
  await engine.armLive('bot', null, 'ENABLE LIVE TRADING');
};

const ed25519Valid = (tx: VersionedTransaction, signer: string) =>
  verify(null, Buffer.from(tx.message.serialize()), createPublicKey({ key: { kty: 'OKP', crv: 'Ed25519', x: Buffer.from(new PublicKey(signer).toBytes()).toString('base64url') }, format: 'jwk' }), Buffer.from(tx.signatures[0]!));

describe('live trading with the bot wallet', () => {
  it('requires your own wallet bound (with the passphrase) and the bot wallet unlocked', async () => {
    expect(() => engine.bindWallet(owner)).toThrow(/passphrase/);
    expect(() => engine.bindWallet(owner, 'wrong passphrase')).toThrow(WrongPassphraseError);
    expect(() => engine.bindWallet(bot.publicKey(), PASS)).toThrow(/bot wallet itself/);
    bot.lock();
    await expect(engine.armLive('bot', null, 'ENABLE LIVE TRADING')).rejects.toMatchObject({
      violations: expect.arrayContaining([expect.stringMatching(/Unlock the bot wallet/), expect.stringMatching(/Bind your own wallet/)]),
    });
    bot.unlock(PASS);
    await armBot();
    expect(store.state.mode).toBe('live');
    expect(store.state.live).toMatchObject({ signer: 'bot', wallet: bot.publicKey() });
  });

  it('prepares, verifies, simulates, signs with the bot key, sends, confirms and books the fill', async () => {
    await armBot();
    const mint = newVerifiedMint();
    const intent = await engine.prepare({ side: 'buy', mint, amountSol: '0.02', slippageBps: 100, wallet: owner /* ignored: bot signs */ });
    expect(intent.wallet).toBe(bot.publicKey());
    expect(intent.summary.simulation).toMatchObject({ ok: true });
    expect(intent.summary.permissions.join(' ')).toMatch(/NO token approvals/);
    expect(w.sent).toHaveLength(0); // nothing signed or sent yet

    const { position } = await engine.executeWithBot(intent.id);
    expect(w.sent).toHaveLength(1);
    expect(ed25519Valid(w.sent[0]!, bot.publicKey()!)).toBe(true);
    expect(w.sent[0]!.message.staticAccountKeys[0]!.toBase58()).toBe(bot.publicKey());
    expect(position).toMatchObject({ mint, wallet: bot.publicKey(), mode: 'live', originalTokenRaw: OUT_TOKENS.toString(), entrySwapLamports: '20000000' });
    expect(position!.fills[0]).toMatchObject({ networkFeeLamports: '5000', priorityFeeLamports: PRIORITY.toString(), rentLamports: RENT.toString() });
    expect(store.state.intents.find((i) => i.id === intent.id)?.status).toBe('confirmed');
  });

  it('refuses to sign a route that asks for a token approval — nothing is signed or sent', async () => {
    await armBot();
    w.injectApprove = true;
    await expect(engine.prepare({ side: 'buy', mint: newVerifiedMint(), amountSol: '0.02', slippageBps: 100 })).rejects.toMatchObject({
      violations: expect.arrayContaining([expect.stringMatching(/Approve/)]),
    });
    expect(w.sent).toHaveLength(0);
  });

  it('does nothing while the bot wallet is locked', async () => {
    await armBot();
    const intent = await engine.prepare({ side: 'buy', mint: newVerifiedMint(), amountSol: '0.02', slippageBps: 100 });
    bot.lock();
    await expect(engine.executeWithBot(intent.id)).rejects.toThrow(/locked/);
    expect(engine.autopilotActive()).toBe(false);
    expect(w.sent).toHaveLength(0);
  });

  it('live autopilot: skips tokens whose research is not verified', async () => {
    await armBot();
    const unverified = { research: async (mint: string) => ({ ...verifiedReport(mint), verdict: 'not_verified', verdictReasons: ['Missing key data: liquidity.'] }) } as unknown as Researcher;
    const strict = new Engine(store, new AuditLog(join(mkdtempSync(join(tmpdir(), 'a-')), 'a.jsonl')), new Alerts(store), new FakeRouter(w), unverified, fakeConnection(w), bot);
    const ap = new Autopilot(store, new AuditLog(join(mkdtempSync(join(tmpdir(), 'b-')), 'b.jsonl')), new Alerts(store), strict, { scan: async () => ({ candidates: [{ mint: Keypair.generate().publicKey.toBase58(), symbol: null, source: 'new_pools', poolAddress: null, dex: null, liquidityUsd: null, volume24hUsd: null, ageMinutes: null, priceChange1hPct: null }], errors: [] }) }, bot);
    store.state.settings.autopilot.acknowledgeNoStopLoss = true;
    ap.enable('START AUTOPILOT');
    await ap.cycle();
    expect(store.state.positions).toHaveLength(0);
    expect(w.sent).toHaveLength(0);
    expect(store.state.autopilot.log.at(-1)).toMatchObject({ action: 'skip' });
    expect(store.state.autopilot.log.at(-1)!.detail).toMatch(/not verified: Missing key data/);
  });

  it('live autopilot: needs the typed confirmation and a stop-loss decision, then buys unattended', async () => {
    await armBot();
    const mint = newVerifiedMint();
    scannerMints = [mint];
    expect(() => autopilot.enable()).toThrow(TradeRejected); // no phrase
    expect(autopilot.startProblems('START AUTOPILOT').join(' ')).toMatch(/stop-loss/);
    store.state.settings.stopLoss = { ...store.state.settings.stopLoss, enabled: true, dropPct: 50 };
    autopilot.enable('START AUTOPILOT');
    expect(engine.autopilotActive()).toBe(true);
    await autopilot.cycle();
    expect(store.state.positions.map((p) => p.mint)).toEqual([mint]);
    expect(store.state.positions[0]!.fills[0]!.reason).toBe('autopilot');
    expect(w.sent).toHaveLength(1);
    expect(ed25519Valid(w.sent[0]!, bot.publicKey()!)).toBe(true);
    // switching back to paper stops the autopilot
    engine.disarmLive();
    expect(store.state.settings.autopilot.enabled).toBe(false);
  });

  it('withdraws only to your own wallet, only with the passphrase, never leaving dust below rent', async () => {
    engine.bindWallet(owner, PASS);
    await expect(engine.withdraw('wrong passphrase', '0.1')).rejects.toThrow(WrongPassphraseError);
    await expect(engine.withdraw(PASS, String((Number(SOL) - 5_000 - 100) / 1e9))).rejects.toThrow(/minimum account balance/);
    const r = await engine.withdraw(PASS, '0.1');
    expect(r.to).toBe(owner);
    const tx = w.sent.at(-1)!;
    expect(ed25519Valid(tx, bot.publicKey()!)).toBe(true);
    const keys = tx.message.staticAccountKeys.map((k) => k.toBase58());
    expect(keys).toContain(owner);
    const ix = tx.message.compiledInstructions[0]!;
    expect(keys[ix.programIdIndex]).toBe(SystemProgram.programId.toBase58());
    expect(Buffer.from(ix.data).readBigUInt64LE(4)).toBe(100_000_000n);
    const all = await engine.withdraw(PASS, 'all');
    expect(all.lamports).toBe((SOL - 5_000n).toString());
  });
});
