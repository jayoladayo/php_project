// On-chain facts via Solana JSON-RPC: mint authorities, Token-2022
// extensions, largest holders, token age/creator, early receipts, and the
// first funder of top holders. All calls are bounded.
import { Connection, PublicKey, type ParsedTransactionWithMeta } from '@solana/web3.js';
import { rpcHost } from '../env.js';
import { waitForSlot } from '../http.js';
import type { HolderInfo } from '../../shared/types.js';
import type { EarlyReceipt, FundingRecord } from './bundles.js';

export function makeConnection(url: string): Connection {
  const host = rpcHost(url);
  return new Connection(url, {
    commitment: 'confirmed',
    confirmTransactionInitialTimeout: 60_000,
    fetchMiddleware: (info, init, fetch) => {
      void waitForSlot(host).then(() => fetch(info, init));
    },
  });
}

export const TOKEN_PROGRAM = 'TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA';
export const TOKEN_2022_PROGRAM = 'TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb';
const BURN_OWNERS = new Set(['1nc1nerator11111111111111111111111111111111', '11111111111111111111111111111111']);
const KNOWN_PROGRAMS: Record<string, string> = {
  '6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P': 'pump.fun bonding curve',
  pAMMBay6oceH9fJKBRHGP5D4bD4sWpmSwMn52FMfXEA: 'PumpSwap pool',
  '675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8': 'Raydium AMM v4',
  CPMMoo8L3F4NbTegBCKVNunggL7H1ZpdTHKxQB5qKP1C: 'Raydium CPMM',
  CAMMCzo5YL8w4VFF8KVHrK22GGUsp5VTaW7grrKgrWqK: 'Raydium CLMM',
  LBUZKhRxPF3XUpBCjp4YzTKgLccjZhTSDM9YuVaPwxo: 'Meteora DLMM',
  Eo7WjKq67rjJQSZxS6z3YkapzY3eMj6Xy8X5EQVn5UaB: 'Meteora pools',
  cpamdpZCGKUy5JxQXB4dcpGPiikHawvSWAd6mEn1sGG: 'Meteora DAMM v2',
  whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc: 'Orca Whirlpool',
  strmRqUCoQUgGUan5YhzUZa6KqdzwX5L6FpUxfmKg5m: 'Streamflow (lock/vesting)',
};
// Extensions that carry no transfer risk on their own.
const BENIGN_EXTENSIONS = new Set(['metadataPointer', 'tokenMetadata', 'groupPointer', 'groupMemberPointer', 'tokenGroup', 'tokenGroupMember']);

export interface MintFacts {
  tokenProgram: string;
  decimals: number;
  supplyRaw: string;
  mintAuthority: string | 'none';
  freezeAuthority: string | 'none';
  /** Token-2022 extensions that are present AND active. */
  riskyExtensions: string[];
  name: string | null;
  symbol: string | null;
}

type ParsedExt = { extension: string; state?: Record<string, unknown> };

function extensionActive(e: ParsedExt): boolean {
  const s = e.state ?? {};
  switch (e.extension) {
    case 'transferHook':
      return s.programId != null;
    case 'permanentDelegate':
      return s.delegate != null;
    case 'defaultAccountState':
      return s.accountState === 'frozen';
    case 'transferFeeConfig': {
      const bps = (k: string) => Number((s[k] as { transferFeeBasisPoints?: number } | undefined)?.transferFeeBasisPoints ?? 0);
      return bps('newerTransferFee') > 0 || bps('olderTransferFee') > 0;
    }
    case 'mintCloseAuthority':
      return s.closeAuthority != null;
    case 'pausableConfig':
      return s.authority != null || s.paused === true;
    default:
      return true;
  }
}

export async function getMintFacts(conn: Connection, mint: string): Promise<MintFacts> {
  const info = await conn.getParsedAccountInfo(new PublicKey(mint));
  const acct = info.value;
  if (!acct) throw new Error('mint account not found');
  const program = acct.owner.toBase58();
  if (program !== TOKEN_PROGRAM && program !== TOKEN_2022_PROGRAM) throw new Error(`account is not an SPL token mint (owner ${program})`);
  const data = acct.data as { parsed?: { type: string; info: Record<string, unknown> } };
  if (data.parsed?.type !== 'mint') throw new Error('account is not a mint');
  const i = data.parsed.info;
  const exts = (i.extensions as ParsedExt[] | undefined) ?? [];
  const meta = exts.find((e) => e.extension === 'tokenMetadata')?.state as { name?: string; symbol?: string } | undefined;
  return {
    tokenProgram: program,
    decimals: Number(i.decimals),
    supplyRaw: String(i.supply),
    mintAuthority: (i.mintAuthority as string | null) ?? 'none',
    freezeAuthority: (i.freezeAuthority as string | null) ?? 'none',
    riskyExtensions: exts.filter((e) => !BENIGN_EXTENSIONS.has(e.extension) && extensionActive(e)).map((e) => e.extension),
    name: meta?.name ?? null,
    symbol: meta?.symbol ?? null,
  };
}

export interface HolderFacts {
  top: HolderInfo[];
  top10WalletPct: number;
  maxSingleWalletPct: number;
  programHeldPct: number;
}

export async function getHolderFacts(conn: Connection, mint: string, supplyRaw: bigint): Promise<HolderFacts> {
  const largest = await conn.getTokenLargestAccounts(new PublicKey(mint));
  const accounts = largest.value;
  const parsed = await conn.getMultipleParsedAccounts(accounts.map((a) => a.address));
  const owners = parsed.value.map((p) => {
    const d = p?.data as { parsed?: { info?: { owner?: string } } } | undefined;
    return d?.parsed?.info?.owner ?? null;
  });
  const ownerKeys = owners.map((o) => (o ? new PublicKey(o) : null));
  const ownerInfos = await conn.getMultipleAccountsInfo(ownerKeys.filter((k): k is PublicKey => k !== null));
  const ownerProgram = new Map<string, string | null>();
  let j = 0;
  for (const k of ownerKeys) if (k) ownerProgram.set(k.toBase58(), ownerInfos[j++]?.owner.toBase58() ?? null);

  const pct = (raw: bigint) => (supplyRaw === 0n ? 0 : Number((raw * 1_000_000n) / supplyRaw) / 10_000);
  const top: HolderInfo[] = accounts.map((a, idx) => {
    const owner = owners[idx] ?? null;
    let ownerType: HolderInfo['ownerType'] = 'unknown';
    let label: string | undefined;
    if (owner) {
      const prog = ownerProgram.get(owner) ?? null;
      if (BURN_OWNERS.has(owner)) ownerType = 'burn';
      else if (!PublicKey.isOnCurve(new PublicKey(owner).toBytes())) {
        // Off-curve owner = program-derived address: pool vault, bonding curve, lock, etc.
        ownerType = 'program';
        label = (prog && KNOWN_PROGRAMS[prog]) ?? 'program-derived address (pool / vault / lock?)';
      } else ownerType = 'wallet';
    }
    return { tokenAccount: a.address.toBase58(), owner, amountRaw: a.amount, pct: pct(BigInt(a.amount)), ownerType, ...(label ? { label } : {}) };
  });

  const byWallet = new Map<string, bigint>();
  let programRaw = 0n;
  for (const h of top) {
    if (h.ownerType === 'wallet' && h.owner) byWallet.set(h.owner, (byWallet.get(h.owner) ?? 0n) + BigInt(h.amountRaw));
    else if (h.ownerType === 'program') programRaw += BigInt(h.amountRaw);
  }
  const wallets = [...byWallet.values()].sort((a, b) => (b > a ? 1 : b < a ? -1 : 0));
  return {
    top,
    top10WalletPct: pct(wallets.slice(0, 10).reduce((s, x) => s + x, 0n)),
    maxSingleWalletPct: pct(wallets[0] ?? 0n),
    programHeldPct: pct(programRaw),
  };
}

export interface LaunchFacts {
  createdAt: string | null;
  creationSlot: number | null;
  creator: string | null;
  /** False when pagination stopped before the first signature (age is a lower bound). */
  reachedStart: boolean;
  /** Oldest signatures, oldest first. */
  earliest: Array<{ signature: string; slot: number }>;
}

export async function getLaunchFacts(conn: Connection, mint: string, maxPages = 5): Promise<LaunchFacts> {
  const key = new PublicKey(mint);
  let before: string | undefined;
  let all: Array<{ signature: string; slot: number; blockTime?: number | null }> = [];
  let reachedStart = false;
  for (let page = 0; page < maxPages; page++) {
    const sigs = await conn.getSignaturesForAddress(key, { limit: 1000, ...(before ? { before } : {}) });
    all = all.concat(sigs);
    if (sigs.length < 1000) {
      reachedStart = true;
      break;
    }
    before = sigs.at(-1)?.signature;
  }
  const oldest = all.at(-1);
  if (!oldest) return { createdAt: null, creationSlot: null, creator: null, reachedStart, earliest: [] };
  let creator: string | null = null;
  if (reachedStart) {
    const tx = await conn.getParsedTransaction(oldest.signature, { maxSupportedTransactionVersion: 0 });
    creator = tx?.transaction.message.accountKeys[0]?.pubkey.toBase58() ?? null;
  }
  return {
    createdAt: oldest.blockTime ? new Date(oldest.blockTime * 1000).toISOString() : null,
    creationSlot: reachedStart ? oldest.slot : null,
    creator,
    reachedStart,
    earliest: all.slice(-40).reverse().map((s) => ({ signature: s.signature, slot: s.slot })),
  };
}

/** Wallets whose balance of `mint` increased in the given (early) transactions. */
export async function getEarlyReceipts(conn: Connection, mint: string, sigs: string[]): Promise<EarlyReceipt[]> {
  if (!sigs.length) return [];
  const txs: Array<ParsedTransactionWithMeta | null> = [];
  for (let i = 0; i < sigs.length; i += 10) {
    txs.push(...(await conn.getParsedTransactions(sigs.slice(i, i + 10), { maxSupportedTransactionVersion: 0 })));
  }
  const out: EarlyReceipt[] = [];
  txs.forEach((tx, idx) => {
    if (!tx?.meta || tx.meta.err) return;
    const delta = new Map<string, bigint>();
    for (const b of tx.meta.preTokenBalances ?? []) if (b.mint === mint && b.owner) delta.set(b.owner, (delta.get(b.owner) ?? 0n) - BigInt(b.uiTokenAmount.amount));
    for (const b of tx.meta.postTokenBalances ?? []) if (b.mint === mint && b.owner) delta.set(b.owner, (delta.get(b.owner) ?? 0n) + BigInt(b.uiTokenAmount.amount));
    for (const [wallet, d] of delta) {
      if (d > 0n && PublicKey.isOnCurve(new PublicKey(wallet).toBytes())) out.push({ slot: tx.slot, wallet, amountRaw: d, signature: sigs[idx]! });
    }
  });
  return out;
}

/** First observed SOL sender into each wallet (skipped for wallets with >1000 txs of history). */
export async function getFunding(conn: Connection, wallets: string[]): Promise<FundingRecord[]> {
  const out: FundingRecord[] = [];
  for (const w of wallets) {
    try {
      const sigs = await conn.getSignaturesForAddress(new PublicKey(w), { limit: 1000 });
      const first = sigs.length < 1000 ? sigs.at(-1) : undefined;
      if (!first) {
        out.push({ wallet: w, funder: null });
        continue;
      }
      const tx = await conn.getParsedTransaction(first.signature, { maxSupportedTransactionVersion: 0 });
      let funder: string | null = null;
      for (const ix of tx?.transaction.message.instructions ?? []) {
        if ('parsed' in ix && ix.program === 'system' && ix.parsed?.type === 'transfer' && ix.parsed.info?.destination === w) {
          funder = ix.parsed.info.source as string;
          break;
        }
      }
      if (!funder) {
        const payer = tx?.transaction.message.accountKeys[0]?.pubkey.toBase58();
        if (payer && payer !== w) funder = payer;
      }
      out.push({ wallet: w, funder });
    } catch {
      out.push({ wallet: w, funder: null });
    }
  }
  return out;
}
