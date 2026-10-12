// Simulate a swap against current chain state and compare the resulting
// balance changes with the quote, before the wallet is asked to sign.
import { getAssociatedTokenAddressSync } from '@solana/spl-token';
import { PublicKey, type AddressLookupTableAccount, type Connection, type VersionedTransaction } from '@solana/web3.js';
import type { Side } from '../../shared/types.js';

export interface SimulationCheck {
  ok: boolean;
  detail: string;
  logs?: string[];
  unitsConsumed?: number;
}

/** Token amount from SPL token account data (same layout for Token and Token-2022 base). */
export function tokenAmountFromData(data: Uint8Array | null): bigint {
  if (!data || data.length < 72) return 0n;
  return new DataView(data.buffer, data.byteOffset, data.byteLength).getBigUint64(64, true);
}

export async function fetchLookupTables(conn: Connection, tx: VersionedTransaction): Promise<AddressLookupTableAccount[]> {
  const out: AddressLookupTableAccount[] = [];
  for (const l of tx.message.addressTableLookups) {
    const r = await conn.getAddressLookupTable(l.accountKey);
    if (!r.value) throw new Error(`address lookup table ${l.accountKey.toBase58()} not found`);
    out.push(r.value);
  }
  return out;
}

export async function simulateSwap(
  conn: Connection,
  tx: VersionedTransaction,
  p: { user: string; side: Side; tokenMint: string; tokenProgram: string; inAmountRaw: bigint; minOutRaw: bigint; feeBudgetLamports: bigint },
): Promise<SimulationCheck> {
  const user = new PublicKey(p.user);
  const ata = getAssociatedTokenAddressSync(new PublicKey(p.tokenMint), user, false, new PublicKey(p.tokenProgram));
  const [preUser, preAta] = await conn.getMultipleAccountsInfo([user, ata]);
  const sim = await conn.simulateTransaction(tx, {
    sigVerify: false,
    replaceRecentBlockhash: true,
    commitment: 'confirmed',
    accounts: { encoding: 'base64', addresses: [user.toBase58(), ata.toBase58()] },
  });
  const v = sim.value;
  const logs = v.logs?.slice(-30) ?? undefined;
  if (v.err) return { ok: false, detail: `Simulation failed: ${JSON.stringify(v.err)}`, ...(logs ? { logs } : {}) };
  const [postUser, postAta] = v.accounts ?? [];
  if (!postUser) return { ok: false, detail: 'Simulation returned no account state.', ...(logs ? { logs } : {}) };

  const decode = (a: { data: string[] } | null | undefined) => (a?.data?.[0] ? new Uint8Array(Buffer.from(a.data[0], 'base64')) : null);
  const solDelta = BigInt(postUser.lamports) - BigInt(preUser?.lamports ?? 0);
  const tokDelta = tokenAmountFromData(decode(postAta)) - tokenAmountFromData(preAta?.data ?? null);
  const units = v.unitsConsumed ?? undefined;
  const base = { ...(logs ? { logs } : {}), ...(units !== undefined ? { unitsConsumed: units } : {}) };

  if (p.side === 'buy') {
    const spent = -solDelta;
    if (tokDelta < p.minOutRaw) return { ok: false, detail: `Simulated tokens received ${tokDelta} < minimum ${p.minOutRaw}.`, ...base };
    if (spent > p.inAmountRaw + p.feeBudgetLamports) return { ok: false, detail: `Simulated SOL spent ${spent} exceeds amount + fee budget ${p.inAmountRaw + p.feeBudgetLamports}.`, ...base };
    return { ok: true, detail: `Simulation OK: receive ${tokDelta} raw tokens (min ${p.minOutRaw}) for ${Number(spent) / 1e9} SOL including fees/rent.`, ...base };
  }
  const sold = -tokDelta;
  if (sold > p.inAmountRaw) return { ok: false, detail: `Simulated tokens sold ${sold} > requested ${p.inAmountRaw}.`, ...base };
  if (solDelta < p.minOutRaw - p.feeBudgetLamports) return { ok: false, detail: `Simulated SOL received ${solDelta} < minimum ${p.minOutRaw} minus fees.`, ...base };
  return { ok: true, detail: `Simulation OK: sell ${sold} raw tokens for ~${Number(solDelta) / 1e9} SOL net of fees.`, ...base };
}
