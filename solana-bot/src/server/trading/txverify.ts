// Decode an unsigned swap transaction and refuse anything outside a narrow
// allowlist BEFORE the wallet is asked to sign: no token approvals or
// delegations, no authority changes, no SOL or token transfers to third
// parties, only the user as signer and fee payer.
import { getAssociatedTokenAddressSync } from '@solana/spl-token';
import { PublicKey, TransactionMessage, type AddressLookupTableAccount, type VersionedTransaction } from '@solana/web3.js';
import type { Side, TxInstructionView } from '../../shared/types.js';
import { WSOL_MINT } from '../../shared/types.js';

export const PROGRAMS = {
  system: '11111111111111111111111111111111',
  computeBudget: 'ComputeBudget111111111111111111111111111111',
  token: 'TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA',
  token2022: 'TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb',
  ata: 'ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL',
  jupiterV6: 'JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4',
  memo: 'MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr',
  memoV1: 'Memo1UhkJRfHyvLMcVucJwxXeuD728EqVDDwQDxFMNo',
} as const;

const ATA_RENT_LAMPORTS = 2_039_280;

export interface VerifyParams {
  user: string;
  side: Side;
  tokenMint: string;
  tokenProgram: string;
  /** Allow the Memo program (used by the devnet signing test). */
  allowMemoOnly?: boolean;
}

export interface VerifyResult {
  ok: boolean;
  problems: string[];
  instructions: TxInstructionView[];
  permissions: string[];
  priorityFeeMaxLamports: number;
  ataCreations: number;
}

const u32 = (b: Uint8Array, o: number) => new DataView(b.buffer, b.byteOffset, b.byteLength).getUint32(o, true);
const u64 = (b: Uint8Array, o: number) => new DataView(b.buffer, b.byteOffset, b.byteLength).getBigUint64(o, true);

export function verifyTransaction(tx: VersionedTransaction, alts: AddressLookupTableAccount[], p: VerifyParams): VerifyResult {
  const problems: string[] = [];
  const views: TxInstructionView[] = [];
  const user = new PublicKey(p.user);
  const wsolAta = getAssociatedTokenAddressSync(new PublicKey(WSOL_MINT), user, false, new PublicKey(PROGRAMS.token)).toBase58();

  const header = tx.message.header;
  if (header.numRequiredSignatures !== 1) problems.push(`Transaction requires ${header.numRequiredSignatures} signatures; only your wallet should sign.`);
  const feePayer = tx.message.staticAccountKeys[0]?.toBase58();
  if (feePayer !== p.user) problems.push(`Fee payer is ${feePayer}, not your wallet.`);

  let msg: TransactionMessage;
  try {
    msg = TransactionMessage.decompile(tx.message, { addressLookupTableAccounts: alts });
  } catch (e) {
    return { ok: false, problems: [...problems, `Could not decode transaction: ${(e as Error).message}`], instructions: [], permissions: [], priorityFeeMaxLamports: 0, ataCreations: 0 };
  }

  let cuLimit = 200_000;
  let cuPriceMicro = 0n;
  let ataCreations = 0;
  let wraps = false;
  let sawSwap = false;
  const allKeys = new Set<string>();

  for (const ix of msg.instructions) {
    const pid = ix.programId.toBase58();
    const keys = ix.keys.map((k) => k.pubkey.toBase58());
    keys.forEach((k) => allKeys.add(k));
    const d = ix.data;
    const view = (program: string, name: string, explanation: string) => views.push({ program, programId: pid, name, explanation });

    switch (pid) {
      case PROGRAMS.computeBudget: {
        if (d[0] === 2) {
          cuLimit = u32(d, 1);
          view('Compute Budget', 'SetComputeUnitLimit', `Caps compute at ${cuLimit.toLocaleString()} units.`);
        } else if (d[0] === 3) {
          cuPriceMicro = u64(d, 1);
          view('Compute Budget', 'SetComputeUnitPrice', `Priority fee of ${cuPriceMicro} micro-lamports per compute unit.`);
        } else view('Compute Budget', `instruction ${d[0]}`, 'Compute budget setting.');
        break;
      }
      case PROGRAMS.system: {
        const tag = d.length >= 4 ? u32(d, 0) : -1;
        if (tag === 2 && keys[0] === p.user && keys[1] === wsolAta) {
          wraps = true;
          view('System', 'Transfer', `Moves ${Number(u64(d, 4)) / 1e9} SOL into your own wrapped-SOL account for the swap.`);
        } else if (tag === 2) {
          problems.push(`Unexpected SOL transfer from ${keys[0]} to ${keys[1]}.`);
          view('System', 'Transfer', 'REFUSED: SOL transfer to an account that is not yours.');
        } else {
          problems.push(`Unexpected System Program instruction (tag ${tag}).`);
          view('System', `instruction ${tag}`, 'REFUSED: not part of a standard swap.');
        }
        break;
      }
      case PROGRAMS.ata: {
        const kind = d.length === 0 || d[0] === 0 ? 'Create' : d[0] === 1 ? 'CreateIdempotent' : null;
        if (!kind) {
          problems.push('Unexpected Associated Token Account instruction.');
          break;
        }
        if (keys[0] !== p.user || keys[2] !== p.user) problems.push('Token account creation is not paid by and owned by your wallet.');
        ataCreations++;
        view('Associated Token Account', kind, `Creates (if missing) your token account for ${short(keys[3] ?? '?')}. Rent deposit ~${ATA_RENT_LAMPORTS / 1e9} SOL, refundable if the account is closed.`);
        break;
      }
      case PROGRAMS.token:
      case PROGRAMS.token2022: {
        const tag = d[0];
        const prog = pid === PROGRAMS.token ? 'SPL Token' : 'Token-2022';
        if (tag === 17) view(prog, 'SyncNative', 'Updates your wrapped-SOL balance.');
        else if (tag === 9) {
          // CloseAccount: [account, destination, owner]
          if (keys[0] === wsolAta && keys[1] === p.user && keys[2] === p.user) view(prog, 'CloseAccount', 'Closes your temporary wrapped-SOL account and returns the SOL to you.');
          else {
            problems.push(`Unexpected CloseAccount of ${keys[0]} (destination ${keys[1]}).`);
            view(prog, 'CloseAccount', 'REFUSED: closes an account other than your temporary wrapped-SOL account.');
          }
        } else if (tag === 18 && d.length >= 33 && new PublicKey(d.slice(1, 33)).toBase58() === p.user) {
          view(prog, 'InitializeAccount3', 'Initializes a token account owned by you.');
        } else {
          const names: Record<number, string> = { 3: 'Transfer', 4: 'Approve (delegation)', 6: 'SetAuthority', 7: 'MintTo', 8: 'Burn', 12: 'TransferChecked', 13: 'ApproveChecked (delegation)', 14: 'MintToChecked', 15: 'BurnChecked' };
          const name = names[tag ?? -1] ?? `instruction ${tag}`;
          problems.push(`Refused ${prog} ${name}: swaps never need approvals, delegations, authority changes or direct transfers.`);
          view(prog, name, 'REFUSED.');
        }
        break;
      }
      case PROGRAMS.jupiterV6:
        sawSwap = true;
        view('Jupiter v6', 'Swap route', 'Routed swap through Jupiter to DEX programs. Inner effects are checked by simulation.');
        break;
      case PROGRAMS.memo:
      case PROGRAMS.memoV1:
        view('Memo', 'Memo', `Attaches a note: "${new TextDecoder().decode(d).slice(0, 80)}".`);
        break;
      default:
        problems.push(`Unexpected program ${pid} at the top level of the transaction.`);
        view('Unknown', 'unknown', `REFUSED: program ${pid} is not on the allowlist.`);
    }
  }

  if (!p.allowMemoOnly) {
    if (!sawSwap) problems.push('No Jupiter swap instruction found.');
    if (!allKeys.has(p.tokenMint)) problems.push('The token mint you chose does not appear in the transaction.');
  }

  const priorityFeeMaxLamports = Number((cuPriceMicro * BigInt(cuLimit)) / 1_000_000n);
  const permissions = [
    'Your wallet is the only signer and pays the network fee (5,000 lamports per signature).',
    `Priority fee up to ${(priorityFeeMaxLamports / 1e9).toFixed(6)} SOL (${cuLimit.toLocaleString()} compute units).`,
    ...(ataCreations ? [`May create up to ${ataCreations} token account(s) owned by you (refundable rent ~${((ataCreations * ATA_RENT_LAMPORTS) / 1e9).toFixed(5)} SOL).`] : []),
    ...(wraps ? ['Temporarily wraps SOL into your own wSOL account, then closes it back to you.'] : []),
    p.side === 'buy' ? 'Spends SOL from your wallet and deposits the token into your own token account.' : 'Sells tokens from your token account and returns SOL to your wallet.',
    'Requests NO token approvals, delegations, or authority changes — nothing persists after this transaction.',
  ];
  return { ok: problems.length === 0, problems, instructions: views, permissions, priorityFeeMaxLamports, ataCreations };
}

const short = (a: string) => `${a.slice(0, 4)}…${a.slice(-4)}`;
