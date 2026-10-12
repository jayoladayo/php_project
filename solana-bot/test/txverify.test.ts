import { createApproveInstruction, createAssociatedTokenAccountIdempotentInstruction, createCloseAccountInstruction, createSyncNativeInstruction, getAssociatedTokenAddressSync } from '@solana/spl-token';
import { ComputeBudgetProgram, Keypair, PublicKey, SystemProgram, TransactionInstruction, TransactionMessage, VersionedTransaction } from '@solana/web3.js';
import { describe, expect, it } from 'vitest';
import { PROGRAMS, verifyTransaction } from '../src/server/trading/txverify.js';
import { WSOL_MINT } from '../src/shared/types.js';

const user = Keypair.generate().publicKey;
const attacker = Keypair.generate().publicKey;
const mint = Keypair.generate().publicKey;
const wsol = new PublicKey(WSOL_MINT);
const wsolAta = getAssociatedTokenAddressSync(wsol, user);
const tokenAta = getAssociatedTokenAddressSync(mint, user);

const jupiterIx = new TransactionInstruction({
  programId: new PublicKey(PROGRAMS.jupiterV6),
  keys: [user, wsolAta, tokenAta, mint, wsol].map((pubkey) => ({ pubkey, isSigner: false, isWritable: true })),
  data: Buffer.from([1, 2, 3]),
});

function build(ixs: TransactionInstruction[], payer = user) {
  const msg = new TransactionMessage({ payerKey: payer, recentBlockhash: Keypair.generate().publicKey.toBase58(), instructions: ixs }).compileToV0Message();
  return new VersionedTransaction(msg);
}

const standardBuy = () => [
  ComputeBudgetProgram.setComputeUnitLimit({ units: 300_000 }),
  ComputeBudgetProgram.setComputeUnitPrice({ microLamports: 100_000 }),
  createAssociatedTokenAccountIdempotentInstruction(user, wsolAta, user, wsol),
  SystemProgram.transfer({ fromPubkey: user, toPubkey: wsolAta, lamports: 10_000_000 }),
  createSyncNativeInstruction(wsolAta),
  createAssociatedTokenAccountIdempotentInstruction(user, tokenAta, user, mint),
  jupiterIx,
  createCloseAccountInstruction(wsolAta, user, user),
];

const params = { user: user.toBase58(), side: 'buy' as const, tokenMint: mint.toBase58(), tokenProgram: PROGRAMS.token };

describe('verifyTransaction', () => {
  it('accepts a standard Jupiter buy and explains it', () => {
    const r = verifyTransaction(build(standardBuy()), [], params);
    expect(r.problems).toEqual([]);
    expect(r.ok).toBe(true);
    expect(r.priorityFeeMaxLamports).toBe(30_000); // 100k µL × 300k CU / 1e6
    expect(r.ataCreations).toBe(2);
    expect(r.permissions.join(' ')).toMatch(/NO token approvals/);
    expect(r.instructions.map((i) => i.name)).toContain('Swap route');
  });

  it('refuses token approvals (delegation)', () => {
    const r = verifyTransaction(build([...standardBuy(), createApproveInstruction(tokenAta, attacker, user, 1n << 63n)]), [], params);
    expect(r.ok).toBe(false);
    expect(r.problems.join()).toMatch(/Approve/);
  });

  it('refuses SOL transfers to third parties', () => {
    const r = verifyTransaction(build([...standardBuy(), SystemProgram.transfer({ fromPubkey: user, toPubkey: attacker, lamports: 1 })]), [], params);
    expect(r.ok).toBe(false);
    expect(r.problems.join()).toMatch(/Unexpected SOL transfer/);
  });

  it('refuses closing a token account to someone else', () => {
    const r = verifyTransaction(build([...standardBuy(), createCloseAccountInstruction(tokenAta, attacker, user)]), [], params);
    expect(r.ok).toBe(false);
  });

  it('refuses unknown programs, foreign fee payers and missing mints', () => {
    const unknown = new TransactionInstruction({ programId: Keypair.generate().publicKey, keys: [], data: Buffer.alloc(0) });
    expect(verifyTransaction(build([...standardBuy(), unknown]), [], params).problems.join()).toMatch(/not on the allowlist|Unexpected program/);
    expect(verifyTransaction(build(standardBuy(), attacker), [], params).problems.join()).toMatch(/Fee payer/);
    const otherMint = Keypair.generate().publicKey.toBase58();
    expect(verifyTransaction(build(standardBuy()), [], { ...params, tokenMint: otherMint }).problems.join()).toMatch(/mint you chose/);
  });

  it('requires a swap instruction', () => {
    const r = verifyTransaction(build(standardBuy().filter((i) => i !== standardBuy()[6]).slice(0, 5)), [], params);
    expect(r.problems.join()).toMatch(/No Jupiter swap/);
  });
});
