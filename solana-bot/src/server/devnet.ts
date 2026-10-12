// Devnet signing test: lets the user check the wallet-adapter signing flow
// end-to-end without risking real funds. Builds a memo-only transaction.
import { PublicKey, TransactionInstruction, TransactionMessage, VersionedTransaction, type Connection } from '@solana/web3.js';
import { PROGRAMS, verifyTransaction } from './trading/txverify.js';

export async function buildDevnetTest(conn: Connection, wallet: string) {
  const user = new PublicKey(wallet);
  const { blockhash, lastValidBlockHeight } = await conn.getLatestBlockhash('confirmed');
  const memo = new TransactionInstruction({
    programId: new PublicKey(PROGRAMS.memo),
    keys: [{ pubkey: user, isSigner: true, isWritable: false }],
    data: Buffer.from(`solana-research-trader devnet signing test ${new Date().toISOString()}`),
  });
  const msg = new TransactionMessage({ payerKey: user, recentBlockhash: blockhash, instructions: [memo] }).compileToV0Message();
  const tx = new VersionedTransaction(msg);
  const v = verifyTransaction(tx, [], { user: wallet, side: 'buy', tokenMint: '', tokenProgram: PROGRAMS.token, allowMemoOnly: true });
  return {
    txBase64: Buffer.from(tx.serialize()).toString('base64'),
    lastValidBlockHeight,
    blockhash,
    instructions: v.instructions,
    permissions: ['Devnet only: costs a tiny amount of devnet SOL (no real value).', 'Signs a memo; moves no funds; requests no approvals.'],
  };
}

export async function submitDevnetTest(conn: Connection, signedBase64: string, blockhash: string, lastValidBlockHeight: number) {
  const tx = VersionedTransaction.deserialize(Buffer.from(signedBase64, 'base64'));
  const ix = tx.message.compiledInstructions;
  const keys = tx.message.staticAccountKeys;
  if (ix.length !== 1 || keys[ix[0]!.programIdIndex]?.toBase58() !== PROGRAMS.memo) throw new Error('Not the devnet test transaction.');
  const sig = await conn.sendRawTransaction(tx.serialize());
  await conn.confirmTransaction({ signature: sig, blockhash, lastValidBlockHeight }, 'confirmed');
  return { signature: sig, explorer: `https://explorer.solana.com/tx/${sig}?cluster=devnet` };
}
