import type { QuoteSummary } from '../../shared/types.js';

export class NoRouteError extends Error {}

export interface Quote {
  summary: QuoteSummary;
  fetchedAt: Date;
  /** Provider payload needed to build the swap transaction. */
  raw: unknown;
}

export interface QuoteParams {
  inputMint: string;
  outputMint: string;
  amountRaw: bigint;
  slippageBps: number;
}

export interface BuiltSwap {
  txBase64: string;
  lastValidBlockHeight: number;
  prioritizationFeeLamports: number;
}

export interface SwapProvider {
  readonly name: string;
  readonly mock: boolean;
  quote(p: QuoteParams): Promise<Quote>;
  /** Build an unsigned swap transaction for the user's wallet to sign. */
  buildSwap(q: Quote, userPublicKey: string, priorityFeeMaxLamports: number): Promise<BuiltSwap>;
}
