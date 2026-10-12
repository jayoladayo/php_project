// Jupiter swap aggregator (https://dev.jup.ag). Used for quotes, routing and
// building unsigned swap transactions. No token approvals are involved:
// Solana swaps are signed per transaction by the wallet owner.
import { env } from '../env.js';
import { fetchJson, HttpError } from '../http.js';
import { NoRouteError, type BuiltSwap, type Quote, type QuoteParams, type SwapProvider } from './types.js';

interface JupQuote {
  inputMint: string;
  outputMint: string;
  inAmount: string;
  outAmount: string;
  otherAmountThreshold: string;
  swapMode: string;
  slippageBps: number;
  priceImpactPct: string;
  platformFee?: { amount: string; feeBps: number } | null;
  routePlan: Array<{ swapInfo: { ammKey: string; label?: string; inputMint: string; outputMint: string; feeAmount?: string; feeMint?: string }; percent: number }>;
}

export class JupiterProvider implements SwapProvider {
  readonly name = 'Jupiter';
  readonly mock = false;

  private headers(): Record<string, string> {
    return env.jupiterApiKey ? { 'x-api-key': env.jupiterApiKey } : {};
  }

  quoteUrl(p: QuoteParams): string {
    const q = new URLSearchParams({
      inputMint: p.inputMint,
      outputMint: p.outputMint,
      amount: p.amountRaw.toString(),
      slippageBps: String(p.slippageBps),
      swapMode: 'ExactIn',
      restrictIntermediateTokens: 'true',
    });
    return `${env.jupiterBase}/quote?${q}`;
  }

  async quote(p: QuoteParams): Promise<Quote> {
    let r: JupQuote & { error?: string; errorCode?: string };
    try {
      r = await fetchJson<JupQuote & { error?: string }>(this.quoteUrl(p), { headers: this.headers() });
    } catch (e) {
      if (e instanceof HttpError && e.status !== null && e.status >= 400 && e.status < 500 && /route|COULD_NOT_FIND|TOKEN_NOT_TRADABLE/i.test(e.message))
        throw new NoRouteError(e.message);
      throw e;
    }
    if (r.error || !r.outAmount) throw new NoRouteError(r.error ?? 'no route');
    const fetchedAt = new Date();
    return {
      raw: r,
      fetchedAt,
      summary: {
        inputMint: r.inputMint,
        outputMint: r.outputMint,
        inAmount: r.inAmount,
        outAmount: r.outAmount,
        minOutAmount: r.otherAmountThreshold,
        slippageBps: r.slippageBps,
        // Jupiter reports a fraction (0.01 = 1%).
        priceImpactPct: Number(r.priceImpactPct) * 100,
        routeLabels: r.routePlan.map((s) => `${s.swapInfo.label ?? s.swapInfo.ammKey.slice(0, 6)} (${s.percent}%)`),
        platformFeeRaw: r.platformFee?.amount ?? '0',
        fetchedAt: fetchedAt.toISOString(),
        source: 'Jupiter quote API',
      },
    };
  }

  async buildSwap(q: Quote, userPublicKey: string, priorityFeeMaxLamports: number): Promise<BuiltSwap> {
    const r = await fetchJson<{ swapTransaction: string; lastValidBlockHeight: number; prioritizationFeeLamports?: number; simulationError?: unknown }>(
      `${env.jupiterBase}/swap`,
      {
        method: 'POST',
        headers: this.headers(),
        retries: 1,
        body: {
          quoteResponse: q.raw,
          userPublicKey,
          wrapAndUnwrapSol: true,
          dynamicComputeUnitLimit: true,
          prioritizationFeeLamports: { priorityLevelWithMaxLamports: { maxLamports: priorityFeeMaxLamports, priorityLevel: 'high' } },
        },
      },
    );
    if (r.simulationError) throw new Error(`Jupiter reported a simulation error: ${JSON.stringify(r.simulationError)}`);
    return { txBase64: r.swapTransaction, lastValidBlockHeight: r.lastValidBlockHeight, prioritizationFeeLamports: r.prioritizationFeeLamports ?? 0 };
  }
}
