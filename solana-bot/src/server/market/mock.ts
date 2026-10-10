// Offline market simulator for paper trading and demos (MARKET_DATA=mock).
// Constant-product pools give realistic price impact. It can never build a
// real transaction, and research built on it is always flagged as mock.
import { WSOL_MINT } from '../../shared/types.js';
import { NoRouteError, type BuiltSwap, type Quote, type QuoteParams, type SwapProvider } from './types.js';

export interface MockToken {
  mint: string;
  symbol: string;
  name: string;
  decimals: number;
  solReserve: bigint;
  tokenReserve: bigint;
  supplyRaw: bigint;
  freezeAuthority: string | null;
  sellable: boolean;
}

const FEE_BPS = 25n;

export const MOCK_TOKENS: MockToken[] = [
  {
    mint: 'MockGoodToken111111111111111111111111111111',
    symbol: 'MGOOD',
    name: 'Mock Healthy Token',
    decimals: 6,
    solReserve: 2_000n * 1_000_000_000n,
    tokenReserve: 200_000_000n * 1_000_000n,
    supplyRaw: 1_000_000_000n * 1_000_000n,
    freezeAuthority: null,
    sellable: true,
  },
  {
    mint: 'MockRugToken1111111111111111111111111111111',
    symbol: 'MRUG',
    name: 'Mock Honeypot Token',
    decimals: 6,
    solReserve: 30n * 1_000_000_000n,
    tokenReserve: 900_000_000n * 1_000_000n,
    supplyRaw: 1_000_000_000n * 1_000_000n,
    freezeAuthority: 'FreezeAuth111111111111111111111111111111111',
    sellable: false,
  },
];

export class MockProvider implements SwapProvider {
  readonly name = 'Mock AMM (offline)';
  readonly mock = true;
  private tokens = new Map(MOCK_TOKENS.map((t) => [t.mint, { ...t }]));

  token(mint: string): MockToken | undefined {
    return this.tokens.get(mint);
  }

  /** Move the mock price by a multiplier (for testing exit rules). */
  movePrice(mint: string, multiplier: number): void {
    const t = this.tokens.get(mint);
    if (!t) throw new Error('unknown mock token');
    t.solReserve = (t.solReserve * BigInt(Math.round(multiplier * 1_000_000))) / 1_000_000n;
  }

  async quote(p: QuoteParams): Promise<Quote> {
    const buying = p.inputMint === WSOL_MINT;
    const t = this.tokens.get(buying ? p.outputMint : p.inputMint);
    if (!t) throw new NoRouteError('mock: unknown token');
    if (!buying && !t.sellable) throw new NoRouteError('mock: no sell route (honeypot simulation)');
    const [rin, rout] = buying ? [t.solReserve, t.tokenReserve] : [t.tokenReserve, t.solReserve];
    const inAfterFee = (p.amountRaw * (10_000n - FEE_BPS)) / 10_000n;
    const out = (rout * inAfterFee) / (rin + inAfterFee);
    const spotOut = (p.amountRaw * rout) / rin;
    const impact = spotOut === 0n ? 0 : Number(((spotOut - out) * 1_000_000n) / spotOut) / 10_000;
    const fetchedAt = new Date();
    return {
      raw: null,
      fetchedAt,
      summary: {
        inputMint: p.inputMint,
        outputMint: p.outputMint,
        inAmount: p.amountRaw.toString(),
        outAmount: out.toString(),
        minOutAmount: ((out * BigInt(10_000 - p.slippageBps)) / 10_000n).toString(),
        slippageBps: p.slippageBps,
        priceImpactPct: impact,
        routeLabels: ['MockAMM (100%)'],
        platformFeeRaw: '0',
        fetchedAt: fetchedAt.toISOString(),
        source: 'mock',
      },
    };
  }

  async buildSwap(): Promise<BuiltSwap> {
    throw new Error('Mock market data cannot build real transactions. Live trading requires MARKET_DATA=live.');
  }
}
