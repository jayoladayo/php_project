// Offline market simulator for paper trading and demos (MARKET_DATA=mock).
// Constant-product pools give realistic price impact; an optional random walk
// moves prices so the autopilot and the take-profit ladder have something to
// do. It can never build a real transaction, and research built on it is
// always marked as mock (live trading refuses it).
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
  top10Pct: number;
  maxSinglePct: number;
  holders: number;
  ageMinutes: number;
  volume24hUsd: number;
  sells24h: number;
  lpLockedPct: number;
  /** Random walk: mean log-return and volatility per tick. */
  drift: number;
  volatility: number;
}

const FEE_BPS = 25n;
const SOL = 1_000_000_000n;
const UNIT = 1_000_000n; // 6 decimals

const base = {
  decimals: 6,
  supplyRaw: 1_000_000_000n * UNIT,
  freezeAuthority: null,
  sellable: true,
  top10Pct: 15,
  maxSinglePct: 3,
  holders: 2400,
  ageMinutes: 3 * 1440,
  volume24hUsd: 250_000,
  sells24h: 900,
  lpLockedPct: 100,
};

export const MOCK_TOKENS: MockToken[] = [
  { ...base, mint: 'MockGoodToken111111111111111111111111111111', symbol: 'MGOOD', name: 'Mock Healthy Token', solReserve: 2_000n * SOL, tokenReserve: 200_000_000n * UNIT, drift: 0, volatility: 0.01 },
  { ...base, mint: 'MockMoonToken111111111111111111111111111111', symbol: 'MMOON', name: 'Mock Runner (trends up)', solReserve: 400n * SOL, tokenReserve: 300_000_000n * UNIT, drift: 0.03, volatility: 0.04 },
  { ...base, mint: 'MockFadeToken111111111111111111111111111111', symbol: 'MFADE', name: 'Mock Fader (trends down)', solReserve: 300n * SOL, tokenReserve: 250_000_000n * UNIT, drift: -0.02, volatility: 0.03 },
  {
    ...base,
    mint: 'MockWhaLeToken11111111111111111111111111111',
    symbol: 'MWHALE',
    name: 'Mock Whale-Held Token',
    solReserve: 500n * SOL,
    tokenReserve: 100_000_000n * UNIT,
    top10Pct: 68,
    maxSinglePct: 31,
    drift: 0,
    volatility: 0.02,
  },
  {
    ...base,
    mint: 'MockRugToken1111111111111111111111111111111',
    symbol: 'MRUG',
    name: 'Mock Honeypot Token',
    solReserve: 30n * SOL,
    tokenReserve: 900_000_000n * UNIT,
    freezeAuthority: 'FreezeAuth111111111111111111111111111111111',
    sellable: false,
    holders: 40,
    sells24h: 0,
    lpLockedPct: 0,
    drift: 0,
    volatility: 0.05,
  },
];

export const MOCK_SOL_USD = 150;

export class MockProvider implements SwapProvider {
  readonly name = 'Mock AMM (offline)';
  readonly mock = true;
  private tokens = new Map(MOCK_TOKENS.map((t) => [t.mint, { ...t }]));

  constructor(private readonly opts: { randomWalk?: boolean } = {}) {}

  token(mint: string): MockToken | undefined {
    return this.tokens.get(mint);
  }

  list(): MockToken[] {
    return [...this.tokens.values()];
  }

  /** Price in SOL per whole token. */
  price(t: MockToken): number {
    return Number(t.solReserve) / 1e9 / (Number(t.tokenReserve) / 10 ** t.decimals);
  }

  /** Move the mock price by a multiplier (for testing exit rules). */
  movePrice(mint: string, multiplier: number): void {
    const t = this.tokens.get(mint);
    if (!t) throw new Error('unknown mock token');
    t.solReserve = (t.solReserve * BigInt(Math.round(multiplier * 1_000_000))) / 1_000_000n;
  }

  /** One random-walk step for every token (no-op unless enabled). */
  tick(): void {
    if (!this.opts.randomWalk) return;
    for (const t of this.tokens.values()) {
      // Box–Muller normal sample
      const z = Math.sqrt(-2 * Math.log(1 - Math.random())) * Math.cos(2 * Math.PI * Math.random());
      const m = Math.min(1.5, Math.max(0.6, Math.exp(t.drift + t.volatility * z)));
      this.movePrice(t.mint, m);
    }
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
