// Research orchestrator: gathers on-chain, market, sellability, bundle and
// social data from independent sources, timestamps every datapoint, and hands
// the result to assessReport() for flags and a verdict.
import { PublicKey, type Connection } from '@solana/web3.js';
import { env, rpcHost } from '../env.js';
import { MOCK_SOL_USD, type MockProvider } from '../market/mock.js';
import { NoRouteError, type Quote, type SwapProvider } from '../market/types.js';
import type { ResearchReport, Settings, Sourced } from '../../shared/types.js';
import { WSOL_MINT } from '../../shared/types.js';
import { analyzeBundles } from './bundles.js';
import { getEarlyReceipts, getFunding, getHolderFacts, getLaunchFacts, getMintFacts } from './onchain.js';
import { analyzeSocial } from './social.js';
import { dexScreenerUrl, geckoOhlcvUrl, getDexPair, getPriceHistory, getRugcheck, getXPosts, rugcheckPage, xSearchUrl } from './sources.js';
import { assessReport, type ReportDraft } from './verify.js';

type SourceLog = ResearchReport['sources'];

const now = () => new Date().toISOString();
const ok = <T>(value: T | null, source: string, url?: string, note?: string): Sourced<T> => ({
  value,
  source,
  fetchedAt: now(),
  ...(url ? { url } : {}),
  ...(note ? { note } : {}),
});
const none = <T>(source: string, note: string, url?: string): Sourced<T> => ok<T>(null, source, url, note);

async function attempt<T>(log: SourceLog, name: string, url: string, fn: () => Promise<T>): Promise<{ v: T | null; err: string | null }> {
  try {
    const v = await fn();
    log.push({ name, url, fetchedAt: now(), ok: true });
    return { v, err: null };
  } catch (e) {
    const err = (e as Error).message;
    log.push({ name, url, fetchedAt: now(), ok: false, error: err });
    return { v: null, err };
  }
}

export class Researcher {
  constructor(
    private readonly conn: Connection,
    private readonly provider: SwapProvider,
    private readonly settings: () => Settings,
  ) {}

  async research(mint: string): Promise<ResearchReport> {
    if (this.provider.mock) return this.researchMock(mint);
    try {
      new PublicKey(mint);
    } catch {
      throw new Error('Not a valid Solana address.');
    }
    const T = this.settings().verification;
    const log: SourceLog = [];
    const rpc = `rpc://${rpcHost(env.rpcUrl)}`;
    const solscan = `https://solscan.io/token/${mint}`;

    const [mintR, pairR, launchR] = await Promise.all([
      attempt(log, 'Solana RPC: mint account', rpc, () => getMintFacts(this.conn, mint)),
      attempt(log, 'DexScreener', dexScreenerUrl(mint), () => getDexPair(mint)),
      attempt(log, 'Solana RPC: signature history', rpc, () => getLaunchFacts(this.conn, mint)),
    ]);
    const m = mintR.v;
    const pair = pairR.v;
    const launch = launchR.v;
    const probeLamports = BigInt(Math.round(T.probeSol * 1e9));

    const [holdersR, rugR, histR, buyR, socialR] = await Promise.all([
      m ? attempt(log, 'Solana RPC: largest holders', rpc, () => getHolderFacts(this.conn, mint, BigInt(m.supplyRaw))) : Promise.resolve({ v: null, err: 'mint unavailable' }),
      env.rugcheckEnabled ? attempt(log, 'RugCheck', rugcheckPage(mint), () => getRugcheck(mint, pair?.pairAddress ?? null)) : Promise.resolve({ v: null, err: 'disabled' }),
      pair ? attempt(log, 'GeckoTerminal OHLCV', geckoOhlcvUrl(pair.pairAddress), () => getPriceHistory(pair.pairAddress)) : Promise.resolve({ v: null, err: 'no pair' }),
      this.quoteSafe(log, WSOL_MINT, mint, probeLamports),
      env.xBearerToken ? attempt(log, 'X API recent search', xSearchUrl(`"${mint}"`), () => getXPosts(mint)) : Promise.resolve({ v: null, err: 'X_BEARER_TOKEN not set' }),
    ]);
    const buy = buyR.v;
    const sellR = buy ? await this.quoteSafe(log, mint, WSOL_MINT, BigInt(buy.summary.outAmount)) : { v: null, err: 'no buy quote' };
    const sell = sellR.v;

    // Bundle / coordinated-buy signals (only meaningful when launch history was reached).
    let bundles: Sourced<NonNullable<ResearchReport['bundles']['value']>>;
    if (m && launch?.reachedStart && holdersR.v) {
      const holders = holdersR.v;
      const wallets = [...new Set(holders.top.filter((h) => h.ownerType === 'wallet' && h.owner).map((h) => h.owner!))].slice(0, 10);
      const [earlyR, fundR] = await Promise.all([
        attempt(log, 'Solana RPC: launch transactions', rpc, () => getEarlyReceipts(this.conn, mint, launch.earliest.slice(0, 30).map((s) => s.signature))),
        attempt(log, 'Solana RPC: holder funding', rpc, () => getFunding(this.conn, wallets)),
      ]);
      if (earlyR.v && fundR.v) {
        const holdings = new Map<string, bigint>();
        for (const h of holders.top) if (h.owner) holdings.set(h.owner, (holdings.get(h.owner) ?? 0n) + BigInt(h.amountRaw));
        bundles = ok(
          analyzeBundles({ creationSlot: launch.creationSlot, creator: launch.creator, totalSupplyRaw: BigInt(m.supplyRaw), earlyReceipts: earlyR.v, funding: fundR.v, holdingsByWallet: holdings }),
          'Solana RPC (first 30 transactions, top-10 holder funding)',
          solscan,
        );
      } else bundles = none('Solana RPC', earlyR.err ?? fundR.err ?? 'unavailable');
    } else bundles = none('Solana RPC', launch && !launch.reachedStart ? 'launch history beyond scan depth; bundle analysis skipped' : 'launch or holder data unavailable');

    const ageMin = launch?.createdAt ? (Date.now() - new Date(launch.createdAt).getTime()) / 60_000 : null;
    const dexUrl = pair?.url ?? dexScreenerUrl(mint);
    const roundTrip = buy && sell ? (1 - Number(sell.summary.outAmount) / Number(buy.summary.inAmount)) * 100 : null;
    const priceSol = pair && pair.quoteToken.address === WSOL_MINT && pair.priceNative ? Number(pair.priceNative) : null;

    const draft: ReportDraft = {
      mint,
      generatedAt: now(),
      mock: false,
      token: {
        name: ok(m?.name ?? pair?.baseToken.name ?? null, m?.name ? 'Token-2022 metadata' : 'DexScreener', dexUrl),
        symbol: ok(m?.symbol ?? pair?.baseToken.symbol ?? null, m?.symbol ? 'Token-2022 metadata' : 'DexScreener', dexUrl),
        decimals: m ? ok(m.decimals, 'Solana RPC', solscan) : none('Solana RPC', mintR.err ?? ''),
        supplyRaw: m ? ok(m.supplyRaw, 'Solana RPC', solscan) : none('Solana RPC', mintR.err ?? ''),
        tokenProgram: m ? ok(m.tokenProgram, 'Solana RPC', solscan) : none('Solana RPC', mintR.err ?? ''),
        mintAuthority: m ? ok(m.mintAuthority, 'Solana RPC', solscan) : none('Solana RPC', mintR.err ?? ''),
        freezeAuthority: m ? ok(m.freezeAuthority, 'Solana RPC', solscan) : none('Solana RPC', mintR.err ?? ''),
        extensions: m ? ok(m.riskyExtensions, 'Solana RPC', solscan) : none('Solana RPC', mintR.err ?? ''),
        createdAt: ok(launch?.createdAt ?? null, 'Solana RPC (oldest signature)', solscan, launch && !launch.reachedStart ? 'lower bound' : undefined),
        ageMinutes: ok(ageMin, 'Solana RPC (oldest signature)', solscan, launch && !launch.reachedStart ? 'lower bound: history deeper than scan' : undefined),
        creator: ok(launch?.creator ?? null, 'Solana RPC (fee payer of first transaction)', solscan),
      },
      market: {
        priceUsd: ok(pair?.priceUsd ? Number(pair.priceUsd) : null, 'DexScreener', dexUrl),
        priceSol: ok(priceSol, 'DexScreener', dexUrl, priceSol === null ? 'pair not quoted in SOL' : undefined),
        liquidityUsd: ok(pair?.liquidity?.usd ?? null, 'DexScreener', dexUrl),
        volume24hUsd: ok(pair?.volume?.h24 ?? null, 'DexScreener', dexUrl),
        marketCapUsd: ok(pair?.marketCap ?? null, 'DexScreener', dexUrl),
        fdvUsd: ok(pair?.fdv ?? null, 'DexScreener', dexUrl),
        txns24h: ok(pair?.txns?.h24 ?? null, 'DexScreener', dexUrl),
        priceChange: ok(pair?.priceChange ?? null, 'DexScreener', dexUrl),
        priceHistory: ok(histR.v, 'GeckoTerminal (hourly, USD)', pair ? `https://www.geckoterminal.com/solana/pools/${pair.pairAddress}` : undefined, histR.err ?? undefined),
        pairAddress: ok(pair?.pairAddress ?? null, 'DexScreener', dexUrl),
        dex: ok(pair?.dexId ?? null, 'DexScreener', dexUrl),
        lpLockedPct: ok(rugR.v?.lpLockedPct ?? null, 'RugCheck', rugcheckPage(mint), pair?.dexId === 'pumpfun' ? 'bonding curve: LP lock not applicable' : (rugR.err ?? undefined)),
      },
      holders: {
        count: ok(rugR.v?.totalHolders ?? null, 'RugCheck', rugcheckPage(mint)),
        top: holdersR.v ? ok(holdersR.v.top, 'Solana RPC getTokenLargestAccounts', solscan) : none('Solana RPC', holdersR.err ?? ''),
        top10Pct: holdersR.v ? ok(holdersR.v.top10WalletPct, 'Solana RPC (wallet owners only)', solscan, `${holdersR.v.programHeldPct.toFixed(1)}% held by program-derived accounts (pools/vaults/locks) excluded`) : none('Solana RPC', holdersR.err ?? ''),
        maxSingleWalletPct: holdersR.v ? ok(holdersR.v.maxSingleWalletPct, 'Solana RPC', solscan) : none('Solana RPC', holdersR.err ?? ''),
      },
      sellability: {
        buyQuote: buy ? ok(buy.summary, this.provider.name) : none(this.provider.name, buyR.err ?? ''),
        sellQuote: sell ? ok(sell.summary, this.provider.name) : none(this.provider.name, sellR.err ?? ''),
        roundTripLossPct: ok(roundTrip, this.provider.name),
      },
      bundles,
      social: socialR.v ? ok(analyzeSocial(socialR.v, new Date()), 'X API v2 recent search (last 7 days)', `https://x.com/search?q=${mint}`) : none('X API', socialR.err ?? 'unavailable'),
      thirdPartyFlags: rugR.v ? ok(rugR.v.risks, 'RugCheck', rugcheckPage(mint)) : none('RugCheck', rugR.err ?? ''),
      sources: log,
    };
    if (!pair) log.push({ name: 'DexScreener', url: dexUrl, fetchedAt: now(), ok: false, error: 'no pair with this token as base' });
    return assessReport(draft, T, new Date());
  }

  private async quoteSafe(log: SourceLog, inputMint: string, outputMint: string, amount: bigint): Promise<{ v: Quote | null; err: string | null }> {
    try {
      const q = await this.provider.quote({ inputMint, outputMint, amountRaw: amount, slippageBps: this.settings().limits.maxSlippageBps });
      log.push({ name: `${this.provider.name} quote ${inputMint === WSOL_MINT ? 'buy' : 'sell'}`, url: 'https://dev.jup.ag', fetchedAt: now(), ok: true });
      return { v: q, err: null };
    } catch (e) {
      const err = e instanceof NoRouteError ? `no route: ${e.message}` : (e as Error).message;
      log.push({ name: `${this.provider.name} quote`, url: 'https://dev.jup.ag', fetchedAt: now(), ok: false, error: err });
      return { v: null, err };
    }
  }

  private async researchMock(mint: string): Promise<ResearchReport> {
    const mp = this.provider as MockProvider;
    const t = mp.token(mint);
    if (!t) throw new Error('Unknown mock token. In MARKET_DATA=mock mode only the listed mock tokens exist.');
    const T = this.settings().verification;
    const src = 'mock';
    const probe = BigInt(Math.round(T.probeSol * 1e9));
    const log: SourceLog = [{ name: 'Mock market', url: 'about:mock', fetchedAt: now(), ok: true }];
    const buy = await this.quoteSafe(log, WSOL_MINT, mint, probe);
    const sell = buy.v ? await this.quoteSafe(log, mint, WSOL_MINT, BigInt(buy.v.summary.outAmount)) : { v: null, err: 'no buy' };
    const priceSol = mp.price(t);
    const solUsd = MOCK_SOL_USD;
    const draft: ReportDraft = {
      mint,
      generatedAt: now(),
      mock: true,
      token: {
        name: ok(t.name, src), symbol: ok(t.symbol, src), decimals: ok(t.decimals, src), supplyRaw: ok(t.supplyRaw.toString(), src),
        tokenProgram: ok('TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA', src), mintAuthority: ok<string>('none', src),
        freezeAuthority: ok<string>(t.freezeAuthority ?? 'none', src), extensions: ok<string[]>([], src),
        createdAt: ok(new Date(Date.now() - t.ageMinutes * 60_000).toISOString(), src), ageMinutes: ok(t.ageMinutes, src), creator: ok('MockCreator1111111111111111111111111111111', src),
      },
      market: {
        priceUsd: ok(priceSol * solUsd, src), priceSol: ok(priceSol, src),
        liquidityUsd: ok((Number(t.solReserve) / 1e9) * solUsd * 2, src), volume24hUsd: ok(t.volume24hUsd, src),
        marketCapUsd: ok((priceSol * solUsd * Number(t.supplyRaw)) / 10 ** t.decimals, src),
        fdvUsd: ok((priceSol * solUsd * Number(t.supplyRaw)) / 10 ** t.decimals, src), txns24h: ok({ buys: Math.round(t.sells24h * 1.1), sells: t.sells24h }, src),
        priceChange: ok({ h1: 0, h24: 0 }, src), priceHistory: ok([], src), pairAddress: ok('MockPool', src), dex: ok('mock', src), lpLockedPct: ok(t.lpLockedPct, src),
      },
      holders: { count: ok(t.holders, src), top: ok([], src), top10Pct: ok(t.top10Pct, src), maxSingleWalletPct: ok(t.maxSinglePct, src) },
      sellability: {
        buyQuote: buy.v ? ok(buy.v.summary, src) : none(src, buy.err ?? ''),
        sellQuote: sell.v ? ok(sell.v.summary, src) : none(src, sell.err ?? ''),
        roundTripLossPct: ok(buy.v && sell.v ? (1 - Number(sell.v.summary.outAmount) / Number(buy.v.summary.inAmount)) * 100 : null, src),
      },
      bundles: ok({ signals: [], earlyBuyers: 0, sharedFunders: [] }, src),
      social: none('X API', 'mock mode'),
      thirdPartyFlags: ok([], src),
      sources: log,
    };
    return assessReport(draft, T, new Date());
  }
}
