// Types shared by the server and the dashboard.
// Raw on-chain amounts (lamports, token base units) are carried as decimal
// strings so they survive JSON without losing precision; pure logic converts
// them to bigint.

export type Mode = 'paper' | 'live';
export type Side = 'buy' | 'sell';
export type TradeReason = 'manual' | 'take_profit' | 'stop_loss' | 'time_exit';

export const LAMPORTS_PER_SOL = 1_000_000_000;
export const WSOL_MINT = 'So11111111111111111111111111111111111111112';

// ---------------------------------------------------------------- settings

export interface RiskLimits {
  /** Largest single buy, in SOL. */
  maxBuySol: number;
  /** Stop all buys once realized (+ marked unrealized) losses today reach this, in SOL. */
  dailyLossLimitSol: number;
  /** Sum of open cost basis across positions may not exceed this, in SOL. */
  maxTotalExposureSol: number;
  /** Max slippage tolerance that may be requested from the router, in basis points. */
  maxSlippageBps: number;
  /** Reject quotes whose reported price impact exceeds this, in percent. */
  maxPriceImpactPct: number;
  maxOpenPositions: number;
  /** SOL that must remain in the wallet after a buy (network fees, rent). */
  minSolReserve: number;
  /** Also reject a buy larger than the remaining daily-loss budget. */
  capBuyByRemainingDailyLoss: boolean;
}

export interface TakeProfitRule {
  enabled: boolean;
  /** Trigger when the net executable sell price reaches entry × multiple. */
  multiple: number;
  /** Percent of the ORIGINAL token amount of the position to sell. */
  sellFractionPct: number;
  /** Which recorded entry price the multiple is measured against. */
  entryPriceBasis: 'effective_incl_fees' | 'fill_price';
  /** Live mode only: skip the dashboard approval step (the wallet still asks to sign). */
  autoExecuteLive: boolean;
}

export interface ExitRule {
  enabled: boolean;
  sellFractionPct: number;
  autoExecuteLive: boolean;
  /** Whether the rule may still act on a position that is locked after take-profit. */
  applyAfterTakeProfit: boolean;
}

export interface StopLossRule extends ExitRule {
  /** Sell when net executable value falls this many percent below entry. */
  dropPct: number | null;
}

export interface TimeExitRule extends ExitRule {
  maxHoldMinutes: number | null;
}

export interface VerificationThresholds {
  maxDataAgeSec: number;
  minLiquidityUsd: number;
  maxTop10HolderPct: number;
  maxSingleHolderPct: number;
  maxPriceDisagreementPct: number;
  maxRoundTripLossPct: number;
  /** Size of the probe buy used for sellability / round-trip checks, in SOL. */
  probeSol: number;
}

export interface PaperSettings {
  startingBalanceSol: number;
  /** Extra adverse slippage applied to simulated fills, in bps. */
  extraSlippageBps: number;
  priorityFeeLamports: number;
}

export interface Settings {
  limits: RiskLimits;
  takeProfit: TakeProfitRule;
  stopLoss: StopLossRule;
  timeExit: TimeExitRule;
  verification: VerificationThresholds;
  paper: PaperSettings;
  /** Quotes older than this are refused at confirmation time. */
  quoteMaxAgeSec: number;
  monitorIntervalSec: number;
  /** Cap on the priority fee Jupiter may add to a live swap. */
  priorityFeeMaxLamports: number;
  /** Live trading is only possible from this wallet (use a low-balance test wallet). */
  boundWallet: string | null;
  /** Warn when the bound wallet holds more than this, in SOL. */
  testWalletMaxBalanceSol: number;
}

// ---------------------------------------------------------------- research

export interface Sourced<T> {
  value: T | null;
  source: string;
  url?: string;
  fetchedAt: string;
  note?: string;
}

export type Severity = 'info' | 'warn' | 'high' | 'critical';

export interface RiskFlag {
  code: string;
  severity: Severity;
  message: string;
  /** "fact" = read directly from chain/market data; "signal" = heuristic, not proof. */
  kind: 'fact' | 'signal';
  source?: string;
}

export interface HolderInfo {
  tokenAccount: string;
  owner: string | null;
  amountRaw: string;
  pct: number;
  /** wallet | program (pool / vault / lock / bonding curve) | burn | unknown */
  ownerType: 'wallet' | 'program' | 'burn' | 'unknown';
  label?: string;
}

export interface SocialPost {
  id: string;
  url: string;
  text: string;
  createdAt: string;
  likes: number;
  reposts: number;
  replies: number;
  author: {
    id: string;
    username: string;
    createdAt: string | null;
    followers: number;
    following: number;
    postCount: number;
    verified: boolean;
  };
  signals: string[];
}

export interface QuoteSummary {
  inputMint: string;
  outputMint: string;
  inAmount: string;
  outAmount: string;
  minOutAmount: string;
  slippageBps: number;
  priceImpactPct: number;
  routeLabels: string[];
  platformFeeRaw: string;
  fetchedAt: string;
  source: string;
}

export interface ResearchReport {
  mint: string;
  generatedAt: string;
  mock: boolean;
  verdict: 'verified' | 'not_verified';
  /** Why the verdict is what it is (missing / stale / contradictory / flagged). */
  verdictReasons: string[];
  summary: string;
  uncertainty: string[];
  flags: RiskFlag[];
  token: {
    name: Sourced<string>;
    symbol: Sourced<string>;
    decimals: Sourced<number>;
    supplyRaw: Sourced<string>;
    tokenProgram: Sourced<string>;
    mintAuthority: Sourced<string | 'none'>;
    freezeAuthority: Sourced<string | 'none'>;
    extensions: Sourced<string[]>;
    createdAt: Sourced<string>;
    ageMinutes: Sourced<number>;
    creator: Sourced<string>;
  };
  market: {
    priceUsd: Sourced<number>;
    priceSol: Sourced<number>;
    liquidityUsd: Sourced<number>;
    volume24hUsd: Sourced<number>;
    marketCapUsd: Sourced<number>;
    fdvUsd: Sourced<number>;
    txns24h: Sourced<{ buys: number; sells: number }>;
    priceChange: Sourced<Record<string, number>>;
    priceHistory: Sourced<Array<{ t: string; close: number }>>;
    pairAddress: Sourced<string>;
    dex: Sourced<string>;
    lpLockedPct: Sourced<number>;
  };
  holders: {
    count: Sourced<number>;
    top: Sourced<HolderInfo[]>;
    top10Pct: Sourced<number>;
    maxSingleWalletPct: Sourced<number>;
  };
  sellability: {
    buyQuote: Sourced<QuoteSummary>;
    sellQuote: Sourced<QuoteSummary>;
    roundTripLossPct: Sourced<number>;
  };
  bundles: Sourced<{ signals: RiskFlag[]; earlyBuyers: number; sharedFunders: Array<{ funder: string; wallets: string[] }> }>;
  social: Sourced<{ posts: SocialPost[]; signals: RiskFlag[] }>;
  thirdPartyFlags: Sourced<Array<{ name: string; level: string; description: string }>>;
  sources: Array<{ name: string; url: string; fetchedAt: string; ok: boolean; error?: string }>;
}

// ---------------------------------------------------------------- positions

export interface Fill {
  id: string;
  side: Side;
  at: string;
  mode: Mode;
  reason: TradeReason;
  /** buy: lamports spent into the swap; sell: token raw sold */
  inputRaw: string;
  /** buy: token raw received; sell: lamports received from the swap */
  outputRaw: string;
  /** What the quote said we would receive (before slippage). */
  quotedOutRaw: string;
  /** What we asked to swap; inputRaw < requestedInRaw means a partial fill. */
  requestedInRaw: string;
  networkFeeLamports: string;
  priorityFeeLamports: string;
  platformFeeLamports: string;
  /** Refundable account rent deposits (tracked separately, not a cost). */
  rentLamports: string;
  /** Sells only: realized P&L of this fill, net of fees. */
  realizedPnlLamports?: string;
  signature?: string;
  intentId?: string;
}

export interface TakeProfitState {
  state: 'pending' | 'partial' | 'done';
  targetRaw: string;
  soldRaw: string;
  triggeredAt?: string;
  completedAt?: string;
}

export interface Position {
  id: string;
  mint: string;
  symbol: string;
  decimals: number;
  mode: Mode;
  wallet: string | null;
  openedAt: string;
  closedAt?: string;
  status: 'open' | 'closed';
  originalTokenRaw: string;
  heldTokenRaw: string;
  /** Lamports swapped in on entry (fill price basis). */
  entrySwapLamports: string;
  /** Lamports swapped in + entry fees (effective basis). */
  entryCostLamports: string;
  /** Remaining cost basis attributable to tokens still held. */
  costBasisLamports: string;
  realizedPnlLamports: string;
  feesPaidLamports: string;
  rentDepositLamports: string;
  takeProfit: TakeProfitState;
  /** Set once take-profit completes: no automatic trades unless a rule explicitly allows it. */
  locked: boolean;
  fills: Fill[];
}

export interface Mark {
  at: string;
  /** Executable sell quote for the full held amount, minus estimated sell fees. */
  netValueLamports: string;
  priceImpactPct: number;
  multipleOfEntry: number | null;
  stale: boolean;
  error?: string;
}

// ---------------------------------------------------------------- intents

export interface TxInstructionView {
  program: string;
  programId: string;
  name: string;
  explanation: string;
}

export interface IntentSummary {
  side: Side;
  mint: string;
  symbol: string;
  decimals: number;
  inputMint: string;
  outputMint: string;
  inAmountRaw: string;
  estOutRaw: string;
  minOutRaw: string;
  estPriceSolPerToken: number;
  slippageBps: number;
  priceImpactPct: number;
  route: string[];
  fees: {
    networkLamports: string;
    priorityMaxLamports: string;
    platformRaw: string;
    rentDepositLamports: string;
  };
  riskFlags: RiskFlag[];
  verdict: ResearchReport['verdict'] | 'n/a';
  warnings: string[];
  permissions: string[];
  instructions: TxInstructionView[];
  simulation: { ok: boolean; logs?: string[]; detail: string } | null;
}

export interface TradeIntent {
  id: string;
  mode: Mode;
  createdAt: string;
  expiresAt: string;
  status: 'pending' | 'submitted' | 'confirmed' | 'failed' | 'cancelled' | 'expired' | 'unknown';
  reason: TradeReason;
  positionId: string | null;
  wallet: string | null;
  summary: IntentSummary;
  /** Live only: unsigned transaction for the wallet to sign. */
  txBase64?: string;
  messageHashHex?: string;
  lastValidBlockHeight?: number;
  signature?: string;
  error?: string;
  /** The original request, re-checked against limits at confirmation time. */
  request: PrepareRequest;
}

export interface PrepareRequest {
  side: Side;
  /** Buys: token mint. */
  mint?: string;
  /** Sells: the position to sell from. */
  positionId?: string;
  /** Buys: SOL amount as a decimal string (e.g. "0.05"). */
  amountSol?: string;
  /** Sells: raw token amount, or use sellPct. */
  amountRaw?: string;
  sellPct?: number;
  slippageBps: number;
  wallet?: string | null;
  reason?: TradeReason;
  overrideUnverified?: boolean;
  acknowledgeLocked?: boolean;
  proposalId?: string;
}

/** A rule fired but needs the user's approval (or a wallet signature) before a trade is built. */
export interface Proposal {
  id: string;
  createdAt: string;
  positionId: string;
  rule: Exclude<TradeReason, 'manual'>;
  amountRaw: string;
  detail: string;
  autoExecute: boolean;
  status: 'open' | 'dismissed' | 'done';
}

export interface Alert {
  id: string;
  at: string;
  level: 'info' | 'warn' | 'error';
  message: string;
  acknowledged: boolean;
}
