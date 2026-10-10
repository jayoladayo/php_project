import type { Settings } from './types.js';

// Conservative defaults: small sizes, tight limits, exit rules other than
// take-profit disabled until the user sets them.
export const DEFAULT_SETTINGS: Settings = {
  limits: {
    maxBuySol: 0.05,
    dailyLossLimitSol: 0.1,
    maxTotalExposureSol: 0.2,
    maxSlippageBps: 100,
    maxPriceImpactPct: 2,
    maxOpenPositions: 5,
    minSolReserve: 0.02,
    capBuyByRemainingDailyLoss: true,
  },
  takeProfit: {
    enabled: true,
    multiple: 5,
    sellFractionPct: 50,
    entryPriceBasis: 'effective_incl_fees',
    autoExecuteLive: false,
  },
  stopLoss: {
    enabled: false,
    dropPct: null,
    sellFractionPct: 100,
    autoExecuteLive: false,
    applyAfterTakeProfit: false,
  },
  timeExit: {
    enabled: false,
    maxHoldMinutes: null,
    sellFractionPct: 100,
    autoExecuteLive: false,
    applyAfterTakeProfit: false,
  },
  verification: {
    maxDataAgeSec: 300,
    minLiquidityUsd: 20_000,
    maxTop10HolderPct: 40,
    maxSingleHolderPct: 10,
    maxPriceDisagreementPct: 10,
    maxRoundTripLossPct: 8,
    probeSol: 0.01,
  },
  paper: {
    startingBalanceSol: 1,
    extraSlippageBps: 30,
    priorityFeeLamports: 10_000,
  },
  quoteMaxAgeSec: 45,
  monitorIntervalSec: 30,
  priorityFeeMaxLamports: 200_000,
  boundWallet: null,
  testWalletMaxBalanceSol: 1,
};
