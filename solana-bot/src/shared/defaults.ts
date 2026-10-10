import type { Settings } from './types.js';

// Conservative defaults: small sizes, tight limits, autopilot off, exit rules
// other than take-profit disabled until the user sets them.
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
    // 3× → take the initial investment back out; 5× → sell half of the
    // original amount; whatever remains (at least the moon bag) rides.
    levels: [
      { multiple: 3, sell: { kind: 'recover_cost' } },
      { multiple: 5, sell: { kind: 'percent_of_original', pct: 50 } },
    ],
    moonBagPct: 15,
    entryPriceBasis: 'effective_incl_fees',
    autoExecuteLive: false,
  },
  autopilot: {
    enabled: false,
    buySizeSol: 0.02,
    slippageBps: 100,
    maxBuysPerDay: 3,
    scanIntervalSec: 120,
    maxResearchPerScan: 3,
    sources: { newPools: true, trendingPools: true },
    watchlist: [],
    filters: {
      minLiquidityUsd: 30_000,
      minAgeMinutes: 60,
      maxAgeMinutes: 7 * 24 * 60,
      minVolume24hUsd: 50_000,
      minHolders: 300,
      minSells24h: 50,
      maxPriceChange1hPct: 50,
      requireLpLockKnown: true,
      minLpLockedPct: 90,
    },
    recheckAfterMinutes: 60,
    cooldownMinutes: 24 * 60,
    acknowledgeNoStopLoss: false,
  },
  stopLoss: {
    enabled: false,
    dropPct: null,
    sellFractionPct: 100,
    autoExecuteLive: false,
    applyAfterTakeProfit: false,
    includeMoonBag: false,
  },
  timeExit: {
    enabled: false,
    maxHoldMinutes: null,
    sellFractionPct: 100,
    autoExecuteLive: false,
    applyAfterTakeProfit: false,
    includeMoonBag: false,
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
