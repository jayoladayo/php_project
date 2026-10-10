import { z } from 'zod';

const pct = z.number().min(0).max(100);
const mintAddress = z.string().regex(/^[1-9A-HJ-NP-Za-km-z]{32,44}$/, 'not a Solana address');
const exitRule = {
  enabled: z.boolean(),
  sellFractionPct: z.number().gt(0).max(100),
  autoExecuteLive: z.boolean(),
  applyAfterTakeProfit: z.boolean(),
  includeMoonBag: z.boolean(),
};

const TakeProfitSell = z.discriminatedUnion('kind', [
  z.object({ kind: z.literal('recover_cost') }),
  z.object({ kind: z.literal('percent_of_original'), pct: z.number().gt(0).max(100) }),
  z.object({ kind: z.literal('all_except_moon_bag') }),
]);

const TakeProfit = z
  .object({
    enabled: z.boolean(),
    levels: z.array(z.object({ multiple: z.number().gt(1).max(1000), sell: TakeProfitSell })).max(6),
    moonBagPct: z.number().min(0).max(95),
    entryPriceBasis: z.enum(['effective_incl_fees', 'fill_price']),
    autoExecuteLive: z.boolean(),
  })
  .refine((t) => !t.enabled || t.levels.length > 0, { message: 'Add at least one take-profit level, or turn take-profit off.' })
  .refine((t) => new Set(t.levels.map((l) => l.multiple)).size === t.levels.length, { message: 'Each take-profit level needs a different multiple.' })
  .refine((t) => t.levels.reduce((a, l) => a + (l.sell.kind === 'percent_of_original' ? l.sell.pct : 0), 0) + t.moonBagPct <= 100, {
    message: 'Percent-of-original levels plus the moon bag add up to more than 100%.',
  })
  .refine(
    (t) => {
      const all = t.levels.filter((l) => l.sell.kind === 'all_except_moon_bag');
      return all.length === 0 || (all.length === 1 && all[0]!.multiple === Math.max(...t.levels.map((l) => l.multiple)));
    },
    { message: '"Sell all but the moon bag" can only be the last (highest) level.' },
  );

const Autopilot = z.object({
  enabled: z.boolean(),
  buySizeSol: z.number().gt(0).max(1000),
  slippageBps: z.number().int().min(1).max(5000),
  maxBuysPerDay: z.number().int().min(0).max(100),
  scanIntervalSec: z.number().int().min(30).max(3600),
  maxResearchPerScan: z.number().int().min(1).max(20),
  sources: z.object({ newPools: z.boolean(), trendingPools: z.boolean() }),
  watchlist: z.array(mintAddress).max(50),
  filters: z.object({
    minLiquidityUsd: z.number().min(0),
    minAgeMinutes: z.number().min(0),
    maxAgeMinutes: z.number().gt(0).nullable(),
    minVolume24hUsd: z.number().min(0),
    minHolders: z.number().int().min(0),
    minSells24h: z.number().int().min(0),
    maxPriceChange1hPct: z.number().nullable(),
    requireLpLockKnown: z.boolean(),
    minLpLockedPct: pct,
  }),
  recheckAfterMinutes: z.number().int().min(1).max(10_080),
  cooldownMinutes: z.number().int().min(0),
  acknowledgeNoStopLoss: z.boolean(),
});

export const SettingsSchema = z
  .object({
    limits: z.object({
      maxBuySol: z.number().gt(0).max(1000),
      dailyLossLimitSol: z.number().gt(0).max(10_000),
      maxTotalExposureSol: z.number().gt(0).max(10_000),
      maxSlippageBps: z.number().int().min(1).max(5000),
      maxPriceImpactPct: z.number().gt(0).max(50),
      maxOpenPositions: z.number().int().min(1).max(100),
      minSolReserve: z.number().min(0.005).max(100),
      capBuyByRemainingDailyLoss: z.boolean(),
    }),
    takeProfit: TakeProfit,
    autopilot: Autopilot,
    stopLoss: z.object({ ...exitRule, dropPct: pct.gt(0).lt(100).nullable() }),
    timeExit: z.object({ ...exitRule, maxHoldMinutes: z.number().int().min(1).nullable() }),
    verification: z.object({
      maxDataAgeSec: z.number().int().min(30).max(3600),
      minLiquidityUsd: z.number().min(0),
      maxTop10HolderPct: pct,
      maxSingleHolderPct: pct,
      maxPriceDisagreementPct: pct,
      maxRoundTripLossPct: pct,
      probeSol: z.number().gt(0).max(10),
    }),
    paper: z.object({
      startingBalanceSol: z.number().gt(0),
      extraSlippageBps: z.number().int().min(0).max(5000),
      priorityFeeLamports: z.number().int().min(0),
    }),
    quoteMaxAgeSec: z.number().int().min(5).max(120),
    monitorIntervalSec: z.number().int().min(10).max(3600),
    priorityFeeMaxLamports: z.number().int().min(0).max(10_000_000),
    boundWallet: z.string().nullable(),
    testWalletMaxBalanceSol: z.number().gt(0),
  })
  .refine((s) => !s.stopLoss.enabled || s.stopLoss.dropPct !== null, { message: 'Set a stop-loss percentage before enabling it.' })
  .refine((s) => !s.timeExit.enabled || s.timeExit.maxHoldMinutes !== null, { message: 'Set a maximum hold time before enabling the time exit.' })
  .refine((s) => s.autopilot.buySizeSol <= s.limits.maxBuySol, { message: 'Autopilot buy size is above your max buy limit.' })
  .refine((s) => s.autopilot.slippageBps <= s.limits.maxSlippageBps, { message: 'Autopilot slippage is above your max slippage limit.' });

export const PrepareSchema = z.object({
  side: z.enum(['buy', 'sell']),
  mint: z.string().min(32).max(44).optional(),
  positionId: z.string().optional(),
  amountSol: z.string().optional(),
  amountRaw: z.string().regex(/^\d+$/).optional(),
  sellPct: z.number().gt(0).max(100).optional(),
  slippageBps: z.number().int().min(1),
  wallet: z.string().nullable().optional(),
  // 'autopilot' is never accepted from the API: only the autopilot itself makes automatic buys.
  reason: z.enum(['manual', 'take_profit', 'stop_loss', 'time_exit']).optional(),
  overrideUnverified: z.boolean().optional(),
  acknowledgeLocked: z.boolean().optional(),
  proposalId: z.string().optional(),
  tpLevel: z.number().int().min(0).max(10).optional(),
  tpTargetRaw: z.string().regex(/^\d+$/).optional(),
});
