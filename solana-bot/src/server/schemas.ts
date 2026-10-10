import { z } from 'zod';

const pct = z.number().min(0).max(100);
const exitRule = {
  enabled: z.boolean(),
  sellFractionPct: z.number().gt(0).max(100),
  autoExecuteLive: z.boolean(),
  applyAfterTakeProfit: z.boolean(),
};

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
    takeProfit: z.object({
      enabled: z.boolean(),
      multiple: z.number().gt(1).max(1000),
      sellFractionPct: z.number().gt(0).max(100),
      entryPriceBasis: z.enum(['effective_incl_fees', 'fill_price']),
      autoExecuteLive: z.boolean(),
    }),
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
  .refine((s) => !s.timeExit.enabled || s.timeExit.maxHoldMinutes !== null, { message: 'Set a maximum hold time before enabling the time exit.' });

export const PrepareSchema = z.object({
  side: z.enum(['buy', 'sell']),
  mint: z.string().min(32).max(44).optional(),
  positionId: z.string().optional(),
  amountSol: z.string().optional(),
  amountRaw: z.string().regex(/^\d+$/).optional(),
  sellPct: z.number().gt(0).max(100).optional(),
  slippageBps: z.number().int().min(1),
  wallet: z.string().nullable().optional(),
  reason: z.enum(['manual', 'take_profit', 'stop_loss', 'time_exit']).optional(),
  overrideUnverified: z.boolean().optional(),
  acknowledgeLocked: z.boolean().optional(),
  proposalId: z.string().optional(),
});
