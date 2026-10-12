// Pure analysis of early-buyer and funding data. Everything produced here is
// a SIGNAL (a pattern worth a human look), never proof of coordination.
import type { RiskFlag } from '../../shared/types.js';

export interface EarlyReceipt {
  slot: number;
  wallet: string;
  amountRaw: bigint;
  signature: string;
}

export interface FundingRecord {
  wallet: string;
  /** First wallet observed sending SOL to `wallet`, or null when unknown. */
  funder: string | null;
}

export interface BundleInput {
  creationSlot: number | null;
  creator: string | null;
  totalSupplyRaw: bigint;
  earlyReceipts: EarlyReceipt[];
  funding: FundingRecord[];
  /** Current holdings by wallet (raw), for weighting shared-funder clusters. */
  holdingsByWallet: Map<string, bigint>;
  /** Addresses to ignore as funders (known exchanges / programs), if any are configured. */
  ignoreFunders?: Set<string>;
  sameSlotMinWallets?: number;
  earlySlotWindow?: number;
}

export interface BundleResult {
  signals: RiskFlag[];
  earlyBuyers: number;
  sharedFunders: Array<{ funder: string; wallets: string[] }>;
}

const pct = (part: bigint, whole: bigint): number => (whole === 0n ? 0 : Number((part * 1_000_000n) / whole) / 10_000);

export function analyzeBundles(input: BundleInput): BundleResult {
  const signals: RiskFlag[] = [];
  const minWallets = input.sameSlotMinWallets ?? 3;
  const window = input.earlySlotWindow ?? 2;
  const receipts = input.earlyReceipts.filter((r) => r.wallet !== input.creator && r.amountRaw > 0n);

  // Same-slot / launch-window multi-wallet buys.
  const bySlot = new Map<number, Map<string, bigint>>();
  for (const r of receipts) {
    const m = bySlot.get(r.slot) ?? new Map<string, bigint>();
    m.set(r.wallet, (m.get(r.wallet) ?? 0n) + r.amountRaw);
    bySlot.set(r.slot, m);
  }
  for (const [slot, wallets] of [...bySlot.entries()].sort((a, b) => a[0] - b[0])) {
    if (wallets.size < minWallets) continue;
    const total = [...wallets.values()].reduce((s, x) => s + x, 0n);
    const atLaunch = input.creationSlot !== null && slot - input.creationSlot <= window;
    const share = pct(total, input.totalSupplyRaw);
    signals.push({
      code: 'bundle_same_slot',
      kind: 'signal',
      severity: atLaunch && share >= 5 ? 'high' : 'warn',
      message: `${wallets.size} different wallets received tokens in slot ${slot}${atLaunch ? ' (launch window)' : ''}, together ${share.toFixed(2)}% of supply. Same-slot buys can indicate a bundled launch, but can also be bots reacting to the same event.`,
    });
  }

  // Launch-window concentration.
  if (input.creationSlot !== null) {
    let early = 0n;
    const earlyWallets = new Set<string>();
    for (const r of receipts) {
      if (r.slot - input.creationSlot <= window) {
        early += r.amountRaw;
        earlyWallets.add(r.wallet);
      }
    }
    const share = pct(early, input.totalSupplyRaw);
    if (share >= 20) {
      signals.push({
        code: 'launch_window_concentration',
        kind: 'signal',
        severity: 'high',
        message: `${earlyWallets.size} wallets acquired ${share.toFixed(2)}% of supply within ${window} slots of creation.`,
      });
    }
  }

  // Shared funding sources among holders.
  const groups = new Map<string, string[]>();
  for (const f of input.funding) {
    if (!f.funder || input.ignoreFunders?.has(f.funder)) continue;
    const g = groups.get(f.funder) ?? [];
    g.push(f.wallet);
    groups.set(f.funder, g);
  }
  const sharedFunders: BundleResult['sharedFunders'] = [];
  for (const [funder, wallets] of groups) {
    const fromCreator = input.creator !== null && funder === input.creator;
    if (wallets.length < 2 && !fromCreator) continue;
    sharedFunders.push({ funder, wallets });
    const held = wallets.reduce((s, w) => s + (input.holdingsByWallet.get(w) ?? 0n), 0n);
    const share = pct(held, input.totalSupplyRaw);
    signals.push({
      code: fromCreator ? 'creator_funded_holders' : 'shared_funder',
      kind: 'signal',
      severity: fromCreator || share >= 10 ? 'high' : 'warn',
      message: fromCreator
        ? `${wallets.length} top holder(s) were first funded by the token creator; together they hold ${share.toFixed(2)}% of supply.`
        : `${wallets.length} top holders were first funded by the same address ${short(funder)}; together they hold ${share.toFixed(2)}% of supply. A shared funder may be an exchange hot wallet — check before drawing conclusions.`,
    });
  }

  return { signals, earlyBuyers: new Set(receipts.map((r) => r.wallet)).size, sharedFunders };
}

const short = (a: string): string => `${a.slice(0, 4)}…${a.slice(-4)}`;
