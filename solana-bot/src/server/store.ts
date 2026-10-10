// Single-process JSON state store with atomic writes. Holds no secrets: only
// public keys, positions, settings and logs. (The bot wallet's encrypted key
// lives in its own file, see wallet/botWallet.ts.)
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';
import { DEFAULT_SETTINGS } from '../shared/defaults.js';
import type { Alert, AutopilotLogEntry, Mark, Mode, Position, Proposal, ResearchReport, Settings, SignerKind, TakeProfitRule, TradeIntent } from '../shared/types.js';
import { migratePosition } from './trading/accounting.js';

export interface LiveArm {
  armedAt: string;
  wallet: string;
  signer: SignerKind;
}

export interface AutopilotState {
  log: AutopilotLogEntry[];
  /** mint → ISO time until which a rejected token is not re-researched */
  rejected: Record<string, string>;
  lastScanAt: string | null;
  lastScanCandidates: number;
  consecutiveErrors: number;
  lastError: string | null;
}

export interface State {
  version: 1;
  mode: Mode;
  settings: Settings;
  paused: boolean;
  pauseReason: string | null;
  pausedAt: string | null;
  live: LiveArm | null;
  positions: Position[];
  intents: TradeIntent[];
  proposals: Proposal[];
  alerts: Alert[];
  marks: Record<string, Mark>;
  /** Losses outside positions (e.g. fees of failed transactions), counted toward the daily loss limit. */
  lossEvents: Array<{ at: string; lamports: string; mode: Mode; reason: string }>;
  paperBalanceLamports: string;
  research: Record<string, ResearchReport>;
  autopilot: AutopilotState;
}

const freshAutopilot = (): AutopilotState => ({ log: [], rejected: {}, lastScanAt: null, lastScanCandidates: 0, consecutiveErrors: 0, lastError: null });

function fresh(): State {
  return {
    version: 1,
    mode: 'paper',
    settings: structuredClone(DEFAULT_SETTINGS),
    paused: false,
    pauseReason: null,
    pausedAt: null,
    live: null,
    positions: [],
    intents: [],
    proposals: [],
    alerts: [],
    marks: {},
    lossEvents: [],
    paperBalanceLamports: String(Math.round(DEFAULT_SETTINGS.paper.startingBalanceSol * 1e9)),
    research: {},
    autopilot: freshAutopilot(),
  };
}

type LegacyTakeProfit = Partial<TakeProfitRule> & { multiple?: number; sellFractionPct?: number };

/** Take-profit settings from the single-rule version become a one-level ladder (unchanged defaults become the new default ladder). */
function mergeTakeProfit(d: TakeProfitRule, saved: LegacyTakeProfit | undefined): TakeProfitRule {
  if (!saved) return d;
  const { multiple, sellFractionPct, ...rest } = saved;
  const merged = { ...d, ...rest };
  if (!Array.isArray(saved.levels) && multiple !== undefined) {
    const wasDefault = multiple === 5 && (sellFractionPct ?? 50) === 50;
    if (!wasDefault) {
      merged.levels = [{ multiple, sell: { kind: 'percent_of_original', pct: sellFractionPct ?? 50 } }];
      merged.moonBagPct = 0;
    }
  }
  return merged;
}

/** Deep-merge saved settings over defaults so new settings keys get safe defaults. */
export function mergeSettings(saved: Partial<Settings> | undefined): Settings {
  const d = structuredClone(DEFAULT_SETTINGS);
  if (!saved) return d;
  return {
    ...d,
    ...saved,
    limits: { ...d.limits, ...saved.limits },
    takeProfit: mergeTakeProfit(d.takeProfit, saved.takeProfit as LegacyTakeProfit | undefined),
    autopilot: {
      ...d.autopilot,
      ...saved.autopilot,
      sources: { ...d.autopilot.sources, ...saved.autopilot?.sources },
      filters: { ...d.autopilot.filters, ...saved.autopilot?.filters },
    },
    stopLoss: { ...d.stopLoss, ...saved.stopLoss },
    timeExit: { ...d.timeExit, ...saved.timeExit },
    verification: { ...d.verification, ...saved.verification },
    paper: { ...d.paper, ...saved.paper },
  };
}

export class Store {
  state: State;

  constructor(private readonly path: string) {
    mkdirSync(dirname(path), { recursive: true });
    if (existsSync(path)) {
      const saved = JSON.parse(readFileSync(path, 'utf8')) as Partial<State>;
      const legacyMultiple = (saved.settings?.takeProfit as LegacyTakeProfit | undefined)?.multiple ?? 5;
      this.state = {
        ...fresh(),
        ...saved,
        settings: mergeSettings(saved.settings),
        positions: (saved.positions ?? []).map((p) => migratePosition(p, legacyMultiple)),
        live: saved.live ? { ...saved.live, signer: saved.live.signer ?? 'browser' } : null,
        autopilot: { ...freshAutopilot(), ...saved.autopilot },
      };
    } else this.state = fresh();
  }

  save(): void {
    // Keep the file bounded.
    const s = this.state;
    s.intents = s.intents.slice(-300);
    s.alerts = s.alerts.slice(-200);
    s.autopilot.log = s.autopilot.log.slice(-300);
    const now = Date.now();
    for (const [mint, until] of Object.entries(s.autopilot.rejected)) if (new Date(until).getTime() < now) delete s.autopilot.rejected[mint];
    s.proposals = s.proposals.filter((p) => p.status === 'open').concat(s.proposals.filter((p) => p.status !== 'open').slice(-100));
    const keys = Object.keys(s.research);
    if (keys.length > 50) for (const k of keys.slice(0, keys.length - 50)) delete s.research[k];
    const tmp = `${this.path}.tmp`;
    writeFileSync(tmp, JSON.stringify(s, null, 2), { mode: 0o600 });
    renameSync(tmp, this.path);
  }

  update<T>(fn: (s: State) => T): T {
    const r = fn(this.state);
    this.save();
    return r;
  }
}
