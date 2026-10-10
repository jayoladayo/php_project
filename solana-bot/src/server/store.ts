// Single-process JSON state store with atomic writes. Holds no secrets: only
// public keys, positions, settings and logs.
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';
import { DEFAULT_SETTINGS } from '../shared/defaults.js';
import type { Alert, Mark, Mode, Position, Proposal, ResearchReport, Settings, TradeIntent } from '../shared/types.js';

export interface LiveArm {
  armedAt: string;
  wallet: string;
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
}

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
  };
}

/** Deep-merge saved settings over defaults so new settings keys get safe defaults. */
function mergeSettings(saved: Partial<Settings> | undefined): Settings {
  const d = structuredClone(DEFAULT_SETTINGS);
  if (!saved) return d;
  return {
    ...d,
    ...saved,
    limits: { ...d.limits, ...saved.limits },
    takeProfit: { ...d.takeProfit, ...saved.takeProfit },
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
      this.state = { ...fresh(), ...saved, settings: mergeSettings(saved.settings) };
    } else this.state = fresh();
  }

  save(): void {
    // Keep the file bounded.
    const s = this.state;
    s.intents = s.intents.slice(-300);
    s.alerts = s.alerts.slice(-200);
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
