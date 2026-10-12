import type { Alert, AutopilotLogEntry, Mark, Mode, Position, Proposal, ResearchReport, Settings, SignerKind, TakeProfitLevelState, TradeIntent } from '../shared/types.js';

export interface PositionView extends Position {
  view: {
    entryPriceEffective: number;
    entryPriceFill: number;
    levels: Array<{ label: string; triggerPrice: number; state: TakeProfitLevelState['state']; targetRaw: string | null; soldRaw: string }>;
    unrealizedLamports: string | null;
    fills: Array<{ id: string; slippageBps: number; fillRatio: number }>;
  };
  mark: Mark | null;
}

export interface DashboardState {
  mode: Mode;
  paused: boolean;
  pauseReason: string | null;
  live: { armedAt: string; wallet: string; signer: SignerKind } | null;
  settings: Settings;
  positions: PositionView[];
  intents: Omit<TradeIntent, 'txBase64'>[];
  proposals: Proposal[];
  alerts: Alert[];
  paperBalanceLamports: string;
  botWallet: { exists: boolean; publicKey: string | null; unlocked: boolean };
  autopilot: {
    enabled: boolean;
    active: boolean;
    lastScanAt: string | null;
    lastScanCandidates: number;
    consecutiveErrors: number;
    buysToday: number;
    startProblems: string[];
    log: AutopilotLogEntry[];
  };
  env: {
    marketData: 'live' | 'mock';
    provider: string;
    liveTradingAllowed: boolean;
    rpcHost: string;
    publicRpc: boolean;
    socialEnabled: boolean;
    rugcheckEnabled: boolean;
    notifications: string[];
    resumeLiveAfterRestart: boolean;
    mockTokens: Array<{ mint: string; symbol: string; name: string }>;
  };
}

export class ApiError extends Error {
  constructor(
    message: string,
    readonly violations: string[],
  ) {
    super(message);
  }
}

function token(): string | null {
  try {
    return localStorage.getItem('dashboardToken');
  } catch {
    return null;
  }
}

export async function api<T>(path: string, body?: unknown, method?: string): Promise<T> {
  const t = token();
  const res = await fetch(`/api${path}`, {
    method: method ?? (body === undefined ? 'GET' : 'POST'),
    headers: { 'content-type': 'application/json', ...(t ? { 'x-dashboard-token': t } : {}) },
    body: body === undefined ? undefined : JSON.stringify(body),
  });
  const data = (await res.json().catch(() => ({}))) as { error?: string; violations?: string[] };
  if (!res.ok) throw new ApiError(data.error ?? `HTTP ${res.status}`, data.violations ?? []);
  return data as T;
}

export type { ResearchReport, TradeIntent };
