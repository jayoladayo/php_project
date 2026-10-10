import type { Alert, Mark, Mode, Position, Proposal, ResearchReport, Settings, TradeIntent } from '../shared/types.js';

export interface PositionView extends Position {
  view: {
    entryPriceEffective: number;
    entryPriceFill: number;
    takeProfitPrice: number;
    unrealizedLamports: string | null;
    fills: Array<{ id: string; slippageBps: number; fillRatio: number }>;
  };
  mark: Mark | null;
}

export interface DashboardState {
  mode: Mode;
  paused: boolean;
  pauseReason: string | null;
  live: { armedAt: string; wallet: string } | null;
  settings: Settings;
  positions: PositionView[];
  intents: Omit<TradeIntent, 'txBase64'>[];
  proposals: Proposal[];
  alerts: Alert[];
  paperBalanceLamports: string;
  env: {
    marketData: 'live' | 'mock';
    provider: string;
    liveTradingAllowed: boolean;
    rpcHost: string;
    publicRpc: boolean;
    socialEnabled: boolean;
    rugcheckEnabled: boolean;
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
