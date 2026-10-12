// Environment configuration. Secrets (API keys, RPC URLs with keys, the
// optional bot-wallet passphrase) come only from the environment / a secret
// manager and are registered for redaction. The bot never reads or accepts the
// user's own wallet keys; the bot wallet is generated locally.
import 'dotenv/config';
import { registerSecret } from './audit.js';

const bool = (v: string | undefined) => v === 'true' || v === '1';

export const env = {
  port: Number(process.env.PORT ?? 8787),
  host: process.env.HOST ?? '127.0.0.1',
  /** "live" uses real market data (paper or live trading). "mock" is an offline simulator for paper trading only. */
  marketData: (process.env.MARKET_DATA ?? 'live') as 'live' | 'mock',
  /** Practice runs on the simulated market keep their own state, separate from real-data state. */
  dataDir: process.env.DATA_DIR ?? (process.env.MARKET_DATA === 'mock' ? 'data-mock' : 'data'),
  /** The bot wallet lives in one fixed place whatever the market mode, so it is never "lost" between modes. */
  botWalletPath: process.env.BOT_WALLET_PATH ?? 'data/bot-wallet.json',
  rpcUrl: process.env.SOLANA_RPC_URL ?? 'https://api.mainnet-beta.solana.com',
  devnetRpcUrl: process.env.SOLANA_DEVNET_RPC_URL ?? 'https://api.devnet.solana.com',
  jupiterBase: process.env.JUPITER_API_BASE ?? 'https://lite-api.jup.ag/swap/v1',
  jupiterApiKey: process.env.JUPITER_API_KEY || undefined,
  rugcheckEnabled: process.env.RUGCHECK_ENABLED !== 'false',
  xBearerToken: process.env.X_BEARER_TOKEN || undefined,
  alertWebhookUrl: process.env.ALERT_WEBHOOK_URL || undefined,
  telegramBotToken: process.env.TELEGRAM_BOT_TOKEN || undefined,
  telegramChatId: process.env.TELEGRAM_CHAT_ID || undefined,
  /**
   * Optional: unlock the bot wallet at startup (for unattended restarts).
   * Storing the passphrase next to the encrypted key makes the encryption
   * pointless against anyone with access to this machine.
   */
  botWalletPassphrase: process.env.BOT_WALLET_PASSPHRASE || undefined,
  /** Optional: after a restart, return to live mode + autopilot with the bot wallet instead of dropping to paper. */
  resumeLiveAfterRestart: bool(process.env.RESUME_LIVE_AFTER_RESTART),
  /** Hard gate: live mode cannot be armed from the dashboard unless this is true. */
  liveTradingAllowed: bool(process.env.LIVE_TRADING_ALLOWED),
  /** Optional shared secret required on mutating API calls. */
  dashboardToken: process.env.DASHBOARD_TOKEN || undefined,
  /** Host names the API answers to (blocks DNS-rebinding attacks from web pages). */
  allowedHosts: (process.env.ALLOWED_HOSTS ?? 'localhost,127.0.0.1,[::1]')
    .split(',')
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean),
  allowedOrigins: (process.env.ALLOWED_ORIGINS ?? 'http://localhost:5173,http://127.0.0.1:5173,http://localhost:8787,http://127.0.0.1:8787')
    .split(',')
    .map((s) => s.trim())
    .filter(Boolean),
};

for (const s of [env.jupiterApiKey, env.xBearerToken, env.dashboardToken, env.alertWebhookUrl, env.telegramBotToken, env.botWalletPassphrase]) registerSecret(s);
// RPC URLs commonly embed API keys in the path or query.
for (const u of [env.rpcUrl, env.devnetRpcUrl]) {
  try {
    const url = new URL(u);
    for (const v of url.searchParams.values()) registerSecret(v);
    for (const seg of url.pathname.split('/')) if (seg.length >= 20) registerSecret(seg);
  } catch {
    /* invalid URL is reported at startup */
  }
}

/** RPC host for display, never the full URL. */
export const rpcHost = (u: string): string => {
  try {
    return new URL(u).host;
  } catch {
    return 'invalid-url';
  }
};
