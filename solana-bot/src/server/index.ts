import { existsSync } from 'node:fs';
import { join, resolve } from 'node:path';
import express, { type NextFunction, type Request, type Response } from 'express';
import { z } from 'zod';
import { Alerts } from './alerts.js';
import { AuditLog, redactString } from './audit.js';
import { buildDevnetTest, submitDevnetTest } from './devnet.js';
import { env, rpcHost } from './env.js';
import { JupiterProvider } from './market/jupiter.js';
import { MOCK_TOKENS, MockProvider } from './market/mock.js';
import { Researcher } from './research/index.js';
import { makeConnection } from './research/onchain.js';
import { LiveScanner, MockScanner } from './research/scanner.js';
import { PrepareSchema, SettingsSchema } from './schemas.js';
import { Store } from './store.js';
import { big, entryPrice, fillRatio, slippageBps, unrealizedLamports } from './trading/accounting.js';
import { Autopilot } from './trading/autopilot.js';
import { Engine, TradeRejected, requireConfirmed } from './trading/engine.js';
import { levelLabel } from './trading/rules.js';
import { autopilotBuysToday } from './trading/strategy.js';
import { BotWallet, WrongPassphraseError } from './wallet/botWallet.js';
import type { Settings } from '../shared/types.js';

const store = new Store(join(env.dataDir, 'state.json'));
const audit = new AuditLog(join(env.dataDir, 'audit.jsonl'));
const alerts = new Alerts(store);
const conn = makeConnection(env.rpcUrl);
const devnet = makeConnection(env.devnetRpcUrl);
const provider = env.marketData === 'mock' ? new MockProvider({ randomWalk: true }) : new JupiterProvider();
const researcher = new Researcher(conn, provider, () => store.state.settings);
const botWallet = new BotWallet(env.botWalletPath);
const engine = new Engine(store, audit, alerts, provider, researcher, conn, botWallet);
const scanner = provider instanceof MockProvider ? new MockScanner(provider) : new LiveScanner();
const autopilot = new Autopilot(store, audit, alerts, engine, scanner, botWallet);

// ---------------------------------------------------------------- startup safety

// Practice positions on simulated tokens cannot be priced with real market data
// (they would make the monitor fail and pause trading): drop them.
if (!provider.mock) {
  const mockMints = new Set(MOCK_TOKENS.map((t) => t.mint));
  const stale = store.state.positions.filter((p) => mockMints.has(p.mint));
  if (stale.length) {
    store.update((st) => {
      st.positions = st.positions.filter((p) => !mockMints.has(p.mint));
      for (const m of mockMints) delete st.research[m];
    });
    audit.append('startup.removed_mock_positions', { count: stale.length, ids: stale.map((p) => p.id) });
    alerts.raise('info', `Removed ${stale.length} practice position(s) on simulated tokens: they cannot be priced with real market data. (Simulated-market practice now keeps its own data in data-mock/.) Use "Reset paper account" for a fresh paper balance.`);
  }
}

if (env.botWalletPassphrase && botWallet.exists()) {
  try {
    botWallet.unlock(env.botWalletPassphrase);
    console.log('Bot wallet unlocked from BOT_WALLET_PASSPHRASE.');
  } catch {
    alerts.raise('error', 'BOT_WALLET_PASSPHRASE did not unlock the bot wallet.');
  }
}
if (store.state.mode === 'live') {
  const st = store.state;
  const canResume =
    env.resumeLiveAfterRestart && env.liveTradingAllowed && !provider.mock && st.live?.signer === 'bot' && botWallet.isUnlocked() && st.live.wallet === botWallet.publicKey();
  if (canResume) {
    alerts.raise('warn', `Server restarted: resumed LIVE mode with the bot wallet (RESUME_LIVE_AFTER_RESTART=true).${st.settings.autopilot.enabled ? ' The autopilot is running.' : ''}`);
  } else {
    const wasAuto = st.settings.autopilot.enabled;
    engine.disarmLive();
    alerts.raise('warn', `Server restarted: live mode was switched back to paper${wasAuto ? ' and the autopilot was stopped' : ''}. Re-enable it explicitly if you still want it.`);
  }
}

const app = express();
app.disable('x-powered-by');
app.use(express.json({ limit: '256kb' }));

// Only answer to local host names: a web page cannot reach this API through DNS rebinding.
app.use((req: Request, res: Response, next: NextFunction) => {
  const host = (req.headers.host ?? '').toLowerCase().replace(/:\d+$/, '');
  if (!env.allowedHosts.includes(host)) return res.status(403).json({ error: 'host not allowed' });
  next();
});

// Same-origin / allowed-origin and optional token check on every mutating call.
app.use('/api', (req: Request, res: Response, next: NextFunction) => {
  if (req.method !== 'GET') {
    const origin = req.headers.origin;
    if (origin && !env.allowedOrigins.includes(origin)) return res.status(403).json({ error: 'origin not allowed' });
    if (env.dashboardToken && req.headers['x-dashboard-token'] !== env.dashboardToken) return res.status(401).json({ error: 'dashboard token required' });
  }
  next();
});

const s = () => store.state;

app.get('/api/state', (_req, res) => {
  const st = s();
  const basis = st.settings.takeProfit.entryPriceBasis;
  const positions = st.positions.map((p) => {
    const mark = st.marks[p.id] ?? null;
    const unreal = mark && !mark.stale ? unrealizedLamports(p, big(mark.netValueLamports)) : null;
    const entry = entryPrice(p, basis);
    return {
      ...p,
      view: {
        entryPriceEffective: entryPrice(p, 'effective_incl_fees'),
        entryPriceFill: entryPrice(p, 'fill_price'),
        levels: p.takeProfit.levels.map((l) => ({ label: levelLabel(l), triggerPrice: entry * l.multiple, state: l.state, targetRaw: l.targetRaw, soldRaw: l.soldRaw })),
        unrealizedLamports: unreal?.toString() ?? null,
        fills: p.fills.map((f) => ({ id: f.id, slippageBps: slippageBps(f), fillRatio: fillRatio(f) })),
      },
      mark,
    };
  });
  res.json({
    mode: st.mode,
    paused: st.paused,
    pauseReason: st.pauseReason,
    live: st.live,
    settings: st.settings,
    positions,
    intents: st.intents.slice(-50).reverse().map(({ txBase64: _omit, ...i }) => i),
    proposals: st.proposals.filter((p) => p.status === 'open'),
    alerts: st.alerts.filter((a) => !a.acknowledged).slice(-20).reverse(),
    paperBalanceLamports: st.paperBalanceLamports,
    botWallet: { exists: botWallet.exists(), publicKey: botWallet.publicKey(), unlocked: botWallet.isUnlocked() },
    autopilot: {
      enabled: st.settings.autopilot.enabled,
      active: engine.autopilotActive(),
      lastScanAt: st.autopilot.lastScanAt,
      lastScanCandidates: st.autopilot.lastScanCandidates,
      consecutiveErrors: st.autopilot.consecutiveErrors,
      buysToday: autopilotBuysToday(st.positions, st.mode, new Date()),
      startProblems: autopilot.startProblems('START AUTOPILOT').filter((p) => !p.startsWith('Type START')),
      log: st.autopilot.log.slice(-80).reverse(),
    },
    env: {
      marketData: env.marketData,
      provider: provider.name,
      liveTradingAllowed: env.liveTradingAllowed,
      rpcHost: rpcHost(env.rpcUrl),
      publicRpc: /api\.mainnet-beta\.solana\.com/.test(env.rpcUrl),
      socialEnabled: !!env.xBearerToken,
      rugcheckEnabled: env.rugcheckEnabled,
      notifications: [env.alertWebhookUrl ? 'webhook' : null, env.telegramBotToken && env.telegramChatId ? 'telegram' : null].filter(Boolean),
      resumeLiveAfterRestart: env.resumeLiveAfterRestart,
      mockTokens: env.marketData === 'mock' ? MOCK_TOKENS.map((t) => ({ mint: t.mint, symbol: t.symbol, name: t.name })) : [],
    },
  });
});

app.put('/api/settings', (req, res) => {
  // The bound wallet and the autopilot on/off switch have their own guarded endpoints.
  const cur = s().settings;
  const body = { ...req.body, boundWallet: cur.boundWallet, autopilot: { ...(req.body?.autopilot ?? {}), enabled: cur.autopilot.enabled } };
  const parsed = SettingsSchema.safeParse(body);
  if (!parsed.success) return res.status(400).json({ error: 'invalid settings', violations: parsed.error.issues.map((i) => `${i.path.join('.') || 'settings'}: ${i.message}`) });
  store.update((st) => (st.settings = parsed.data as Settings));
  audit.append('settings.updated', { before: cur, after: parsed.data, mode: s().mode });
  res.json({ ok: true });
});

app.post('/api/research', async (req, res) => {
  const { mint } = z.object({ mint: z.string().min(1).max(64) }).parse(req.body);
  res.json(await engine.research(mint));
});
app.get('/api/research/:mint', (req, res) => {
  const r = s().research[req.params.mint];
  return r ? res.json(r) : res.status(404).json({ error: 'not researched yet' });
});

app.post('/api/trades/prepare', async (req, res) => {
  res.json(await engine.prepare(PrepareSchema.parse(req.body)));
});
app.post('/api/trades/:id/confirm-paper', (req, res) => {
  res.json(engine.confirmPaper(req.params.id));
});
app.post('/api/trades/:id/submit', async (req, res) => {
  const { signedTx } = z.object({ signedTx: z.string().min(10).max(4000) }).parse(req.body);
  res.json(requireConfirmed(await engine.submitSigned(req.params.id, signedTx)));
});
app.post('/api/trades/:id/execute', async (req, res) => {
  res.json(await engine.executeWithBot(req.params.id));
});
app.post('/api/trades/:id/cancel', (req, res) => {
  engine.cancel(req.params.id);
  res.json({ ok: true });
});
app.post('/api/trades/:id/reconcile', async (req, res) => {
  res.json(await engine.reconcile(req.params.id));
});
app.post('/api/proposals/:id/dismiss', (req, res) => {
  engine.dismissProposal(req.params.id);
  res.json({ ok: true });
});

app.post('/api/pause', (req, res) => {
  const { reason } = z.object({ reason: z.string().max(200).optional() }).parse(req.body ?? {});
  engine.pause(reason || 'emergency pause pressed', 'user');
  res.json({ ok: true });
});
app.post('/api/resume', (req, res) => {
  const { phrase } = z.object({ phrase: z.string() }).parse(req.body);
  if (phrase !== 'RESUME') return res.status(400).json({ error: 'Type RESUME to confirm.' });
  if (s().intents.some((i) => i.status === 'unknown')) return res.status(409).json({ error: 'Reconcile trades with unknown outcome first.' });
  engine.resume();
  res.json({ ok: true });
});

// ---------------------------------------------------------------- wallets

const Passphrase = z.string().min(1).max(512);

app.post('/api/wallet/bind', (req, res) => {
  const { wallet, passphrase } = z.object({ wallet: z.string().nullable(), passphrase: Passphrase.optional() }).parse(req.body);
  engine.bindWallet(wallet, passphrase);
  res.json({ ok: true });
});
app.post('/api/botwallet/create', (req, res) => {
  const { passphrase } = z.object({ passphrase: Passphrase }).parse(req.body);
  const publicKey = botWallet.create(passphrase);
  audit.append('botwallet.created', { publicKey });
  alerts.raise('info', `Bot wallet created: ${publicKey}. Send it only what you are prepared to lose.`);
  res.json({ publicKey });
});
app.post('/api/botwallet/unlock', (req, res) => {
  const { passphrase } = z.object({ passphrase: Passphrase }).parse(req.body);
  botWallet.unlock(passphrase);
  audit.append('botwallet.unlocked', {});
  res.json({ ok: true });
});
app.post('/api/botwallet/lock', (_req, res) => {
  botWallet.lock();
  audit.append('botwallet.locked', {});
  if (s().live?.signer === 'bot') alerts.raise('warn', 'Bot wallet locked: live trading with it is on hold until you unlock it again.');
  res.json({ ok: true });
});
app.get('/api/botwallet/balance', async (_req, res) => {
  const publicKey = botWallet.publicKey();
  if (!publicKey) return res.status(404).json({ error: 'no bot wallet' });
  const lamports = await engine.balance('live', publicKey);
  res.json({ publicKey, lamports: lamports?.toString() ?? null });
});
app.post('/api/botwallet/withdraw', async (req, res) => {
  const b = z.object({ passphrase: Passphrase, amountSol: z.string().optional(), all: z.boolean().optional() }).parse(req.body);
  if (!b.all && !b.amountSol) return res.status(400).json({ error: 'Enter an amount or choose "everything".' });
  res.json(await engine.withdraw(b.passphrase, b.all ? 'all' : b.amountSol!));
});

// ---------------------------------------------------------------- live mode & autopilot

app.post('/api/live/arm', async (req, res) => {
  const b = z.object({ signer: z.enum(['browser', 'bot']).default('browser'), wallet: z.string().nullable().optional(), phrase: z.string() }).parse(req.body);
  res.json(await engine.armLive(b.signer, b.wallet ?? null, b.phrase));
});
app.post('/api/live/disarm', (_req, res) => {
  engine.disarmLive();
  res.json({ ok: true });
});
app.post('/api/autopilot/start', (req, res) => {
  const { phrase } = z.object({ phrase: z.string().optional() }).parse(req.body ?? {});
  autopilot.enable(phrase);
  res.json({ ok: true });
});
app.post('/api/autopilot/stop', (_req, res) => {
  autopilot.disable('stopped by user');
  res.json({ ok: true });
});
app.post('/api/autopilot/scan-now', (_req, res) => {
  void autopilot.cycle();
  res.json({ ok: true });
});

app.post('/api/paper/reset', (_req, res) => {
  if (s().mode !== 'paper') return res.status(409).json({ error: 'Switch to paper mode first.' });
  autopilot.disable('paper account reset');
  store.update((st) => {
    const paperIds = new Set(st.positions.filter((p) => p.mode === 'paper').map((p) => p.id));
    st.positions = st.positions.filter((p) => p.mode !== 'paper');
    for (const id of paperIds) delete st.marks[id];
    st.intents = st.intents.filter((i) => i.mode !== 'paper');
    st.proposals = st.proposals.filter((p) => !paperIds.has(p.positionId));
    st.lossEvents = st.lossEvents.filter((e) => e.mode !== 'paper');
    st.paperBalanceLamports = String(Math.round(st.settings.paper.startingBalanceSol * 1e9));
    st.autopilot.log = [];
    st.autopilot.rejected = {};
  });
  audit.append('paper.reset', {});
  res.json({ ok: true });
});

app.post('/api/alerts/:id/ack', (req, res) => {
  alerts.acknowledge(req.params.id);
  res.json({ ok: true });
});
app.get('/api/audit', (req, res) => {
  const limit = Math.min(Number(req.query.limit ?? 100), 1000);
  res.json({ entries: audit.tail(limit).reverse(), chainBrokenAt: audit.verify() });
});

app.post('/api/devnet/test-tx', async (req, res) => {
  const { wallet } = z.object({ wallet: z.string() }).parse(req.body);
  res.json(await buildDevnetTest(devnet, wallet));
});
app.post('/api/devnet/submit', async (req, res) => {
  const b = z.object({ signedTx: z.string(), blockhash: z.string(), lastValidBlockHeight: z.number() }).parse(req.body);
  const r = await submitDevnetTest(devnet, b.signedTx, b.blockhash, b.lastValidBlockHeight);
  audit.append('devnet.test', r);
  res.json(r);
});
app.post('/api/devnet/airdrop', async (req, res) => {
  const { wallet } = z.object({ wallet: z.string() }).parse(req.body);
  const { PublicKey } = await import('@solana/web3.js');
  const sig = await devnet.requestAirdrop(new PublicKey(wallet), 500_000_000);
  res.json({ signature: sig });
});

app.post('/api/mock/move-price', (req, res) => {
  if (!(provider instanceof MockProvider)) return res.status(404).json({ error: 'mock mode only' });
  const { mint, multiplier } = z.object({ mint: z.string(), multiplier: z.number().gt(0).max(1000) }).parse(req.body);
  provider.movePrice(mint, multiplier);
  audit.append('mock.price_moved', { mint, multiplier });
  void engine.monitorTick();
  res.json({ ok: true });
});

app.use((err: unknown, _req: Request, res: Response, _next: NextFunction) => {
  // Malformed JSON: the parser's message quotes part of the body (which may hold a passphrase), so never echo or log it.
  if ((err as { type?: string })?.type === 'entity.parse.failed') return res.status(400).json({ error: 'invalid JSON body' });
  if (err instanceof TradeRejected) return res.status(422).json({ error: err.message, violations: err.violations });
  if (err instanceof WrongPassphraseError) return res.status(422).json({ error: err.message, violations: [] });
  if (err instanceof z.ZodError) return res.status(400).json({ error: 'invalid request', violations: err.issues.map((i) => `${i.path.join('.')}: ${i.message}`) });
  const message = redactString((err as Error)?.message ?? 'internal error');
  audit.append('error', { message });
  res.status(500).json({ error: message });
});

// Serve the built dashboard in production.
const dist = resolve('dist');
if (existsSync(dist)) {
  app.use(express.static(dist));
  app.get(/^(?!\/api).*/, (_req, res) => res.sendFile(join(dist, 'index.html')));
}

app.listen(env.port, env.host, () => {
  console.log(`solana-research-trader API on http://${env.host}:${env.port}  mode=${s().mode} market=${env.marketData} rpc=${rpcHost(env.rpcUrl)} liveAllowed=${env.liveTradingAllowed}`);
  if (env.host !== '127.0.0.1' && env.host !== 'localhost' && !env.dashboardToken) console.warn('WARNING: listening on a non-local interface without DASHBOARD_TOKEN.');
  audit.append('server.start', { mode: s().mode, marketData: env.marketData, rpcHost: rpcHost(env.rpcUrl), liveTradingAllowed: env.liveTradingAllowed, botWallet: botWallet.publicKey() });
  engine.start();
  autopilot.start();
  // The offline simulator moves prices so the autopilot and the ladder have something to react to.
  if (provider instanceof MockProvider) setInterval(() => provider.tick(), 15_000);
});
