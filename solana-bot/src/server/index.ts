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
import { PrepareSchema, SettingsSchema } from './schemas.js';
import { Store } from './store.js';
import { big, entryPrice, slippageBps, fillRatio, unrealizedLamports } from './trading/accounting.js';
import { Engine, TradeRejected } from './trading/engine.js';
import type { Settings } from '../shared/types.js';

const store = new Store(join(env.dataDir, 'state.json'));
const audit = new AuditLog(join(env.dataDir, 'audit.jsonl'));
const alerts = new Alerts(store);
const conn = makeConnection(env.rpcUrl);
const devnet = makeConnection(env.devnetRpcUrl);
const provider = env.marketData === 'mock' ? new MockProvider() : new JupiterProvider();
const researcher = new Researcher(conn, provider, () => store.state.settings);
const engine = new Engine(store, audit, alerts, provider, researcher, conn);

// Never resume into live mode after a restart without the user re-enabling it.
if (store.state.mode === 'live') {
  engine.disarmLive();
  alerts.raise('warn', 'Server restarted: live mode was switched back to paper. Re-enable it explicitly if you still want it.');
}

const app = express();
app.disable('x-powered-by');
app.use(express.json({ limit: '256kb' }));

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
    const entryEff = entryPrice(p, 'effective_incl_fees');
    return {
      ...p,
      view: {
        entryPriceEffective: entryEff,
        entryPriceFill: entryPrice(p, 'fill_price'),
        takeProfitPrice: entryPrice(p, basis) * st.settings.takeProfit.multiple,
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
    env: {
      marketData: env.marketData,
      provider: provider.name,
      liveTradingAllowed: env.liveTradingAllowed,
      rpcHost: rpcHost(env.rpcUrl),
      publicRpc: /api\.mainnet-beta\.solana\.com/.test(env.rpcUrl),
      socialEnabled: !!env.xBearerToken,
      rugcheckEnabled: env.rugcheckEnabled,
      mockTokens: env.marketData === 'mock' ? MOCK_TOKENS.map((t) => ({ mint: t.mint, symbol: t.symbol, name: t.name })) : [],
    },
  });
});

app.put('/api/settings', (req, res) => {
  const parsed = SettingsSchema.safeParse({ ...req.body, boundWallet: s().settings.boundWallet });
  if (!parsed.success) return res.status(400).json({ error: 'invalid settings', violations: parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`) });
  const before = s().settings;
  store.update((st) => (st.settings = parsed.data as Settings));
  audit.append('settings.updated', { before, after: parsed.data, mode: s().mode });
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
  const body = PrepareSchema.parse(req.body);
  const intent = await engine.prepare(body);
  res.json(intent);
});
app.post('/api/trades/:id/confirm-paper', (req, res) => {
  res.json(engine.confirmPaper(req.params.id));
});
app.post('/api/trades/:id/submit', async (req, res) => {
  const { signedTx } = z.object({ signedTx: z.string().min(10).max(4000) }).parse(req.body);
  res.json(await engine.submitSigned(req.params.id, signedTx));
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

app.post('/api/wallet/bind', (req, res) => {
  const { wallet } = z.object({ wallet: z.string().nullable() }).parse(req.body);
  engine.bindWallet(wallet);
  res.json({ ok: true });
});
app.post('/api/live/arm', async (req, res) => {
  const { wallet, phrase } = z.object({ wallet: z.string(), phrase: z.string() }).parse(req.body);
  res.json(await engine.armLive(wallet, phrase));
});
app.post('/api/live/disarm', (_req, res) => {
  engine.disarmLive();
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
  if (err instanceof TradeRejected) return res.status(422).json({ error: err.message, violations: err.violations });
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
  audit.append('server.start', { mode: s().mode, marketData: env.marketData, rpcHost: rpcHost(env.rpcUrl), liveTradingAllowed: env.liveTradingAllowed });
  engine.start();
});
