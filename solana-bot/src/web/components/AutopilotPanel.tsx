import { useEffect, useState } from 'react';
import { api, ApiError, type DashboardState } from '../api.js';
import { ago, short } from '../format.js';
import type { AutopilotSettings } from '../../shared/types.js';

type Editable = Omit<AutopilotSettings, 'enabled'>;
const strip = ({ enabled: _e, ...rest }: AutopilotSettings): Editable => rest;

export function AutopilotPanel({ state, onChange }: { state: DashboardState; onChange?: () => void }) {
  const ap = state.autopilot;
  const [cfg, setCfg] = useState<Editable>(strip(state.settings.autopilot));
  const [watch, setWatch] = useState(state.settings.autopilot.watchlist.join('\n'));
  const [phrase, setPhrase] = useState('');
  const [msg, setMsg] = useState<{ ok: boolean; text: string; v?: string[] } | null>(null);
  const [open, setOpen] = useState<number | null>(null);
  const saved = JSON.stringify(strip(state.settings.autopilot));
  const current = JSON.stringify({ ...cfg, watchlist: parseWatch(watch) });
  const dirty = saved !== current;
  useEffect(() => {
    if (!dirty) {
      setCfg(strip(state.settings.autopilot));
      setWatch(state.settings.autopilot.watchlist.join('\n'));
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [saved]);

  const live = state.mode === 'live';
  const run = async (fn: () => Promise<string>) => {
    setMsg(null);
    try {
      setMsg({ ok: true, text: await fn() });
      onChange?.();
    } catch (e) {
      setMsg({ ok: false, text: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };
  const save = () => run(async () => (await api('/settings', { ...state.settings, autopilot: { ...cfg, watchlist: parseWatch(watch), enabled: state.settings.autopilot.enabled } }, 'PUT'), 'Strategy saved.'));

  const n = (label: string, value: number | null, onChange: (v: number | null) => void, hint?: string, nullable = false) => (
    <label className="field">
      <span>{label}</span>
      <input type="number" step="any" value={value ?? ''} placeholder={nullable ? 'no limit' : ''} onChange={(e) => onChange(e.target.value === '' && nullable ? null : Number(e.target.value))} />
      {hint && <small className="muted">{hint}</small>}
    </label>
  );
  const f = cfg.filters;
  const setF = (patch: Partial<Editable['filters']>) => setCfg({ ...cfg, filters: { ...f, ...patch } });

  const status = !ap.enabled ? 'OFF' : state.paused ? 'ON — paused' : ap.active ? 'RUNNING' : 'ON — waiting';
  return (
    <section>
      <div className={`box ${ap.enabled ? (live ? 'live' : 'verdict verified') : ''}`}>
        <div className="row">
          <h2 className="grow">Autopilot: {status} <span className={`badge ${state.mode}`}>{live ? 'LIVE' : 'PAPER'}</span></h2>
          {ap.enabled ? (
            <>
              <button onClick={() => run(async () => (await api('/autopilot/scan-now', {}), 'Scan started.'))}>Scan now</button>
              <button className="danger" onClick={() => run(async () => (await api('/autopilot/stop', {}), 'Autopilot stopped. Open positions keep their exit rules.'))}>Stop autopilot</button>
            </>
          ) : (
            <>
              {live && <input placeholder="Type START AUTOPILOT" value={phrase} onChange={(e) => setPhrase(e.target.value)} />}
              <button className="primary" disabled={dirty || ap.startProblems.length > 0 || (live && phrase !== 'START AUTOPILOT')} onClick={() => run(async () => (await api('/autopilot/start', { phrase }), live ? 'LIVE autopilot started.' : 'Paper autopilot started.'))}>
                Start autopilot
              </button>
            </>
          )}
        </div>
        <p className="small">
          Last scan {ago(ap.lastScanAt)} · {ap.lastScanCandidates} pools seen · buys today {ap.buysToday} / {state.settings.autopilot.maxBuysPerDay} · {live ? `live with ${state.live?.signer === 'bot' ? 'the bot wallet' : 'your browser wallet'}` : 'paper money only'}
        </p>
        {!ap.enabled && ap.startProblems.length > 0 && (
          <ul className="neg small">{ap.startProblems.map((p) => <li key={p}>{p}</li>)}</ul>
        )}
        {dirty && <p className="small neg">Save the strategy changes below before starting.</p>}
        <p className="muted small">
          How it decides: it discovers tokens from new and trending pools (and your watchlist), researches the best few in full, and buys <b>only</b> tokens that pass verification <i>and</i> every filter below — never because of social posts, paid promotion or a price spike. Every buy is limited by your risk limits, the daily buy cap and per-token cooldowns. Exits follow your take-profit ladder (and stop-loss / time exit if enabled). In paper mode nothing real happens; live autopilot needs the bot wallet.
        </p>
      </div>

      <h3>Strategy</h3>
      <div className="fields">
        {n('Buy size per token (SOL)', cfg.buySizeSol, (v) => setCfg({ ...cfg, buySizeSol: v ?? 0 }), `max buy limit: ${state.settings.limits.maxBuySol} SOL`)}
        {n('Slippage (bps)', cfg.slippageBps, (v) => setCfg({ ...cfg, slippageBps: v ?? 0 }), `limit: ${state.settings.limits.maxSlippageBps}`)}
        {n('Max buys per day', cfg.maxBuysPerDay, (v) => setCfg({ ...cfg, maxBuysPerDay: v ?? 0 }))}
        {n('Scan every (seconds)', cfg.scanIntervalSec, (v) => setCfg({ ...cfg, scanIntervalSec: v ?? 120 }), 'min 30')}
        {n('Tokens researched per scan', cfg.maxResearchPerScan, (v) => setCfg({ ...cfg, maxResearchPerScan: v ?? 1 }), 'each full research uses ~30–40 RPC calls')}
        {n('Re-check a rejected token after (min)', cfg.recheckAfterMinutes, (v) => setCfg({ ...cfg, recheckAfterMinutes: v ?? 60 }))}
        {n('Do not re-buy a token within (min)', cfg.cooldownMinutes, (v) => setCfg({ ...cfg, cooldownMinutes: v ?? 0 }))}
      </div>
      <div className="row">
        <label className="field check"><input type="checkbox" checked={cfg.sources.newPools} onChange={(e) => setCfg({ ...cfg, sources: { ...cfg.sources, newPools: e.target.checked } })} /> <span>Discover from new pools (GeckoTerminal)</span></label>
        <label className="field check"><input type="checkbox" checked={cfg.sources.trendingPools} onChange={(e) => setCfg({ ...cfg, sources: { ...cfg.sources, trendingPools: e.target.checked } })} /> <span>Discover from trending pools (GeckoTerminal)</span></label>
      </div>
      <label className="field">
        <span>Watchlist (one mint address per line; still fully researched and filtered)</span>
        <textarea rows={3} className="mono" value={watch} onChange={(e) => setWatch(e.target.value)} />
      </label>

      <h3>Buy filters (all must pass, on top of verification)</h3>
      <div className="fields">
        {n('Min liquidity (USD)', f.minLiquidityUsd, (v) => setF({ minLiquidityUsd: v ?? 0 }))}
        {n('Min age (minutes)', f.minAgeMinutes, (v) => setF({ minAgeMinutes: v ?? 0 }), 'most rugs happen in the first hour')}
        {n('Max age (minutes)', f.maxAgeMinutes, (v) => setF({ maxAgeMinutes: v }), 'blank = any age', true)}
        {n('Min 24h volume (USD)', f.minVolume24hUsd, (v) => setF({ minVolume24hUsd: v ?? 0 }))}
        {n('Min holders', f.minHolders, (v) => setF({ minHolders: v ?? 0 }))}
        {n('Min sells in 24h', f.minSells24h, (v) => setF({ minSells24h: v ?? 0 }), 'proof that other people can sell')}
        {n('Max 1h price change (%)', f.maxPriceChange1hPct, (v) => setF({ maxPriceChange1hPct: v }), 'skip vertical spikes; blank = no limit', true)}
        {n('Min LP locked/burned (%)', f.minLpLockedPct, (v) => setF({ minLpLockedPct: v ?? 0 }))}
        <label className="field check"><input type="checkbox" checked={f.requireLpLockKnown} onChange={(e) => setF({ requireLpLockKnown: e.target.checked })} /> <span>Skip tokens whose LP lock status is unknown</span></label>
      </div>
      {!state.settings.stopLoss.enabled && (
        <label className="override">
          <input type="checkbox" checked={cfg.acknowledgeNoStopLoss} onChange={(e) => setCfg({ ...cfg, acknowledgeNoStopLoss: e.target.checked })} /> Run the live autopilot <b>without a stop-loss</b>: I accept that any position can go to zero. (Or enable a stop-loss in Limits & rules.)
        </label>
      )}
      <div className="row sticky">
        <button className="primary" disabled={!dirty} onClick={save}>Save strategy</button>
        <button disabled={!dirty} onClick={() => { setCfg(strip(state.settings.autopilot)); setWatch(state.settings.autopilot.watchlist.join('\n')); }}>Discard</button>
        {msg && <span className={msg.ok ? 'pos' : 'neg'}>{msg.text} {msg.v?.join('; ')}</span>}
      </div>

      <h3>Activity</h3>
      {ap.log.length === 0 ? (
        <p className="muted">Nothing yet.</p>
      ) : (
        <table className="list small">
          <thead><tr><th>When</th><th>What</th><th>Token</th><th>Details</th></tr></thead>
          <tbody>
            {ap.log.map((e, i) => (
              <tr key={i}>
                <td>{ago(e.at)}</td>
                <td><span className={`badge ${e.action === 'buy' ? 'paper' : e.action === 'error' ? 'live' : ''}`}>{e.action}</span></td>
                <td>{e.mint ? <><b>{e.symbol ?? '?'}</b> <a className="mono" href={`https://dexscreener.com/solana/${e.mint}`} target="_blank" rel="noreferrer">{short(e.mint)}</a>{e.source && <span className="muted"> · {e.source.replace('_', ' ')}</span>}</> : '—'}</td>
                <td>
                  {e.detail}
                  {e.reasons && e.reasons.length > 1 && (
                    <>
                      {' '}<button className="link" onClick={() => setOpen(open === i ? null : i)}>{open === i ? 'hide' : `all ${e.reasons.length} checks`}</button>
                      {open === i && <ul>{e.reasons.map((r) => <li key={r}>{r}</li>)}</ul>}
                    </>
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </section>
  );
}

function parseWatch(s: string): string[] {
  return [...new Set(s.split(/[\s,]+/).map((x) => x.trim()).filter(Boolean))];
}
