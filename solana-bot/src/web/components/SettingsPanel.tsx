import { useEffect, useState } from 'react';
import { api, ApiError, type DashboardState } from '../api.js';
import type { Settings, TakeProfitLevel, TakeProfitRule, TakeProfitSell } from '../../shared/types.js';

type Path = [keyof Settings, string] | [keyof Settings];

// The autopilot section is edited (and saved) in its own tab.
const withoutAutopilot = (s: Settings) => JSON.stringify({ ...s, autopilot: null });

/** Rough illustration of the ladder on a 1,000-token position (ignores fees and slippage). */
export function ladderPreview(tp: Pick<TakeProfitRule, 'levels' | 'moonBagPct'>): string[] {
  const original = 1000;
  const moon = (original * tp.moonBagPct) / 100;
  let held = original;
  let recovered = 0; // in units of the initial investment
  const lines: string[] = [];
  for (const l of [...tp.levels].sort((a, b) => a.multiple - b.multiple)) {
    const sellable = Math.max(0, held - moon);
    let sell = 0;
    if (l.sell.kind === 'recover_cost') sell = Math.min(sellable, Math.max(0, ((1 - recovered) * original) / l.multiple));
    else if (l.sell.kind === 'percent_of_original') sell = Math.min(sellable, (original * l.sell.pct) / 100);
    else sell = sellable;
    held -= sell;
    recovered += (sell * l.multiple) / original;
    lines.push(`at ${l.multiple}×: sell ≈ ${Math.round(sell)} tokens (≈ ${(sell * l.multiple / original).toFixed(2)}× your stake back)`);
  }
  lines.push(`left riding as moon bag: ≈ ${Math.round(held)} tokens (${tp.moonBagPct}% minimum protected) — total taken out ≈ ${recovered.toFixed(2)}× your stake`);
  return lines;
}

export function SettingsPanel({ state, onChange }: { state: DashboardState; onChange?: () => void }) {
  const [s, setS] = useState<Settings>(structuredClone(state.settings));
  const [msg, setMsg] = useState<{ ok: boolean; text: string; v?: string[] } | null>(null);
  const dirty = withoutAutopilot(s) !== withoutAutopilot(state.settings);
  useEffect(() => {
    if (!dirty) setS(structuredClone(state.settings));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [state.settings]);

  const get = (p: Path): unknown => (p.length === 1 ? s[p[0]] : (s[p[0]] as unknown as Record<string, unknown>)[p[1]!]);
  const set = (p: Path, v: unknown) =>
    setS((prev) => {
      const n = structuredClone(prev) as unknown as Record<string, Record<string, unknown> | unknown>;
      if (p.length === 1) n[p[0]] = v;
      else (n[p[0]] as Record<string, unknown>)[p[1]!] = v;
      return n as unknown as Settings;
    });

  const num = ({ p, label, hint, nullable }: { p: Path; label: string; hint?: string; nullable?: boolean }) => {
    const v = get(p) as number | null;
    return (
      <label className="field">
        <span>{label}</span>
        <input type="number" step="any" value={v ?? ''} placeholder={nullable ? 'not set' : ''} onChange={(e) => set(p, e.target.value === '' && nullable ? null : Number(e.target.value))} />
        {hint && <small className="muted">{hint}</small>}
      </label>
    );
  };
  const bool = ({ p, label, hint }: { p: Path; label: string; hint?: string }) => (
    <label className="field check">
      <input type="checkbox" checked={!!get(p)} onChange={(e) => set(p, e.target.checked)} /> <span>{label}</span>
      {hint && <small className="muted">{hint}</small>}
    </label>
  );

  const tp = s.takeProfit;
  const setTp = (patch: Partial<TakeProfitRule>) => setS((prev) => ({ ...prev, takeProfit: { ...prev.takeProfit, ...patch } }));
  const setLevel = (i: number, l: TakeProfitLevel) => setTp({ levels: tp.levels.map((x, j) => (j === i ? l : x)) });
  const kindOf = (k: string): TakeProfitSell => (k === 'percent_of_original' ? { kind: 'percent_of_original', pct: 25 } : k === 'all_except_moon_bag' ? { kind: 'all_except_moon_bag' } : { kind: 'recover_cost' });

  const save = async () => {
    setMsg(null);
    try {
      await api('/settings', { ...s, autopilot: state.settings.autopilot }, 'PUT');
      setMsg({ ok: true, text: 'Saved. Changes are recorded in the audit log.' });
      onChange?.();
    } catch (e) {
      setMsg({ ok: false, text: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };

  return (
    <section>
      <p className="muted">All limits are enforced before every trade (manual, rules and autopilot; paper and live), and again right before a trade is confirmed. Buys never use borrowed funds: only SOL already in the wallet, minus a reserve.</p>
      <h3>Risk limits</h3>
      <div className="fields">
        {num({ p: ['limits', 'maxBuySol'], label: 'Max buy (SOL)' })}
        {num({ p: ['limits', 'dailyLossLimitSol'], label: 'Daily loss limit (SOL)', hint: 'realized losses + failed-tx fees + marked unrealized losses, UTC day' })}
        {num({ p: ['limits', 'maxTotalExposureSol'], label: 'Max total exposure (SOL)', hint: 'sum of open cost basis' })}
        {num({ p: ['limits', 'maxSlippageBps'], label: 'Max slippage (bps)', hint: '100 bps = 1%' })}
        {num({ p: ['limits', 'maxPriceImpactPct'], label: 'Max price impact (%)' })}
        {num({ p: ['limits', 'maxOpenPositions'], label: 'Max open positions' })}
        {num({ p: ['limits', 'minSolReserve'], label: 'SOL reserve kept in wallet' })}
        {bool({ p: ['limits', 'capBuyByRemainingDailyLoss'], label: 'Cap each buy by remaining daily-loss budget' })}
      </div>

      <h3>Take-profit ladder</h3>
      <div className="row">
        <label className="field check">
          <input type="checkbox" checked={tp.enabled} onChange={(e) => setTp({ enabled: e.target.checked })} /> <span>Enabled</span>
        </label>
      </div>
      <table className="list ladder">
        <thead>
          <tr><th>#</th><th>When the price reaches</th><th>Sell</th><th></th></tr>
        </thead>
        <tbody>
          {tp.levels.map((l, i) => (
            <tr key={i}>
              <td>{i + 1}</td>
              <td>
                <input type="number" step="any" min={1.01} value={l.multiple} onChange={(e) => setLevel(i, { ...l, multiple: Number(e.target.value) })} style={{ width: 80 }} /> × entry
              </td>
              <td>
                <select value={l.sell.kind} onChange={(e) => setLevel(i, { ...l, sell: kindOf(e.target.value) })}>
                  <option value="recover_cost">enough to get my initial investment back</option>
                  <option value="percent_of_original">a percentage of the original amount</option>
                  <option value="all_except_moon_bag">everything except the moon bag</option>
                </select>
                {l.sell.kind === 'percent_of_original' && (
                  <>
                    {' '}
                    <input type="number" step="any" min={1} max={100} value={l.sell.pct} onChange={(e) => setLevel(i, { ...l, sell: { kind: 'percent_of_original', pct: Number(e.target.value) } })} style={{ width: 70 }} /> %
                  </>
                )}
              </td>
              <td><button className="link" onClick={() => setTp({ levels: tp.levels.filter((_, j) => j !== i) })}>remove</button></td>
            </tr>
          ))}
        </tbody>
      </table>
      <div className="row">
        <button disabled={tp.levels.length >= 6} onClick={() => setTp({ levels: [...tp.levels, { multiple: (tp.levels.at(-1)?.multiple ?? 1) + 2, sell: { kind: 'percent_of_original', pct: 25 } }] })}>+ Add level</button>
        <label className="field" style={{ maxWidth: 260 }}>
          <span>Moon bag (% of original never sold by take-profit)</span>
          <input type="number" step="any" min={0} max={95} value={tp.moonBagPct} onChange={(e) => setTp({ moonBagPct: Number(e.target.value) })} />
        </label>
        <label className="field" style={{ maxWidth: 300 }}>
          <span>Entry price basis</span>
          <select value={tp.entryPriceBasis} onChange={(e) => setTp({ entryPriceBasis: e.target.value as TakeProfitRule['entryPriceBasis'] })}>
            <option value="effective_incl_fees">Entry incl. fees (conservative)</option>
            <option value="fill_price">Fill price only</option>
          </select>
        </label>
        {bool({ p: ['takeProfit', 'autoExecuteLive'], label: 'Live (browser wallet): skip dashboard approval', hint: 'your wallet still asks you to sign; with the bot wallet + autopilot, exits are always automatic' })}
      </div>
      <div className="box small">
        <b>Example on a 1,000-token position</b> (ignores fees/slippage):
        <ul>{ladderPreview(tp).map((l) => <li key={l}>{l}</li>)}</ul>
        Levels trigger on the <i>executable</i> sell price after estimated fees. "Initial investment back" sells enough to recover the full entry cost even at your worst allowed slippage. Ladder changes apply to positions opened afterwards. When the last level is done the position is locked: the rest rides as the moon bag and nothing sells it automatically unless a rule below is set to apply after take-profit.
      </div>

      <h3>Stop-loss (off until you set it)</h3>
      <div className="fields">
        {bool({ p: ['stopLoss', 'enabled'], label: 'Enabled' })}
        {num({ p: ['stopLoss', 'dropPct'], label: 'Drop below entry (%)', nullable: true })}
        {num({ p: ['stopLoss', 'sellFractionPct'], label: 'Sell % of current holding' })}
        {bool({ p: ['stopLoss', 'includeMoonBag'], label: 'May also sell the moon bag' })}
        {bool({ p: ['stopLoss', 'applyAfterTakeProfit'], label: 'Also applies after the ladder finished' })}
        {bool({ p: ['stopLoss', 'autoExecuteLive'], label: 'Live (browser wallet): skip dashboard approval' })}
      </div>

      <h3>Time-based exit (off until you set it)</h3>
      <div className="fields">
        {bool({ p: ['timeExit', 'enabled'], label: 'Enabled' })}
        {num({ p: ['timeExit', 'maxHoldMinutes'], label: 'Max hold (minutes)', nullable: true })}
        {num({ p: ['timeExit', 'sellFractionPct'], label: 'Sell % of current holding' })}
        {bool({ p: ['timeExit', 'includeMoonBag'], label: 'May also sell the moon bag' })}
        {bool({ p: ['timeExit', 'applyAfterTakeProfit'], label: 'Also applies after the ladder finished' })}
        {bool({ p: ['timeExit', 'autoExecuteLive'], label: 'Live (browser wallet): skip dashboard approval' })}
      </div>

      <h3>Verification thresholds</h3>
      <div className="fields">
        {num({ p: ['verification', 'maxDataAgeSec'], label: 'Max data age (s)' })}
        {num({ p: ['verification', 'minLiquidityUsd'], label: 'Min liquidity (USD)' })}
        {num({ p: ['verification', 'maxTop10HolderPct'], label: 'Max top-10 wallet share (%)' })}
        {num({ p: ['verification', 'maxSingleHolderPct'], label: 'Max single wallet share (%)' })}
        {num({ p: ['verification', 'maxPriceDisagreementPct'], label: 'Max source disagreement (%)' })}
        {num({ p: ['verification', 'maxRoundTripLossPct'], label: 'Max round-trip loss (%)' })}
        {num({ p: ['verification', 'probeSol'], label: 'Probe size (SOL)' })}
      </div>

      <h3>Execution</h3>
      <div className="fields">
        {num({ p: ['quoteMaxAgeSec'], label: 'Quote expiry (s)' })}
        {num({ p: ['monitorIntervalSec'], label: 'Monitor interval (s)' })}
        {num({ p: ['priorityFeeMaxLamports'], label: 'Max priority fee (lamports)' })}
        {num({ p: ['testWalletMaxBalanceSol'], label: 'Wallet balance warning (SOL)' })}
        {num({ p: ['paper', 'extraSlippageBps'], label: 'Paper: simulated extra slippage (bps)' })}
        {num({ p: ['paper', 'priorityFeeLamports'], label: 'Paper: priority fee (lamports)' })}
      </div>

      <div className="row sticky">
        <button className="primary" disabled={!dirty} onClick={save}>Save settings</button>
        <button disabled={!dirty} onClick={() => setS(structuredClone(state.settings))}>Discard</button>
        {msg && <span className={msg.ok ? 'pos' : 'neg'}>{msg.text} {msg.v?.join('; ')}</span>}
      </div>
    </section>
  );
}
