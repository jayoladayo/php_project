import { useEffect, useState } from 'react';
import { api, ApiError, type DashboardState } from '../api.js';
import type { Settings } from '../../shared/types.js';

type Path = [keyof Settings, string] | [keyof Settings];

export function SettingsPanel({ state }: { state: DashboardState }) {
  const [s, setS] = useState<Settings>(structuredClone(state.settings));
  const [msg, setMsg] = useState<{ ok: boolean; text: string; v?: string[] } | null>(null);
  const dirty = JSON.stringify(s) !== JSON.stringify(state.settings);
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
        <input
          type="number"
          step="any"
          value={v ?? ''}
          placeholder={nullable ? 'not set' : ''}
          onChange={(e) => set(p, e.target.value === '' && nullable ? null : Number(e.target.value))}
        />
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

  const save = async () => {
    setMsg(null);
    try {
      await api('/settings', s, 'PUT');
      setMsg({ ok: true, text: 'Saved. Changes are recorded in the audit log.' });
    } catch (e) {
      setMsg({ ok: false, text: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };

  return (
    <section>
      <p className="muted">All limits are enforced before every trade (paper and live), and again right before a trade is confirmed. Buys never use borrowed funds: only SOL already in the wallet, minus a reserve.</p>
      <h3>Risk limits</h3>
      <div className="fields">
        {num({ p: ['limits', 'maxBuySol'], label: "Max buy (SOL)" })}
        {num({ p: ['limits', 'dailyLossLimitSol'], label: "Daily loss limit (SOL)", hint: "realized losses + failed-tx fees + marked unrealized losses, UTC day" })}
        {num({ p: ['limits', 'maxTotalExposureSol'], label: "Max total exposure (SOL)", hint: "sum of open cost basis" })}
        {num({ p: ['limits', 'maxSlippageBps'], label: "Max slippage (bps)", hint: "100 bps = 1%" })}
        {num({ p: ['limits', 'maxPriceImpactPct'], label: "Max price impact (%)" })}
        {num({ p: ['limits', 'maxOpenPositions'], label: "Max open positions" })}
        {num({ p: ['limits', 'minSolReserve'], label: "SOL reserve kept in wallet" })}
        {bool({ p: ['limits', 'capBuyByRemainingDailyLoss'], label: "Cap each buy by remaining daily-loss budget" })}
      </div>

      <h3>Take-profit</h3>
      <div className="fields">
        {bool({ p: ['takeProfit', 'enabled'], label: "Enabled" })}
        {num({ p: ['takeProfit', 'multiple'], label: "Trigger multiple (× entry)" })}
        {num({ p: ['takeProfit', 'sellFractionPct'], label: "Sell % of ORIGINAL amount", hint: "applies to new positions" })}
        <label className="field">
          <span>Entry price basis</span>
          <select value={s.takeProfit.entryPriceBasis} onChange={(e) => set(['takeProfit', 'entryPriceBasis'], e.target.value)}>
            <option value="effective_incl_fees">Entry incl. fees (conservative)</option>
            <option value="fill_price">Fill price only</option>
          </select>
          <small className="muted">Trigger compares the executable sell quote, minus estimated sell fees, to this entry price.</small>
        </label>
        {bool({ p: ['takeProfit', 'autoExecuteLive'], label: "Live: skip dashboard approval", hint: "your wallet still asks you to sign" })}
      </div>
      <p className="muted small">After the take-profit sale completes the position is locked: no further automatic buys or sells for it unless a rule below is set to apply after take-profit, or you approve a trade manually.</p>

      <h3>Stop-loss (off until you set it)</h3>
      <div className="fields">
        {bool({ p: ['stopLoss', 'enabled'], label: "Enabled" })}
        {num({ p: ['stopLoss', 'dropPct'], label: "Drop below entry (%)", nullable: true })}
        {num({ p: ['stopLoss', 'sellFractionPct'], label: "Sell % of current holding" })}
        {bool({ p: ['stopLoss', 'applyAfterTakeProfit'], label: "Also applies after take-profit" })}
        {bool({ p: ['stopLoss', 'autoExecuteLive'], label: "Live: skip dashboard approval" })}
      </div>

      <h3>Time-based exit (off until you set it)</h3>
      <div className="fields">
        {bool({ p: ['timeExit', 'enabled'], label: "Enabled" })}
        {num({ p: ['timeExit', 'maxHoldMinutes'], label: "Max hold (minutes)", nullable: true })}
        {num({ p: ['timeExit', 'sellFractionPct'], label: "Sell % of current holding" })}
        {bool({ p: ['timeExit', 'applyAfterTakeProfit'], label: "Also applies after take-profit" })}
        {bool({ p: ['timeExit', 'autoExecuteLive'], label: "Live: skip dashboard approval" })}
      </div>

      <h3>Verification thresholds</h3>
      <div className="fields">
        {num({ p: ['verification', 'maxDataAgeSec'], label: "Max data age (s)" })}
        {num({ p: ['verification', 'minLiquidityUsd'], label: "Min liquidity (USD)" })}
        {num({ p: ['verification', 'maxTop10HolderPct'], label: "Max top-10 wallet share (%)" })}
        {num({ p: ['verification', 'maxSingleHolderPct'], label: "Max single wallet share (%)" })}
        {num({ p: ['verification', 'maxPriceDisagreementPct'], label: "Max source disagreement (%)" })}
        {num({ p: ['verification', 'maxRoundTripLossPct'], label: "Max round-trip loss (%)" })}
        {num({ p: ['verification', 'probeSol'], label: "Probe size (SOL)" })}
      </div>

      <h3>Execution</h3>
      <div className="fields">
        {num({ p: ['quoteMaxAgeSec'], label: "Quote expiry (s)" })}
        {num({ p: ['monitorIntervalSec'], label: "Monitor interval (s)" })}
        {num({ p: ['priorityFeeMaxLamports'], label: "Max priority fee (lamports)" })}
        {num({ p: ['testWalletMaxBalanceSol'], label: "Test-wallet balance warning (SOL)" })}
        {num({ p: ['paper', 'extraSlippageBps'], label: "Paper: simulated extra slippage (bps)" })}
        {num({ p: ['paper', 'priorityFeeLamports'], label: "Paper: priority fee (lamports)" })}
      </div>

      <div className="row sticky">
        <button className="primary" disabled={!dirty} onClick={save}>Save settings</button>
        <button disabled={!dirty} onClick={() => setS(structuredClone(state.settings))}>Discard</button>
        {msg && <span className={msg.ok ? 'pos' : 'neg'}>{msg.text} {msg.v?.join('; ')}</span>}
      </div>
    </section>
  );
}
