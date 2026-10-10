import { useState } from 'react';
import { api, ApiError, type DashboardState, type PositionView, type TradeIntent } from '../api.js';
import { ago, pnlClass, price, short, sol, tokens } from '../format.js';
import type { Proposal } from '../../shared/types.js';

export function Positions({ state, onIntent }: { state: DashboardState; onIntent: (i: TradeIntent, autoSign?: boolean) => void }) {
  const [err, setErr] = useState<{ msg: string; v: string[] } | null>(null);
  const [showClosed, setShowClosed] = useState(false);
  const tp = state.settings.takeProfit;
  const mine = state.positions.filter((p) => p.mode === state.mode);
  const open = mine.filter((p) => p.status === 'open');
  const shown = showClosed ? mine : open;

  const exposure = open.reduce((a, p) => a + Number(p.costBasisLamports), 0);
  const unreal = open.reduce((a, p) => a + Number(p.view.unrealizedLamports ?? 0), 0);
  const realized = mine.reduce((a, p) => a + Number(p.realizedPnlLamports), 0);
  const fees = mine.reduce((a, p) => a + Number(p.feesPaidLamports), 0);
  const today = new Date().toISOString().slice(0, 10);
  const realizedToday = mine.flatMap((p) => p.fills).filter((f) => f.realizedPnlLamports && f.at.startsWith(today)).reduce((a, f) => a + Number(f.realizedPnlLamports), 0);

  const sell = async (p: PositionView, pct: number) => {
    setErr(null);
    try {
      onIntent(
        await api<TradeIntent>('/trades/prepare', {
          side: 'sell',
          positionId: p.id,
          sellPct: pct,
          slippageBps: state.settings.limits.maxSlippageBps,
          wallet: state.live?.wallet ?? null,
          acknowledgeLocked: p.locked ? window.confirm(`${p.symbol} is locked after take-profit. Approve this manual sell anyway?`) : false,
        }),
      );
    } catch (e) {
      setErr({ msg: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };

  const approve = async (pr: Proposal, autoSign = false) => {
    setErr(null);
    try {
      onIntent(
        await api<TradeIntent>('/trades/prepare', {
          side: 'sell',
          positionId: pr.positionId,
          amountRaw: pr.amountRaw,
          slippageBps: state.settings.limits.maxSlippageBps,
          wallet: state.live?.wallet ?? null,
          reason: pr.rule,
          proposalId: pr.id,
          acknowledgeLocked: true,
          ...(pr.tpLevel !== undefined ? { tpLevel: pr.tpLevel, tpTargetRaw: pr.tpTargetRaw } : {}),
        }),
        autoSign,
      );
    } catch (e) {
      setErr({ msg: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };

  return (
    <section>
      <div className="cards">
        {state.mode === 'paper' && (
          <div className="card">
            <div className="muted small">Paper balance</div>
            <div className="big">{sol(state.paperBalanceLamports)}</div>
            <button
              className="link"
              onClick={() => window.confirm(`Reset the paper account? This deletes all paper positions and history and sets the balance back to ${state.settings.paper.startingBalanceSol} SOL. (Live positions are not touched.)`) && api('/paper/reset', {}).catch((e) => setErr({ msg: (e as Error).message, v: [] }))}
            >
              reset paper account
            </button>
          </div>
        )}
        <Card label="Open exposure (cost basis)" value={sol(exposure)} sub={`limit ${state.settings.limits.maxTotalExposureSol} SOL`} />
        <Card label="Unrealized P&L" value={sol(unreal)} cls={pnlClass(unreal)} sub="net of est. sell fees" />
        <Card label="Realized P&L" value={sol(realized)} cls={pnlClass(realized)} sub={`today ${sol(realizedToday)} / loss limit ${state.settings.limits.dailyLossLimitSol} SOL`} />
        <Card label="Fees paid" value={sol(fees, 6)} />
        <Card
          label="Take-profit ladder"
          value={tp.enabled ? tp.levels.map((l) => `${l.multiple}×`).join(' → ') : 'off'}
          sub={tp.enabled ? `${tp.levels.map((l) => (l.sell.kind === 'recover_cost' ? 'recover stake' : l.sell.kind === 'percent_of_original' ? `${l.sell.pct}%` : 'all but bag')).join(' · ')} · moon bag ${tp.moonBagPct}%` : ''}
        />
      </div>

      {err && <div className="box err">{err.msg}{err.v.length > 0 && <ul>{err.v.map((v) => <li key={v}>{v}</li>)}</ul>}</div>}

      {state.proposals.length > 0 && (
        <div className="box warn">
          <h3>Rule triggers awaiting you</h3>
          {state.proposals.map((pr) => {
            const p = state.positions.find((x) => x.id === pr.positionId);
            return (
              <div key={pr.id} className="row">
                <span><b>{pr.rule.replace('_', '-')}</b> {p?.symbol}: {pr.detail} <span className="muted">{ago(pr.createdAt)}</span></span>
                <button className="primary" onClick={() => approve(pr)}>Review & approve</button>
                <button onClick={() => api(`/proposals/${pr.id}/dismiss`, {})}>Dismiss</button>
              </div>
            );
          })}
        </div>
      )}

      <div className="row">
        <h3 className="grow">Positions ({state.mode})</h3>
        <label><input type="checkbox" checked={showClosed} onChange={(e) => setShowClosed(e.target.checked)} /> show closed</label>
      </div>
      {shown.length === 0 ? (
        <p className="muted">No positions.</p>
      ) : (
        <table className="list">
          <thead>
            <tr>
              <th>Token</th><th>Held / original</th><th>Entry price<br /><span className="muted">fill · incl. fees</span></th><th>Cost basis</th>
              <th>Value (net)</th><th>Unrealized</th><th>Realized</th><th>Now × entry</th><th>Take-profit</th><th></th>
            </tr>
          </thead>
          <tbody>
            {shown.map((p) => (
              <PositionRow key={p.id} p={p} onSell={sell} />
            ))}
          </tbody>
        </table>
      )}

      <h3>Recent trades</h3>
      <table className="list small">
        <thead><tr><th>When</th><th>Mode</th><th>Side</th><th>Token</th><th>Reason</th><th>Status</th><th>Signature</th><th></th></tr></thead>
        <tbody>
          {state.intents.slice(0, 15).map((i) => (
            <tr key={i.id}>
              <td>{ago(i.createdAt)}</td><td>{i.mode}</td><td>{i.summary.side}</td><td>{i.summary.symbol}</td><td>{i.reason}</td>
              <td className={i.status === 'confirmed' ? 'pos' : i.status === 'unknown' || i.status === 'failed' ? 'neg' : ''} title={i.error}>{i.status}</td>
              <td className="mono">{i.signature ? <a href={`https://solscan.io/tx/${i.signature}`} target="_blank" rel="noreferrer">{short(i.signature)}</a> : '—'}</td>
              <td>{(i.status === 'unknown' || i.status === 'submitted') && <button onClick={() => api(`/trades/${i.id}/reconcile`, {}).catch((e) => setErr({ msg: (e as Error).message, v: [] }))}>Reconcile</button>}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </section>
  );
}

function PositionRow({ p, onSell }: { p: PositionView; onSell: (p: PositionView, pct: number) => void }) {
  const [open, setOpen] = useState(false);
  const tp = p.takeProfit;
  return (
    <>
      <tr>
        <td><b>{p.symbol}</b>{p.fills[0]?.reason === 'autopilot' && <span className="badge small" title="opened by the autopilot">auto</span>}<br /><span className="mono small">{short(p.mint)}</span></td>
        <td>{tokens(p.heldTokenRaw, p.decimals)}<br /><span className="muted small">of {tokens(p.originalTokenRaw, p.decimals)}</span></td>
        <td>{price(p.view.entryPriceFill)}<br /><span className="muted small">{price(p.view.entryPriceEffective)}</span></td>
        <td>{sol(p.costBasisLamports)}</td>
        <td>{p.mark ? (p.mark.stale ? <span className="neg" title={p.mark.error}>stale</span> : sol(p.mark.netValueLamports)) : '—'}<br /><span className="muted small">{p.mark && ago(p.mark.at)}</span></td>
        <td className={pnlClass(p.view.unrealizedLamports)}>{sol(p.view.unrealizedLamports)}</td>
        <td className={pnlClass(p.realizedPnlLamports)}>{sol(p.realizedPnlLamports)}</td>
        <td>{p.mark?.multipleOfEntry != null ? `${p.mark.multipleOfEntry.toFixed(2)}×` : '—'}</td>
        <td className="small">
          {p.view.levels.length === 0 && <span className="muted">no ladder</span>}
          {p.view.levels.map((l, i) => (
            <div key={i} title={`trigger price ${price(l.triggerPrice)} SOL`}>
              <span className={`lvl ${l.state}`}>{l.state === 'done' ? '✓' : l.state === 'partial' ? '◐' : l.state === 'skipped' ? '–' : '○'}</span> {l.label}
              {l.state !== 'pending' && <span className="muted"> · sold {tokens(l.soldRaw, p.decimals)}{l.targetRaw ? ` / ${tokens(l.targetRaw, p.decimals)}` : ''}</span>}
            </div>
          ))}
          {tp.moonBagRaw !== '0' && <div className="muted">moon bag {tokens(tp.moonBagRaw, p.decimals)}{p.locked && ' · riding (locked)'}</div>}
        </td>
        <td>
          {p.status === 'open' && (
            <div className="row nowrap">
              {[25, 50, 100].map((x) => <button key={x} onClick={() => onSell(p, x)}>Sell {x}%</button>)}
            </div>
          )}
          <button className="link" onClick={() => setOpen(!open)}>{open ? 'hide' : 'fills'}</button>
        </td>
      </tr>
      {open && (
        <tr>
          <td colSpan={10}>
            <table className="list small">
              <thead><tr><th>When</th><th>Side</th><th>Reason</th><th>In</th><th>Out (actual)</th><th>Quoted out</th><th>Slippage vs quote</th><th>Fill ratio</th><th>Fees</th><th>Rent</th><th>Realized</th><th>Sig</th></tr></thead>
              <tbody>
                {p.fills.map((f) => {
                  const v = p.view.fills.find((x) => x.id === f.id);
                  const isBuy = f.side === 'buy';
                  return (
                    <tr key={f.id}>
                      <td>{new Date(f.at).toLocaleString()}</td><td>{f.side}</td><td>{f.reason}</td>
                      <td>{isBuy ? sol(f.inputRaw, 6) : tokens(f.inputRaw, p.decimals)}</td>
                      <td>{isBuy ? tokens(f.outputRaw, p.decimals) : sol(f.outputRaw, 6)}</td>
                      <td>{isBuy ? tokens(f.quotedOutRaw, p.decimals) : sol(f.quotedOutRaw, 6)}</td>
                      <td>{v ? `${(v.slippageBps / 100).toFixed(2)}%` : '—'}</td>
                      <td>{v ? `${(v.fillRatio * 100).toFixed(1)}%` : '—'}</td>
                      <td>{sol(Number(f.networkFeeLamports) + Number(f.priorityFeeLamports) + Number(f.platformFeeLamports), 6)}</td>
                      <td>{f.rentLamports !== '0' ? sol(f.rentLamports, 5) : '—'}</td>
                      <td className={pnlClass(f.realizedPnlLamports)}>{f.realizedPnlLamports ? sol(f.realizedPnlLamports, 6) : '—'}</td>
                      <td className="mono">{f.signature ? <a href={`https://solscan.io/tx/${f.signature}`} target="_blank" rel="noreferrer">{short(f.signature)}</a> : 'paper'}</td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
            <p className="muted small">
              Realized P&L = SOL received − sell fees − cost basis of the tokens sold (average cost, entry fees included). Rent is a refundable deposit and not counted as a cost.
              Partial fills: when the rule's sell would exceed the price-impact limit it sells a smaller chunk and retries the remainder next cycle (only while the trigger still holds).
            </p>
          </td>
        </tr>
      )}
    </>
  );
}

function Card({ label, value, sub, cls }: { label: string; value: string; sub?: string; cls?: string }) {
  return (
    <div className="card">
      <div className="muted small">{label}</div>
      <div className={`big ${cls ?? ''}`}>{value}</div>
      {sub && <div className="muted small">{sub}</div>}
    </div>
  );
}
