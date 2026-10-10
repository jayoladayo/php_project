import { useState } from 'react';
import { api, ApiError, type DashboardState, type ResearchReport, type TradeIntent } from '../api.js';
import { ago, price, short, usd } from '../format.js';
import type { RiskFlag, Sourced } from '../../shared/types.js';

function Src<T>({ s }: { s: Sourced<T> }) {
  return (
    <span className="src" title={s.note ?? ''}>
      {s.url ? <a href={s.url} target="_blank" rel="noreferrer">{s.source}</a> : s.source} · {ago(s.fetchedAt)}
      {s.note && <> · <i>{s.note}</i></>}
    </span>
  );
}

function Row<T>({ label, s, fmt }: { label: string; s: Sourced<T>; fmt?: (v: T) => string }) {
  return (
    <tr>
      <th>{label}</th>
      <td>{s.value === null ? <span className="muted">unknown</span> : fmt ? fmt(s.value) : String(s.value)}</td>
      <td><Src s={s} /></td>
    </tr>
  );
}

const Flag = ({ f }: { f: RiskFlag }) => (
  <li className={`flag ${f.severity}`}>
    <span className="sev">{f.severity}</span> <span className="kind">{f.kind === 'signal' ? 'signal (not proof)' : 'fact'}</span> {f.message}
  </li>
);

export function Research({ state, onIntent }: { state: DashboardState; onIntent: (i: TradeIntent) => void }) {
  const [mint, setMint] = useState('');
  const [report, setReport] = useState<ResearchReport | null>(null);
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState<{ msg: string; v: string[] } | null>(null);
  const [amount, setAmount] = useState(String(Math.min(0.01, state.settings.limits.maxBuySol)));
  const [slip, setSlip] = useState(String(Math.min(50, state.settings.limits.maxSlippageBps)));
  const [override, setOverride] = useState(false);

  const run = async (m = mint) => {
    setBusy(true);
    setErr(null);
    setOverride(false);
    try {
      setReport(await api<ResearchReport>('/research', { mint: m.trim() }));
    } catch (e) {
      setErr({ msg: (e as Error).message, v: [] });
    } finally {
      setBusy(false);
    }
  };

  const buy = async () => {
    if (!report) return;
    setErr(null);
    try {
      const intent = await api<TradeIntent>('/trades/prepare', {
        side: 'buy',
        mint: report.mint,
        amountSol: amount,
        slippageBps: Number(slip),
        wallet: state.live?.wallet ?? null,
        overrideUnverified: override,
      });
      onIntent(intent);
    } catch (e) {
      setErr({ msg: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };

  const r = report;
  const facts = r?.flags.filter((f) => f.kind === 'fact') ?? [];
  const signals = r?.flags.filter((f) => f.kind === 'signal') ?? [];

  return (
    <section>
      <div className="row">
        <input className="grow mono" placeholder="Token mint address" value={mint} onChange={(e) => setMint(e.target.value)} onKeyDown={(e) => e.key === 'Enter' && run()} />
        <button className="primary" disabled={busy || !mint.trim()} onClick={() => run()}>{busy ? 'Researching…' : 'Research'}</button>
      </div>
      {state.env.mockTokens.length > 0 && (
        <p className="muted">
          Mock tokens:{' '}
          {state.env.mockTokens.map((t) => (
            <button key={t.mint} className="link" onClick={() => { setMint(t.mint); void run(t.mint); }}>{t.symbol} ({t.name})</button>
          ))}
        </p>
      )}
      {err && <div className="box err">{err.msg}{err.v.length > 0 && <ul>{err.v.map((v) => <li key={v}>{v}</li>)}</ul>}</div>}

      {r && (
        <>
          <div className={`box verdict ${r.verdict}`}>
            <h2>{r.verdict === 'verified' ? 'Passed automated checks' : 'NOT VERIFIED'}</h2>
            <p>{r.summary}</p>
            <ul>{r.verdictReasons.map((x) => <li key={x}>{x}</li>)}</ul>
            <p className="muted">Generated {ago(r.generatedAt)} ({r.generatedAt}). Research never predicts profit; "passed" means the data looked complete and consistent, not that the token is safe.</p>
          </div>

          <div className="grid2">
            <div>
              <h3>Facts (from chain / market data)</h3>
              {facts.length ? <ul className="flags">{facts.map((f) => <Flag key={f.code + f.message} f={f} />)}</ul> : <p className="muted">No fact-based flags.</p>}
            </div>
            <div>
              <h3>Signals (patterns, not proof)</h3>
              {signals.length ? <ul className="flags">{signals.map((f) => <Flag key={f.code + f.message} f={f} />)}</ul> : <p className="muted">No signals raised.</p>}
            </div>
          </div>

          <h3>Uncertainty</h3>
          <ul className="muted">{r.uncertainty.map((u) => <li key={u}>{u}</li>)}</ul>

          <h3>Token</h3>
          <table className="kv3"><tbody>
            <Row label="Name / symbol" s={r.token.symbol} fmt={(v) => `${r.token.name.value ?? '?'} (${v})`} />
            <Row label="Mint" s={{ ...r.token.tokenProgram, value: r.mint }} />
            <Row label="Token program" s={r.token.tokenProgram} fmt={(v) => (v.startsWith('Tokenz') ? 'Token-2022' : 'SPL Token')} />
            <Row label="Mint authority" s={r.token.mintAuthority} />
            <Row label="Freeze authority" s={r.token.freezeAuthority} />
            <Row label="Active extensions" s={r.token.extensions} fmt={(v) => (v.length ? v.join(', ') : 'none')} />
            <Row label="Created" s={r.token.createdAt} />
            <Row label="Age" s={r.token.ageMinutes} fmt={(v) => `${(v / 60).toFixed(1)} h`} />
            <Row label="Creator" s={r.token.creator} fmt={short} />
          </tbody></table>

          <h3>Market</h3>
          <table className="kv3"><tbody>
            <Row label="Price" s={r.market.priceUsd} fmt={(v) => `$${price(v)}`} />
            <Row label="Price (SOL)" s={r.market.priceSol} fmt={price} />
            <Row label="Liquidity" s={r.market.liquidityUsd} fmt={usd} />
            <Row label="Volume 24h" s={r.market.volume24hUsd} fmt={usd} />
            <Row label="Market cap" s={r.market.marketCapUsd} fmt={usd} />
            <Row label="FDV" s={r.market.fdvUsd} fmt={usd} />
            <Row label="Txns 24h" s={r.market.txns24h} fmt={(v) => `${v.buys} buys / ${v.sells} sells`} />
            <Row label="Price change" s={r.market.priceChange} fmt={(v) => Object.entries(v).map(([k, x]) => `${k} ${x}%`).join(' · ')} />
            <Row label="Price history" s={r.market.priceHistory} fmt={(v) => (v.length ? `${v.length} hourly closes, ${price(v[0]?.close)} → ${price(v.at(-1)?.close)} USD` : 'none')} />
            <Row label="DEX / pair" s={r.market.dex} fmt={(v) => `${v} ${short(r.market.pairAddress.value)}`} />
            <Row label="LP locked/burned" s={r.market.lpLockedPct} fmt={(v) => `${v.toFixed(1)}%`} />
          </tbody></table>

          <h3>Holders</h3>
          <table className="kv3"><tbody>
            <Row label="Holder count" s={r.holders.count} fmt={(v) => v.toLocaleString()} />
            <Row label="Top-10 wallets" s={r.holders.top10Pct} fmt={(v) => `${v.toFixed(2)}%`} />
            <Row label="Largest wallet" s={r.holders.maxSingleWalletPct} fmt={(v) => `${v.toFixed(2)}%`} />
          </tbody></table>
          {r.holders.top.value && r.holders.top.value.length > 0 && (
            <details>
              <summary>Top token accounts</summary>
              <table className="list"><thead><tr><th>Owner</th><th>Type</th><th>%</th></tr></thead><tbody>
                {r.holders.top.value.map((h) => (
                  <tr key={h.tokenAccount}><td className="mono"><a href={`https://solscan.io/account/${h.owner}`} target="_blank" rel="noreferrer">{short(h.owner)}</a></td><td>{h.label ?? h.ownerType}</td><td>{h.pct.toFixed(2)}</td></tr>
                ))}
              </tbody></table>
            </details>
          )}

          <h3>Sellability</h3>
          <table className="kv3"><tbody>
            <Row label="Probe buy quote" s={r.sellability.buyQuote} fmt={(q) => `impact ${q.priceImpactPct.toFixed(2)}% via ${q.routeLabels.join(', ')}`} />
            <Row label="Probe sell quote" s={r.sellability.sellQuote} fmt={(q) => `impact ${q.priceImpactPct.toFixed(2)}% via ${q.routeLabels.join(', ')}`} />
            <Row label="Round-trip loss" s={r.sellability.roundTripLossPct} fmt={(v) => `${v.toFixed(2)}%`} />
          </tbody></table>

          <h3>Bundles / coordinated buys</h3>
          <p><Src s={r.bundles} /></p>
          {r.bundles.value && (
            <p>
              {r.bundles.value.earlyBuyers} early buyer wallet(s) analysed; {r.bundles.value.sharedFunders.length} shared-funder cluster(s).
              {r.bundles.value.sharedFunders.map((c) => <span key={c.funder} className="mono"> [{short(c.funder)} → {c.wallets.map(short).join(', ')}]</span>)}
            </p>
          )}

          <h3>X / Twitter</h3>
          <p><Src s={r.social} /> <span className="muted">Social posts are context only and never a reason to buy.</span></p>
          {r.social.value?.posts.map((p) => (
            <div key={p.id} className="post">
              <a href={p.url} target="_blank" rel="noreferrer">@{p.author.username}</a> · {new Date(p.createdAt).toLocaleString()} · {p.author.followers.toLocaleString()} followers · account since {p.author.createdAt?.slice(0, 10) ?? '?'}
              <div>{p.text}</div>
              {p.signals.length > 0 && <div className="neg small">{p.signals.join(' · ')}</div>}
            </div>
          ))}

          <h3>Sources</h3>
          <table className="list"><tbody>
            {r.sources.map((s, i) => (
              <tr key={i}><td>{s.ok ? '✓' : '✗'}</td><td><a href={s.url.startsWith('http') ? s.url : undefined} target="_blank" rel="noreferrer">{s.name}</a></td><td>{s.fetchedAt}</td><td className="neg small">{s.error}</td></tr>
            ))}
          </tbody></table>

          <div className="box">
            <h3>Buy ({state.mode.toUpperCase()})</h3>
            <div className="row">
              <label>Amount (SOL) <input value={amount} onChange={(e) => setAmount(e.target.value)} size={8} /></label>
              <label>Slippage (bps) <input value={slip} onChange={(e) => setSlip(e.target.value)} size={5} /></label>
              <span className="muted">Limits: max buy {state.settings.limits.maxBuySol} SOL, max slippage {state.settings.limits.maxSlippageBps} bps, max impact {state.settings.limits.maxPriceImpactPct}%.</span>
            </div>
            {r.verdict !== 'verified' && (
              <label className="override">
                <input type="checkbox" checked={override} onChange={(e) => setOverride(e.target.checked)} /> I reviewed this report and accept that this token is <b>NOT verified</b>. (Critical flags still block the trade.)
              </label>
            )}
            <button className="primary" onClick={buy}>Get quote & review</button>
          </div>
        </>
      )}
    </section>
  );
}
