import { useWallet } from '@solana/wallet-adapter-react';
import { WalletMultiButton } from '@solana/wallet-adapter-react-ui';
import { useCallback, useEffect, useRef, useState } from 'react';
import { api, type DashboardState, type TradeIntent } from './api.js';
import { ago } from './format.js';
import { Audit } from './components/Audit.js';
import { LivePanel } from './components/LivePanel.js';
import { Positions } from './components/Positions.js';
import { Research } from './components/Research.js';
import { SettingsPanel } from './components/SettingsPanel.js';
import { TradeModal } from './components/TradeModal.js';

const TABS = ['Positions', 'Research', 'Limits & rules', 'Live mode', 'Audit log'] as const;
type Tab = (typeof TABS)[number];

export function App() {
  const [state, setState] = useState<DashboardState | null>(null);
  const [offline, setOffline] = useState(false);
  const [tab, setTab] = useState<Tab>('Positions');
  const [modal, setModal] = useState<{ intent: TradeIntent; autoSign: boolean } | null>(null);
  const { publicKey } = useWallet();
  const handledProposals = useRef(new Set<string>());

  const refresh = useCallback(async () => {
    try {
      setState(await api<DashboardState>('/state'));
      setOffline(false);
    } catch {
      setOffline(true);
    }
  }, []);

  useEffect(() => {
    void refresh();
    const t = setInterval(refresh, 3000);
    return () => clearInterval(t);
  }, [refresh]);

  // Live automation that the user explicitly enabled: open the wallet prompt
  // for rule triggers flagged autoExecute. The wallet still asks to sign.
  useEffect(() => {
    if (!state || state.mode !== 'live' || state.paused || modal || !publicKey) return;
    const pr = state.proposals.find((p) => p.autoExecute && !handledProposals.current.has(p.id));
    if (!pr) return;
    handledProposals.current.add(pr.id);
    api<TradeIntent>('/trades/prepare', {
      side: 'sell',
      positionId: pr.positionId,
      amountRaw: pr.amountRaw,
      slippageBps: state.settings.limits.maxSlippageBps,
      wallet: publicKey.toBase58(),
      reason: pr.rule,
      proposalId: pr.id,
      acknowledgeLocked: true,
    })
      .then((intent) => setModal({ intent, autoSign: true }))
      .catch(() => undefined);
  }, [state, modal, publicKey]);

  const openIntent = (intent: TradeIntent, autoSign = false) => setModal({ intent, autoSign });

  const pause = async () => {
    await api('/pause', { reason: 'emergency pause pressed in dashboard' });
    setModal(null);
    void refresh();
  };
  const resume = async () => {
    const phrase = window.prompt('Type RESUME to allow trading again:');
    if (phrase) await api('/resume', { phrase }).catch((e) => alert((e as Error).message));
    void refresh();
  };

  if (!state) return <div className="wrap"><p>{offline ? 'Cannot reach the local server. Is `npm run dev` running?' : 'Loading…'}</p></div>;

  return (
    <div className="wrap">
      <header>
        <h1>Solana Research Trader</h1>
        <span className={`badge ${state.mode}`}>{state.mode === 'live' ? 'LIVE' : 'PAPER'}</span>
        {state.env.marketData === 'mock' && <span className="badge warnb">MOCK DATA</span>}
        <span className="grow" />
        {state.paused ? (
          <button className="resume" onClick={resume}>Paused — resume…</button>
        ) : (
          <button className="pause" onClick={pause} title="Stops new trades and cancels pending unsigned trades">EMERGENCY PAUSE</button>
        )}
        <WalletMultiButton />
      </header>
      {offline && <div className="box err">Lost connection to the local server. No trades can be made until it is back.</div>}
      {state.paused && <div className="box err"><b>Trading paused:</b> {state.pauseReason}</div>}
      {state.alerts.map((a) => (
        <div key={a.id} className={`alert ${a.level}`}>
          <span className="grow">{a.message} <span className="muted small">{ago(a.at)}</span></span>
          <button className="link" onClick={() => api(`/alerts/${a.id}/ack`, {}).then(refresh)}>dismiss</button>
        </div>
      ))}
      <p className="disclaimer">Research tool, not financial advice. No outcome or profit is promised; new Solana tokens frequently go to zero. Social media posts are never treated as evidence.</p>

      {state.env.marketData === 'mock' && state.env.mockTokens.length > 0 && <MockControls state={state} onDone={refresh} />}

      <nav>
        {TABS.map((t) => (
          <button key={t} className={t === tab ? 'tab active' : 'tab'} onClick={() => setTab(t)}>
            {t}
            {t === 'Positions' && state.proposals.length > 0 && <span className="dot">{state.proposals.length}</span>}
          </button>
        ))}
      </nav>
      {tab === 'Positions' && <Positions state={state} onIntent={openIntent} />}
      {tab === 'Research' && <Research state={state} onIntent={openIntent} />}
      {tab === 'Limits & rules' && <SettingsPanel state={state} />}
      {tab === 'Live mode' && <LivePanel state={state} />}
      {tab === 'Audit log' && <Audit />}

      {modal && (
        <TradeModal
          key={modal.intent.id}
          intent={modal.intent}
          autoSign={modal.autoSign}
          onClose={() => {
            setModal(null);
            void refresh();
          }}
        />
      )}
    </div>
  );
}

function MockControls({ state, onDone }: { state: DashboardState; onDone: () => void }) {
  const [mint, setMint] = useState(state.env.mockTokens[0]!.mint);
  const move = (m: number) => api('/mock/move-price', { mint, multiplier: m }).then(() => setTimeout(onDone, 500));
  return (
    <div className="box small row">
      <b>Mock market:</b>
      <select value={mint} onChange={(e) => setMint(e.target.value)}>
        {state.env.mockTokens.map((t) => <option key={t.mint} value={t.mint}>{t.symbol}</option>)}
      </select>
      {[0.5, 0.9, 1.1, 2, 5.5].map((m) => <button key={m} onClick={() => move(m)}>price ×{m}</button>)}
      <span className="muted">Use this to watch the take-profit rule fire in paper mode.</span>
    </div>
  );
}
