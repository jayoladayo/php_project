// Everything the user must review before live trading: architecture, data
// sources, permissions, limits, a sample transaction flow, a devnet signing
// test, and the explicit enable step.
import { useWallet } from '@solana/wallet-adapter-react';
import { VersionedTransaction } from '@solana/web3.js';
import { useState } from 'react';
import { api, ApiError, type DashboardState } from '../api.js';
import { short } from '../format.js';

export function LivePanel({ state }: { state: DashboardState }) {
  const { publicKey, signTransaction } = useWallet();
  const wallet = publicKey?.toBase58() ?? null;
  const [phrase, setPhrase] = useState('');
  const [reviewed, setReviewed] = useState(false);
  const [msg, setMsg] = useState<{ ok: boolean; text: string; v?: string[] } | null>(null);
  const L = state.settings.limits;
  const T = state.settings.takeProfit;

  const run = async (fn: () => Promise<string>) => {
    setMsg(null);
    try {
      setMsg({ ok: true, text: await fn() });
    } catch (e) {
      setMsg({ ok: false, text: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };

  const devnetTest = () =>
    run(async () => {
      if (!wallet || !signTransaction) throw new Error('Connect a wallet first (switch the wallet to devnet for this test).');
      const t = await api<{ txBase64: string; blockhash: string; lastValidBlockHeight: number }>('/devnet/test-tx', { wallet });
      const tx = VersionedTransaction.deserialize(Uint8Array.from(atob(t.txBase64), (c) => c.charCodeAt(0)));
      const signed = await signTransaction(tx);
      const r = await api<{ explorer: string }>('/devnet/submit', { signedTx: btoa(String.fromCharCode(...signed.serialize())), blockhash: t.blockhash, lastValidBlockHeight: t.lastValidBlockHeight });
      return `Devnet signing test succeeded: ${r.explorer}`;
    });

  const checks: Array<[string, boolean]> = [
    ['Server allows live trading (LIVE_TRADING_ALLOWED=true)', state.env.liveTradingAllowed],
    ['Real market data (not mock)', state.env.marketData === 'live'],
    ['Dedicated RPC endpoint (not the public rate-limited one)', !state.env.publicRpc],
    ['Wallet connected', !!wallet],
    ['Connected wallet is bound for live trading', !!wallet && state.settings.boundWallet === wallet],
    ['Not paused', !state.paused],
    ['You reviewed architecture, data sources, permissions, limits and the sample flow below', reviewed],
  ];

  return (
    <section>
      {state.mode === 'live' ? (
        <div className="box live">
          <h2>LIVE mode is ON for {short(state.live?.wallet)}</h2>
          <p>Every trade still shows a review screen and requires your wallet's signature.</p>
          <button onClick={() => run(async () => (await api('/live/disarm', {}), 'Back to paper mode.'))}>Switch back to paper mode</button>
        </div>
      ) : (
        <div className="box">
          <h2>Paper mode (default)</h2>
          <p>Nothing is signed or sent. Paper fills use real quotes (or the mock market) plus {state.settings.paper.extraSlippageBps} bps simulated slippage and estimated fees.</p>
        </div>
      )}

      <h3>1. Architecture</h3>
      <pre className="diagram">{`Browser dashboard (React)                 Local server (Node, 127.0.0.1)
 ├ Wallet adapter (Phantom/Solflare/…) ◄──┐ ├ Research: RPC + DexScreener + GeckoTerminal
 │   signs ONE reviewed tx at a time      │ │   + RugCheck + Jupiter probe quotes + X API
 ├ Review modal (amounts, fees, route,    │ ├ Risk engine: limits checked before every trade
 │   flags, permissions, simulation)      │ ├ Tx builder: Jupiter swap → decode → allowlist
 └ Emergency pause                        │ │   check → simulate → hash
                                          └─┤ Submit: verify signed tx == reviewed tx → send
                                            │   → confirm → read actual balances → accounting
                                            ├ Monitor: marks, 5× rule, optional SL / time exit
                                            └ Audit log (hash-chained JSONL, secrets redacted)
 The server never holds or asks for a private key or seed phrase.`}</pre>

      <h3>2. Data sources</h3>
      <ul>
        <li><b>Solana RPC</b> ({state.env.rpcHost}): mint & freeze authority, Token-2022 extensions, supply, largest holders, launch history, early buyers, holder funding, simulation, sending/confirming.</li>
        <li><b>DexScreener</b>: price, liquidity, volume, market cap, pair. <b>GeckoTerminal</b>: hourly price history.</li>
        <li><b>RugCheck</b> {state.env.rugcheckEnabled ? '' : '(disabled)'}: LP lock/burn status, holder count, third-party risk notes (treated as signals).</li>
        <li><b>{state.env.provider}</b>: quotes, routes, price impact, sellability probe, unsigned swap transactions.</li>
        <li><b>X API</b> {state.env.socialEnabled ? '' : '(not configured)'}: recent posts mentioning the mint; used only to flag hype/coordination, never to justify a buy.</li>
      </ul>

      <h3>3. Permissions</h3>
      <ul>
        <li>The wallet signs one swap transaction at a time. Solana swaps need no token approvals; any approval, delegation, authority change, or transfer to a third party is refused before you are asked to sign.</li>
        <li>Only allowlisted top-level programs: Compute Budget, System (wrap SOL to your own account), SPL Token / Token-2022 (sync/close your wSOL), Associated Token Account (create your own), Jupiter v6, Memo.</li>
        <li>The server checks that the transaction you sign is byte-for-byte the one you reviewed.</li>
      </ul>

      <h3>4. Current risk limits</h3>
      <ul>
        <li>Max buy {L.maxBuySol} SOL · daily loss {L.dailyLossLimitSol} SOL · exposure {L.maxTotalExposureSol} SOL · {L.maxOpenPositions} positions</li>
        <li>Slippage ≤ {L.maxSlippageBps} bps · price impact ≤ {L.maxPriceImpactPct}% · keep {L.minSolReserve} SOL reserve</li>
        <li>Take-profit: {T.enabled ? `at ${T.multiple}× sell ${T.sellFractionPct}% of the original amount, then lock` : 'off'}; stop-loss {state.settings.stopLoss.enabled ? `${state.settings.stopLoss.dropPct}%` : 'off'}; time exit {state.settings.timeExit.enabled ? `${state.settings.timeExit.maxHoldMinutes} min` : 'off'}</li>
        <li>Live rule automation: {T.autoExecuteLive ? 'take-profit opens the wallet prompt without dashboard approval' : 'every rule trigger needs your approval in the dashboard'}</li>
      </ul>

      <h3>5. Sample transaction flow (buy)</h3>
      <ol>
        <li>You research a mint → report with facts, signals, sources, timestamps, verdict.</li>
        <li>You enter amount + slippage → server refreshes stale research, gets a fresh Jupiter quote, checks every limit.</li>
        <li>Server asks Jupiter for an unsigned transaction for <i>your</i> public key, resolves lookup tables, decodes every instruction, rejects anything outside the allowlist, confirms the mint is in the transaction.</li>
        <li>Server simulates it against current chain state: tokens received ≥ minimum, SOL spent ≤ amount + fee budget.</li>
        <li>You see the review screen (token, mint, amount, est. price, min received, fees, slippage, route, flags, permissions, decoded instructions, simulation result) and click Sign.</li>
        <li>Your wallet shows its own prompt; you approve. The signed transaction returns to the server, which checks it matches what you reviewed, re-checks limits and pause state, and sends it.</li>
        <li>Server confirms it, reads the actual balance changes (handles slippage, failure, expiry), records the fill and fees, and logs everything. If the outcome is uncertain it pauses trading and alerts you.</li>
      </ol>

      <h3>6. Test safely first</h3>
      <ul>
        <li>Paper-trade first; then use a <b>separate low-balance wallet</b> created only for this bot. Warning threshold: {state.settings.testWalletMaxBalanceSol} SOL.</li>
        <li>
          Devnet signing test (switch your wallet to devnet): checks the wallet connection and signing flow with no real funds.{' '}
          <button disabled={!wallet} onClick={() => run(async () => { await api('/devnet/airdrop', { wallet }); return 'Requested 0.5 devnet SOL (devnet faucets are rate-limited).'; })}>Request devnet airdrop</button>{' '}
          <button disabled={!wallet} onClick={devnetTest}>Sign devnet test</button>
        </li>
        <li>Jupiter routes only exist on mainnet, so real swaps cannot be rehearsed on devnet — use small amounts.</li>
      </ul>

      <h3>7. Bind wallet & enable live mode</h3>
      <p>
        Bound wallet: <span className="mono">{state.settings.boundWallet ?? 'none'}</span>{' '}
        <button disabled={!wallet || state.mode === 'live'} onClick={() => run(async () => (await api('/wallet/bind', { wallet }), `Bound ${short(wallet)}.`))}>Bind connected wallet</button>{' '}
        <button disabled={!state.settings.boundWallet || state.mode === 'live'} onClick={() => run(async () => (await api('/wallet/bind', { wallet: null }), 'Unbound.'))}>Unbind</button>
      </p>
      <ul className="checklist">
        {checks.map(([label, ok]) => <li key={label} className={ok ? 'pos' : 'neg'}>{ok ? '✓' : '✗'} {label}</li>)}
      </ul>
      <label className="override"><input type="checkbox" checked={reviewed} onChange={(e) => setReviewed(e.target.checked)} /> I reviewed the architecture, data sources, permissions, risk limits and sample flow, and I understand trading these tokens can lose all funds.</label>
      {state.mode !== 'live' && (
        <div className="row">
          <input placeholder="Type ENABLE LIVE TRADING" value={phrase} onChange={(e) => setPhrase(e.target.value)} />
          <button
            className="danger"
            disabled={!checks.every(([, ok]) => ok) || phrase !== 'ENABLE LIVE TRADING'}
            onClick={() => run(async () => { const r = await api<{ warnings: string[] }>('/live/arm', { wallet, phrase }); return `Live mode enabled. ${r.warnings.join(' ')}`; })}
          >
            Enable live trading
          </button>
        </div>
      )}
      {msg && <div className={`box ${msg.ok ? '' : 'err'}`}>{msg.text}{msg.v && msg.v.length > 0 && <ul>{msg.v.map((v) => <li key={v}>{v}</li>)}</ul>}</div>}
    </section>
  );
}
