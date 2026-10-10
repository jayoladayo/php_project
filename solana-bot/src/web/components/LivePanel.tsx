// Everything the user must review before live trading: architecture, data
// sources, permissions, limits, a sample transaction flow, a devnet signing
// test, the choice of signer, and the explicit enable step.
import { useWallet } from '@solana/wallet-adapter-react';
import { VersionedTransaction } from '@solana/web3.js';
import { useState } from 'react';
import { api, ApiError, type DashboardState } from '../api.js';
import { short } from '../format.js';
import type { SignerKind } from '../../shared/types.js';
import { ladderPreview } from './SettingsPanel.js';

export function LivePanel({ state, onChange }: { state: DashboardState; onChange?: () => void }) {
  const { publicKey, signTransaction } = useWallet();
  const wallet = publicKey?.toBase58() ?? null;
  const [signer, setSigner] = useState<SignerKind>(state.botWallet.exists ? 'bot' : 'browser');
  const [phrase, setPhrase] = useState('');
  const [reviewed, setReviewed] = useState(false);
  const [msg, setMsg] = useState<{ ok: boolean; text: string; v?: string[] } | null>(null);
  const L = state.settings.limits;
  const T = state.settings.takeProfit;
  const bw = state.botWallet;

  const run = async (fn: () => Promise<string>) => {
    setMsg(null);
    try {
      setMsg({ ok: true, text: await fn() });
      onChange?.();
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

  const bindConnected = () =>
    run(async () => {
      const passphrase = bw.exists ? (window.prompt('Bot wallet passphrase (your wallet is also where bot-wallet withdrawals go):') ?? '') : undefined;
      await api('/wallet/bind', { wallet, passphrase });
      return `Bound ${short(wallet)}.`;
    });

  const common: Array<[string, boolean]> = [
    ['Server allows live trading (LIVE_TRADING_ALLOWED=true in .env)', state.env.liveTradingAllowed],
    ['Real market data (not the mock market)', state.env.marketData === 'live'],
    ['Dedicated RPC endpoint (not the public, rate-limited one)', !state.env.publicRpc],
    ['Not paused', !state.paused],
  ];
  const signerChecks: Array<[string, boolean]> =
    signer === 'bot'
      ? [
          ['Bot wallet created (Bot wallet tab)', bw.exists],
          ['Bot wallet unlocked', bw.unlocked],
          ['Your own wallet saved as the withdrawal destination', !!state.settings.boundWallet],
        ]
      : [
          ['Browser wallet connected', !!wallet],
          ['Connected wallet is your bound wallet', !!wallet && state.settings.boundWallet === wallet],
        ];
  const checks: Array<[string, boolean]> = [...common, ...signerChecks, ['You reviewed architecture, data sources, permissions, limits and the sample flow below', reviewed]];

  return (
    <section>
      {state.mode === 'live' ? (
        <div className="box live">
          <h2>LIVE mode is ON — {state.live?.signer === 'bot' ? 'bot wallet' : 'browser wallet'} {short(state.live?.wallet)}</h2>
          <p>
            {state.live?.signer === 'bot'
              ? 'The bot wallet signs on its own: exit rules execute automatically while the autopilot runs, and the autopilot (Autopilot tab) buys within your limits. Manual trades still show a review screen.'
              : 'Every trade shows a review screen and needs your wallet’s signature.'}
          </p>
          <button onClick={() => run(async () => (await api('/live/disarm', {}), 'Back to paper mode. The autopilot was stopped.'))}>Switch back to paper mode</button>
        </div>
      ) : (
        <div className="box">
          <h2>Paper mode (default)</h2>
          <p>Nothing is signed or sent. Paper fills use real quotes (or the mock market) plus {state.settings.paper.extraSlippageBps} bps simulated slippage and estimated fees. The autopilot works in paper mode too — run it here first.</p>
        </div>
      )}

      <h3>1. Architecture</h3>
      <pre className="diagram">{`Browser dashboard (React)                 Local server (Node, 127.0.0.1)
 ├ Wallet adapter (Phantom/Solflare/…)    ├ Research: RPC + DexScreener + GeckoTerminal
 │   signs ONE reviewed tx at a time      │   + RugCheck + Jupiter probe quotes + X API
 ├ Review modal, Autopilot, Bot wallet    ├ Autopilot: discover → research → filters → buy
 └ Emergency pause                        ├ Risk engine: limits checked before every trade
                                          ├ Tx builder: Jupiter swap → decode → allowlist
                                          │   check → simulate → hash
                                          ├ Signer: your browser wallet, or the bot wallet
                                          │   (encrypted key, signs only verified swaps
                                          │   and withdrawals to your wallet)
                                          ├ Submit: signed tx must equal verified tx → send
                                          │   → confirm → read actual balances → accounting
                                          ├ Monitor: take-profit ladder, moon bag, optional
                                          │   stop-loss / time exit
                                          └ Audit log (hash-chained JSONL, secrets redacted)
 Your own wallet's seed phrase / private key is never needed.`}</pre>

      <h3>2. Data sources</h3>
      <ul>
        <li><b>Solana RPC</b> ({state.env.rpcHost}): mint & freeze authority, Token-2022 extensions, supply, largest holders, launch history, early buyers, holder funding, simulation, sending/confirming.</li>
        <li><b>DexScreener</b>: price, liquidity, volume, market cap, pair. <b>GeckoTerminal</b>: hourly price history; new & trending pools for autopilot discovery.</li>
        <li><b>RugCheck</b> {state.env.rugcheckEnabled ? '' : '(disabled)'}: LP lock/burn status, holder count, third-party risk notes (treated as signals).</li>
        <li><b>{state.env.provider}</b>: quotes, routes, price impact, sellability probe, unsigned swap transactions.</li>
        <li><b>X API</b> {state.env.socialEnabled ? '' : '(not configured)'}: recent posts mentioning the mint; used only to flag hype/coordination, never to justify a buy.</li>
      </ul>

      <h3>3. Permissions</h3>
      <ul>
        <li>Each swap is one transaction. Solana swaps need no token approvals; any approval, delegation, authority change, or transfer to a third party is refused before anything is signed.</li>
        <li>Only allowlisted top-level programs: Compute Budget, System (wrap SOL to your own account), SPL Token / Token-2022 (sync/close your wSOL), Associated Token Account (create your own), Jupiter v6, Memo.</li>
        <li>The signed transaction must be byte-for-byte the verified one. The bot wallet can additionally sign only SOL withdrawals to your saved wallet, and only with the passphrase.</li>
      </ul>

      <h3>4. Current risk limits</h3>
      <ul>
        <li>Max buy {L.maxBuySol} SOL · daily loss {L.dailyLossLimitSol} SOL · exposure {L.maxTotalExposureSol} SOL · {L.maxOpenPositions} positions</li>
        <li>Slippage ≤ {L.maxSlippageBps} bps · price impact ≤ {L.maxPriceImpactPct}% · keep {L.minSolReserve} SOL reserve</li>
        <li>
          Take-profit ladder: {T.enabled ? ladderPreview(T).join('; ') : 'off'}; stop-loss {state.settings.stopLoss.enabled ? `${state.settings.stopLoss.dropPct}%` : 'off'}; time exit {state.settings.timeExit.enabled ? `${state.settings.timeExit.maxHoldMinutes} min` : 'off'}
        </li>
        <li>Autopilot: {state.settings.autopilot.buySizeSol} SOL per token, at most {state.settings.autopilot.maxBuysPerDay} buys per day, verified tokens passing every filter only.</li>
      </ul>

      <h3>5. Sample transaction flow (autopilot buy with the bot wallet)</h3>
      <ol>
        <li>The autopilot finds a pool, researches the token (facts, signals, sources, timestamps) and checks verification + every filter. Most tokens are rejected here; each decision is in the Autopilot activity log.</li>
        <li>It requests a fresh Jupiter quote and checks every risk limit.</li>
        <li>The server asks Jupiter for an unsigned transaction for the bot wallet, resolves lookup tables, decodes every instruction, rejects anything outside the allowlist, and confirms the mint is in the transaction.</li>
        <li>It simulates it against current chain state: tokens received ≥ minimum, SOL spent ≤ amount + fee budget.</li>
        <li>Only then does the bot wallet sign exactly those bytes; the server sends, confirms, reads the actual balance changes, books the fill, logs it and notifies you.</li>
        <li>The monitor then runs the take-profit ladder. If any outcome is uncertain, trading pauses and you are alerted.</li>
      </ol>

      <h3>6. Test safely first</h3>
      <ul>
        <li>Run the autopilot in <b>paper mode</b> first (with real data, not just the mock market) and look at what it would have bought.</li>
        <li>Fund the bot wallet with a <b>small</b> amount only. Warning threshold: {state.settings.testWalletMaxBalanceSol} SOL.</li>
        <li>
          Browser-wallet devnet signing test (switch your wallet to devnet):{' '}
          <button disabled={!wallet} onClick={() => run(async () => { await api('/devnet/airdrop', { wallet }); return 'Requested 0.5 devnet SOL (devnet faucets are rate-limited).'; })}>Request devnet airdrop</button>{' '}
          <button disabled={!wallet} onClick={devnetTest}>Sign devnet test</button>
        </li>
        <li>Jupiter routes only exist on mainnet, so real swaps cannot be rehearsed on devnet.</li>
      </ul>

      <h3>7. Choose who signs & enable live mode</h3>
      <div className="row">
        <label className="field check">
          <input type="radio" name="signer" checked={signer === 'bot'} onChange={() => setSigner('bot')} disabled={state.mode === 'live'} /> <span><b>Bot wallet</b> — fully automatic (autopilot, exits while you are away)</span>
        </label>
        <label className="field check">
          <input type="radio" name="signer" checked={signer === 'browser'} onChange={() => setSigner('browser')} disabled={state.mode === 'live'} /> <span><b>My browser wallet</b> — I approve every trade</span>
        </label>
      </div>
      {signer === 'browser' && (
        <p>
          Bound wallet: <span className="mono">{state.settings.boundWallet ?? 'none'}</span>{' '}
          <button disabled={!wallet || state.mode === 'live'} onClick={bindConnected}>Bind connected wallet</button>
        </p>
      )}
      {signer === 'bot' && !bw.exists && <p>Create and fund the bot wallet in the <b>Bot wallet</b> tab first.</p>}
      <ul className="checklist">
        {checks.map(([label, ok]) => <li key={label} className={ok ? 'pos' : 'neg'}>{ok ? '✓' : '✗'} {label}</li>)}
      </ul>
      <label className="override"><input type="checkbox" checked={reviewed} onChange={(e) => setReviewed(e.target.checked)} /> I reviewed the architecture, data sources, permissions, risk limits and sample flow, and I understand trading these tokens can lose all funds. No profit is promised.</label>
      {state.mode !== 'live' && (
        <div className="row">
          <input placeholder="Type ENABLE LIVE TRADING" value={phrase} onChange={(e) => setPhrase(e.target.value)} />
          <button
            className="danger"
            disabled={!checks.every(([, ok]) => ok) || phrase !== 'ENABLE LIVE TRADING'}
            onClick={() => run(async () => { const r = await api<{ warnings: string[] }>('/live/arm', { signer, wallet, phrase }); return `Live mode enabled. ${r.warnings.join(' ')}`; })}
          >
            Enable live trading
          </button>
        </div>
      )}
      {msg && <div className={`box ${msg.ok ? '' : 'err'}`}>{msg.text}{msg.v && msg.v.length > 0 && <ul>{msg.v.map((v) => <li key={v}>{v}</li>)}</ul>}</div>}
    </section>
  );
}
