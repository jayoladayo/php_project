// The bot's own wallet: create it, fund it ("drop" SOL into it), unlock it so
// it can sign unattended, and withdraw back to your own wallet.
import { useWallet } from '@solana/wallet-adapter-react';
import { useEffect, useState } from 'react';
import { api, ApiError, type DashboardState } from '../api.js';
import { short, sol } from '../format.js';

export function BotWalletPanel({ state, onChange }: { state: DashboardState; onChange?: () => void }) {
  const bw = state.botWallet;
  const { publicKey } = useWallet();
  const connected = publicKey?.toBase58() ?? null;
  const [pass, setPass] = useState(''); // create
  const [pass2, setPass2] = useState('');
  const [unlockPass, setUnlockPass] = useState('');
  const [bindPass, setBindPass] = useState('');
  const [wdPass, setWdPass] = useState('');
  const [balance, setBalance] = useState<string | null>(null);
  const [amount, setAmount] = useState('');
  const [bindTo, setBindTo] = useState('');
  const [msg, setMsg] = useState<{ ok: boolean; text: string; v?: string[] } | null>(null);
  const [copied, setCopied] = useState(false);

  const refreshBalance = () =>
    bw.exists ? api<{ lamports: string | null }>('/botwallet/balance').then((r) => setBalance(r.lamports)).catch(() => setBalance(null)) : Promise.resolve();
  useEffect(() => {
    void refreshBalance();
    const t = setInterval(refreshBalance, 20_000);
    return () => clearInterval(t);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [bw.exists, bw.publicKey]);

  const run = async (fn: () => Promise<string>) => {
    setMsg(null);
    try {
      setMsg({ ok: true, text: await fn() });
      onChange?.();
    } catch (e) {
      setMsg({ ok: false, text: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    }
  };
  const clear = () => {
    for (const f of [setPass, setPass2, setUnlockPass, setBindPass, setWdPass]) f('');
  };

  return (
    <section>
      <div className="box">
        <h2>Bot wallet — for fully automatic trading</h2>
        <p>
          A browser wallet (Phantom, Solflare…) asks you to approve every transaction, so it cannot trade while you are away. For unattended trading the bot uses <b>its own wallet</b>:
        </p>
        <ul>
          <li>The bot creates the wallet itself. You never paste a seed phrase or private key anywhere.</li>
          <li>Its key is stored <b>encrypted</b> on this computer (in <span className="mono">data/bot-wallet.json</span>) with a passphrase you choose. It is only usable while unlocked.</li>
          <li>It only signs swaps that passed the safety check and simulation, and withdrawals to <b>your own wallet</b>.</li>
          <li>You decide the risk by how much SOL you send to it. Only send what you are prepared to lose.</li>
        </ul>
      </div>

      {!bw.exists ? (
        <div className="box">
          <h3>Create the bot wallet</h3>
          <p className="muted">Choose a passphrase of at least 12 characters and write it down. Without it the wallet cannot be unlocked — there is no recovery.</p>
          <div className="row">
            <input type="password" placeholder="passphrase (12+ characters)" value={pass} onChange={(e) => setPass(e.target.value)} autoComplete="new-password" />
            <input type="password" placeholder="repeat passphrase" value={pass2} onChange={(e) => setPass2(e.target.value)} autoComplete="new-password" />
            <button
              className="primary"
              disabled={pass.length < 12 || pass !== pass2}
              onClick={() => run(async () => { const r = await api<{ publicKey: string }>('/botwallet/create', { passphrase: pass }); clear(); return `Created ${r.publicKey}. Send a small amount of SOL to it to start.`; })}
            >
              Create bot wallet
            </button>
          </div>
          {pass.length > 0 && pass2.length > 0 && pass !== pass2 && <p className="neg small">Passphrases do not match.</p>}
        </div>
      ) : (
        <>
          <div className="cards">
            <div className="card">
              <div className="muted small">Bot wallet address (send SOL here)</div>
              <div className="mono">{bw.publicKey}</div>
              <button className="link" onClick={() => { void navigator.clipboard?.writeText(bw.publicKey ?? ''); setCopied(true); setTimeout(() => setCopied(false), 1500); }}>{copied ? 'copied ✓' : 'copy address'}</button>{' '}
              <a href={`https://solscan.io/account/${bw.publicKey}`} target="_blank" rel="noreferrer">view on Solscan</a>
            </div>
            <div className="card">
              <div className="muted small">Balance (mainnet)</div>
              <div className="big">{balance === null ? '—' : sol(balance)}</div>
              <button className="link" onClick={() => void refreshBalance()}>refresh</button>
            </div>
            <div className="card">
              <div className="muted small">Status</div>
              <div className={`big ${bw.unlocked ? 'pos' : 'neg'}`}>{bw.unlocked ? 'Unlocked' : 'Locked'}</div>
              <div className="muted small">{bw.unlocked ? 'can sign trades until locked or the server restarts' : 'cannot sign anything'}</div>
            </div>
          </div>

          <div className="box">
            <h3>{bw.unlocked ? 'Lock' : 'Unlock'}</h3>
            {bw.unlocked ? (
              <button onClick={() => run(async () => (await api('/botwallet/lock', {}), 'Locked. Automatic live trading is on hold.'))}>Lock bot wallet</button>
            ) : (
              <div className="row">
                <input type="password" placeholder="passphrase" value={unlockPass} onChange={(e) => setUnlockPass(e.target.value)} />
                <button className="primary" disabled={!unlockPass} onClick={() => run(async () => { await api('/botwallet/unlock', { passphrase: unlockPass }); clear(); return 'Unlocked.'; })}>Unlock</button>
              </div>
            )}
            <p className="muted small">The bot wallet locks itself whenever the server restarts.</p>
          </div>

          <div className="box">
            <h3>Your own wallet (withdrawal destination)</h3>
            <p>
              Withdrawals from the bot wallet can only go to: <span className="mono">{state.settings.boundWallet ?? 'not set'}</span>
            </p>
            <div className="row">
              <input className="grow mono" placeholder="your wallet address (or connect Phantom and use it)" value={bindTo} onChange={(e) => setBindTo(e.target.value)} />
              {connected && <button onClick={() => setBindTo(connected)}>use connected wallet {short(connected)}</button>}
            </div>
            <div className="row">
              <input type="password" placeholder="bot wallet passphrase" value={bindPass} onChange={(e) => setBindPass(e.target.value)} />
              <button disabled={!bindTo || !bindPass || state.mode === 'live'} onClick={() => run(async () => { await api('/wallet/bind', { wallet: bindTo.trim(), passphrase: bindPass }); clear(); return 'Saved your wallet as the withdrawal destination.'; })}>
                Save my wallet
              </button>
              {state.mode === 'live' && <span className="muted small">switch to paper mode to change it</span>}
            </div>
          </div>

          <div className="box">
            <h3>Withdraw SOL to your wallet</h3>
            <div className="row">
              <input placeholder="amount in SOL" value={amount} onChange={(e) => setAmount(e.target.value)} size={10} />
              <input type="password" placeholder="bot wallet passphrase" value={wdPass} onChange={(e) => setWdPass(e.target.value)} />
              <button disabled={!state.settings.boundWallet || !wdPass || !amount} onClick={() => run(async () => { const r = await api<{ signature: string }>('/botwallet/withdraw', { passphrase: wdPass, amountSol: amount.trim() }); clear(); void refreshBalance(); return `Sent. Signature ${r.signature}`; })}>
                Withdraw amount
              </button>
              <button disabled={!state.settings.boundWallet || !wdPass} onClick={() => window.confirm('Withdraw ALL SOL from the bot wallet to your wallet?') && run(async () => { const r = await api<{ signature: string }>('/botwallet/withdraw', { passphrase: wdPass, all: true }); clear(); void refreshBalance(); return `Sent everything. Signature ${r.signature}`; })}>
                Withdraw everything
              </button>
            </div>
            <p className="muted small">
              Tokens the bot holds (for example moon bags) stay in the bot wallet; sell them from the Positions tab, or import the bot wallet into Phantom: stop the bot and run <span className="mono">npm.cmd run wallet:export</span> in the solana-bot folder (asks for the passphrase, prints the private key on your screen only).
            </p>
          </div>
        </>
      )}
      {msg && <div className={`box ${msg.ok ? '' : 'err'}`}>{msg.text}{msg.v && msg.v.length > 0 && <ul>{msg.v.map((v) => <li key={v}>{v}</li>)}</ul>}</div>}
    </section>
  );
}
