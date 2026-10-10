// Pre-trade review: shows token, mint, amounts, price, fees, slippage, route,
// risk flags, permissions and (live) the decoded instructions + simulation.
// Live trades are signed by the user's wallet; the server only sends the
// exact transaction that was reviewed here.
import { useWallet } from '@solana/wallet-adapter-react';
import { VersionedTransaction } from '@solana/web3.js';
import { useEffect, useState } from 'react';
import { api, ApiError, type TradeIntent } from '../api.js';
import type { SignerKind } from '../../shared/types.js';
import { price, sol, tokens } from '../format.js';

export function TradeModal({ intent, onClose, autoSign = false, signer = null }: { intent: TradeIntent; onClose: (done: boolean) => void; autoSign?: boolean; signer?: SignerKind | null }) {
  const { signTransaction, publicKey } = useWallet();
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<{ msg: string; v: string[] } | null>(null);
  const secondsLeft = () => Math.max(0, Math.round((new Date(intent.expiresAt).getTime() - Date.now()) / 1000));
  const [left, setLeft] = useState(secondsLeft);
  const S = intent.summary;
  const live = intent.mode === 'live';
  const botSigns = live && signer === 'bot';

  useEffect(() => {
    const t = setInterval(() => setLeft(secondsLeft()), 250);
    return () => clearInterval(t);
  }, [intent.expiresAt]);

  const confirm = async () => {
    setBusy(true);
    setError(null);
    try {
      if (!live) {
        await api(`/trades/${intent.id}/confirm-paper`, {});
      } else if (botSigns) {
        await api(`/trades/${intent.id}/execute`, {});
      } else {
        if (!signTransaction || !publicKey) throw new ApiError('Connect your wallet first.', []);
        if (publicKey.toBase58() !== intent.wallet) throw new ApiError('Connected wallet differs from the wallet this trade was prepared for.', []);
        if (!intent.txBase64) throw new ApiError('Missing transaction.', []);
        const tx = VersionedTransaction.deserialize(Uint8Array.from(atob(intent.txBase64), (c) => c.charCodeAt(0)));
        const signed = await signTransaction(tx); // the wallet shows its own approval prompt
        const b64 = btoa(String.fromCharCode(...signed.serialize()));
        await api(`/trades/${intent.id}/submit`, { signedTx: b64 });
      }
      onClose(true);
    } catch (e) {
      setError({ msg: (e as Error).message, v: e instanceof ApiError ? e.violations : [] });
    } finally {
      setBusy(false);
    }
  };

  useEffect(() => {
    if (autoSign && live && !botSigns) void confirm();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const cancel = async () => {
    await api(`/trades/${intent.id}/cancel`, {}).catch(() => undefined);
    onClose(false);
  };

  const inLabel = S.side === 'buy' ? sol(S.inAmountRaw, 6) : `${tokens(S.inAmountRaw, S.decimals)} ${S.symbol}`;
  const outLabel = (raw: string) => (S.side === 'buy' ? `${tokens(raw, S.decimals)} ${S.symbol}` : sol(raw, 6));
  const serious = S.riskFlags.filter((f) => f.severity === 'critical' || f.severity === 'high');

  return (
    <div className="modal-bg">
      <div className="modal">
        <h2>
          Review {live ? <span className="badge live">LIVE</span> : <span className="badge paper">PAPER</span>} {S.side.toUpperCase()} {S.symbol}
        </h2>
        {intent.reason !== 'manual' && <p className="note">Triggered by rule: {intent.reason.replace('_', '-')}</p>}
        <table className="kv">
          <tbody>
            <tr><th>Token</th><td>{S.symbol}</td></tr>
            <tr><th>Mint address</th><td className="mono">{S.mint}</td></tr>
            <tr><th>You pay</th><td>{inLabel}</td></tr>
            <tr><th>Estimated receive</th><td>{outLabel(S.estOutRaw)}</td></tr>
            <tr><th>Minimum receive</th><td>{outLabel(S.minOutRaw)} <span className="muted">(after {S.slippageBps / 100}% slippage tolerance; the swap reverts below this)</span></td></tr>
            <tr><th>Estimated price</th><td>{price(S.estPriceSolPerToken)} SOL / token</td></tr>
            <tr><th>Price impact</th><td>{S.priceImpactPct.toFixed(3)}%</td></tr>
            <tr><th>Route</th><td>{S.route.join(' → ')}</td></tr>
            <tr>
              <th>Fees</th>
              <td>
                network {sol(S.fees.networkLamports, 6)} · priority up to {sol(S.fees.priorityMaxLamports, 6)}
                {S.fees.platformRaw !== '0' && ` · platform ${S.fees.platformRaw} raw`}
                {S.fees.rentDepositLamports !== '0' && ` · refundable account rent ${sol(S.fees.rentDepositLamports, 5)}`}
              </td>
            </tr>
            <tr><th>Research verdict</th><td className={S.verdict === 'verified' ? 'pos' : 'neg'}>{S.verdict}</td></tr>
          </tbody>
        </table>

        {serious.length > 0 && (
          <div className="box warn">
            <b>Risk flags</b>
            <ul>{serious.map((f) => <li key={f.code}>[{f.severity}/{f.kind}] {f.message}</li>)}</ul>
          </div>
        )}
        {S.warnings.length > 0 && <div className="box warn"><ul>{S.warnings.map((w) => <li key={w}>{w}</li>)}</ul></div>}

        <div className="box">
          <b>Permissions & effects</b>
          <ul>{S.permissions.map((p) => <li key={p}>{p}</li>)}</ul>
        </div>
        {live && (
          <details>
            <summary>Decoded instructions ({S.instructions.length}) and simulation</summary>
            <ol>{S.instructions.map((i, k) => <li key={k}><b>{i.program}</b> {i.name}: {i.explanation}</li>)}</ol>
            {S.simulation && <p className={S.simulation.ok ? 'pos' : 'neg'}>{S.simulation.detail}</p>}
          </details>
        )}
        <p className="muted">
          Fees and slippage reduce what you receive; the final fill can be anywhere between the estimate and the minimum. Quote expires in {left}s.
          {live && !botSigns && ' Your wallet will ask you to approve this exact transaction. If the wallet changes it, the server refuses to send it.'}
          {botSigns && ' The bot wallet will sign exactly this verified and simulated transaction and send it.'}
        </p>
        {error && (
          <div className="box err">
            {error.msg}
            {error.v.length > 0 && <ul>{error.v.map((v) => <li key={v}>{v}</li>)}</ul>}
          </div>
        )}
        <div className="row">
          <button className="primary" disabled={busy || left === 0} onClick={confirm}>
            {busy ? 'Working…' : botSigns ? 'Execute with bot wallet' : live ? 'Sign with wallet & send' : 'Confirm paper trade'}
          </button>
          <button onClick={cancel} disabled={busy}>Cancel</button>
        </div>
      </div>
    </div>
  );
}
