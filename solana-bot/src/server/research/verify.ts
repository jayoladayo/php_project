// Pure: turn collected research data into risk flags and a verdict.
// "verified" only means the automated checks found complete, fresh,
// consistent data with no high/critical flags. It is NOT a safety guarantee.
import type { ResearchReport, RiskFlag, Sourced, VerificationThresholds } from '../../shared/types.js';

export type ReportDraft = Omit<ResearchReport, 'verdict' | 'verdictReasons' | 'summary' | 'uncertainty' | 'flags'>;

const DANGEROUS_EXTENSIONS: Record<string, { severity: RiskFlag['severity']; why: string }> = {
  permanentDelegate: { severity: 'critical', why: 'a permanent delegate can move or burn tokens from any holder' },
  nonTransferable: { severity: 'critical', why: 'tokens cannot be transferred (cannot be sold)' },
  transferHook: { severity: 'high', why: 'a transfer hook program runs on every transfer and can block sells' },
  pausableConfig: { severity: 'critical', why: 'an authority can pause all transfers' },
  confidentialTransferMint: { severity: 'warn', why: 'confidential transfers hide amounts from analysis' },
  defaultAccountState: { severity: 'high', why: 'new token accounts may start frozen' },
  transferFeeConfig: { severity: 'warn', why: 'a fee is taken on every transfer' },
  mintCloseAuthority: { severity: 'warn', why: 'the mint account can be closed' },
  interestBearingConfig: { severity: 'warn', why: 'displayed balances change over time' },
  scaledUiAmountConfig: { severity: 'warn', why: 'displayed balances can be rescaled by an authority' },
};

export function assessReport(draft: ReportDraft, t: VerificationThresholds, now: Date, extraFlags: RiskFlag[] = []): ResearchReport {
  const flags: RiskFlag[] = [...extraFlags];
  const missing: string[] = [];
  const stale: string[] = [];
  const contradictions: string[] = [];
  const uncertainty: string[] = [];

  const need = <T>(label: string, s: Sourced<T>): T | null => {
    if (s.value === null || s.value === undefined) {
      missing.push(label);
      return null;
    }
    if ((now.getTime() - new Date(s.fetchedAt).getTime()) / 1000 > t.maxDataAgeSec) stale.push(label);
    return s.value;
  };
  const opt = <T>(label: string, s: Sourced<T>, why: string): T | null => {
    if (s.value === null || s.value === undefined) {
      uncertainty.push(`${label} unavailable${s.note ? ` (${s.note})` : ''}: ${why}`);
      return null;
    }
    return s.value;
  };

  // ---- token program / authorities (facts read from chain)
  const mintAuth = need('mint authority', draft.token.mintAuthority);
  if (mintAuth && mintAuth !== 'none')
    flags.push({ code: 'mint_authority_active', kind: 'fact', severity: 'critical', message: `Mint authority ${mintAuth} can create unlimited new tokens.`, source: draft.token.mintAuthority.source });
  const freezeAuth = need('freeze authority', draft.token.freezeAuthority);
  if (freezeAuth && freezeAuth !== 'none')
    flags.push({ code: 'freeze_authority_active', kind: 'fact', severity: 'critical', message: `Freeze authority ${freezeAuth} can freeze holders' token accounts (honeypot risk).`, source: draft.token.freezeAuthority.source });
  const exts = need('token extensions', draft.token.extensions) ?? [];
  for (const e of exts) {
    const d = DANGEROUS_EXTENSIONS[e];
    if (d) flags.push({ code: `ext_${e}`, kind: 'fact', severity: d.severity, message: `Token-2022 extension "${e}": ${d.why}.`, source: draft.token.extensions.source });
  }
  const decimals = need('decimals', draft.token.decimals);
  const supply = need('supply', draft.token.supplyRaw);
  const age = opt('token age', draft.token.ageMinutes, 'very new tokens carry extra risk');
  if (age !== null && age < 60) flags.push({ code: 'very_new_token', kind: 'fact', severity: 'warn', message: `Token is only ${age.toFixed(0)} minutes old.` });
  if (draft.token.ageMinutes.note?.includes('lower bound')) uncertainty.push(`Token age is a lower bound: ${draft.token.ageMinutes.note}`);

  // ---- market
  const liq = need('liquidity', draft.market.liquidityUsd);
  if (liq !== null && liq < t.minLiquidityUsd)
    flags.push({ code: 'low_liquidity', kind: 'fact', severity: 'high', message: `Liquidity $${Math.round(liq).toLocaleString()} is below the $${t.minLiquidityUsd.toLocaleString()} minimum.`, source: draft.market.liquidityUsd.source });
  need('price', draft.market.priceUsd);
  opt('24h volume', draft.market.volume24hUsd, 'activity level unknown');
  opt('market cap', draft.market.marketCapUsd, 'valuation unknown');
  opt('price history', draft.market.priceHistory, 'cannot judge volatility');
  const lp = opt('LP lock/burn status', draft.market.lpLockedPct, 'liquidity could be withdrawn at any time');
  if (lp !== null && lp < 80)
    flags.push({ code: 'lp_not_locked', kind: 'fact', severity: 'high', message: `Only ${lp.toFixed(1)}% of LP is locked or burned; the rest can be withdrawn.`, source: draft.market.lpLockedPct.source });

  // ---- holders
  const top10 = need('top-10 holder share', draft.holders.top10Pct);
  if (top10 !== null && top10 > t.maxTop10HolderPct)
    flags.push({ code: 'holder_concentration', kind: 'fact', severity: 'high', message: `Top 10 non-program wallets hold ${top10.toFixed(1)}% of supply (limit ${t.maxTop10HolderPct}%).` });
  const single = need('largest wallet share', draft.holders.maxSingleWalletPct);
  if (single !== null && single > t.maxSingleHolderPct)
    flags.push({ code: 'single_holder', kind: 'fact', severity: 'high', message: `One wallet holds ${single.toFixed(1)}% of supply (limit ${t.maxSingleHolderPct}%).` });
  opt('holder count', draft.holders.count, 'distribution breadth unknown');
  const creator = draft.token.creator.value;
  if (creator && draft.holders.top.value) {
    const c = draft.holders.top.value.find((h) => h.owner === creator);
    if (c && c.pct > 5) flags.push({ code: 'creator_holds', kind: 'fact', severity: 'warn', message: `Creator wallet still holds ${c.pct.toFixed(1)}% of supply.` });
  }

  // ---- sellability
  const buyQ = need('buy quote', draft.sellability.buyQuote);
  const sellQ = draft.sellability.sellQuote;
  if (sellQ.value === null) {
    flags.push({ code: 'no_sell_route', kind: 'fact', severity: 'critical', message: `No sell route found${sellQ.note ? ` (${sellQ.note})` : ''}. The token may not be sellable.`, source: sellQ.source });
    missing.push('sell quote');
  } else need('sell quote', sellQ);
  const rt = draft.sellability.roundTripLossPct.value;
  if (rt !== null && rt > t.maxRoundTripLossPct)
    flags.push({ code: 'round_trip_loss', kind: 'fact', severity: 'high', message: `Buying then immediately selling a probe amount would lose ${rt.toFixed(1)}% — consistent with hidden taxes, thin liquidity, or honeypot-like behaviour.` });
  if (sellQ.value && sellQ.value.priceImpactPct > 10)
    flags.push({ code: 'sell_impact', kind: 'fact', severity: 'high', message: `Selling the probe amount has ${sellQ.value.priceImpactPct.toFixed(1)}% price impact.` });
  uncertainty.push('Sellability is inferred from router quotes for a small probe, not from an executed sell; a token can still block or tax sells it has not been tested with.');

  // ---- cross-source consistency
  const dexSol = draft.market.priceSol.value;
  if (buyQ && dexSol && decimals !== null) {
    const routerSol = Number(buyQ.inAmount) / 1e9 / (Number(buyQ.outAmount) / 10 ** decimals);
    const diff = (Math.abs(routerSol - dexSol) / dexSol) * 100;
    // Allow for the quote's own price impact before calling it a contradiction.
    if (diff - buyQ.priceImpactPct > t.maxPriceDisagreementPct)
      contradictions.push(`router price ${routerSol.toPrecision(4)} SOL vs market data ${dexSol.toPrecision(4)} SOL (${diff.toFixed(1)}% apart)`);
  } else uncertainty.push('Could not cross-check router price against market data.');
  const fdv = draft.market.fdvUsd.value;
  const pUsd = draft.market.priceUsd.value;
  if (fdv && pUsd && supply && decimals !== null) {
    const impliedSupply = fdv / pUsd;
    const chainSupply = Number(supply) / 10 ** decimals;
    if (chainSupply > 0 && Math.abs(impliedSupply - chainSupply) / chainSupply > 0.05)
      contradictions.push(`market-data supply ${impliedSupply.toExponential(3)} vs on-chain supply ${chainSupply.toExponential(3)}`);
  }

  // ---- signals (bundles, social, third parties)
  const bundles = opt('bundle/coordinated-buy analysis', draft.bundles, 'coordinated launches cannot be ruled out');
  if (bundles) flags.push(...bundles.signals);
  const social = draft.social.value;
  if (social) flags.push(...social.signals);
  else uncertainty.push(`X/Twitter data unavailable${draft.social.note ? ` (${draft.social.note})` : ''}. This does not affect the verdict: social posts are never treated as evidence of safety.`);
  const third = draft.thirdPartyFlags.value;
  if (third) {
    for (const r of third) {
      const lvl = r.level.toLowerCase();
      if (lvl === 'danger' || lvl === 'error')
        flags.push({ code: 'third_party_danger', kind: 'signal', severity: 'high', message: `Third-party report: ${r.name} — ${r.description}`, source: draft.thirdPartyFlags.source });
      else if (lvl === 'warn' || lvl === 'warning')
        flags.push({ code: 'third_party_warn', kind: 'signal', severity: 'warn', message: `Third-party report: ${r.name} — ${r.description}`, source: draft.thirdPartyFlags.source });
    }
  }
  if (draft.mock) flags.push({ code: 'mock_data', kind: 'fact', severity: 'high', message: 'MOCK market data — paper trading only.' });

  // ---- verdict
  const reasons: string[] = [];
  if (missing.length) reasons.push(`Missing key data: ${missing.join(', ')}.`);
  if (stale.length) reasons.push(`Stale data: ${stale.join(', ')}.`);
  if (contradictions.length) reasons.push(`Contradictory data: ${contradictions.join('; ')}.`);
  const serious = flags.filter((f) => f.severity === 'high' || f.severity === 'critical');
  if (serious.length) reasons.push(`Serious flags: ${serious.map((f) => f.code).join(', ')}.`);
  const verdict = reasons.length === 0 ? 'verified' : 'not_verified';
  if (verdict === 'verified') reasons.push('All required data present, fresh and consistent; no high or critical flags. This is not a guarantee of safety or profit.');

  const crit = flags.filter((f) => f.severity === 'critical').length;
  const high = flags.filter((f) => f.severity === 'high').length;
  const warn = flags.filter((f) => f.severity === 'warn').length;
  const summary =
    `${draft.token.symbol.value ?? 'Unknown token'}: ${verdict === 'verified' ? 'passed automated checks' : 'NOT VERIFIED'} — ` +
    `${crit} critical, ${high} high, ${warn} warning flag(s); ` +
    `liquidity ${liq !== null ? `$${Math.round(liq).toLocaleString()}` : 'unknown'}, ` +
    `top-10 holders ${top10 !== null ? `${top10.toFixed(1)}%` : 'unknown'}, ` +
    `age ${age !== null ? `${age.toFixed(0)} min` : 'unknown'}.`;

  return { ...draft, flags, verdict, verdictReasons: reasons, summary, uncertainty };
}
