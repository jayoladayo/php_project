# Solana Research Trader

A local research dashboard and **paper-first** trading bot for Solana tokens.
It collects timestamped evidence from independent on-chain and market sources,
explains risks and uncertainty, and lets you trade through **your own wallet**
(Phantom, Solflare, Backpack, … via the Solana wallet adapter). Every live
trade is reviewed on screen and signed by your wallet; the bot never holds,
asks for, or stores a private key or seed phrase.

> This is a research tool, not financial advice. It does not predict or
> promise profit. Most newly launched Solana tokens lose most or all of their
> value. Social-media hype is never treated as evidence.

---

## Quick start

Requirements: Node.js 20+.

```bash
cd solana-bot
npm install
cp .env.example .env          # edit it; see "Configuration"

# 1. Offline demo (no network needed): mock market, paper trading only
MARKET_DATA=mock npm run dev
# open http://127.0.0.1:5173

# 2. Real market data, still paper trading (default mode)
npm run dev

# Tests
npm test
```

**Windows (PowerShell):** use `npm.cmd` instead of `npm` if PowerShell says
"running scripts is disabled", and set variables with `$env:`:

```powershell
npm.cmd install
$env:MARKET_DATA="mock"; npm.cmd run dev
```

Keep that window open while using the dashboard. Pressing Ctrl+C in it stops
the bot (to copy text from it, use Ctrl+Shift+C or right-click).

`npm run dev` starts the API server on `127.0.0.1:8787` and the dashboard on
`127.0.0.1:5173`. For a single process, `npm run build && npm start` serves the
built dashboard from `http://127.0.0.1:8787`.

### Staged rollout

1. **Mock paper trading** (`MARKET_DATA=mock`): learn the flow. Use the
   *Mock market* buttons to move prices and watch the 5× rule fire.
2. **Paper trading on real data** (default): research real mints; fills use
   real Jupiter quotes plus simulated slippage and fees. Nothing is signed.
3. **Devnet signing test**: *Live mode* tab → connect your wallet on devnet →
   *Request devnet airdrop* → *Sign devnet test*. This proves the wallet-adapter
   signing path end-to-end with no real funds. (Jupiter routes exist only on
   mainnet, so real swaps can't be rehearsed on devnet.)
4. **Live, tiny, separate wallet**: create a new wallet just for this bot, fund
   it with a small amount, set `LIVE_TRADING_ALLOWED=true`, restart, then in
   *Live mode*: bind the wallet, review the checklist, type
   `ENABLE LIVE TRADING`. Restarting the server always drops back to paper mode.

---

## What the research covers

For each mint the report shows every datapoint with its **source, link, and
fetch time**, separated into **facts** (read from chain/market data) and
**signals** (patterns that warrant a look, not proof).

| Area | Data | Source |
|---|---|---|
| Token | mint, supply, decimals, program (SPL / Token-2022), **mint authority**, **freeze authority**, active Token-2022 extensions (permanent delegate, transfer hook, transfer fee, default-frozen, non-transferable, pausable…) | Solana RPC |
| Age / creator | oldest signature time (lower bound if history is deep), fee payer of the first tx | Solana RPC |
| Market | price (USD/SOL), liquidity, 24h volume, market cap, FDV, txn counts, price change | DexScreener |
| Price history | 72 hourly closes | GeckoTerminal |
| LP | locked/burned % (when verifiable; n/a for bonding curves) | RugCheck |
| Holders | top token accounts with owner classification (wallet vs program-derived pool/vault/lock vs burn), top-10 wallet share, largest wallet, creator holdings, holder count | RPC, RugCheck |
| Sellability | probe buy quote → sell quote of the received tokens, round-trip loss, sell-side price impact, "no sell route" | Jupiter |
| Bundles | ≥3 wallets receiving tokens in the same slot (launch window), launch-window supply concentration, top holders sharing a first funder, holders funded by the creator | RPC (first 30 txs, top-10 holders' first funder) |
| Social | recent X posts mentioning the mint (links, dates, author age/followers), copied text across accounts, posting bursts, new/tiny accounts, paid-promo and hype language | X API v2 |
| Third party | RugCheck risk notes (as signals) | RugCheck |

**Verdict.** A token is `verified` only if all required data is present,
fresh (≤ `maxDataAgeSec`), consistent across sources (router price vs market
price, on-chain supply vs FDV/price), and there are no high/critical flags or
signals. Otherwise it is **NOT VERIFIED** with the reasons listed.
"Verified" means the checks passed — never that the token is safe.
Unverified tokens are never traded automatically; a manual trade needs an
explicit acknowledgement, and critical flags (active mint/freeze authority,
dangerous extensions, no sell route) block trading outright.

---

## Trading rules

| Rule | Default | Notes |
|---|---|---|
| Mode | **paper** | Live needs `LIVE_TRADING_ALLOWED=true` + bound wallet + typed phrase |
| Max buy | 0.05 SOL | |
| Daily loss limit | 0.1 SOL | realized losses + failed-tx fees + marked unrealized losses (UTC day); buys are also capped by the remaining budget |
| Total exposure | 0.2 SOL | sum of open cost basis (incl. fees) |
| Max slippage | 100 bps | |
| Max price impact | 2% | |
| SOL reserve | 0.02 SOL | buys only spend SOL already in the wallet; nothing is borrowed |
| Take-profit | **5× → sell 50% of the original amount** | configurable multiple, fraction and entry basis |
| Stop-loss | off | enable + set % in *Limits & rules* |
| Time exit | off | enable + set minutes |
| Buys | always user-initiated | no automatic buys, never from social/bundle/price signals |

All limits are checked when a trade is prepared **and again** right before it
is confirmed/sent.

### The 5× take-profit rule exactly

* **Entry price** is recorded per position (each buy is its own position):
  `effective = (SOL swapped + network + priority fees) ÷ tokens received`
  (default), or `fill price = SOL swapped ÷ tokens received`. Refundable
  token-account rent is shown separately and not counted as a cost.
* **Trigger**: every monitor cycle the bot requests an *executable* sell quote
  for the remaining target amount and subtracts estimated sell fees. It fires
  when `net proceeds ÷ tokens ≥ 5 × entry price` (exact integer math, tested at
  the boundary).
* **Size**: 50% of the **original** token amount of that position (capped at
  what is still held). Manual sells do not count toward it.
* **Partial fills**: if selling the whole target would exceed the price-impact
  limit, it sells the largest halving that fits (down to 1/16) and retries the
  remainder next cycle — only while the trigger still holds. The position shows
  `partial` with sold/target amounts.
* **After it completes** the position is **locked**: no further automatic buys
  or sells for it unless you enable a stop-loss/time-exit with *applies after
  take-profit*, or approve a manual trade.
* **Fees and slippage** are shown per fill (actual vs quoted output, slippage
  in %, fill ratio, fees, rent) and realized P&L is net of all fees.
* In **paper** mode the rule executes automatically. In **live** mode it
  creates a proposal you approve in the dashboard; if you enable *Live: skip
  dashboard approval*, the dashboard opens the wallet prompt directly. A browser
  wallet always asks you to sign — the bot cannot sign unattended.

---

## Wallet security and permissions

* Connect with the standard wallet adapter (Wallet Standard auto-detection).
* The server builds an **unsigned** Jupiter swap for your public key, resolves
  address lookup tables, decodes every top-level instruction and **refuses**:
  token `Approve`/`ApproveChecked` (delegations), `SetAuthority`, mint/burn,
  direct token transfers, SOL transfers to anyone but your own wSOL account,
  closing any account other than your temporary wSOL account, extra signers,
  a foreign fee payer, unknown programs, and swaps whose mint isn't in the
  transaction.
* It then **simulates** the transaction against current state and checks you
  receive at least the minimum and spend no more than amount + fee budget.
* The review screen shows token, mint, amount, estimated and minimum output,
  estimated price, price impact, route, fees (network, priority cap, platform,
  rent), risk flags, permissions, decoded instructions and simulation result.
* After your wallet signs, the server verifies the signed message is
  **byte-identical** to what you reviewed (otherwise it refuses to send), then
  re-checks pause and limits, sends, confirms, and reads the **actual** balance
  changes for accounting.
* No secrets are logged: the audit log redacts secret-looking keys, API keys in
  URLs, registered env secrets, key-shaped base58 strings, 64-byte key arrays
  and seed-phrase-shaped text, and is hash-chained to detect tampering.
* The API binds to `127.0.0.1`, checks `Origin` on mutating calls and supports
  an optional `DASHBOARD_TOKEN`.

## Failure handling

| Situation | Behaviour |
|---|---|
| Stale quote (> `quoteMaxAgeSec`, default 45 s) | refused; prepare again |
| Stale research | refreshed before buys; stale data → not verified |
| RPC / API errors, rate limits | per-host throttling, bounded retries with backoff/Retry-After; failed sources are listed in the report |
| Market data down 3 monitor cycles in a row | **auto-pause** + alert |
| Preflight rejection | trade marked failed, nothing executed |
| On-chain failure | marked failed; fee counted toward daily loss |
| Blockhash expired, never landed | marked expired |
| Outcome unknown (send/confirm uncertain, unreadable result) | marked unknown, **trading paused**, alert; use *Reconcile* before resuming |
| Signed tx differs from reviewed tx | not sent, alert |
| Server restart while live | drops back to paper mode |

**Emergency pause** (red button) stops new trades, cancels every pending
unsigned trade, and stops rule execution. Already-submitted transactions cannot
be recalled from the network; they are tracked to completion. Resuming requires
typing `RESUME` and reconciling any unknown outcomes.

## Audit log

`data/audit.jsonl` records research inputs (verdict, flags, sources), every
prepared quote and summary, rejections with reasons, approvals, fills,
signatures, rule triggers, settings changes, pause/resume and live arm/disarm.
View it in the *Audit log* tab, which also verifies the hash chain.

## Architecture

```
src/
  shared/            types + default settings
  server/
    index.ts         Express API (127.0.0.1)
    env.ts           environment config, secret registration
    audit.ts         hash-chained JSONL audit log + redaction
    http.ts          rate-limited fetch with retries
    store.ts         JSON state (atomic writes)
    market/          Jupiter provider, offline mock AMM
    research/        RPC facts, DexScreener/GeckoTerminal/RugCheck/X sources,
                     bundle + social analyzers, verdict (assessReport)
    trading/         accounting, rules (5× etc.), risk limits, tx verification,
                     simulation, engine (prepare/confirm/submit/monitor)
    devnet.ts        devnet signing test
  web/               React dashboard + wallet adapter
test/                vitest: accounting, 5× trigger, rules, risk, research,
                     tx verification, audit, engine (paper end-to-end)
```

## Known limitations

* Bundle analysis inspects the first 30 transactions and the first funder of
  the top-10 wallet holders; for tokens with very deep history it is skipped
  and age is shown as a lower bound. A shared funder can be an exchange.
* Holder count comes from RugCheck; LP lock status is not applicable to
  bonding-curve tokens.
* Sellability is inferred from quotes for a small probe, not an executed sell.
* Some wallets modify transactions when signing (e.g. adding priority fees or
  guard instructions). The server then refuses to send, by design; use a wallet
  that signs the transaction as presented.
* Third-party API response formats can change; parsing is defensive and
  failures appear as missing data (→ not verified), never as a pass.
