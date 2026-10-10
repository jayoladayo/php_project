# Solana Research Trader

A local research dashboard and trading bot for Solana tokens. It collects
timestamped evidence from independent on-chain and market sources, explains
risks and uncertainty, and trades in one of three ways:

* **manually**, through your own browser wallet (Phantom, Solflare, Backpack…),
  approving every transaction;
* **automatically in paper mode** (simulated money) — the default;
* **automatically for real**, with the bot's own encrypted **bot wallet** and the
  **autopilot**, after you explicitly enable it.

> This is a research tool, not financial advice. It does not predict or
> promise profit. Most newly launched Solana tokens lose most or all of their
> value, and automated trading can lose money faster than manual trading.
> Social-media hype is never treated as evidence.

---

## Quick start (Windows)

Requirements: [Node.js](https://nodejs.org) 20 or newer.

1. Download the code (green **Code** button → **Download ZIP**) and unzip it.
2. Open the `solana-bot` folder and double-click:
   * **`start-bot-practice.cmd`** — practice on a simulated market (prices move
     by themselves; no internet data, no real money), or
   * **`start-bot.cmd`** — real market data (still paper money until you enable
     live trading).
3. The dashboard opens at **http://127.0.0.1:8787**. Keep the black window open;
   closing it stops the bot.

From a terminal instead (PowerShell: use `npm.cmd`, set variables with `$env:`):

```powershell
npm.cmd install
$env:MARKET_DATA="mock"; npm.cmd run dev    # practice market, dashboard on http://127.0.0.1:5173
npm.cmd run dev                             # real data, paper money
npm.cmd test                                # run the test suite
```

On macOS/Linux use `npm` and `MARKET_DATA=mock npm run dev`.

Do not press Ctrl+C in the bot's window unless you want to stop it (to copy
text from it, use Ctrl+Shift+C or right-click).

---

## How to use it, step by step

1. **Practice** (`start-bot-practice.cmd`): open the **Autopilot** tab → **Start
   autopilot**. Watch it research the simulated tokens, buy the ones that pass,
   skip the bad ones (with reasons), and sell through the take-profit ladder as
   prices drift. Use the *Mock market* buttons to move prices yourself.
2. **Paper trade on real data** (`start-bot.cmd`): set up a dedicated RPC
   endpoint first (see *Configuration*), start the autopilot in paper mode, and
   let it run for a while. Check what it bought and why in the activity log.
3. **Go live, small**: create the bot wallet (**Bot wallet** tab), save your own
   wallet as the withdrawal destination, send the bot wallet a small amount of
   SOL, set `LIVE_TRADING_ALLOWED=true` in `.env`, restart, then in **Live
   mode** choose *Bot wallet*, review everything and type `ENABLE LIVE TRADING`.
   Finally start the autopilot (type `START AUTOPILOT`).

---

## The take-profit ladder and moon bag

Configure it in **Limits & rules**. Default:

| Level | When | Sells |
|---|---|---|
| 1 | price reaches **3×** your entry | enough tokens to **get your initial investment back** |
| 2 | price reaches **5×** your entry | **50% of the original** amount |
| — | — | the rest (at least **15%**, the **moon bag**) is never sold by take-profit |

Example on 1,000 tokens (ignoring fees): at 3× it sells ≈ 333 (your stake back),
at 5× it sells 500, and ≈ 167 ride as the moon bag.

* Up to 6 levels; each sells either *my initial investment back*, *a percentage
  of the original amount*, or *everything except the moon bag*.
* Levels trigger on the **executable** sell price (a real quote) after estimated
  fees, compared with your recorded entry price (by default including entry
  fees). One level per check, in order.
* "Initial investment back" sells enough to recover the full entry cost even at
  the worst fill your slippage limit allows, minus anything earlier sells
  already returned.
* If a sale would exceed the price-impact limit it sells in smaller chunks
  (partial fills) and continues next cycle while the price still qualifies.
* When the last level is done the position is **locked**: the moon bag rides and
  nothing sells it automatically unless you enable a stop-loss/time exit with
  *also applies after the ladder* (and *may also sell the moon bag*).
* Ladder changes apply to positions opened afterwards.

Stop-loss and time exit are optional and **off** until you set them. They keep
the moon bag unless you allow otherwise. The live autopilot requires you to
either enable a stop-loss or explicitly acknowledge running without one.

---

## The autopilot

Every scan (default 2 minutes) it:

1. checks your limits first (daily buy cap, open positions, exposure, daily loss,
   balance) and does nothing else if a buy would be blocked anyway;
2. **discovers** tokens from GeckoTerminal *new pools* and *trending pools* plus
   your **watchlist** (paid-promotion feeds and social media are not used);
3. **pre-filters** on pool data, skips tokens it holds, bought recently
   (cooldown) or rejected recently;
4. runs the **full research** on the best few (default 3 per scan);
5. **buys only** if the token is **verified** *and* passes every filter:

| Filter (default) | Why |
|---|---|
| liquidity ≥ $30,000 | you can get out |
| age ≥ 60 min and ≤ 7 days | most rugs happen in the first hour |
| 24h volume ≥ $50,000 | real activity |
| holders ≥ 300 | not a handful of wallets |
| sells in 24h ≥ 50 | other people have been able to sell |
| 1h price change ≤ 50% | never chase a vertical spike |
| LP lock/burn known and ≥ 90% | liquidity can't be pulled |

Plus everything verification already requires (mint & freeze authority revoked,
no dangerous Token-2022 extensions, sell route exists, acceptable round-trip
loss and holder concentration, no high-severity bundle/social signals,
consistent and fresh data).

Buys use the configured size (default **0.02 SOL**), at most **3 per day**, and
always within every risk limit. Every decision — buy, skip (with the failed
checks), error — is in the **Autopilot → Activity** log and the audit log.
Exits then follow the ladder automatically. If token discovery fails 3 times in
a row, or any transaction outcome is uncertain, trading pauses and you are
alerted.

Most real tokens will be rejected. That is intentional.

---

## The bot wallet (unattended trading)

A browser wallet asks you to approve every transaction, so it cannot trade while
you are away. For that the bot uses its own wallet:

* The bot **generates** it. You never paste a seed phrase or private key.
* The key is **encrypted** (scrypt + AES-256-GCM) with your passphrase in
  `data/bot-wallet.json` and only usable while **unlocked**. It locks itself on
  every restart.
* It signs only (a) swaps that passed the allowlist check and simulation and
  (b) SOL withdrawals to **your own saved wallet**, which also require the
  passphrase. There is no API that returns the key or signs anything else.
* You choose the risk by how much SOL you send it. **Only send what you are
  prepared to lose**, and withdraw profits regularly.
* To see it in Phantom or move tokens yourself: stop the bot and run
  `npm.cmd run wallet:export` (asks for the passphrase, prints the private key
  on your screen only). `npm.cmd run wallet:address` prints the address.
* Back up `data/bot-wallet.json` **and** remember the passphrase. Lose either
  and funds in the bot wallet are unrecoverable.

### Keeping it running

The bot only trades while its window is open and the computer is awake: set
Windows *Power & sleep* to never sleep while it runs. After a restart the bot
comes back in **paper mode** with the bot wallet **locked** and the autopilot
**stopped**; re-enable them in the dashboard. For a machine that must recover
unattended you can set `BOT_WALLET_PASSPHRASE` and
`RESUME_LIVE_AFTER_RESTART=true` — but then anyone with access to that machine
can use the wallet, which defeats the encryption.

### Notifications

Set `TELEGRAM_BOT_TOKEN` + `TELEGRAM_CHAT_ID` (create a bot with @BotFather) or
`ALERT_WEBHOOK_URL` (Slack/Discord) to get every automatic buy, sell, pause and
error on your phone.

---

## Research

For each mint the report shows every datapoint with its **source, link and
fetch time**, separated into **facts** and **signals** (patterns, not proof):

| Area | Data | Source |
|---|---|---|
| Token | mint, supply, decimals, program, **mint authority**, **freeze authority**, active Token-2022 extensions | Solana RPC |
| Age / creator | oldest signature time (lower bound if history is deep), first fee payer | Solana RPC |
| Market | price, liquidity, volume, market cap, FDV, txns, price change | DexScreener |
| Price history | 72 hourly closes | GeckoTerminal |
| LP | locked/burned % (n/a for bonding curves) | RugCheck |
| Holders | top accounts classified wallet / pool-vault-lock / burn, top-10 share, largest wallet, creator share, holder count | RPC, RugCheck |
| Sellability | probe buy → sell quote, round-trip loss, sell impact, "no sell route" | Jupiter |
| Bundles | same-slot launch buys, launch concentration, shared/creator funders of top holders | RPC |
| Social | recent X posts (links, dates, account age/followers), copied text, bursts, paid/hype language | X API |

`verified` only means the automated checks found complete, fresh, consistent
data with no high/critical flags — never that a token is safe.

---

## Risk limits (defaults, all editable)

Max buy 0.05 SOL · daily loss 0.1 SOL · total exposure 0.2 SOL · max 5 open
positions · slippage ≤ 1% · price impact ≤ 2% · 0.02 SOL always kept in the
wallet · no borrowed funds. Checked when a trade is prepared **and** again just
before it is signed/confirmed — for manual trades, rules and the autopilot alike.

## Transaction safety

Before anything is signed: the unsigned Jupiter transaction is decoded and
refused if it contains token approvals/delegations, authority changes, transfers
to anyone else, extra signers, unknown programs, or a different mint; then it is
simulated (tokens received ≥ minimum, SOL spent ≤ amount + fee budget). The
signed bytes must equal the verified bytes. After sending, the actual balance
changes are read from the chain for accounting. Uncertain outcomes pause
trading until you *Reconcile*.

**Emergency pause** stops everything new (autopilot, rules, manual) and cancels
unsigned trades.

The API binds to `127.0.0.1`, answers only to local host names (DNS-rebinding
protection), checks `Origin` on every change, and supports `DASHBOARD_TOKEN`.
Secrets are redacted from logs; the audit log is hash-chained.

---

## Configuration (`.env`)

Copy `.env.example` to `.env`. The most important settings:

* `SOLANA_RPC_URL` — a dedicated endpoint (e.g. a free Helius key). The public
  endpoint is heavily rate-limited; research and the autopilot need many calls.
* `JUPITER_API_BASE` / `JUPITER_API_KEY` — if quotes fail on the keyless
  endpoint, get a free key at portal.jup.ag and use `https://api.jup.ag/swap/v1`.
* `LIVE_TRADING_ALLOWED=true` — required before live mode can be enabled.
* `TELEGRAM_BOT_TOKEN`, `TELEGRAM_CHAT_ID`, `ALERT_WEBHOOK_URL` — notifications.
* `X_BEARER_TOKEN` — optional X/Twitter research.

Practice runs (`MARKET_DATA=mock`) keep their state in `data-mock/`, real-data
runs in `data/`; the bot wallet always lives in `data/bot-wallet.json`.

## Architecture

```
src/
  shared/            types + default settings
  server/
    index.ts         Express API (127.0.0.1), startup safety
    env.ts, audit.ts, http.ts, store.ts, alerts.ts
    market/          Jupiter provider, offline mock market (random walk)
    research/        RPC facts, DexScreener/GeckoTerminal/RugCheck/X, bundle +
                     social analyzers, verdict, autopilot scanner
    trading/         accounting (ladder, moon bag), rules, risk limits,
                     strategy filters, autopilot, tx verification,
                     simulation, engine
    wallet/          encrypted bot wallet + CLI (address / export)
  web/               React dashboard + wallet adapter
test/                accounting, ladder rules, risk, strategy, research,
                     tx verification, bot wallet, audit, paper engine +
                     autopilot, live bot-wallet path (fake RPC/router)
```

## Known limitations

* Live trading paths are tested against a fake RPC node and router that build
  real Solana transactions, but not against mainnet from the development
  environment. Start live with a tiny balance.
* Bundle analysis inspects the first 30 transactions and the first funder of the
  top-10 holders; deep-history tokens skip it. A shared funder can be an exchange.
* Holder count comes from RugCheck; LP lock status is not applicable to
  bonding-curve tokens (with the default filters those are skipped).
* Sellability is inferred from quotes for a small probe, not an executed sell.
* Some browser wallets modify transactions when signing; the server then
  refuses to send them, by design. (The bot wallet does not have this issue.)
* Third-party API formats can change; failures show up as missing data
  (→ not verified → not bought), never as a pass.
