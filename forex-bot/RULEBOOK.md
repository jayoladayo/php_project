# Forex bot rulebook — DRAFT v0.1 (nothing is built from this until the owner approves it)

This turns the owner's agent prompt into rules the bot enforces in code. A deterministic program is used for execution because
every rule can be unit-tested and backtested; an AI chat agent cannot be tested that way. Nothing here promises profit.

## 1. Account settings (proposed — owner to confirm the items marked ASK)
| Setting | Value |
|---|---|
| Mode | **paper** now. The text "[Approve live mode]" in the prompt was a template placeholder, so it is NOT treated as live approval. Live needs a separate, typed confirmation after the validation gate (section 6). |
| Platform | MetaTrader 5 (broker name: ASK). Trade-only; the bot never deposits, withdraws or changes permissions, and never asks for passwords. |
| Starting capital | $50 |
| Pairs | EURUSD, GBPUSD (ASK: add USDJPY / others?) |
| Timeframes | D1/H4 = bias and key levels, H1 = structure + setup zone, M15 = confirmation trigger |
| Scan | Entries evaluated at each M15 bar close; open trades monitored every 10 s. (A 10-minute scan would only ever see the same closed bar twice.) |
| Timezone | ASK (all internal times are UTC) |
| Max risk per trade | 2% of equity ($1.00) as the hard ceiling, 1% ($0.50) preferred — see the $50 problem below |
| Max total open risk | 3% ($1.50), counting same-currency exposure together (long EURUSD + long GBPUSD = one short-USD bet) |
| Daily loss limit | 3% ($1.50): stop opening trades for the day, alert |
| Weekly loss limit | 6% ($3.00) |
| Max drawdown before stopping | 15% from peak equity ($7.50): bot pauses and stays paused until the owner resumes it |
| Max open positions | 2 |
| News blackout | no new entries 30 min before / 30 min after high-impact events for either currency in the pair (60 min for rate decisions, NFP, CPI); the calendar is a file the bot reads; no calendar or a stale calendar = no trading |
| Trading hours | 07:00–17:00 UTC (London + New York overlap); never during rollover (21:00–23:00 UTC), weekends, bank-holiday sessions or the days around New Year |
| Daily trade cap | 2 trades per day; 2 losses in a row = done for the day |

### The $50 problem (read this)
A 0.01 lot (the usual smallest trade) on EURUSD is worth about $0.10 per pip. A sensible H1 stop of 15–25 pips therefore risks
$1.50–$2.50 = 3–5% of a $50 account, which is above the 2% ceiling. The bot will not round up past the cap, so on a standard
account it will skip most setups. Options: (a) open a **cent account** (the minimum trade is far smaller, so 1% sizing works),
(b) raise the cap to 4–5% per trade (not recommended: ten losses in a row would remove most of the account), (c) wait until the
account is bigger. Also: at 1–2% risk and a 1:2 reward, even a good strategy moves a $50 account by cents per trade, and spreads
and commissions are a large share of that. Treat the first months as learning, not income.

## 2. Hard safety rules (enforced in code)
1. Missing, stale or inconsistent price feed, calendar, account data or broker link = no order placed or modified (live/demo).
2. Live only when mode is `live`, the account number in config matches, and the owner types the confirmation at start.
3. Every order has a broker-side stop-loss. Size is computed from the stop distance and the risk cap, rounded down; if the stop can't be placed or the size can't be verified, no trade.
4. Limits are never exceeded; correlated exposure is summed per currency. On a loss limit: stop opening, alert; open trades keep their stops (optionally flattened if `flatten_on_daily_limit` is on).
5. No martingale, grid, averaging down, revenge trading, or widening a stop. Stops only ever move toward profit.
6. News is a VETO only, never a reason to enter. The bot cannot "verify a headline with two sources" by itself, so it never trades on headlines.
7. No invented data: every decision records the data source and timestamp; anything unavailable is logged as unavailable.
8. After any rejected, duplicated or partial order, or a connection drop: no blind retry; reconcile with the broker's positions first, then pause and alert if they disagree.
9. Pause + alert when: execution differs from plan, data fails, a limit is near, or live results diverge from testing.

## 3. The strategy ("SMC-Structure v0.1" — to be backtested before it is allowed to trade)
Built from your materials: market structure/CHOCH, premium-discount and POI, liquidity, candle confirmation (pin bar / engulfing), top-down analysis, 1–2% risk.
1. **Bias (H4, confirmed by D1):** swing highs/lows (a swing = highest/lowest of N=3 bars each side). Bullish = higher highs + higher lows; bearish = the reverse. No clear structure = no trade.
2. **Setup A (continuation, default on):** the last H1 impulse leg in the bias direction; price retraces into the discount half (buys) or premium half (sells) of that leg = the POI zone, ideally at an H4/D1 level or a liquidity sweep (a wick through a prior swing that closes back inside).
3. **Setup B (reversal after CHOCH, off until tested separately):** H1 break of the last lower high (bullish) or higher low (bearish) against the prior trend, then a retrace into the POI.
4. **Trigger (confirmation entry):** an M15 pin bar or engulfing candle in the trade direction inside the POI. Aggressive entry (no confirmation) is off.
5. **Stop:** beyond the leg's origin swing plus 0.25–0.5 × ATR(14, H1); never on or just past a round number (pushed a few pips past it); skip if the stop would be under 10 or over 30 pips (EURUSD scale, scaled by ATR for other pairs).
6. **Take profit:** the next opposite liquidity level, and only if the reward is at least **1:2** (aim 1:3 where structure allows). If 1:2 isn't available the answer is NO TRADE.
7. **Management:** stop to breakeven at +1R; no partial closes while lots are tiny; optional trailing behind H1 swings; close after 48 h open; no new rules mid-trade.
8. **Cost check before entry:** spread ≤ 20% of the stop distance, plus commission, swap and a slippage allowance counted in the risk; unclear risk = no trade.

## 4. Pre-trade record (written to the audit log and shown on the dashboard)
Pair/direction; timestamp and data sources; strategy version and each condition met; entry type/price; stop and target; size and the calculation; account-currency risk and total open risk after entry; spread, cost estimate and event risks; what invalidates the setup. If any condition fails the log says `NO TRADE` and which one.

## 5. Status report (every scan)
Conditions per pair, calendar events in the window, open positions, equity and drawdown, daily/weekly loss used, total open risk, trades taken, paused or not. Every signal, decision, order, fill, modification and error is in the tamper-evident audit log.

## 6. Validation gate before demo→live (proposed — ASK to change)
- Backtest ≥ 2 years of M15/H1/H4 data for each pair, with the last 6 months held out as out-of-sample; ≥ 100 trades; costs (real spread, commission, swap, slippage, missed fills) included; look-ahead checked; report expectancy, win/loss size, max drawdown and longest losing streak.
- Then ≥ 8 weeks and ≥ 30 trades forward on a **demo** account, results within a stated tolerance of the backtest.
- Only then may the owner ask for live, and live still needs the typed confirmation each start. A good backtest is not proof of future profit.
