# Forex bot for MetaTrader 5

Automatic trading through your own MetaTrader 5 terminal. **Paper mode is the default**: it uses real prices but
simulates the trades, so nothing touches your account until you move up a level yourself.

| Mode | Prices | Trades | Needs |
|---|---|---|---|
| `backtest` | CSV file | simulated | nothing (works anywhere) |
| `paper` | live from MT5 | simulated | MT5 open on your PC |
| `demo` | live from MT5 | real orders on a **demo** account | MT5 logged into a demo account |
| `live` | live from MT5 | **real money** | explicit approval, see below |

## Safety rules built in
- Every trade is sized so that hitting its stop-loss costs at most `risk_per_trade_pct` of equity (rounded **down**; if the broker's minimum lot would risk more, the trade is skipped).
- Every order carries a stop-loss that lives on the broker's server, so closing the bot or losing internet never leaves an unprotected trade. The bot refuses to open a trade without one.
- Limits: max open trades, total open risk, daily loss limit (blocks new trades, optionally closes everything), max spread, stale-price check, trading hours.
- The bot only manages trades carrying its own magic number; your manual trades are never touched.
- Every order is pre-checked and the broker's reply is verified. A refused order is logged as failed, never as filled.
- 5 errors in a row pause the bot. The pause flag and the daily-loss baseline survive restarts.
- Emergency pause button on the dashboard. Tamper-evident audit log (`python -m forexbot verify-audit`). Secrets are redacted from it.
- No passwords in code or config. If you must pass a login, the password is read from the `MT5_PASSWORD` environment variable only. Without a login the bot uses the account already logged into the terminal.
- `demo` mode refuses to start on a real account. `live` mode needs: a real account, your account number written in `config.json` (`live.account_login`), and you typing `TRADE REAL MONEY` at each start.

## Setup (Windows)
1. Install Python 3.11+ and MetaTrader 5; log MT5 into a **demo** account. In MT5: Tools → Options → Expert Advisors → tick "Allow algorithmic trading".
2. Double-click `check-mt5.cmd`: it connects, prints the account type and the symbols' contract data. No orders are sent.
3. Edit `config.json` (created from `config.example.json` on first run): your symbols (exact names as your broker shows them, e.g. `EURUSD` or `EURUSD.m`), timeframe, risk.
4. Double-click `start-paper.cmd`, open http://127.0.0.1:8800.
5. Try the backtest tooling: `python -m forexbot backtest --csv EURUSD_H1.csv` (columns `time,open,high,low,close[,volume]`; export from MT5 via History Center) or `--sample` for synthetic data.
6. Let paper mode run for a couple of weeks, compare with the backtest, then `start-demo.cmd` for a demo account. Only after that consider live.

## The strategy
The built-in `ema_trend` strategy is a **generic placeholder** so the whole pipeline can run. It has no proven edge.
Put your pairs, timeframe and rules in `strategy_notes.md`; they get coded as a strategy class in `forexbot/strategy.py`
(a strategy only proposes a trade; sizing, risk checks and execution are shared and cannot be bypassed).

## Exits (config `exits`)
Breakeven after N×R profit, partial close at N×R, ATR trailing stop, max holding time, plus the fixed stop and target on each trade.

## Honest limits
- Backtests are approximate (fixed spread, no swaps or slippage; if stop and target fall in one bar, the stop is assumed to hit first). Paper mode checks stops on each poll, not on every tick, so it can miss brief wicks.
- Forex is leveraged and most retail traders lose money. Nothing here guarantees profit; the risk limits reduce, not remove, the chance of loss.
- The MT5 order code could not be run against a real terminal in the build environment; it is tested against a fake MT5 module. That is why demo comes first.

## Tests
`pip install -r requirements-dev.txt` then `python -m pytest`.
