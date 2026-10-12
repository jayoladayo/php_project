# Digest of training batch 1 (my reading, to be confirmed by the owner)

Source files (kept out of the repo): SteveFx slides on entry types & risk, market structure & CHOCH, premium/discount/POI/liquidity,
lot size, spread/pips/leverage; "The Candlestick Trading Bible" (KohanFx); CFI "Complete Guide to Trading" (general background).

## Market structure
- Bullish: higher highs + higher lows. Bearish: lower highs + lower lows. Range: sideways.
- CHOCH (change of character): bullish when price breaks a lower high in a downtrend; bearish when price breaks a higher low in an uptrend.

## Premium / discount / POI
- Measure the last leg (swing low -> swing high, or reverse). 50% = equilibrium. Above = premium (sell zone), below = discount (buy zone).
- Bullish POI: the higher low (start of the up-leg) up to the high; price must retrace into the discount half (<= 50%) before a buy.
- Bearish POI: mirror image; price must retrace into the premium half (>= 50%) before a sell.

## Liquidity
- Buy-side liquidity = stops above highs / lower highs / resistance. Sell-side = stops below lows / higher lows / support.
- Imbalances / fair value gaps tend to be revisited. Avoid obvious stop placement (stop hunts).

## Entries
- Aggressive: enter at first sign of the setup (can give the best R:R, more false signals).
- Confirmation: wait for a lower-timeframe candle/structure confirmation (fewer false entries, may miss the trade).
- Pin bar (book): long tail, small body, at a key level, in the trend direction, on 1H/4H/D1; aggressive entry at the close, stop beyond the tail,
  target = next key level; conservative entry = 50% retrace of the pin bar range. Also engulfing and inside-bar setups; 21 MA as dynamic support/resistance.
- Top-down: check the higher timeframe (1H -> D1; 4H -> W1/D1) for trend and key levels first; avoid trading 5-minute noise.

## Risk
- Risk 1-2% per trade (slides). Minimum R:R 1:2 (one slide says 1:3 in its summary). Always use a stop; never move it to avoid a loss.
- Lot sizes: 1.0 = 100k units ($10/pip), 0.1 = $1/pip, 0.01 = $0.10/pip. Pip = 0.0001 (0.01 for JPY). Spread widens at news/low liquidity.
- High leverage amplifies losses (the slides suggest 1:10-1:50 for beginners).

## Gaps that must be pinned down before coding (questions for the owner)
- Exact swing-high/low definition (e.g. N bars each side), and which timeframe(s) for the trend vs. the entry.
- Is a trade "CHOCH + retrace into POI + candle confirmation", or something else? What is the stop (beyond the POI/swing?) and the target (next liquidity?).

---
# Batch 2: "ProFx 4.0" (Forex21) — only the user manual was read

Not used, on purpose: the .ex4 expert advisor, the 7 .ex4 indicators, ProFx32.dll and the chart template. They are compiled,
licence-locked commercial binaries (the licence needs per-account registration with the vendor) shipped from a third-party
download site. I do not run, decompile or copy them, and the owner should not install the DLL/EX4 files on a funded account.
The proprietary indicators (Probability Cloud, Strength Oscillator, Multi Analyzes) cannot be reproduced from the manual.

## Rules in the manual that a bot can implement with standard, public indicators
Entry (all must agree): signal arrow; probability-cloud filter; signal bar opened beyond the custom MA; momentum increasing;
higher-timeframe (H4 or D1) agrees; prefer EU/US sessions; ignore tight-range/choppy signals.
Exit: stay in while indicators agree; close when the strength lines cross AND Heiken Ashi colour flips; close immediately on a
high-impact news spike; optional trailing stop and fixed TP.
Timeframes: M30 for entries with H4 for trend confirmation; EURUSD/GBPUSD first.
General: max 1-2 trades per day; no trading on holidays / around new year; keep a trade log; tighten stops before major news;
trade with the trend; stop = last swing high/low with a minimum distance; risk 1% per trade (percent-of-equity sizing).
Components named: Heiken Ashi, a moving average, a stochastic (Lane), a strength oscillator, a probability cloud, a multi-indicator.

## Notes
- It is an MetaTrader 4 expert advisor; our bot talks to MetaTrader 5.
- The manual's claims of "constant profits" are marketing, not evidence; any ProFx-style rules must be backtested like any other.

---
# Batch 3: reading list (30 free research papers) + download script

Read in full: `forex_reading_list.md` and `download_forex_materials.sh`. The script only curls PDFs from bis.org, nber.org, federalreserve.gov,
fraser.stlouisfed.org, aqr.com and similar into ./forex_materials. I did NOT run it (the sandbox blocked running a user-supplied script) and
none of the PDFs were available to me, so nothing below is quoted from the papers: it is my prior knowledge of what these well-known papers
conclude, to be replaced by the real text if the PDFs are uploaded.

## Takeaways relevant to the bot (from prior knowledge, unverified against the PDFs)
- Technical rules (Neely & Weller; Menkhoff & Taylor): FX technical rules earned excess returns in older data, much smaller or gone in
  recent decades, and costs matter a lot -> backtest with spread + slippage, hold out recent data, avoid over-fitting parameters.
- Order clustering (Osler): stops and orders cluster at round numbers and just beyond obvious highs/lows; breaches of those cause cascades.
  Consistent with the batch-1 liquidity/stop-hunt material -> do not put stops on round numbers; prefer an ATR buffer beyond swing points.
- Trend / momentum (Hurst-Ooi-Pedersen; Menkhoff et al.): trend following has long evidence across assets, but with long drawdowns; currency
  momentum profits are concentrated where costs are high -> volatility-scaled sizing, realistic costs, expect losing streaks.
- Carry (Brunnermeier et al.; Menkhoff et al.): carry earns a premium but crashes when volatility spikes; not an intraday-bot idea.
- News (Andersen et al.): macro announcements cause jumps and spread blow-outs -> news blackout window + spread filter (already have the spread filter).
- Execution / conduct (BIS algos; FX Global Code): last look, slippage and wide spreads in thin liquidity -> avoid rollover hours and low liquidity.
- Positioning (CFTC COT): weekly, lagging data; possible slow bias filter, not an entry signal.
- Deep-learning review (Sezer et al.): reported accuracies are often inflated by leakage -> no ML until a clean walk-forward test exists.
