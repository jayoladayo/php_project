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
