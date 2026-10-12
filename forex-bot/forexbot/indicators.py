from __future__ import annotations

from typing import Sequence

from .models import Bar


def ema(values: Sequence[float], period: int) -> list[float]:
    if period < 1 or not values:
        return []
    k = 2.0 / (period + 1)
    out = [values[0]]
    for v in values[1:]:
        out.append(v * k + out[-1] * (1 - k))
    return out


def sma(values: Sequence[float], period: int) -> list[float | None]:
    out: list[float | None] = []
    run = 0.0
    for i, v in enumerate(values):
        run += v
        if i >= period:
            run -= values[i - period]
        out.append(run / period if i >= period - 1 else None)
    return out


def atr(bars: Sequence[Bar], period: int = 14) -> list[float | None]:
    """Wilder's ATR. Entries before `period` bars exist are None."""
    out: list[float | None] = [None] * len(bars)
    if len(bars) <= period:
        return out
    trs = []
    for i, b in enumerate(bars):
        if i == 0:
            trs.append(b.high - b.low)
        else:
            pc = bars[i - 1].close
            trs.append(max(b.high - b.low, abs(b.high - pc), abs(b.low - pc)))
    cur = sum(trs[1 : period + 1]) / period
    out[period] = cur
    for i in range(period + 1, len(bars)):
        cur = (cur * (period - 1) + trs[i]) / period
        out[i] = cur
    return out


def rsi(values: Sequence[float], period: int = 14) -> list[float | None]:
    out: list[float | None] = [None] * len(values)
    if len(values) <= period:
        return out
    gains = losses = 0.0
    for i in range(1, period + 1):
        d = values[i] - values[i - 1]
        gains += max(d, 0)
        losses += max(-d, 0)
    ag, al = gains / period, losses / period
    out[period] = 100.0 if al == 0 else 100 - 100 / (1 + ag / al)
    for i in range(period + 1, len(values)):
        d = values[i] - values[i - 1]
        ag = (ag * (period - 1) + max(d, 0)) / period
        al = (al * (period - 1) + max(-d, 0)) / period
        out[i] = 100.0 if al == 0 else 100 - 100 / (1 + ag / al)
    return out
