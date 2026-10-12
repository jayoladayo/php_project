"""Strategies. A strategy only *proposes* a trade (Signal); sizing, risk checks and
execution are done elsewhere. Add your own by subclassing Strategy and registering it.

The default `ema_trend` is a generic placeholder so the bot can run end-to-end. It is
NOT a proven edge: backtest and paper-trade any strategy before trusting it.
"""
from __future__ import annotations

from typing import Optional, Sequence

from .indicators import atr, ema
from .models import Bar, Signal


class Strategy:
    name = "base"

    def __init__(self, **params):
        self.params = params

    def min_bars(self) -> int:
        return 50

    def evaluate(self, bars: Sequence[Bar]) -> Optional[Signal]:  # bars: closed only, oldest first
        raise NotImplementedError


class EmaTrend(Strategy):
    """Fast/slow EMA cross in the direction of a slow trend EMA; ATR stop; fixed R:R target."""

    name = "ema_trend"

    def __init__(self, fast=20, slow=50, trend=200, atr_period=14, atr_mult=2.0, rr=2.0, allow_short=True, **extra):
        super().__init__(fast=fast, slow=slow, trend=trend, atr_period=atr_period, atr_mult=atr_mult, rr=rr, allow_short=allow_short, **extra)
        if not (0 < fast < slow < trend):
            raise ValueError("ema_trend needs 0 < fast < slow < trend")
        self.fast, self.slow, self.trend = fast, slow, trend
        self.atr_period, self.atr_mult, self.rr, self.allow_short = atr_period, atr_mult, rr, allow_short

    def min_bars(self) -> int:
        return self.trend + self.atr_period + 5

    def evaluate(self, bars: Sequence[Bar]) -> Optional[Signal]:
        if len(bars) < self.min_bars():
            return None
        closes = [b.close for b in bars]
        f, s, t = ema(closes, self.fast), ema(closes, self.slow), ema(closes, self.trend)
        a = atr(bars, self.atr_period)[-1]
        if not a or a <= 0:
            return None
        crossed_up = f[-2] <= s[-2] and f[-1] > s[-1]
        crossed_dn = f[-2] >= s[-2] and f[-1] < s[-1]
        dist = a * self.atr_mult
        if crossed_up and closes[-1] > t[-1]:
            return Signal("buy", dist, dist * self.rr, f"EMA{self.fast} crossed above EMA{self.slow}, price above EMA{self.trend}, ATR stop {self.atr_mult}x")
        if self.allow_short and crossed_dn and closes[-1] < t[-1]:
            return Signal("sell", dist, dist * self.rr, f"EMA{self.fast} crossed below EMA{self.slow}, price below EMA{self.trend}, ATR stop {self.atr_mult}x")
        return None


REGISTRY: dict[str, type[Strategy]] = {EmaTrend.name: EmaTrend}


def build(name: str, params: dict) -> Strategy:
    if name not in REGISTRY:
        raise ValueError(f"Unknown strategy '{name}'. Available: {', '.join(REGISTRY)}")
    return REGISTRY[name](**params)
