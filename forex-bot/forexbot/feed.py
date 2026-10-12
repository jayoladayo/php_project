"""Price feeds. ReplayFeed replays historical bars (backtests); MT5Feed reads from a MetaTrader 5 terminal."""
from __future__ import annotations

import csv
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional, Sequence

from .models import TIMEFRAME_SECONDS, Bar, SymbolInfo, Tick


def default_symbol_info(name: str) -> SymbolInfo:
    """Rough defaults for a USD account (EURUSD-like, or JPY pair if the name ends in JPY).
    Backtest sizing is approximate; real runs read the broker's exact contract data."""
    jpy = name.upper().endswith("JPY")
    return SymbolInfo(name, 3 if jpy else 5, 0.001 if jpy else 0.00001, 0.001 if jpy else 0.00001, 0.65 if jpy else 1.0, 0.01, 100.0, 0.01)


def _parse_time(t: str) -> int:
    if t.replace(".", "", 1).isdigit():
        return int(float(t))
    if len(t) > 4 and t[4] == ".":  # MT5 export style 2024.01.31 14:00
        t = t.replace(".", "-", 2)
    dt = datetime.fromisoformat(t.replace("Z", "+00:00"))
    return int((dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)).timestamp())


def load_csv(path: str | Path) -> list[Bar]:
    """CSV with header: time,open,high,low,close[,volume]. `time` is epoch seconds or ISO-8601 (UTC)."""
    bars: list[Bar] = []
    with open(path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            row = {k.strip().lower(): v for k, v in row.items()}
            t = row["time"].strip()
            ts = _parse_time(t)
            bars.append(Bar(ts, float(row["open"]), float(row["high"]), float(row["low"]), float(row["close"]), float(row.get("volume") or 0)))
    bars.sort(key=lambda b: b.time)
    return bars


class ReplayFeed:
    live = False

    def __init__(self, data: dict[str, list[Bar]], timeframe: str, spread_points: float = 10, infos: Optional[dict[str, SymbolInfo]] = None):
        self.data, self.tf_sec = data, TIMEFRAME_SECONDS[timeframe]
        self.spread_points = spread_points
        self.infos = {s: (infos or {}).get(s) or default_symbol_info(s) for s in data}
        self.i = -1
        self.length = min(len(v) for v in data.values())

    def advance(self) -> bool:
        if self.i + 1 >= self.length:
            return False
        self.i += 1
        return True

    def now(self) -> datetime:
        sym = next(iter(self.data))
        return datetime.fromtimestamp(self.data[sym][self.i].time + self.tf_sec, tz=timezone.utc)

    def symbol_info(self, symbol: str) -> SymbolInfo:
        return self.infos[symbol]

    def bars(self, symbol: str, timeframe: str, count: int) -> list[Bar]:
        lo = max(0, self.i + 1 - count)
        return self.data[symbol][lo : self.i + 1]

    def tick(self, symbol: str) -> Optional[Tick]:
        if self.i < 0:
            return None
        b = self.data[symbol][self.i]
        return Tick(b.time + self.tf_sec, b.close, b.close + self.spread_points * self.infos[symbol].point)

    def intrabar(self, symbol: str) -> Optional[tuple[float, float, float]]:
        """(open, low, high) of the bar that just formed — used to trigger stops realistically."""
        if self.i < 0:
            return None
        b = self.data[symbol][self.i]
        return b.open, b.low, b.high


class MT5Feed:
    """Read-only market data from a MetaTrader 5 terminal (the `MetaTrader5` Python package, Windows only)."""

    live = True

    def __init__(self, mt5):
        self.mt5 = mt5

    def symbol_info(self, symbol: str) -> SymbolInfo:
        self.mt5.symbol_select(symbol, True)
        i = self.mt5.symbol_info(symbol)
        if i is None:
            raise RuntimeError(f"MT5 does not know symbol '{symbol}' (check the exact name your broker uses, e.g. EURUSD.m)")
        return SymbolInfo(symbol, i.digits, i.point, i.trade_tick_size, i.trade_tick_value, i.volume_min, i.volume_max, i.volume_step, getattr(i, "trade_stops_level", 0))

    def bars(self, symbol: str, timeframe: str, count: int) -> list[Bar]:
        tf = getattr(self.mt5, f"TIMEFRAME_{timeframe}")
        rows = self.mt5.copy_rates_from_pos(symbol, tf, 0, count + 1)
        if rows is None or len(rows) == 0:
            raise RuntimeError(f"no bars returned for {symbol}: {self.mt5.last_error()}")
        out = [Bar(int(r["time"]), float(r["open"]), float(r["high"]), float(r["low"]), float(r["close"]), float(r["tick_volume"])) for r in rows]
        return out[:-1]  # drop the still-forming bar: strategies only ever see closed bars

    def tick(self, symbol: str) -> Optional[Tick]:
        t = self.mt5.symbol_info_tick(symbol)
        return None if t is None else Tick(int(t.time), float(t.bid), float(t.ask))

    def intrabar(self, symbol: str):
        return None
