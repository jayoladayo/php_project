from __future__ import annotations

from dataclasses import dataclass, field
from typing import Literal, Optional

Side = Literal["buy", "sell"]

TIMEFRAME_SECONDS = {"M1": 60, "M5": 300, "M15": 900, "M30": 1800, "H1": 3600, "H4": 14400, "D1": 86400}


@dataclass(frozen=True)
class Bar:
    time: int  # open time, epoch seconds (UTC)
    open: float
    high: float
    low: float
    close: float
    volume: float = 0.0


@dataclass(frozen=True)
class Tick:
    time: int
    bid: float
    ask: float

    @property
    def spread(self) -> float:
        return self.ask - self.bid


@dataclass(frozen=True)
class SymbolInfo:
    name: str
    digits: int
    point: float
    tick_size: float
    tick_value: float  # account-currency profit of one tick_size move on 1 lot
    volume_min: float
    volume_max: float
    volume_step: float
    stops_level_points: int = 0


@dataclass(frozen=True)
class Account:
    login: int
    balance: float
    equity: float
    currency: str
    kind: str  # "demo" | "real" | "paper"


@dataclass
class Position:
    id: str
    symbol: str
    side: Side
    lots: float
    entry: float
    sl: Optional[float]
    tp: Optional[float]
    opened_at: int
    comment: str = ""
    profit: float = 0.0  # floating, account currency


@dataclass
class OrderResult:
    ok: bool
    position_id: Optional[str] = None
    price: Optional[float] = None
    error: Optional[str] = None
    detail: dict = field(default_factory=dict)


@dataclass
class Signal:
    side: Side
    sl_distance: float  # price distance, > 0
    tp_distance: Optional[float]
    reason: str
