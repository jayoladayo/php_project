from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from .models import TIMEFRAME_SECONDS

MODES = ("paper", "demo", "live")


class ConfigError(ValueError):
    pass


@dataclass
class Risk:
    risk_per_trade_pct: float = 0.5
    max_open_positions: int = 3
    max_total_risk_pct: float = 2.0
    daily_loss_limit_pct: float = 2.0
    flatten_on_daily_limit: bool = False
    max_lots: float = 1.0
    max_spread_points: float = 30
    max_tick_age_sec: int = 120
    min_rr: float = 0.0
    one_position_per_symbol: bool = True


@dataclass
class Exits:
    breakeven_at_r: Optional[float] = 1.0
    partial_close_at_r: Optional[float] = None
    partial_close_pct: float = 50
    trailing_atr_mult: Optional[float] = None
    max_hold_hours: Optional[float] = None


@dataclass
class Sessions:
    enabled: bool = False
    utc_start: str = "07:00"
    utc_end: str = "20:00"
    weekdays: list[int] = field(default_factory=lambda: [0, 1, 2, 3, 4])  # Mon=0


@dataclass
class Paper:
    starting_balance: float = 10000.0
    commission_per_lot: float = 7.0
    currency: str = "USD"
    spread_points: float = 10  # used only when replaying a CSV (no live spread)


@dataclass
class MT5:
    terminal_path: Optional[str] = None
    login: Optional[int] = None  # optional: omit to use the account already logged in to the terminal
    server: Optional[str] = None
    magic: int = 26001
    deviation_points: int = 20


@dataclass
class LiveGate:
    account_login: Optional[int] = None  # real-money trading is refused unless this matches the connected account


@dataclass
class Config:
    mode: str = "paper"
    symbols: list[str] = field(default_factory=lambda: ["EURUSD"])
    timeframe: str = "H1"
    strategy_name: str = "ema_trend"
    strategy_params: dict = field(default_factory=dict)
    risk: Risk = field(default_factory=Risk)
    exits: Exits = field(default_factory=Exits)
    sessions: Sessions = field(default_factory=Sessions)
    paper: Paper = field(default_factory=Paper)
    mt5: MT5 = field(default_factory=MT5)
    live: LiveGate = field(default_factory=LiveGate)
    poll_seconds: int = 10
    dashboard_port: int = 8800
    data_dir: str = "data"

    def validate(self) -> "Config":
        if self.mode not in MODES:
            raise ConfigError(f"mode must be one of {MODES}")
        if self.timeframe not in TIMEFRAME_SECONDS:
            raise ConfigError(f"timeframe must be one of {list(TIMEFRAME_SECONDS)}")
        if not self.symbols:
            raise ConfigError("symbols must not be empty")
        r = self.risk
        if not 0 < r.risk_per_trade_pct <= 5:
            raise ConfigError("risk.risk_per_trade_pct must be between 0 and 5")
        if not 0 < r.daily_loss_limit_pct <= 20:
            raise ConfigError("risk.daily_loss_limit_pct must be between 0 and 20")
        if r.max_total_risk_pct < r.risk_per_trade_pct:
            raise ConfigError("risk.max_total_risk_pct must be >= risk_per_trade_pct")
        if r.max_open_positions < 1 or r.max_lots <= 0:
            raise ConfigError("risk.max_open_positions >= 1 and max_lots > 0 required")
        if not 0 < self.exits.partial_close_pct < 100:
            raise ConfigError("exits.partial_close_pct must be between 0 and 100")
        if self.poll_seconds < 1:
            raise ConfigError("poll_seconds must be >= 1")
        return self


def _fill(cls, data: Optional[dict]):
    data = data or {}
    unknown = set(data) - set(cls.__dataclass_fields__)
    if unknown:
        raise ConfigError(f"Unknown {cls.__name__} setting(s): {', '.join(sorted(unknown))}")
    return cls(**data)


def from_dict(d: dict) -> Config:
    d = dict(d)
    strat = d.pop("strategy", {}) or {}
    cfg = Config(
        mode=d.pop("mode", "paper"),
        symbols=d.pop("symbols", ["EURUSD"]),
        timeframe=d.pop("timeframe", "H1"),
        strategy_name=strat.get("name", "ema_trend"),
        strategy_params=strat.get("params", {}),
        risk=_fill(Risk, d.pop("risk", None)),
        exits=_fill(Exits, d.pop("exits", None)),
        sessions=_fill(Sessions, d.pop("sessions", None)),
        paper=_fill(Paper, d.pop("paper", None)),
        mt5=_fill(MT5, d.pop("mt5", None)),
        live=_fill(LiveGate, d.pop("live", None)),
        poll_seconds=d.pop("poll_seconds", 10),
        dashboard_port=d.pop("dashboard_port", 8800),
        data_dir=d.pop("data_dir", "data"),
    )
    if d:
        raise ConfigError(f"Unknown setting(s): {', '.join(sorted(d))}")
    return cfg.validate()


def load(path: str | Path) -> Config:
    p = Path(path)
    if not p.exists():
        raise ConfigError(f"Config file not found: {p}. Copy config.example.json to config.json first.")
    try:
        return from_dict(json.loads(p.read_text(encoding="utf-8")))
    except json.JSONDecodeError as e:
        raise ConfigError(f"{p} is not valid JSON: {e.msg} (line {e.lineno})") from None
