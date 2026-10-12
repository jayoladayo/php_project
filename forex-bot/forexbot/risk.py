from __future__ import annotations

import math
from datetime import datetime, timezone
from typing import Optional

from .config import Config, Sessions
from .models import Account, Position, Signal, SymbolInfo, Tick


def floor_to_step(x: float, step: float) -> float:
    return math.floor(x / step + 1e-9) * step


def money_per_lot(distance: float, info: SymbolInfo) -> float:
    """Account-currency value of a `distance` price move on one lot."""
    return distance / info.tick_size * info.tick_value


def size_lots(equity: float, risk_pct: float, sl_distance: float, info: SymbolInfo, max_lots: float) -> tuple[float, list[str]]:
    """Largest lot size (rounded DOWN) whose stop-loss loss is <= risk_pct of equity. Never rounds up."""
    notes: list[str] = []
    per_lot = money_per_lot(sl_distance, info)
    if per_lot <= 0:
        return 0.0, ["cannot size: zero stop distance or tick value"]
    lots = (equity * risk_pct / 100.0) / per_lot
    cap = min(max_lots, info.volume_max)
    if lots > cap:
        notes.append(f"size capped at {cap} lots")
        lots = cap
    lots = floor_to_step(lots, info.volume_step)
    if lots < info.volume_min - 1e-12:
        return 0.0, [f"risk budget gives {lots:.4f} lots, below the broker minimum {info.volume_min} — not rounding up past your risk limit"]
    return round(lots, 8), notes


def in_session(s: Sessions, now: datetime) -> bool:
    if not s.enabled:
        return True
    if now.weekday() not in s.weekdays:
        return False
    hm = now.strftime("%H:%M")
    a, b = s.utc_start, s.utc_end
    return (a <= hm < b) if a <= b else (hm >= a or hm < b)


def open_risk_pct(positions: list[Position], infos: dict[str, SymbolInfo], equity: float) -> float:
    """Money at risk if every stop is hit now (positions without a stop count as unbounded)."""
    total = 0.0
    for p in positions:
        info = infos.get(p.symbol)
        if info is None or p.sl is None:
            return float("inf")
        d = (p.entry - p.sl) if p.side == "buy" else (p.sl - p.entry)
        total += max(d, 0.0) / info.tick_size * info.tick_value * p.lots  # stop at/after breakeven risks nothing
    return total / equity * 100.0 if equity > 0 else float("inf")


def pre_trade_violations(
    cfg: Config, *, paused: bool, account: Account, day_start_equity: Optional[float], positions: list[Position],
    infos: dict[str, SymbolInfo], symbol: str, info: SymbolInfo, tick: Optional[Tick], signal: Signal,
    lots: float, now: datetime, tick_age_check: bool = True,
) -> list[str]:
    r, v = cfg.risk, []
    if paused:
        v.append("bot is paused")
    if tick is None:
        v.append("no price data for the symbol")
    else:
        spread_pts = tick.spread / info.point
        if spread_pts > r.max_spread_points:
            v.append(f"spread {spread_pts:.1f} points > limit {r.max_spread_points}")
        if tick_age_check and r.max_tick_age_sec and now.timestamp() - tick.time > r.max_tick_age_sec:
            v.append(f"price data is stale ({int(now.timestamp() - tick.time)} s old) — market closed or feed down")
    if not in_session(cfg.sessions, now):
        v.append("outside the allowed trading session")
    if len(positions) >= r.max_open_positions:
        v.append(f"max open positions reached ({r.max_open_positions})")
    if r.one_position_per_symbol and any(p.symbol == symbol for p in positions):
        v.append(f"already holding {symbol}")
    if day_start_equity and account.equity <= day_start_equity * (1 - r.daily_loss_limit_pct / 100.0):
        v.append(f"daily loss limit reached ({r.daily_loss_limit_pct}% of the day's starting equity)")
    if signal.sl_distance <= 0:
        v.append("signal has no valid stop-loss")
    elif info.stops_level_points and signal.sl_distance / info.point < info.stops_level_points:
        v.append(f"stop-loss closer than the broker minimum ({info.stops_level_points} points)")
    if signal.tp_distance is not None and signal.sl_distance > 0 and signal.tp_distance / signal.sl_distance < r.min_rr:
        v.append(f"reward:risk below your minimum {r.min_rr}")
    if lots <= 0:
        v.append("position size is zero")
    else:
        new_risk = money_per_lot(signal.sl_distance, info) * lots / account.equity * 100.0 if account.equity > 0 else 0
        cur = open_risk_pct(positions, infos, account.equity)
        if cur + new_risk > r.max_total_risk_pct + 1e-9:
            v.append(f"total open risk would be {cur + new_risk:.2f}% > limit {r.max_total_risk_pct}%")
    return v
