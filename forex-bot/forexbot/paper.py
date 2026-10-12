"""Simulated broker: fills against a price feed, charges spread + commission, never touches real money."""
from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Optional

from .models import Account, OrderResult, Position, Side
from .risk import floor_to_step


def _pnl(side: Side, entry: float, exit_: float, lots: float, info) -> float:
    d = (exit_ - entry) if side == "buy" else (entry - exit_)
    return d / info.tick_size * info.tick_value * lots


class PaperBroker:
    kind = "paper"

    def __init__(self, feed, starting_balance=10000.0, commission_per_lot=7.0, currency="USD", path: Optional[str | Path] = None, clock=None):
        self.feed, self.commission, self.currency = feed, commission_per_lot, currency
        self.balance, self._next, self.positions_, self.closed = float(starting_balance), 1, {}, []
        self.path = Path(path) if path else None
        self.clock = clock or (lambda: __import__("time").time())
        if self.path and self.path.exists():
            d = json.loads(self.path.read_text(encoding="utf-8"))
            self.balance, self._next, self.closed = d["balance"], d["next"], d["closed"]
            self.positions_ = {k: Position(**v) for k, v in d["positions"].items()}

    # ------------------------------------------------------------ persistence
    def _save(self) -> None:
        if not self.path:
            return
        self.path.parent.mkdir(parents=True, exist_ok=True)
        tmp = self.path.with_suffix(".tmp")
        tmp.write_text(json.dumps({"balance": self.balance, "next": self._next, "closed": self.closed[-500:], "positions": {k: vars(v) for k, v in self.positions_.items()}}), encoding="utf-8")
        os.replace(tmp, self.path)

    # ------------------------------------------------------------ read
    def _float(self, p: Position) -> float:
        t = self.feed.tick(p.symbol)
        if t is None:
            return 0.0
        return _pnl(p.side, p.entry, t.bid if p.side == "buy" else t.ask, p.lots, self.feed.symbol_info(p.symbol))

    def account(self) -> Account:
        return Account(0, self.balance, self.balance + sum(self._float(p) for p in self.positions_.values()), self.currency, "paper")

    def positions(self) -> list[Position]:
        for p in self.positions_.values():
            p.profit = self._float(p)
        return list(self.positions_.values())

    # ------------------------------------------------------------ orders
    def open(self, symbol: str, side: Side, lots: float, sl: Optional[float], tp: Optional[float], comment: str = "") -> OrderResult:
        info, t = self.feed.symbol_info(symbol), self.feed.tick(symbol)
        if t is None:
            return OrderResult(False, error="no price")
        if lots < info.volume_min or lots > info.volume_max or abs(lots - floor_to_step(lots, info.volume_step)) > 1e-9:
            return OrderResult(False, error=f"invalid volume {lots}")
        price = t.ask if side == "buy" else t.bid
        pid = str(self._next)
        self._next += 1
        self.balance -= self.commission * lots
        self.positions_[pid] = Position(pid, symbol, side, lots, price, sl, tp, int(self.clock()), comment)
        self._save()
        return OrderResult(True, pid, price)

    def close(self, position_id: str, lots: Optional[float] = None, price: Optional[float] = None) -> OrderResult:
        p = self.positions_.get(position_id)
        if p is None:
            return OrderResult(False, error="no such position")
        info, t = self.feed.symbol_info(p.symbol), self.feed.tick(p.symbol)
        if price is None:
            if t is None:
                return OrderResult(False, error="no price")
            price = t.bid if p.side == "buy" else t.ask
        lots = p.lots if lots is None or lots >= p.lots - 1e-12 else lots
        pnl = _pnl(p.side, p.entry, price, lots, info)
        self.balance += pnl
        self.closed.append({"symbol": p.symbol, "side": p.side, "lots": lots, "entry": p.entry, "exit": price, "pnl": pnl, "closed_at": int(self.clock())})
        if lots >= p.lots - 1e-12:
            del self.positions_[position_id]
        else:
            p.lots = round(p.lots - lots, 8)
        self._save()
        return OrderResult(True, position_id, price, detail={"pnl": pnl})

    def modify(self, position_id: str, sl: Optional[float] = None, tp: Optional[float] = None) -> OrderResult:
        p = self.positions_.get(position_id)
        if p is None:
            return OrderResult(False, error="no such position")
        if sl is not None:
            p.sl = sl
        if tp is not None:
            p.tp = tp
        self._save()
        return OrderResult(True, position_id)

    # ------------------------------------------------------------ stops
    def update(self) -> list[dict]:
        """Trigger stop-loss / take-profit. If both could have been hit in one bar, the stop wins (conservative)."""
        events = []
        for p in list(self.positions_.values()):
            t = self.feed.tick(p.symbol)
            if t is None:
                continue
            ib = self.feed.intrabar(p.symbol)
            spread = t.ask - t.bid
            o, lo, hi = ib if ib else (t.bid, t.bid, t.bid)  # extremes in BID terms
            if p.side == "buy":
                sl_hit = p.sl is not None and lo <= p.sl
                tp_hit = p.tp is not None and hi >= p.tp
                sl_px = min(p.sl, o) if sl_hit else None  # gap through the stop fills at the open
                tp_px = max(p.tp, o) if tp_hit else None
            else:
                sl_hit = p.sl is not None and hi + spread >= p.sl
                tp_hit = p.tp is not None and lo + spread <= p.tp
                sl_px = max(p.sl, o + spread) if sl_hit else None
                tp_px = min(p.tp, o + spread) if tp_hit else None
            if sl_hit:
                r = self.close(p.id, price=sl_px)
                events.append({"id": p.id, "symbol": p.symbol, "exit": "stop_loss", "price": sl_px, "pnl": r.detail.get("pnl")})
            elif tp_hit:
                r = self.close(p.id, price=tp_px)
                events.append({"id": p.id, "symbol": p.symbol, "exit": "take_profit", "price": tp_px, "pnl": r.detail.get("pnl")})
        return events
