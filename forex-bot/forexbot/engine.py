"""The trading loop: manage open positions, look for entries on each newly closed bar, enforce every risk rule."""
from __future__ import annotations

import threading
from datetime import datetime, timezone
from typing import Callable, Optional

from . import risk
from .audit import AuditLog
from .config import Config
from .indicators import atr
from .models import Position, TIMEFRAME_SECONDS
from .state import StateStore
from .strategy import Strategy

MAX_CONSECUTIVE_ERRORS = 5


class Engine:
    def __init__(self, cfg: Config, broker, feed, strategy: Strategy, audit: AuditLog, state: StateStore, clock: Optional[Callable[[], datetime]] = None):
        self.cfg, self.broker, self.feed, self.strategy, self.audit, self.state = cfg, broker, feed, strategy, audit, state
        self.clock = clock or (lambda: datetime.now(timezone.utc))
        self.lock = threading.RLock()
        self.errors = 0
        self.last_error: Optional[str] = None
        self.last_decisions: dict[str, str] = {}
        self.infos: dict = {}

    # ------------------------------------------------------------ pause / resume
    @property
    def paused(self) -> bool:
        return bool(self.state.data["paused"])

    def pause(self, reason: str, by: str = "user") -> None:
        with self.lock:
            self.state.set(paused=True, pause_reason=reason)
            self.audit.append("paused", {"reason": reason, "by": by})

    def resume(self) -> None:
        with self.lock:
            self.errors = 0
            self.state.set(paused=False, pause_reason=None)
            self.audit.append("resumed", {})

    # ------------------------------------------------------------ main step
    def step(self) -> None:
        """One pass. Safe to call repeatedly; entries happen at most once per symbol per closed bar."""
        with self.lock:
            try:
                now = self.clock()
                acct = self.broker.account()
                self._roll_day(acct, now)
                positions = self.broker.positions()
                self._manage(positions, now)
                if self._daily_limit_hit(acct) and self.cfg.risk.flatten_on_daily_limit:
                    self._flatten("daily loss limit")
                for sym in self.cfg.symbols:
                    self._look_for_entry(sym, now)
                self.errors = 0
            except Exception as e:  # noqa: BLE001 - never crash the loop; stops live on the broker anyway
                self.errors += 1
                self.last_error = f"{type(e).__name__}: {e}"
                self.audit.append("error", {"error": self.last_error, "consecutive": self.errors})
                if self.errors >= MAX_CONSECUTIVE_ERRORS and not self.paused:
                    self.pause(f"{self.errors} errors in a row (last: {self.last_error})", by="system")

    def _roll_day(self, acct, now) -> None:
        day = now.strftime("%Y-%m-%d")
        if self.state.data.get("day") != day or not self.state.data.get("day_start_equity"):
            self.state.set(day=day, day_start_equity=acct.equity)
            self.audit.append("day_start", {"day": day, "equity": acct.equity})

    def _daily_limit_hit(self, acct) -> bool:
        d0 = self.state.data.get("day_start_equity")
        return bool(d0) and acct.equity <= d0 * (1 - self.cfg.risk.daily_loss_limit_pct / 100)

    def _info(self, sym):
        if sym not in self.infos:
            self.infos[sym] = self.feed.symbol_info(sym)
        return self.infos[sym]

    # ------------------------------------------------------------ entries
    def _look_for_entry(self, sym: str, now: datetime) -> None:
        tf_sec = TIMEFRAME_SECONDS[self.cfg.timeframe]
        bars = self.feed.bars(sym, self.cfg.timeframe, max(self.strategy.min_bars() + 5, 300))
        if len(bars) < self.strategy.min_bars():
            self.last_decisions[sym] = f"waiting for history ({len(bars)}/{self.strategy.min_bars()} bars)"
            return
        last = bars[-1]
        if self.state.data["last_bar"].get(sym) == last.time:
            return  # already evaluated this bar
        self.state.data["last_bar"][sym] = last.time
        self.state.save()
        if now.timestamp() - (last.time + tf_sec) > tf_sec * 2:
            self.last_decisions[sym] = "latest closed bar is old — market closed or feed stale; not trading"
            return
        sig = self.strategy.evaluate(bars)
        if sig is None:
            self.last_decisions[sym] = f"no signal on bar {datetime.fromtimestamp(last.time, timezone.utc):%Y-%m-%d %H:%M}"
            return
        info, tick, acct = self._info(sym), self.feed.tick(sym), self.broker.account()
        lots, notes = risk.size_lots(acct.equity, self.cfg.risk.risk_per_trade_pct, sig.sl_distance, info, self.cfg.risk.max_lots)
        positions = self.broker.positions()
        for p in positions:
            self._info(p.symbol)
        viol = risk.pre_trade_violations(
            self.cfg, paused=self.paused, account=acct, day_start_equity=self.state.data.get("day_start_equity"), positions=positions,
            infos=self.infos, symbol=sym, info=info, tick=tick, signal=sig, lots=lots, now=now, tick_age_check=getattr(self.feed, "live", False),
        )
        if lots <= 0:
            viol += [n for n in notes if "below the broker minimum" in n]
        sig_data = {"symbol": sym, "side": sig.side, "reason": sig.reason, "sl_distance": sig.sl_distance, "tp_distance": sig.tp_distance, "lots": lots}
        if viol:
            self.last_decisions[sym] = f"{sig.side} signal blocked: {viol[0]}"
            self.audit.append("signal_rejected", {**sig_data, "violations": viol})
            return
        price = tick.ask if sig.side == "buy" else tick.bid
        sl = price - sig.sl_distance if sig.side == "buy" else price + sig.sl_distance
        tp = None if sig.tp_distance is None else (price + sig.tp_distance if sig.side == "buy" else price - sig.tp_distance)
        res = self.broker.open(sym, sig.side, lots, round(sl, info.digits), None if tp is None else round(tp, info.digits), "fb " + self.strategy.name)
        if not res.ok:
            self.last_decisions[sym] = f"order failed: {res.error}"
            self.audit.append("order_failed", {**sig_data, "error": res.error})
            return
        self.state.data["meta"][res.position_id] = {"risk_dist": sig.sl_distance, "partial_done": False, "opened_at": int(now.timestamp())}
        self.state.save()
        self.last_decisions[sym] = f"opened {sig.side} {lots} lots @ {res.price}"
        self.audit.append("order_opened", {**sig_data, "position_id": res.position_id, "price": res.price, "sl": sl, "tp": tp, "notes": notes, "mode": self.cfg.mode})

    # ------------------------------------------------------------ management
    def _manage(self, positions: list[Position], now: datetime) -> None:
        for ev in self.broker.update():
            self.audit.append("paper_exit", ev)
            self.state.data["meta"].pop(ev["id"], None)
        positions = self.broker.positions()
        ex = self.cfg.exits
        meta_all = self.state.data["meta"]
        live_ids = {p.id for p in positions}
        for pid in [k for k in meta_all if k not in live_ids]:
            meta_all.pop(pid)  # position closed (stop/target hit on the broker)
        for p in positions:
            info, tick = self._info(p.symbol), self.feed.tick(p.symbol)
            if tick is None:
                continue
            m = meta_all.setdefault(p.id, {"risk_dist": abs(p.entry - p.sl) if p.sl else 0.0, "partial_done": False, "opened_at": p.opened_at})
            px = tick.bid if p.side == "buy" else tick.ask
            gain = (px - p.entry) if p.side == "buy" else (p.entry - px)
            rd = m.get("risk_dist") or 0.0
            r_mult = gain / rd if rd > 0 else 0.0

            if ex.max_hold_hours and now.timestamp() - m.get("opened_at", p.opened_at) > ex.max_hold_hours * 3600:
                self._close(p, None, "time_exit")
                continue
            if ex.partial_close_at_r and not m["partial_done"] and r_mult >= ex.partial_close_at_r:
                part = risk.floor_to_step(p.lots * ex.partial_close_pct / 100, info.volume_step)
                if part >= info.volume_min and p.lots - part >= info.volume_min:
                    if self._close(p, part, "partial_take_profit"):
                        m["partial_done"] = True
                else:
                    m["partial_done"] = True  # too small to split; don't retry every pass
            if ex.breakeven_at_r and r_mult >= ex.breakeven_at_r and p.sl is not None:
                be = p.entry
                better = (p.sl < be) if p.side == "buy" else (p.sl > be)
                if better and ((px > be) if p.side == "buy" else (px < be)):
                    self._modify(p, be, "breakeven")
            if ex.trailing_atr_mult and r_mult > 0:
                bars = self.feed.bars(p.symbol, self.cfg.timeframe, 60)
                a = atr(bars, 14)[-1] if len(bars) > 15 else None
                if a:
                    new = px - ex.trailing_atr_mult * a if p.side == "buy" else px + ex.trailing_atr_mult * a
                    cur = p.sl
                    improves = cur is None or (new > cur if p.side == "buy" else new < cur)
                    safe = (new > p.entry) if p.side == "buy" else (new < p.entry)
                    if improves and safe:
                        self._modify(p, round(new, info.digits), "trailing")
        self.state.save()

    def _close(self, p: Position, lots, why: str) -> bool:
        res = self.broker.close(p.id, lots)
        self.audit.append("position_closed" if res.ok and lots is None else "position_reduced" if res.ok else "close_failed",
                          {"position_id": p.id, "symbol": p.symbol, "lots": lots or p.lots, "why": why, "error": res.error, "price": res.price})
        return res.ok

    def _modify(self, p: Position, sl: float, why: str) -> None:
        res = self.broker.modify(p.id, sl=sl)
        self.audit.append("stop_moved" if res.ok else "modify_failed", {"position_id": p.id, "symbol": p.symbol, "new_sl": sl, "why": why, "error": res.error})

    def _flatten(self, why: str) -> None:
        for p in self.broker.positions():
            self._close(p, None, why)
        if not self.paused:
            self.pause(f"flattened: {why}", by="system")

    # ------------------------------------------------------------ dashboard snapshot
    def snapshot(self) -> dict:
        with self.lock:
            acct = self.broker.account()
            d0 = self.state.data.get("day_start_equity")
            pos = self.broker.positions()
            return {
                "mode": self.cfg.mode, "paused": self.paused, "pause_reason": self.state.data.get("pause_reason"),
                "account": {"login": acct.login, "kind": acct.kind, "balance": acct.balance, "equity": acct.equity, "currency": acct.currency},
                "day_pnl": None if not d0 else acct.equity - d0, "daily_limit_pct": self.cfg.risk.daily_loss_limit_pct,
                "symbols": self.cfg.symbols, "timeframe": self.cfg.timeframe, "strategy": self.cfg.strategy_name,
                "decisions": dict(self.last_decisions), "last_error": self.last_error, "errors_in_a_row": self.errors,
                "positions": [vars(p) for p in pos], "audit": self.audit.tail(30),
            }

