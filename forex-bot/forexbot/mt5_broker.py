"""MetaTrader 5 broker. Requires the `MetaTrader5` package (Windows) and a running, logged-in MT5 terminal.

Safety rules baked in:
 - only positions carrying THIS bot's magic number are ever managed (your manual trades are never touched);
 - every entry carries a stop-loss that lives on the broker's server, so a crash or lost connection cannot leave
   an unprotected position;
 - every order is pre-checked with order_check and the broker's return code is verified (no assumed fills);
 - credentials: prefer the account already logged in to the terminal. If you must pass a login, the password comes
   from the MT5_PASSWORD environment variable only and is never logged.
"""
from __future__ import annotations

import os
from typing import Optional

from .config import MT5 as MT5Config
from .feed import MT5Feed
from .models import Account, OrderResult, Position, Side

DONE = (10009,)  # TRADE_RETCODE_DONE (10008 = placed; market orders should be DONE)


class MT5Broker(MT5Feed):
    def __init__(self, mt5, cfg: MT5Config):
        super().__init__(mt5)
        self.cfg = cfg

    # ------------------------------------------------------------ connection
    def connect(self) -> Account:
        kw = {}
        if self.cfg.terminal_path:
            kw["path"] = self.cfg.terminal_path
        if self.cfg.login:
            pw = os.environ.get("MT5_PASSWORD")
            if not pw:
                raise RuntimeError("mt5.login is set but the MT5_PASSWORD environment variable is not. Set it (never put it in the config), or remove login to use the account already logged in to the terminal.")
            kw.update(login=int(self.cfg.login), password=pw, server=self.cfg.server)
        if not self.mt5.initialize(**kw):
            raise RuntimeError(f"Could not connect to the MetaTrader 5 terminal: {self.mt5.last_error()}. Is MT5 open and logged in?")
        return self.account()

    def shutdown(self) -> None:
        self.mt5.shutdown()

    def account(self) -> Account:
        a = self.mt5.account_info()
        if a is None:
            raise RuntimeError(f"account_info failed: {self.mt5.last_error()}")
        mode = a.trade_mode
        kind = "demo" if mode == getattr(self.mt5, "ACCOUNT_TRADE_MODE_DEMO", 0) else "real" if mode == getattr(self.mt5, "ACCOUNT_TRADE_MODE_REAL", 2) else "contest"
        return Account(int(a.login), float(a.balance), float(a.equity), a.currency, kind)

    # ------------------------------------------------------------ positions
    def positions(self) -> list[Position]:
        rows = self.mt5.positions_get() or []
        out = []
        for r in rows:
            if r.magic != self.cfg.magic:
                continue
            side: Side = "buy" if r.type == getattr(self.mt5, "POSITION_TYPE_BUY", 0) else "sell"
            out.append(Position(str(r.ticket), r.symbol, side, float(r.volume), float(r.price_open), float(r.sl) or None, float(r.tp) or None, int(r.time), r.comment, float(r.profit)))
        return out

    # ------------------------------------------------------------ orders
    def _filling(self, symbol: str):
        mode = self.mt5.symbol_info(symbol).filling_mode
        if mode & 2:
            return self.mt5.ORDER_FILLING_IOC
        if mode & 1:
            return self.mt5.ORDER_FILLING_FOK
        return self.mt5.ORDER_FILLING_RETURN

    def _send(self, req: dict) -> OrderResult:
        chk = self.mt5.order_check(req)
        if chk is None or chk.retcode not in (0, *DONE):
            return OrderResult(False, error=f"order_check rejected: {getattr(chk, 'retcode', None)} {getattr(chk, 'comment', self.mt5.last_error())}")
        res = self.mt5.order_send(req)
        if res is None:
            return OrderResult(False, error=f"order_send returned nothing: {self.mt5.last_error()}")
        if res.retcode not in DONE:
            return OrderResult(False, error=f"broker refused: {res.retcode} {res.comment}", detail={"retcode": res.retcode})
        return OrderResult(True, str(res.order), float(res.price), detail={"retcode": res.retcode, "deal": getattr(res, "deal", None)})

    def open(self, symbol: str, side: Side, lots: float, sl: Optional[float], tp: Optional[float], comment: str = "") -> OrderResult:
        if sl is None:
            return OrderResult(False, error="refusing to open a position without a stop-loss")
        t = self.tick(symbol)
        if t is None:
            return OrderResult(False, error="no price")
        digits = self.symbol_info(symbol).digits
        buy = side == "buy"
        req = {
            "action": self.mt5.TRADE_ACTION_DEAL, "symbol": symbol, "volume": lots,
            "type": self.mt5.ORDER_TYPE_BUY if buy else self.mt5.ORDER_TYPE_SELL,
            "price": t.ask if buy else t.bid, "sl": round(sl, digits), "tp": round(tp, digits) if tp else 0.0,
            "deviation": self.cfg.deviation_points, "magic": self.cfg.magic, "comment": comment[:30],
            "type_time": self.mt5.ORDER_TIME_GTC, "type_filling": self._filling(symbol),
        }
        return self._send(req)

    def close(self, position_id: str, lots: Optional[float] = None) -> OrderResult:
        pos = next((p for p in self.positions() if p.id == position_id), None)
        if pos is None:
            return OrderResult(False, error="no such position (already closed?)")
        t = self.tick(pos.symbol)
        if t is None:
            return OrderResult(False, error="no price")
        buy_close = pos.side == "sell"
        req = {
            "action": self.mt5.TRADE_ACTION_DEAL, "symbol": pos.symbol, "volume": lots or pos.lots, "position": int(position_id),
            "type": self.mt5.ORDER_TYPE_BUY if buy_close else self.mt5.ORDER_TYPE_SELL,
            "price": t.ask if buy_close else t.bid, "deviation": self.cfg.deviation_points, "magic": self.cfg.magic,
            "comment": "fb close", "type_time": self.mt5.ORDER_TIME_GTC, "type_filling": self._filling(pos.symbol),
        }
        return self._send(req)

    def modify(self, position_id: str, sl: Optional[float] = None, tp: Optional[float] = None) -> OrderResult:
        pos = next((p for p in self.positions() if p.id == position_id), None)
        if pos is None:
            return OrderResult(False, error="no such position")
        digits = self.symbol_info(pos.symbol).digits
        req = {
            "action": self.mt5.TRADE_ACTION_SLTP, "symbol": pos.symbol, "position": int(position_id), "magic": self.cfg.magic,
            "sl": round(sl if sl is not None else (pos.sl or 0.0), digits), "tp": round(tp if tp is not None else (pos.tp or 0.0), digits),
        }
        return self._send(req)

    def update(self) -> list[dict]:
        return []  # stops are enforced by the broker's server
