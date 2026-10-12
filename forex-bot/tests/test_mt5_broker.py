"""MT5Broker against a fake `MetaTrader5` module (the real one only exists on Windows)."""
from types import SimpleNamespace as NS

import pytest

from forexbot.config import MT5 as MT5Config
from forexbot.mt5_broker import MT5Broker


class FakeMT5:
    TRADE_ACTION_DEAL, TRADE_ACTION_SLTP = 1, 6
    ORDER_TYPE_BUY, ORDER_TYPE_SELL = 0, 1
    ORDER_FILLING_FOK, ORDER_FILLING_IOC, ORDER_FILLING_RETURN = 0, 1, 2
    ORDER_TIME_GTC = 0
    POSITION_TYPE_BUY = 0
    ACCOUNT_TRADE_MODE_DEMO, ACCOUNT_TRADE_MODE_REAL = 0, 2
    TIMEFRAME_H1 = 16385

    def __init__(self, trade_mode=0, retcode=10009, check_code=0):
        self.trade_mode, self.retcode, self.check_code = trade_mode, retcode, check_code
        self.sent, self.init_kw = [], None
        self.pos = [NS(ticket=11, symbol="EURUSD", type=0, volume=0.1, price_open=1.1, sl=1.097, tp=0.0, time=5, magic=26001, comment="", profit=3.0),
                    NS(ticket=99, symbol="EURUSD", type=0, volume=1.0, price_open=1.2, sl=0.0, tp=0.0, time=5, magic=0, comment="manual", profit=0)]

    def initialize(self, **kw): self.init_kw = kw; return True
    def shutdown(self): pass
    def last_error(self): return (1, "err")
    def account_info(self): return NS(login=123, balance=1000.0, equity=1000.0, currency="USD", trade_mode=self.trade_mode)
    def symbol_select(self, s, on): return True
    def symbol_info(self, s): return NS(digits=5, point=1e-5, trade_tick_size=1e-5, trade_tick_value=1.0, volume_min=0.01, volume_max=100, volume_step=0.01, trade_stops_level=10, filling_mode=2)
    def symbol_info_tick(self, s): return NS(time=1000, bid=1.1, ask=1.1001)
    def positions_get(self, **kw): return self.pos
    def order_check(self, req): return NS(retcode=self.check_code, comment="chk")
    def order_send(self, req):
        self.sent.append(req)
        return NS(retcode=self.retcode, comment="done" if self.retcode == 10009 else "no", order=555, deal=1, price=req.get("price", 0.0))
    def copy_rates_from_pos(self, s, tf, a, n):
        return [{"time": i, "open": 1, "high": 2, "low": 0.5, "close": 1.5, "tick_volume": 3} for i in range(n)]


def broker(**kw):
    m = FakeMT5(**kw)
    return m, MT5Broker(m, MT5Config())


def test_only_this_bots_magic_positions_are_visible():
    _, b = broker()
    assert [p.id for p in b.positions()] == ["11"]


def test_account_kind_is_detected():
    assert broker(trade_mode=0)[1].connect().kind == "demo"
    assert broker(trade_mode=2)[1].connect().kind == "real"


def test_open_always_sends_a_stop_loss_magic_and_verified_fill():
    m, b = broker()
    r = b.open("EURUSD", "buy", 0.1, 1.097, 1.106, "fb test")
    assert r.ok and r.position_id == "555"
    req = m.sent[0]
    assert req["sl"] == 1.097 and req["magic"] == 26001 and req["price"] == 1.1001 and req["type_filling"] == m.ORDER_FILLING_IOC


def test_open_refuses_without_stop_loss():
    m, b = broker()
    r = b.open("EURUSD", "buy", 0.1, None, None)
    assert not r.ok and "stop-loss" in r.error and m.sent == []


def test_broker_refusal_is_reported_not_assumed_filled():
    m, b = broker(retcode=10019)
    r = b.open("EURUSD", "buy", 0.1, 1.097, None)
    assert not r.ok and "10019" in r.error


def test_failed_order_check_sends_nothing():
    m, b = broker(check_code=10019)
    assert not b.open("EURUSD", "buy", 0.1, 1.097, None).ok and m.sent == []


def test_close_and_modify_target_the_position_ticket():
    m, b = broker()
    assert b.close("11", 0.05).ok
    c = m.sent[-1]
    assert c["position"] == 11 and c["type"] == m.ORDER_TYPE_SELL and c["price"] == 1.1 and c["volume"] == 0.05
    assert b.modify("11", sl=1.1).ok
    assert m.sent[-1]["action"] == m.TRADE_ACTION_SLTP and m.sent[-1]["sl"] == 1.1
    assert not b.close("99").ok  # a manual trade (other magic) can never be closed by the bot


def test_login_password_comes_only_from_the_environment(monkeypatch):
    monkeypatch.delenv("MT5_PASSWORD", raising=False)
    m = FakeMT5()
    b = MT5Broker(m, MT5Config(login=123, server="Demo"))
    with pytest.raises(RuntimeError, match="MT5_PASSWORD"):
        b.connect()
    monkeypatch.setenv("MT5_PASSWORD", "pw")
    b.connect()
    assert m.init_kw["password"] == "pw"


def test_bars_drop_the_forming_bar():
    _, b = broker()
    assert len(b.bars("EURUSD", "H1", 10)) == 10
