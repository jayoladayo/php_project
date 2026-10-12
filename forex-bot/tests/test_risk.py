from datetime import datetime, timezone

import pytest

from forexbot import risk
from forexbot.config import from_dict
from forexbot.models import Account, Position, Signal, SymbolInfo, Tick

INFO = SymbolInfo("EURUSD", 5, 0.00001, 0.00001, 1.0, 0.01, 100, 0.01)
NOW = datetime(2024, 1, 3, 12, 0, tzinfo=timezone.utc)


def test_size_risks_at_most_the_budget_and_rounds_down():
    lots, _ = risk.size_lots(10000, 0.5, 0.0030, INFO, 10)  # $50 budget, 30-pip stop = $300/lot
    assert lots == pytest.approx(0.16)
    assert risk.money_per_lot(0.0030, INFO) * lots <= 50


def test_size_never_rounds_up_to_the_minimum():
    lots, notes = risk.size_lots(100, 0.5, 0.0100, INFO, 10)  # $0.50 budget vs $1000/lot
    assert lots == 0 and "below the broker minimum" in notes[0]


def test_size_respects_max_lots():
    lots, notes = risk.size_lots(1_000_000, 1, 0.0010, INFO, 2.0)
    assert lots == 2.0 and "capped" in notes[0]


def test_session_window_and_weekend():
    s = from_dict({"sessions": {"enabled": True, "utc_start": "07:00", "utc_end": "20:00"}}).sessions
    assert risk.in_session(s, NOW)
    assert not risk.in_session(s, datetime(2024, 1, 3, 22, 0, tzinfo=timezone.utc))
    assert not risk.in_session(s, datetime(2024, 1, 6, 12, 0, tzinfo=timezone.utc))  # Saturday


def violations(cfg=None, **kw):
    cfg = from_dict(cfg or {})
    base = dict(paused=False, account=Account(1, 10000, 10000, "USD", "demo"), day_start_equity=10000, positions=[], infos={"EURUSD": INFO},
                symbol="EURUSD", info=INFO, tick=Tick(int(NOW.timestamp()), 1.1, 1.10010), signal=Signal("buy", 0.003, 0.006, "t"), lots=0.1, now=NOW)
    base.update(kw)
    return risk.pre_trade_violations(cfg, **base)


def test_clean_trade_passes():
    assert violations() == []


def test_blocks():
    assert any("paused" in v for v in violations(paused=True))
    assert any("spread" in v for v in violations(tick=Tick(int(NOW.timestamp()), 1.1, 1.2)))
    assert any("stale" in v for v in violations(tick=Tick(int(NOW.timestamp()) - 3600, 1.1, 1.1001)))
    assert any("daily loss" in v for v in violations(account=Account(1, 10000, 9700, "USD", "demo")))
    assert any("zero" in v for v in violations(lots=0))
    pos = [Position("1", "EURUSD", "buy", 0.1, 1.1, 1.097, None, 0)]
    assert any("already holding" in v for v in violations(positions=pos))
    assert any("reward:risk" in v for v in violations({"risk": {"min_rr": 3}}))


def test_total_risk_cap_counts_open_stops_and_unprotected_positions():
    held = [Position("1", "GBPUSD", "buy", 1.0, 1.25, 1.2475, None, 0)]  # $250 = 2.5% > the 2% cap
    infos = {"EURUSD": INFO, "GBPUSD": SymbolInfo("GBPUSD", 5, 0.00001, 0.00001, 1.0, 0.01, 100, 0.01)}
    assert any("total open risk" in v for v in violations(positions=held, infos=infos))
    naked = [Position("1", "GBPUSD", "buy", 0.1, 1.25, None, None, 0)]
    assert any("total open risk" in v for v in violations(positions=naked, infos=infos))
    breakeven = [Position("1", "GBPUSD", "buy", 5.0, 1.25, 1.25, None, 0)]
    assert not any("total open risk" in v for v in violations(positions=breakeven, infos=infos, cfg=None))
