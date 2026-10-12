import pytest

from forexbot.models import Signal
from .helpers import make, run_until

FLAT = [1.1000] * 10


def test_stop_loss_hit_closes_at_the_stop_and_loses_about_the_risk(tmp_path):
    closes = FLAT + [1.1000, 1.0980, 1.0950]
    cfg, feed, broker, eng = make(tmp_path, closes, [Signal("buy", 0.0030, 0.0060, "t")])
    run_until(feed, eng, len(closes))
    assert broker.positions_ == {}
    loss = broker.balance - 10000
    assert loss == pytest.approx(-50, rel=0.15)  # 0.5% of 10k, plus the spread paid on entry
    kinds = [e["event"] for e in eng.audit.tail(50)]
    assert "order_opened" in kinds and "paper_exit" in kinds


def test_take_profit_hit(tmp_path):
    closes = FLAT + [1.1000, 1.1030, 1.1070]
    cfg, feed, broker, eng = make(tmp_path, closes, [Signal("buy", 0.0030, 0.0060, "t")], {"exits": {"breakeven_at_r": None}})
    run_until(feed, eng, len(closes))
    assert broker.balance > 10000 and not broker.positions_


def test_gap_through_the_stop_fills_at_the_worse_price(tmp_path):
    closes = FLAT + [1.1000, 1.0900]
    cfg, feed, broker, eng = make(tmp_path, closes, [Signal("buy", 0.0030, None, "t")])
    last = feed.data["EURUSD"][-1]
    feed.data["EURUSD"][-1] = type(last)(last.time, 1.0900, 1.0905, 1.0890, 1.0900, 1)  # opens far below the stop
    run_until(feed, eng, len(closes))
    assert broker.closed[-1]["exit"] == 1.0900 and broker.closed[-1]["exit"] < 1.0970 # filled at the gap price, not the stop
    assert broker.closed[-1]["exit"] <= 1.0970 and broker.balance - 10000 < -50


def test_paused_bot_opens_nothing_but_resumes(tmp_path):
    closes = FLAT + [1.1] * 4
    cfg, feed, broker, eng = make(tmp_path, closes, [Signal("buy", 0.003, 0.006, "t")] * 5)
    eng.pause("test")
    run_until(feed, eng, len(closes))
    assert broker.positions_ == {}
    assert any(e["event"] == "signal_rejected" and "paused" in str(e["data"]) for e in eng.audit.tail(50))


def test_one_entry_per_bar_even_if_step_is_called_often(tmp_path):
    cfg, feed, broker, eng = make(tmp_path, FLAT + [1.1] * 3, [Signal("buy", 0.003, 0.006, "t")] * 9, {"risk": {"one_position_per_symbol": False, "max_open_positions": 9}})
    for _ in range(10):
        feed.advance()
    for _ in range(5):
        eng.step()
    assert len(broker.positions_) == 1


def test_breakeven_then_stopped_out_loses_nothing(tmp_path):
    closes = FLAT + [1.1000, 1.1035, 1.1000, 1.0950]
    cfg, feed, broker, eng = make(tmp_path, closes, [Signal("buy", 0.0030, 0.0200, "t")])
    run_until(feed, eng, len(closes))
    assert not broker.positions_
    assert any(e["event"] == "stop_moved" for e in eng.audit.tail(50))
    assert broker.balance - 10000 > -15  # only the entry spread, not the full risk


def test_partial_close_takes_half_off_at_the_target(tmp_path):
    closes = FLAT + [1.1000, 1.1035, 1.1036]
    cfg, feed, broker, eng = make(tmp_path, closes, [Signal("buy", 0.0030, 0.0300, "t")], {"exits": {"breakeven_at_r": None, "partial_close_at_r": 1.0, "partial_close_pct": 50}})
    run_until(feed, eng, len(closes))
    (p,) = broker.positions_.values()
    assert p.lots == pytest.approx(0.08) and broker.closed[-1]["lots"] == pytest.approx(0.08)


def test_daily_loss_limit_blocks_new_entries(tmp_path):
    cfg, feed, broker, eng = make(tmp_path, FLAT + [1.1] * 3, [Signal("buy", 0.003, 0.006, "t")] * 5, {"risk": {"one_position_per_symbol": False}})
    feed.advance(); feed.advance(); feed.advance()
    eng.step()
    eng.state.set(day_start_equity=20000)  # equity is now 50% below the day's start
    feed.advance()
    eng.step()
    assert eng.last_decisions["EURUSD"].startswith("buy signal blocked: daily loss limit")


def test_repeated_errors_pause_the_bot(tmp_path):
    cfg, feed, broker, eng = make(tmp_path, FLAT, [])
    broker.account = lambda: (_ for _ in ()).throw(RuntimeError("feed down"))
    for _ in range(5):
        eng.step()
    assert eng.paused and "errors in a row" in eng.state.data["pause_reason"]


def test_pause_survives_restart(tmp_path):
    cfg, feed, broker, eng = make(tmp_path, FLAT, [])
    eng.pause("manual")
    from forexbot.state import StateStore
    assert StateStore(tmp_path / "s.json").data["paused"] is True
