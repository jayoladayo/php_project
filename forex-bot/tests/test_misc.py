import json
import threading
import urllib.error
import urllib.request

import pytest

from forexbot.audit import AuditLog
from forexbot.backtest import run_backtest, synthetic_bars
from forexbot.config import ConfigError, from_dict
from forexbot.dashboard import make_server
from forexbot.indicators import atr, ema, rsi
from forexbot.strategy import build
from .helpers import make


def test_audit_chain_detects_tampering_and_redacts(tmp_path):
    a = AuditLog(tmp_path / "x.jsonl")
    a.append("one", {"password": "hunter2", "ok": 1})
    a.append("two", {"nested": {"api_key": "k"}})
    assert a.verify()[0]
    text = (tmp_path / "x.jsonl").read_text()
    assert "hunter2" not in text and '"k"' not in text
    (tmp_path / "x.jsonl").write_text(text.replace('"ok":1', '"ok":2').replace('"ok": 1', '"ok": 2'))
    assert not AuditLog(tmp_path / "x.jsonl").verify()[0]


def test_config_rejects_bad_values_and_typos():
    for bad in ({"mode": "yolo"}, {"risk": {"risk_per_trade_pct": 50}}, {"risk": {"typo": 1}}, {"timeframe": "H7"}, {"nope": 1}):
        with pytest.raises(ConfigError):
            from_dict(bad)
    assert from_dict({}).mode == "paper"  # safe default


def test_indicators_basic():
    assert ema([1, 1, 1], 3) == [1, 1, 1]
    r = rsi(list(range(1, 40)), 14)
    assert r[-1] == 100
    from .helpers import bars
    assert atr(bars([1.0] * 30), 14)[-1] > 0


def test_ema_strategy_rejects_bad_params_and_signals_on_cross():
    with pytest.raises(ValueError):
        build("ema_trend", {"fast": 50, "slow": 20})
    with pytest.raises(ValueError):
        build("nope", {})
    from .helpers import bars
    closes = [1.0 + i * 0.001 for i in range(150)] + [1.15 - i * 0.002 for i in range(15)] + [1.12 + i * 0.002 for i in range(30)]  # uptrend, dip, recovery
    sigs = [build("ema_trend", {"fast": 5, "slow": 20, "trend": 50}).evaluate(bars(closes[:n])) for n in range(120, len(closes))]
    assert any(x and x.side == "buy" for x in sigs)


def test_backtest_runs_and_is_deterministic():
    cfg = from_dict({})
    data = {"EURUSD": synthetic_bars(2500)}
    a, b = run_backtest(cfg, data), run_backtest(cfg, data)
    a.pop("audit_dir"); b.pop("audit_dir")
    assert a == b and a["trades"] > 0 and a["max_drawdown_pct"] < 50


def test_dashboard_blocks_foreign_host_and_requires_token(tmp_path, monkeypatch):
    monkeypatch.setenv("DASHBOARD_TOKEN", "s3cret")
    cfg, feed, broker, eng = make(tmp_path, [1.1] * 5, [])
    srv = make_server(eng, 0)
    port = srv.server_address[1]
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    try:
        def call(path, method="GET", headers=None):
            req = urllib.request.Request(f"http://127.0.0.1:{port}{path}", method=method, headers=headers or {}, data=b"" if method == "POST" else None)
            try:
                return urllib.request.urlopen(req).status
            except urllib.error.HTTPError as e:
                return e.code
        feed.advance()
        assert call("/api/state") == 200
        assert call("/api/state", headers={"Host": "evil.example"}) == 403
        assert call("/api/pause", "POST") == 401
        assert not eng.paused
        assert call("/api/pause", "POST", {"X-Token": "s3cret"}) == 200 and eng.paused
        assert call("/api/pause", "POST", {"X-Token": "s3cret", "Origin": "http://evil.example"}) == 403
    finally:
        srv.shutdown()
