from datetime import datetime, timezone

from forexbot.audit import AuditLog
from forexbot.config import from_dict
from forexbot.engine import Engine
from forexbot.feed import ReplayFeed
from forexbot.models import Bar, Signal
from forexbot.paper import PaperBroker
from forexbot.state import StateStore
from forexbot.strategy import Strategy

T0 = int(datetime(2024, 1, 2, tzinfo=timezone.utc).timestamp())


def bars(closes, tf=3600, spread=0.0004):
    out, prev = [], closes[0]
    for i, c in enumerate(closes):
        out.append(Bar(T0 + i * tf, prev, max(prev, c) + spread, min(prev, c) - spread, c, 1))
        prev = c
    return out


class Scripted(Strategy):
    """Fires the queued signals, one per evaluated bar."""
    name = "scripted"

    def __init__(self, signals):
        super().__init__()
        self.signals = list(signals)

    def min_bars(self):
        return 3

    def evaluate(self, b):
        return self.signals.pop(0) if self.signals else None


def make(tmp_path, closes, signals, cfg=None, spread_points=10):
    cfg = from_dict({"symbols": ["EURUSD"], **(cfg or {})})
    feed = ReplayFeed({"EURUSD": bars(closes)}, cfg.timeframe, spread_points)
    broker = PaperBroker(feed, 10000, 0.0, "USD", clock=lambda: feed.now().timestamp())
    eng = Engine(cfg, broker, feed, Scripted(signals), AuditLog(tmp_path / "a.jsonl"), StateStore(tmp_path / "s.json"), clock=feed.now)
    return cfg, feed, broker, eng


def run_until(feed, eng, n):
    for _ in range(n):
        feed.advance()
        eng.step()
