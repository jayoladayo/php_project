from __future__ import annotations

import tempfile
from datetime import datetime, timezone
from pathlib import Path

from .audit import AuditLog
from .config import Config
from .engine import Engine
from .feed import ReplayFeed
from .models import Bar
from .paper import PaperBroker
from .state import StateStore
from .strategy import build


def run_backtest(cfg: Config, data: dict[str, list[Bar]], infos=None, quiet: bool = True) -> dict:
    """Replay bars through the SAME engine and risk rules used in live trading. Approximate: spread is fixed,
    stops that both fall inside one bar resolve against you, swaps/slippage are not modelled."""
    feed = ReplayFeed(data, cfg.timeframe, cfg.paper.spread_points, infos)
    tmp = Path(tempfile.mkdtemp(prefix="fxbt-"))
    broker = PaperBroker(feed, cfg.paper.starting_balance, cfg.paper.commission_per_lot, cfg.paper.currency, clock=lambda: feed.now().timestamp())
    cfg2 = cfg
    eng = Engine(cfg2, broker, feed, build(cfg.strategy_name, cfg.strategy_params), AuditLog(tmp / "audit.jsonl"), StateStore(tmp / "state.json"), clock=feed.now)
    peak, max_dd = cfg.paper.starting_balance, 0.0
    while feed.advance():
        eng.step()
        eq = broker.account().equity
        peak = max(peak, eq)
        max_dd = max(max_dd, (peak - eq) / peak * 100)
        if eng.paused and eng.state.data.get("pause_reason", "").startswith("flattened"):
            eng.resume()  # backtest keeps going the next day
    for p in list(broker.positions_.values()):  # close what is still open at the final price
        broker.close(p.id)
    t = broker.closed
    wins = [x["pnl"] for x in t if x["pnl"] > 0]
    losses = [x["pnl"] for x in t if x["pnl"] <= 0]
    gross_loss = -sum(losses)
    return {
        "bars": feed.length, "trades": len(t), "win_rate_pct": round(100 * len(wins) / len(t), 1) if t else 0.0,
        "net_pnl": round(broker.balance - cfg.paper.starting_balance, 2), "final_balance": round(broker.balance, 2),
        "profit_factor": round(sum(wins) / gross_loss, 2) if gross_loss else None, "max_drawdown_pct": round(max_dd, 2),
        "avg_win": round(sum(wins) / len(wins), 2) if wins else 0.0, "avg_loss": round(-gross_loss / len(losses), 2) if losses else 0.0,
        "audit_dir": str(tmp),
    }


def synthetic_bars(n: int = 6000, start: float = 1.10, seed: int = 7, tf_sec: int = 3600) -> list[Bar]:
    """SYNTHETIC random-walk data with slow regime drift — only for trying the tooling, not for judging a strategy."""
    import random

    rnd = random.Random(seed)
    t0 = int(datetime(2024, 1, 1, tzinfo=timezone.utc).timestamp())
    px, drift, out = start, 0.0, []
    for i in range(n):
        if i % 400 == 0:
            drift = rnd.uniform(-0.00004, 0.00004)
        o = px
        pts = [o]
        for _ in range(4):
            pts.append(pts[-1] + drift + rnd.gauss(0, 0.0004))
        px = pts[-1]
        out.append(Bar(t0 + i * tf_sec, round(o, 5), round(max(pts), 5), round(min(pts), 5), round(px, 5), 100))
    return out
