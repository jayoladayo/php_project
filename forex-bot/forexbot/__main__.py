from __future__ import annotations

import argparse
import csv
import json
import os
import sys
import time
from pathlib import Path

from . import __version__
from .audit import AuditLog
from .backtest import run_backtest, synthetic_bars
from .config import ConfigError, load
from .dashboard import serve_in_thread
from .engine import Engine
from .feed import MT5Feed, load_csv
from .paper import PaperBroker
from .state import StateStore
from .strategy import build

LIVE_PHRASE = "TRADE REAL MONEY"


def _mt5_module():
    try:
        import MetaTrader5 as mt5  # type: ignore
    except ImportError:
        sys.exit("The MetaTrader5 package is not installed (it only exists for Windows). On your Windows PC run: pip install MetaTrader5\n"
                 "Meanwhile you can try the tooling offline:  python -m forexbot backtest --sample")
    return mt5


def cmd_run(args) -> None:
    cfg = load(args.config)
    if args.mode:
        cfg.mode = args.mode
        cfg.validate()
    data = Path(cfg.data_dir)
    audit, state = AuditLog(data / "audit.jsonl"), StateStore(data / "state.json")
    mt5 = _mt5_module()
    from .mt5_broker import MT5Broker

    real = MT5Broker(mt5, cfg.mt5)
    acct = real.connect()
    print(f"Connected to MT5 account {acct.login} ({acct.kind}), balance {acct.balance:.2f} {acct.currency}")

    if cfg.mode == "paper":
        broker, feed = PaperBroker(real, cfg.paper.starting_balance, cfg.paper.commission_per_lot, cfg.paper.currency, path=data / "paper.json"), real
        print("PAPER mode: real prices, simulated trades. No orders are sent to your broker.")
    else:
        broker = feed = real
        if cfg.mode == "demo" and acct.kind != "demo":
            sys.exit(f"Refusing to start: mode is 'demo' but the connected account is '{acct.kind}'. Log the MT5 terminal into a DEMO account.")
        if cfg.mode == "live":
            if acct.kind != "real":
                sys.exit("Mode 'live' needs a real-money account; the connected account is not one. (Use mode 'demo' for demo accounts.)")
            if cfg.live.account_login != acct.login:
                sys.exit(f"Refusing to start: set live.account_login to {acct.login} in config.json to confirm this is the account you mean.")
            phrase = os.environ.get("FOREX_LIVE_APPROVAL") or input(f"\nREAL MONEY. The bot will place trades on account {acct.login} by itself.\nType {LIVE_PHRASE} to continue: ")
            if phrase.strip() != LIVE_PHRASE:
                sys.exit("Not confirmed. Nothing was started.")
            audit.append("live_approved", {"login": acct.login})
        print(f"{cfg.mode.upper()} mode: orders WILL be sent to account {acct.login}.")

    strategy = build(cfg.strategy_name, cfg.strategy_params)
    engine = Engine(cfg, broker, feed, strategy, audit, state)
    audit.append("started", {"mode": cfg.mode, "symbols": cfg.symbols, "timeframe": cfg.timeframe, "strategy": cfg.strategy_name, "params": cfg.strategy_params, "risk": vars(cfg.risk), "exits": vars(cfg.exits), "version": __version__})
    held = broker.positions()
    print(f"Open bot positions found: {len(held)}" + ("".join(f"\n  {p.symbol} {p.side} {p.lots} @ {p.entry} SL {p.sl} TP {p.tp}" for p in held)))
    if engine.paused:
        print(f"NOTE: the bot is PAUSED ({state.data.get('pause_reason')}). Resume it from the dashboard.")
    srv = serve_in_thread(engine, cfg.dashboard_port)
    print(f"Dashboard: http://127.0.0.1:{cfg.dashboard_port}   (Ctrl+C to stop; stops live on the broker, so open trades stay protected)")
    try:
        while True:
            engine.step()
            time.sleep(cfg.poll_seconds)
    except KeyboardInterrupt:
        print("\nStopping.")
    finally:
        audit.append("stopped", {})
        srv.shutdown()
        if cfg.mode != "paper":
            real.shutdown()


def cmd_backtest(args) -> None:
    cfg = load(args.config) if Path(args.config).exists() else __import__("forexbot.config", fromlist=["from_dict"]).from_dict({})
    sym = args.symbol or cfg.symbols[0]
    if args.sample:
        bars = synthetic_bars()
        print("Using SYNTHETIC random data — this only proves the tooling works, it says nothing about real markets.")
    else:
        bars = load_csv(args.csv)
    cfg.symbols = [sym]
    cfg.sessions.enabled = False
    res = run_backtest(cfg, {sym: bars})
    print(json.dumps(res, indent=2))


def cmd_check(args) -> None:
    cfg = load(args.config)
    mt5 = _mt5_module()
    from .mt5_broker import MT5Broker

    b = MT5Broker(mt5, cfg.mt5)
    a = b.connect()
    print(f"Account {a.login}: {a.kind}, balance {a.balance} {a.currency}")
    for s in cfg.symbols:
        i, t = b.symbol_info(s), b.tick(s)
        print(f"{s}: digits {i.digits}, min lot {i.volume_min}, step {i.volume_step}, tick value {i.tick_value}, spread {(t.spread / i.point) if t else '?'} points")
    print("Check complete. No orders were sent.")
    b.shutdown()


def cmd_audit(args) -> None:
    ok, msg = AuditLog(Path(load(args.config).data_dir) / "audit.jsonl").verify()
    print("Audit log:", "intact" if ok else "TAMPERED", f"({msg})")


def main(argv=None) -> None:
    ap = argparse.ArgumentParser(prog="forexbot", description="Forex auto-trading bot for MetaTrader 5")
    ap.add_argument("--config", default="config.json")
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run", help="start the bot")
    r.add_argument("--mode", choices=["paper", "demo", "live"])
    r.set_defaults(fn=cmd_run)
    b = sub.add_parser("backtest", help="replay a CSV (time,open,high,low,close) through the bot")
    b.add_argument("--csv")
    b.add_argument("--symbol")
    b.add_argument("--sample", action="store_true", help="use built-in synthetic data")
    b.set_defaults(fn=cmd_backtest)
    sub.add_parser("check", help="connect to MT5 and print account/symbol info (sends no orders)").set_defaults(fn=cmd_check)
    sub.add_parser("verify-audit", help="verify the audit log hash chain").set_defaults(fn=cmd_audit)
    args = ap.parse_args(argv)
    try:
        if args.cmd == "backtest" and not (args.csv or args.sample):
            ap.error("backtest needs --csv FILE or --sample")
        args.fn(args)
    except ConfigError as e:
        sys.exit(f"Config problem: {e}")


if __name__ == "__main__":
    main()
