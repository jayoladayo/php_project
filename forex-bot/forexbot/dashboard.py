"""Tiny local dashboard (stdlib only). Binds to 127.0.0.1, checks Host/Origin, optional DASHBOARD_TOKEN for actions."""
from __future__ import annotations

import hmac
import json
import os
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

PAGE = """<!doctype html><meta charset=utf-8><title>Forex bot</title>
<style>body{font:14px system-ui;margin:0;background:#0f1419;color:#e6e6e6}main{max-width:1000px;margin:auto;padding:16px}
.card{background:#1a212b;border-radius:8px;padding:12px 16px;margin:10px 0}.row{display:flex;gap:12px;flex-wrap:wrap}.row>.card{flex:1;min-width:150px}
h1{font-size:18px}h3{margin:0 0 6px;font-size:13px;color:#9aa7b4;font-weight:600}.big{font-size:22px}table{width:100%;border-collapse:collapse}
td,th{text-align:left;padding:4px 8px;border-bottom:1px solid #2a3340;font-size:13px}.pos{color:#4ade80}.neg{color:#f87171}.warn{background:#4a3b10}
button{padding:8px 14px;border:0;border-radius:6px;font-weight:600;cursor:pointer}.stop{background:#dc2626;color:#fff}.go{background:#16a34a;color:#fff}
.mode{padding:2px 8px;border-radius:99px;background:#334155;font-size:12px}.live{background:#b91c1c}</style>
<main><h1>Forex bot <span id=mode class=mode></span></h1><div id=app>loading…</div></main>
<script>
const $=s=>document.querySelector(s),fmt=(n,d=2)=>n==null?'—':Number(n).toLocaleString(undefined,{minimumFractionDigits:d,maximumFractionDigits:d});
const cls=n=>n>0?'pos':n<0?'neg':'';
async function act(p){const t=localStorage.getItem('dtoken')||'';const r=await fetch('/api/'+p,{method:'POST',headers:{'X-Token':t}});if(r.status==401){const x=prompt('Dashboard token');if(x){localStorage.setItem('dtoken',x);return act(p)}}load()}
async function load(){const s=await (await fetch('/api/state')).json();$('#mode').textContent=s.mode+' · '+s.account.kind;$('#mode').className='mode '+(s.mode=='live'?'live':'');
const esc=x=>String(x??'').replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
$('#app').innerHTML=`<div class="card ${s.paused?'warn':''}"><b>${s.paused?'PAUSED — no new trades. '+esc(s.pause_reason||''):'Running'}</b>
 <span style=float:right>${s.paused?'<button class=go onclick="act(\\'resume\\')">Resume</button>':'<button class=stop onclick="act(\\'pause\\')">EMERGENCY PAUSE</button>'}</span></div>
<div class=row><div class=card><h3>Balance</h3><div class=big>${fmt(s.account.balance)} ${s.account.currency}</div></div>
<div class=card><h3>Equity</h3><div class=big>${fmt(s.account.equity)}</div></div>
<div class=card><h3>Today (limit −${s.daily_limit_pct}%)</h3><div class="big ${cls(s.day_pnl)}">${fmt(s.day_pnl)}</div></div>
<div class=card><h3>Strategy</h3><div>${esc(s.strategy)} · ${esc(s.timeframe)}<br>${s.symbols.map(esc).join(', ')}</div></div></div>
<div class=card><h3>Open positions (${s.positions.length})</h3><table><tr><th>Symbol<th>Side<th>Lots<th>Entry<th>SL<th>TP<th>P&L</tr>${s.positions.map(p=>`<tr><td>${esc(p.symbol)}<td>${p.side}<td>${p.lots}<td>${p.entry}<td>${p.sl??'—'}<td>${p.tp??'—'}<td class="${cls(p.profit)}">${fmt(p.profit)}</tr>`).join('')||'<tr><td colspan=7>none'}</table></div>
<div class=card><h3>Latest decision per symbol</h3>${Object.entries(s.decisions).map(([k,v])=>`<div><b>${esc(k)}</b>: ${esc(v)}</div>`).join('')||'—'}${s.last_error?`<div class=neg>Last error: ${esc(s.last_error)}</div>`:''}</div>
<div class=card><h3>Audit log</h3><table>${s.audit.slice().reverse().map(e=>`<tr><td>${esc(e.at.slice(11,19))}<td>${esc(e.event)}<td>${esc(JSON.stringify(e.data)).slice(0,160)}</tr>`).join('')}</table></div>`}
load();setInterval(load,5000)
</script>"""


def make_server(engine, port: int) -> ThreadingHTTPServer:
    token = os.environ.get("DASHBOARD_TOKEN")
    allowed = {"localhost", "127.0.0.1", "[::1]"}

    class H(BaseHTTPRequestHandler):
        def log_message(self, *a):  # keep the console quiet and never log request contents
            pass

        def _host_ok(self) -> bool:
            host = (self.headers.get("Host") or "").rsplit(":", 1)[0] if not (self.headers.get("Host") or "").startswith("[") else "[::1]"
            return host in allowed

        def _send(self, code, body, ctype="application/json"):
            b = body if isinstance(body, bytes) else body.encode()
            self.send_response(code)
            self.send_header("Content-Type", ctype)
            self.send_header("Content-Length", str(len(b)))
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self.wfile.write(b)

        def do_GET(self):
            if not self._host_ok():
                return self._send(403, '{"error":"bad host"}')
            if self.path == "/":
                return self._send(200, PAGE, "text/html; charset=utf-8")
            if self.path == "/api/state":
                return self._send(200, json.dumps(engine.snapshot(), default=str))
            self._send(404, '{"error":"not found"}')

        def do_POST(self):
            if not self._host_ok():
                return self._send(403, '{"error":"bad host"}')
            origin = self.headers.get("Origin")
            if origin and origin.split("://", 1)[-1].rsplit(":", 1)[0] not in allowed:
                return self._send(403, '{"error":"bad origin"}')
            if token and not hmac.compare_digest(self.headers.get("X-Token", ""), token):
                return self._send(401, '{"error":"token required"}')
            if self.path == "/api/pause":
                engine.pause("paused from the dashboard")
            elif self.path == "/api/resume":
                engine.resume()
            else:
                return self._send(404, '{"error":"not found"}')
            self._send(200, '{"ok":true}')

    return ThreadingHTTPServer(("127.0.0.1", port), H)


def serve_in_thread(engine, port: int) -> ThreadingHTTPServer:
    srv = make_server(engine, port)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv
