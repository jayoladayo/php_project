"""Append-only, hash-chained JSONL audit log. Secrets are redacted before writing."""
from __future__ import annotations

import hashlib
import json
import re
import threading
from datetime import datetime, timezone
from pathlib import Path

SECRET_KEY = re.compile(r"pass(word|phrase)?|secret|token|private|seed|api[_-]?key", re.I)


def redact(v):
    if isinstance(v, dict):
        return {k: ("[redacted]" if SECRET_KEY.search(str(k)) else redact(x)) for k, x in v.items()}
    if isinstance(v, (list, tuple)):
        return [redact(x) for x in v]
    return v


class AuditLog:
    def __init__(self, path: str | Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._lock = threading.Lock()
        self._seq, self._prev = 0, "0" * 64
        if self.path.exists():
            for line in self.path.read_text(encoding="utf-8").splitlines():
                if line.strip():
                    e = json.loads(line)
                    self._seq, self._prev = e["seq"], e["hash"]

    def append(self, event: str, data: dict | None = None) -> dict:
        with self._lock:
            self._seq += 1
            body = {"seq": self._seq, "at": datetime.now(timezone.utc).isoformat(), "event": event, "data": redact(data or {}), "prev": self._prev}
            h = hashlib.sha256(json.dumps(body, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
            entry = {**body, "hash": h}
            with self.path.open("a", encoding="utf-8") as f:
                f.write(json.dumps(entry) + "\n")
            self._prev = h
            return entry

    def tail(self, n: int = 50) -> list[dict]:
        if not self.path.exists():
            return []
        lines = [l for l in self.path.read_text(encoding="utf-8").splitlines() if l.strip()]
        return [json.loads(l) for l in lines[-n:]]

    def verify(self) -> tuple[bool, str]:
        prev = "0" * 64
        if not self.path.exists():
            return True, "empty"
        for i, line in enumerate(self.path.read_text(encoding="utf-8").splitlines(), 1):
            if not line.strip():
                continue
            e = json.loads(line)
            h = e.pop("hash")
            if e["prev"] != prev or hashlib.sha256(json.dumps(e, sort_keys=True, separators=(",", ":")).encode()).hexdigest() != h:
                return False, f"chain broken at line {i}"
            prev = h
        return True, "ok"
