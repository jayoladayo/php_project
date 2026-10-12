"""Small JSON state file (atomic writes). Holds the pause flag and daily-loss baseline,
so a restart cannot silently un-pause the bot or reset the daily loss limit."""
from __future__ import annotations

import json
import os
import threading
from pathlib import Path


class StateStore:
    def __init__(self, path: str | Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._lock = threading.RLock()
        self.data: dict = {"paused": False, "pause_reason": None, "day": None, "day_start_equity": None, "meta": {}, "last_bar": {}}
        if self.path.exists():
            self.data.update(json.loads(self.path.read_text(encoding="utf-8")))

    def save(self) -> None:
        with self._lock:
            tmp = self.path.with_suffix(".tmp")
            tmp.write_text(json.dumps(self.data, indent=1), encoding="utf-8")
            os.replace(tmp, self.path)

    def set(self, **kw) -> None:
        with self._lock:
            self.data.update(kw)
            self.save()
