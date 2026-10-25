"""Builds opted-in re-engagement digests. Writes files; never sends anything itself."""
import json
from .recommend import recommend

DAY = 86400


def build_digests(con, now, lapsed_days=7, exclude_users=(), k=5):
    out = []
    rows = con.execute("""SELECT u.id, MAX(p.started_at) last FROM users u
        LEFT JOIN plays p ON p.user_id=u.id WHERE u.email_opt_in=1 GROUP BY u.id""").fetchall()
    for r in rows:
        if r["id"] in exclude_users or r["last"] is None or now - r["last"] < lapsed_days * DAY:
            continue
        recs = recommend(con, r["id"], k, exclude_users)
        titles = [con.execute("SELECT title, artist FROM tracks WHERE id=?", (t,)).fetchone() for t, _ in recs]
        out.append({"user_id": r["id"], "subject": "New picks for you",
                    "tracks": [f'{t["title"]} - {t["artist"]}' for t in titles]})
    return out


def write(digests, path):
    with open(path, "w") as f:
        json.dump(digests, f, indent=2)
