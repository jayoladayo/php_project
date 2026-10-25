import math

DAY = 86400


def retention(con, day_n, exclude_users=()):
    """Share of users with a play exactly day_n..day_n+1 days after signup (eligible users only)."""
    ex = set(exclude_users)
    hit = total = 0
    last = con.execute("SELECT MAX(started_at) FROM plays").fetchone()[0] or 0
    for u in con.execute("SELECT id, created_at FROM users"):
        if u["id"] in ex or u["created_at"] + (day_n + 1) * DAY > last:
            continue
        total += 1
        hit += con.execute("SELECT 1 FROM plays WHERE user_id=? AND started_at BETWEEN ? AND ? LIMIT 1",
                           (u["id"], u["created_at"] + day_n * DAY, u["created_at"] + (day_n + 1) * DAY)).fetchone() is not None
    return hit / total if total else 0.0, total


def completion_rate(con, exclude_users=()):
    ex = set(exclude_users)
    rows = con.execute("SELECT p.user_id, p.listened_s*1.0/t.duration_s f FROM plays p JOIN tracks t ON t.id=p.track_id")
    vals = [r["f"] for r in rows if r["user_id"] not in ex]
    return sum(v >= 0.9 for v in vals) / len(vals) if vals else 0.0


def two_proportion_z(success_a, n_a, success_b, n_b):
    """Two-sided z-test. Returns (lift, p_value)."""
    pa, pb = success_a / n_a, success_b / n_b
    p = (success_a + success_b) / (n_a + n_b)
    se = math.sqrt(p * (1 - p) * (1 / n_a + 1 / n_b))
    if se == 0:
        return pb - pa, 1.0
    z = (pb - pa) / se
    return pb - pa, math.erfc(abs(z) / math.sqrt(2))
