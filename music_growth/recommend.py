"""Item-item collaborative filtering on completion-weighted implicit feedback."""
import math
from collections import defaultdict


def _vectors(con, exclude_users=()):
    ex = set(exclude_users)
    item_users = defaultdict(dict)
    for r in con.execute("""SELECT p.user_id, p.track_id, SUM(MIN(1.0, p.listened_s*1.0/t.duration_s)) w
                            FROM plays p JOIN tracks t ON t.id=p.track_id GROUP BY 1,2"""):
        if r["user_id"] not in ex:
            item_users[r["track_id"]][r["user_id"]] = r["w"]
    return item_users


def _cos(a, b):
    common = a.keys() & b.keys()
    if not common:
        return 0.0
    dot = sum(a[k] * b[k] for k in common)
    return dot / (math.sqrt(sum(v * v for v in a.values())) * math.sqrt(sum(v * v for v in b.values())))


def recommend(con, user_id, k=10, exclude_users=()):
    iu = _vectors(con, exclude_users)
    heard = {t for t, us in iu.items() if user_id in us}
    scores = defaultdict(float)
    for h in heard:
        w = iu[h][user_id]
        for t, us in iu.items():
            if t not in heard:
                scores[t] += w * _cos(iu[h], us)
    return sorted(scores.items(), key=lambda x: -x[1])[:k]
