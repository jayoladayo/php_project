"""Flag bot-like listening so growth metrics reflect real people."""
MIN_COUNTED_S = 30  # a stream counts after 30s, a common industry convention


def flag_users(con, max_plays_per_day=250, min_distinct_tracks=3, min_plays=100):
    flagged = {}
    rows = con.execute("""SELECT user_id, COUNT(*) n, COUNT(DISTINCT track_id) d,
        (MAX(started_at)-MIN(started_at))/86400.0 span,
        AVG(listened_s) avg_s FROM plays GROUP BY user_id""").fetchall()
    for r in rows:
        reasons = []
        per_day = r["n"] / max(r["span"], 1)
        if per_day > max_plays_per_day:
            reasons.append("implausible volume")
        if r["n"] >= min_plays and r["d"] < min_distinct_tracks:
            reasons.append("repeats too few tracks")
        if r["n"] >= min_plays and MIN_COUNTED_S <= r["avg_s"] <= MIN_COUNTED_S + 2:
            reasons.append("plays stop just past the counting threshold")
        if reasons:
            flagged[r["user_id"]] = reasons
    return flagged
