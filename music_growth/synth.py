"""Synthetic dataset for development and research. Rows are flagged is_synthetic=1 so they can
never be mixed into production metrics by accident."""
import random

DAY = 86400
GENRES = ["pop", "rock", "hiphop", "jazz", "electronic", "folk"]


def generate(con, users=200, tracks=120, days=30, seed=7, bots=0, now=1_700_000_000):
    rnd = random.Random(seed)
    for t in range(tracks):
        con.execute("INSERT INTO tracks VALUES (?,?,?,?,?,?)",
                    (t, f"Track {t}", f"Artist {t % 25}", GENRES[t % len(GENRES)],
                     rnd.randint(120, 300), now - rnd.randint(0, days) * DAY))
    for u in range(users):
        start = now - rnd.randint(0, days) * DAY
        con.execute("INSERT INTO users VALUES (?,?,?)", (u, start, rnd.random() < 0.6))
        fav = rnd.sample(GENRES, 2)
        for _ in range(rnd.randint(5, 80)):
            tr = rnd.randrange(tracks)
            g = GENRES[tr % len(GENRES)]
            if g not in fav and rnd.random() < 0.7:
                continue
            dur = con.execute("SELECT duration_s FROM tracks WHERE id=?", (tr,)).fetchone()[0]
            frac = 1.0 if g in fav and rnd.random() < 0.7 else rnd.random() * 0.4
            con.execute("INSERT INTO plays (user_id,track_id,started_at,listened_s,device_id,is_synthetic)"
                        " VALUES (?,?,?,?,?,1)",
                        (u, tr, min(now, start + rnd.randint(0, days) * DAY + rnd.randint(0, DAY)),
                         int(dur * frac), f"dev{u}"))
    for b in range(bots):  # labeled bot-like traffic, for testing the fraud filter
        uid = users + b
        con.execute("INSERT INTO users VALUES (?,?,0)", (uid, now - DAY))
        for i in range(300):
            con.execute("INSERT INTO plays (user_id,track_id,started_at,listened_s,device_id,is_synthetic)"
                        " VALUES (?,?,?,?,?,1)", (uid, 0, now - DAY + i * 35, 31, "botdev"))
    con.commit()
