import argparse, time
from . import db, synth, fraud, analytics, digest, recommend


def main(argv=None):
    ap = argparse.ArgumentParser(prog="music_growth")
    ap.add_argument("--db", default="growth.sqlite")
    sub = ap.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("seed"); s.add_argument("--bots", type=int, default=3)
    sub.add_parser("report")
    d = sub.add_parser("digests"); d.add_argument("--out", default="digests.json")
    r = sub.add_parser("recommend"); r.add_argument("user", type=int)
    a = ap.parse_args(argv)
    con = db.connect(a.db)
    if a.cmd == "seed":
        synth.generate(con, bots=a.bots); print("seeded synthetic data")
        return
    bad = fraud.flag_users(con)
    if a.cmd == "report":
        print(f"flagged bot-like users: {len(bad)}")
        for n in (1, 7):
            v, tot = analytics.retention(con, n, bad); print(f"D{n} retention: {v:.1%} (n={tot})")
        print(f"completion rate: {analytics.completion_rate(con, bad):.1%}")
    elif a.cmd == "digests":
        now = con.execute("SELECT MAX(started_at) FROM plays").fetchone()[0]
        ds = digest.build_digests(con, now, exclude_users=bad); digest.write(ds, a.out)
        print(f"wrote {len(ds)} digests to {a.out}")
    elif a.cmd == "recommend":
        for t, sc in recommend.recommend(con, a.user, exclude_users=bad):
            print(t, round(sc, 3))


if __name__ == "__main__":
    main()
