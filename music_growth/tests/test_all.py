import unittest
from music_growth import db, synth, fraud, analytics, recommend, digest


class T(unittest.TestCase):
    def setUp(self):
        self.con = db.connect()
        synth.generate(self.con, users=60, tracks=40, bots=2)

    def test_bots_flagged_and_humans_not(self):
        bad = fraud.flag_users(self.con)
        self.assertEqual(set(bad), {60, 61})

    def test_recommend_excludes_heard(self):
        recs = recommend.recommend(self.con, 1, exclude_users=[60, 61])
        heard = {r[0] for r in self.con.execute("SELECT track_id FROM plays WHERE user_id=1")}
        self.assertTrue(recs and not heard & {t for t, _ in recs})

    def test_digest_only_opted_in(self):
        now = self.con.execute("SELECT MAX(started_at) FROM plays").fetchone()[0] + 30 * 86400
        for d in digest.build_digests(self.con, now, exclude_users=[60, 61]):
            self.assertEqual(self.con.execute("SELECT email_opt_in FROM users WHERE id=?", (d["user_id"],)).fetchone()[0], 1)

    def test_ztest(self):
        lift, p = analytics.two_proportion_z(100, 1000, 150, 1000)
        self.assertAlmostEqual(lift, 0.05); self.assertLess(p, 0.01)


if __name__ == "__main__":
    unittest.main()
