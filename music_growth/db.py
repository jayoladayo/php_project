import sqlite3

SCHEMA = """
CREATE TABLE IF NOT EXISTS users  (id INTEGER PRIMARY KEY, created_at INTEGER, email_opt_in INTEGER DEFAULT 0);
CREATE TABLE IF NOT EXISTS tracks (id INTEGER PRIMARY KEY, title TEXT, artist TEXT, genre TEXT,
                                   duration_s INTEGER, released_at INTEGER);
CREATE TABLE IF NOT EXISTS plays  (id INTEGER PRIMARY KEY, user_id INTEGER, track_id INTEGER,
                                   started_at INTEGER, listened_s INTEGER, device_id TEXT,
                                   is_synthetic INTEGER DEFAULT 0);
CREATE INDEX IF NOT EXISTS plays_user ON plays(user_id, started_at);
CREATE INDEX IF NOT EXISTS plays_track ON plays(track_id);
CREATE TABLE IF NOT EXISTS experiments (user_id INTEGER, name TEXT, variant TEXT, PRIMARY KEY (user_id, name));
"""


def connect(path=":memory:"):
    con = sqlite3.connect(path)
    con.row_factory = sqlite3.Row
    con.executescript(SCHEMA)
    return con
