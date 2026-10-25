"""One-shot local preview of a track link. Plays once, interactively; Ctrl+C stops it.

Optional dependencies (only needed for `preview`): pip install yt-dlp python-vlc
"""
import time
import webbrowser


def play_platform_link(page_url):
    import yt_dlp
    import vlc

    ydl_opts = {"format": "bestaudio/best", "quiet": True, "noplaylist": True}

    print("Extracting audio stream from link...")
    with yt_dlp.YoutubeDL(ydl_opts) as ydl:
        info = ydl.extract_info(page_url, download=False)
        stream_url = info["url"]
        title = info.get("title", "Unknown Track")

    print(f"Now Playing: {title}")

    instance = vlc.Instance("--no-video")
    player = instance.media_player_new()
    player.set_media(instance.media_new(stream_url))
    player.play()

    time.sleep(3)  # wait for buffer

    try:
        while player.get_state() in [vlc.State.Opening, vlc.State.Buffering, vlc.State.Playing]:
            time.sleep(1)
    except KeyboardInterrupt:
        print("\nPlayback stopped by user.")
        player.stop()


def launch_music_link(url):
    print(f"Opening link in default browser: {url}")
    webbrowser.open(url, new=2)  # new tab
