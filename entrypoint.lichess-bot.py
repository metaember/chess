#!/usr/bin/env python3
"""Docker entrypoint for lichess-bot with smart graceful shutdown.

Problem: lichess-bot's quit_after_all_games_finish mode needs TWO signals
to fully exit (first sets 'terminated', second sets 'force_quit'). Docker
only sends one SIGTERM, so the bot hangs on blocking HTTP reads until the
stop_grace_period expires and Docker SIGKILLs it.

Solution: This wrapper forwards the first SIGTERM, checks the lichess API
for active games, and sends the second signal at the right time:
  - No active games  -> force quit within seconds
  - Active games     -> poll until they finish, then force quit
"""

import os
import signal
import subprocess
import sys
import time

import requests

POLL_INTERVAL = 30
FORCE_QUIT_DELAY = 5


def get_active_games():
    """Check lichess API for games in progress. Returns None on API failure."""
    token = os.environ.get("LICHESS_BOT_TOKEN")
    if not token:
        return None
    try:
        resp = requests.get(
            "https://lichess.org/api/account/playing",
            headers={"Authorization": f"Bearer {token}"},
            timeout=10,
        )
        resp.raise_for_status()
        return resp.json().get("nowPlaying", [])
    except Exception:
        return None


def main():
    proc = subprocess.Popen(["python", "lichess-bot.py", "-v"])
    terminated = False

    def handler(sig, frame):
        nonlocal terminated
        if terminated:
            # Second manual signal - force quit now
            proc.send_signal(signal.SIGTERM)
            return
        terminated = True
        proc.send_signal(signal.SIGTERM)

    signal.signal(signal.SIGTERM, handler)
    signal.signal(signal.SIGINT, handler)

    # Wait for child exit or termination signal
    while proc.poll() is None:
        if terminated:
            break
        try:
            time.sleep(1)
        except InterruptedError:
            continue

    if proc.poll() is not None:
        sys.exit(proc.returncode)

    # Got SIGTERM - child notified. Check for active games.
    games = get_active_games()

    if games:
        # Games in progress - wait for them to finish
        print(f"[entrypoint] {len(games)} active game(s), waiting for them to finish...")
        while proc.poll() is None:
            try:
                time.sleep(POLL_INTERVAL)
            except InterruptedError:
                pass
            games = get_active_games()
            if games is None:
                # API error - keep waiting, don't kill a potential game
                continue
            if not games:
                print("[entrypoint] All games finished.")
                break
    else:
        if games is None:
            print("[entrypoint] Could not reach lichess API, assuming no active games.")
        else:
            print("[entrypoint] No active games.")

    # Send second SIGTERM (force_quit) to break blocking HTTP reads
    if proc.poll() is None:
        time.sleep(FORCE_QUIT_DELAY)
        if proc.poll() is None:
            print("[entrypoint] Sending force_quit signal.")
            proc.send_signal(signal.SIGTERM)
            try:
                proc.wait(timeout=15)
            except subprocess.TimeoutExpired:
                print("[entrypoint] Force killing.")
                proc.kill()
                proc.wait()

    sys.exit(proc.returncode or 0)


if __name__ == "__main__":
    main()
