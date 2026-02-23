#!/usr/bin/env python3
"""
Analyze rustchess bot's losses and draws from Lichess.

Downloads games, replays positions, and categorizes errors as:
- Depth issues: engine needed more search depth (Stockfish also misses at same depth)
- Eval issues: engine's evaluation is wrong (Stockfish finds correct move at same depth)

Usage:
    uv run scripts/analyze_losses.py --max-games 5 --verbose
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import chess
import chess.engine
import requests


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------

@dataclass
class MoveAnalysis:
    ply: int
    move_number: int
    move_uci: str
    move_san: str
    side: str  # "white" | "black"
    fen_before: str

    # Custom engine
    engine_eval_cp: int | None = None
    engine_best_move: str | None = None
    engine_depth: int | None = None

    # Stockfish at same depth
    sf_same_depth_eval_cp: int | None = None
    sf_same_depth_best_move: str | None = None

    # Stockfish deep (ground truth)
    sf_deep_eval_cp: int | None = None
    sf_deep_best_move: str | None = None
    sf_deep_depth: int = 0

    # Eval swing (from bot's perspective, positive = bot lost advantage)
    eval_before_deep: int | None = None
    eval_after_deep: int | None = None
    eval_swing_cp: int | None = None

    is_mistake: bool = False
    is_blunder: bool = False
    error_type: str | None = None  # "depth_issue" | "eval_issue" | None


@dataclass
class GameAnalysis:
    game_id: str
    url: str
    bot_color: str
    opponent: str
    opponent_rating: int | None
    time_control: str
    result: str  # "loss" | "draw"
    termination: str
    num_plies: int
    opening: str

    moves: list[MoveAnalysis] = field(default_factory=list)

    total_mistakes: int = 0
    total_blunders: int = 0
    depth_issues: int = 0
    eval_issues: int = 0

    # Resignation analysis
    resign_position_eval: int | None = None
    resign_justified: bool | None = None

    # Draw analysis
    draw_missed_win: bool = False
    draw_missed_win_ply: int | None = None
    draw_best_eval: int | None = None


@dataclass
class AnalysisReport:
    timestamp: str
    bot_username: str
    games_analyzed: int
    config: dict[str, Any]
    games: list[GameAnalysis] = field(default_factory=list)

    total_losses: int = 0
    total_draws: int = 0
    resignations: int = 0
    justified_resignations: int = 0
    unjustified_resignations: int = 0
    checkmates: int = 0
    timeouts: int = 0
    total_mistakes: int = 0
    total_blunders: int = 0
    total_depth_issues: int = 0
    total_eval_issues: int = 0


# ---------------------------------------------------------------------------
# Score helpers
# ---------------------------------------------------------------------------

MATE_CP = 10_000


def score_to_cp(score: chess.engine.PovScore, perspective: chess.Color) -> int:
    """Convert a PovScore to centipawns from the given color's perspective."""
    white_score = score.white()
    cp = white_score.score(mate_score=MATE_CP)
    if cp is None:
        return 0
    return cp if perspective == chess.WHITE else -cp


# ---------------------------------------------------------------------------
# Lichess API
# ---------------------------------------------------------------------------

def fetch_games(
    username: str,
    max_games: int,
    rated_only: bool,
    since_hours: float | None = None,
) -> list[dict[str, Any]]:
    """Download losses and draws from Lichess as ndjson."""
    url = f"https://lichess.org/api/games/user/{username}"
    params: dict[str, Any] = {
        "max": max_games * 4,  # fetch extra, we filter below
        "moves": "true",
        "clocks": "true",
        "opening": "true",
        "pgnInJson": "false",
        "sort": "dateDesc",
    }
    if rated_only:
        params["rated"] = "true"
    if since_hours is not None:
        since_ms = int((time.time() - since_hours * 3600) * 1000)
        params["since"] = since_ms

    headers = {"Accept": "application/x-ndjson"}

    print(f"Fetching games for {username} from Lichess...")
    resp = requests.get(url, params=params, headers=headers, stream=True, timeout=30)
    resp.raise_for_status()

    qualifying: list[dict[str, Any]] = []
    total_scanned = 0

    for line in resp.iter_lines():
        if not line:
            continue
        game = json.loads(line)
        total_scanned += 1

        # Only standard games
        if game.get("variant") != "standard":
            continue

        # Must have moves
        if not game.get("moves"):
            continue

        # Determine bot color
        white_id = (game.get("players", {}).get("white", {}).get("user", {}).get("id") or "").lower()
        black_id = (game.get("players", {}).get("black", {}).get("user", {}).get("id") or "").lower()
        bot_lower = username.lower()

        if white_id == bot_lower:
            bot_color = "white"
        elif black_id == bot_lower:
            bot_color = "black"
        else:
            continue

        # Check if bot lost or drew
        winner = game.get("winner")
        status = game.get("status", "")

        if winner is None:
            # Draw or ongoing
            if status in ("draw", "stalemate"):
                game["_bot_color"] = bot_color
                game["_result"] = "draw"
                qualifying.append(game)
            # Other statuses without winner (aborted, etc.) — skip
            continue
        elif winner != bot_color:
            # Bot lost
            game["_bot_color"] = bot_color
            game["_result"] = "loss"
            qualifying.append(game)

        if len(qualifying) >= max_games:
            break

    print(f"  Scanned {total_scanned} games, found {len(qualifying)} losses/draws")
    return qualifying


# ---------------------------------------------------------------------------
# Engine helpers
# ---------------------------------------------------------------------------

def open_engine(path: str, name: str) -> chess.engine.SimpleEngine:
    """Open a UCI engine, with a helpful error message on failure."""
    try:
        engine = chess.engine.SimpleEngine.popen_uci(path)
        print(f"  {name} started: {path}")
        return engine
    except Exception as e:
        print(f"ERROR: Could not start {name} at '{path}': {e}", file=sys.stderr)
        if "uci" in name.lower() or "custom" in name.lower():
            print("  Hint: run `cargo build --release --bin uci` first", file=sys.stderr)
        sys.exit(1)


def analyze_position(
    engine: chess.engine.SimpleEngine,
    board: chess.Board,
    depth: int,
    time_limit: float = 30.0,
) -> tuple[int | None, str | None, int]:
    """
    Analyze a position at a given depth.
    Returns (eval_cp_from_white, best_move_uci, depth_reached).
    eval is from WHITE's perspective (caller normalizes).
    """
    try:
        info = engine.analyse(
            board,
            chess.engine.Limit(depth=depth),
            info=chess.engine.INFO_ALL,
        )
    except chess.engine.EngineTerminatedError:
        return None, None, 0
    except chess.engine.EngineError:
        return None, None, 0

    score = info.get("score")
    pv = info.get("pv", [])
    depth_reached = info.get("depth", depth)

    eval_cp: int | None = None
    if score is not None:
        eval_cp = score.white().score(mate_score=MATE_CP)

    best_move_uci: str | None = None
    if pv:
        best_move_uci = pv[0].uci()

    return eval_cp, best_move_uci, depth_reached


# ---------------------------------------------------------------------------
# Error classification
# ---------------------------------------------------------------------------

def classify_error(ma: MoveAnalysis) -> str | None:
    """
    Determine if a mistake/blunder is a depth issue or eval issue.

    - depth_issue: Stockfish at the same depth also doesn't find the
      ground-truth best move. The position requires deeper search.
    - eval_issue: Stockfish at the same depth DOES find the ground-truth
      best move, but our engine doesn't. Our eval/ordering is wrong.
    """
    if ma.sf_deep_best_move is None:
        return None

    # Did the engine play the right move?
    if ma.engine_best_move == ma.sf_deep_best_move:
        # Engine found the best move but played something else?
        # This means the engine's best != what was actually played — the
        # "mistake" was the game move, not the engine's analysis.
        # But actually engine_best_move IS what the engine would play.
        # If the game move differs from both, it might be a book move or
        # time-pressure situation. Classify based on SF comparison.
        pass

    engine_found = ma.engine_best_move == ma.sf_deep_best_move
    sf_same_found = ma.sf_same_depth_best_move == ma.sf_deep_best_move

    if engine_found:
        # Engine found the right move in analysis — the actual game move
        # might differ due to book, time pressure, etc. Hard to classify.
        return None

    if sf_same_found:
        # SF found it at the same depth, our engine didn't → eval issue
        return "eval_issue"
    else:
        # SF also missed at this depth → depth issue
        return "depth_issue"


# ---------------------------------------------------------------------------
# Per-game analysis
# ---------------------------------------------------------------------------

def analyze_game(
    game_data: dict[str, Any],
    custom_engine: chess.engine.SimpleEngine,
    stockfish: chess.engine.SimpleEngine,
    analysis_depth: int,
    sf_deep_depth: int,
    mistake_threshold: int,
    blunder_threshold: int,
    resign_threshold: int,
    verbose: bool,
) -> GameAnalysis:
    """Analyze a single game."""
    game_id = game_data["id"]
    bot_color_str = game_data["_bot_color"]
    bot_color = chess.WHITE if bot_color_str == "white" else chess.BLACK
    result = game_data["_result"]
    status = game_data.get("status", "unknown")

    # Parse opponent info
    opp_side = "black" if bot_color_str == "white" else "white"
    opp_data = game_data.get("players", {}).get(opp_side, {})
    opp_user = opp_data.get("user", {})
    opponent = opp_user.get("name", opp_user.get("id", "Anonymous"))
    opp_rating = opp_data.get("rating")

    # Time control
    clock = game_data.get("clock", {})
    tc_initial = clock.get("initial", 0)
    tc_increment = clock.get("increment", 0)
    time_control = f"{tc_initial // 60}+{tc_increment}" if clock else "unknown"

    # Opening
    opening = game_data.get("opening", {}).get("name", "Unknown")

    # Parse moves
    moves_san_str = game_data.get("moves", "")
    san_moves = moves_san_str.split()

    ga = GameAnalysis(
        game_id=game_id,
        url=f"https://lichess.org/{game_id}",
        bot_color=bot_color_str,
        opponent=opponent,
        opponent_rating=opp_rating,
        time_control=time_control,
        result=result,
        termination=status,
        num_plies=len(san_moves),
        opening=opening,
    )

    board = chess.Board()
    move_analyses: list[MoveAnalysis] = []

    # Cache for SF deep evals (fen -> eval_cp from white's perspective)
    sf_deep_cache: dict[str, int] = {}

    def get_sf_deep_eval(b: chess.Board) -> int | None:
        """Get SF deep eval, using cache."""
        fen = b.fen()
        if fen in sf_deep_cache:
            return sf_deep_cache[fen]
        eval_cp, _, _ = analyze_position(stockfish, b, sf_deep_depth)
        if eval_cp is not None:
            sf_deep_cache[fen] = eval_cp
        return eval_cp

    for ply_idx, san in enumerate(san_moves):
        try:
            move = board.parse_san(san)
        except ValueError:
            if verbose:
                print(f"    WARNING: Could not parse move '{san}' at ply {ply_idx}")
            board = chess.Board()  # Reset to avoid cascading errors
            break

        side_to_move = board.turn
        is_bot_move = side_to_move == bot_color
        move_number = ply_idx // 2 + 1

        if is_bot_move:
            fen_before = board.fen()

            if verbose:
                side_str = "W" if side_to_move == chess.WHITE else "B"
                print(f"    Move {move_number}{'. ' if side_to_move == chess.WHITE else '...'}{san} ({side_str})", end="", flush=True)

            # 1) Custom engine analysis
            eng_eval_white, eng_best, eng_depth = analyze_position(
                custom_engine, board, analysis_depth
            )
            eng_eval = (eng_eval_white if eng_eval_white is not None else None)

            # 2) Stockfish at same depth
            actual_depth = eng_depth if eng_depth > 0 else analysis_depth
            sf_same_eval_white, sf_same_best, _ = analyze_position(
                stockfish, board, actual_depth
            )

            # 3) Stockfish deep (ground truth) — eval BEFORE the move
            sf_deep_eval_before_white = get_sf_deep_eval(board)

            # 4) Make the actual game move, get eval AFTER
            board.push(move)
            sf_deep_eval_after_white = get_sf_deep_eval(board)

            # Normalize evals to bot's perspective
            sign = 1 if bot_color == chess.WHITE else -1

            eng_eval_bot = eng_eval * sign if eng_eval is not None else None
            sf_same_eval_bot = sf_same_eval_white * sign if sf_same_eval_white is not None else None
            sf_deep_before_bot = sf_deep_eval_before_white * sign if sf_deep_eval_before_white is not None else None
            sf_deep_after_bot = sf_deep_eval_after_white * sign if sf_deep_eval_after_white is not None else None

            # Eval swing: positive means bot lost advantage
            eval_swing: int | None = None
            if sf_deep_before_bot is not None and sf_deep_after_bot is not None:
                eval_swing = sf_deep_before_bot - sf_deep_after_bot

            is_mistake = eval_swing is not None and eval_swing > mistake_threshold
            is_blunder = eval_swing is not None and eval_swing > blunder_threshold

            ma = MoveAnalysis(
                ply=ply_idx,
                move_number=move_number,
                move_uci=move.uci(),
                move_san=san,
                side=bot_color_str,
                fen_before=fen_before,
                engine_eval_cp=eng_eval_bot,
                engine_best_move=eng_best,
                engine_depth=eng_depth,
                sf_same_depth_eval_cp=sf_same_eval_bot,
                sf_same_depth_best_move=sf_same_best,
                sf_deep_eval_cp=sf_deep_before_bot,
                sf_deep_best_move=None,  # filled below
                sf_deep_depth=sf_deep_depth,
                eval_before_deep=sf_deep_before_bot,
                eval_after_deep=sf_deep_after_bot,
                eval_swing_cp=eval_swing,
                is_mistake=is_mistake,
                is_blunder=is_blunder,
            )

            # Get the SF deep best move for classification
            # (We already ran SF deep on the position, but analyze_position
            #  returns the best move too — reuse the cache approach)
            # Actually re-analyze to get the best move (cached eval only stores cp)
            _, sf_deep_best, _ = analyze_position(stockfish, chess.Board(fen_before), sf_deep_depth)
            ma.sf_deep_best_move = sf_deep_best

            # Classify errors
            if is_mistake or is_blunder:
                ma.error_type = classify_error(ma)

            move_analyses.append(ma)

            if verbose:
                swing_str = f" swing={eval_swing:+d}cp" if eval_swing is not None else ""
                err_str = f" [{ma.error_type}]" if ma.error_type else ""
                flag = " BLUNDER!" if is_blunder else (" mistake" if is_mistake else "")
                print(f"{swing_str}{flag}{err_str}")
        else:
            # Opponent's move — just play it
            board.push(move)

    ga.moves = move_analyses

    # Aggregate stats
    ga.total_mistakes = sum(1 for m in move_analyses if m.is_mistake)
    ga.total_blunders = sum(1 for m in move_analyses if m.is_blunder)
    ga.depth_issues = sum(1 for m in move_analyses if m.error_type == "depth_issue")
    ga.eval_issues = sum(1 for m in move_analyses if m.error_type == "eval_issue")

    # --- Resignation analysis ---
    if status == "resign" and result == "loss":
        # Eval the final position with SF deep
        final_eval_white = get_sf_deep_eval(board)
        if final_eval_white is not None:
            final_eval_bot = final_eval_white * (1 if bot_color == chess.WHITE else -1)
            ga.resign_position_eval = final_eval_bot
            ga.resign_justified = final_eval_bot <= resign_threshold

    # --- Draw analysis ---
    if result == "draw":
        best_eval_for_bot = None
        best_eval_ply = None
        for ma in move_analyses:
            if ma.sf_deep_eval_cp is not None:
                if best_eval_for_bot is None or ma.sf_deep_eval_cp > best_eval_for_bot:
                    best_eval_for_bot = ma.sf_deep_eval_cp
                    best_eval_ply = ma.ply
        if best_eval_for_bot is not None and best_eval_for_bot >= 100:
            ga.draw_missed_win = True
            ga.draw_missed_win_ply = best_eval_ply
            ga.draw_best_eval = best_eval_for_bot

    return ga


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------

def print_terminal_report(report: AnalysisReport) -> None:
    """Print a human-readable summary to the terminal."""
    print()
    print("=" * 60)
    print(f"  RustChess Loss Analysis Report")
    print(f"  Bot: {report.bot_username}")
    print(f"  Games analyzed: {report.games_analyzed}")
    print(f"  {report.timestamp}")
    print("=" * 60)

    # Loss breakdown
    print()
    print("--- Loss Breakdown ---")
    print(f"  Resignations:  {report.resignations}", end="")
    if report.resignations > 0:
        print(f"  ({report.justified_resignations} justified, {report.unjustified_resignations} unjustified)")
    else:
        print()
    print(f"  Checkmates:    {report.checkmates}")
    print(f"  Timeouts:      {report.timeouts}")

    # Draw breakdown
    draws_with_missed = sum(1 for g in report.games if g.result == "draw" and g.draw_missed_win)
    print()
    print("--- Draw Breakdown ---")
    print(f"  Total draws:   {report.total_draws}")
    print(f"  Missed wins:   {draws_with_missed}")

    # Error analysis
    total_errors = report.total_depth_issues + report.total_eval_issues
    print()
    print("--- Error Analysis (bot's moves only) ---")
    print(f"  Total mistakes (>{report.config['mistake_threshold']}cp swing): {report.total_mistakes}")
    print(f"  Total blunders (>{report.config['blunder_threshold']}cp swing): {report.total_blunders}")
    if total_errors > 0:
        depth_pct = report.total_depth_issues / total_errors * 100
        eval_pct = report.total_eval_issues / total_errors * 100
        print(f"  Depth issues:  {report.total_depth_issues} ({depth_pct:.0f}%)")
        print(f"  Eval issues:   {report.total_eval_issues} ({eval_pct:.0f}%)")
    else:
        print(f"  Depth issues:  {report.total_depth_issues}")
        print(f"  Eval issues:   {report.total_eval_issues}")

    # Worst blunders
    all_blunders: list[tuple[str, MoveAnalysis]] = []
    for g in report.games:
        for m in g.moves:
            if m.is_blunder:
                all_blunders.append((g.game_id, m))
    all_blunders.sort(key=lambda x: -(x[1].eval_swing_cp or 0))

    if all_blunders:
        print()
        print("--- Worst Blunders ---")
        for i, (gid, m) in enumerate(all_blunders[:10]):
            err = f"[{m.error_type}]" if m.error_type else ""
            eng_move = m.engine_best_move or "?"
            sf_move = m.sf_deep_best_move or "?"
            print(f"  {i+1}. Game {gid} move {m.move_number} ({m.move_san}): "
                  f"{m.eval_swing_cp:+d}cp swing {err}")
            print(f"     Engine wanted: {eng_move}, SF best: {sf_move}")
            print(f"     FEN: {m.fen_before}")

    # Unjustified resignations
    unjustified = [g for g in report.games if g.resign_justified is False]
    if unjustified:
        print()
        print("--- Unjustified Resignations ---")
        for g in unjustified:
            print(f"  Game {g.game_id}: resigned at eval {g.resign_position_eval:+d}cp "
                  f"(threshold: {report.config['resign_threshold']}cp)")
            print(f"    {g.url}")

    # Draws with missed wins
    missed_win_games = [g for g in report.games if g.draw_missed_win]
    if missed_win_games:
        print()
        print("--- Draws With Missed Wins ---")
        for g in missed_win_games:
            ply = g.draw_missed_win_ply or 0
            move_num = ply // 2 + 1
            print(f"  Game {g.game_id}: best eval was +{g.draw_best_eval}cp at move {move_num}")
            print(f"    {g.url}")

    # Per-game summary table
    print()
    print("--- Per-Game Summary ---")
    print(f"  {'ID':<12} {'Result':<8} {'Term':<12} {'Opp':<16} {'TC':<8} "
          f"{'Mistakes':<10} {'Blunders':<10} {'Depth':<8} {'Eval':<8}")
    print(f"  {'-'*12} {'-'*8} {'-'*12} {'-'*16} {'-'*8} "
          f"{'-'*10} {'-'*10} {'-'*8} {'-'*8}")
    for g in report.games:
        print(f"  {g.game_id:<12} {g.result:<8} {g.termination:<12} "
              f"{g.opponent[:16]:<16} {g.time_control:<8} "
              f"{g.total_mistakes:<10} {g.total_blunders:<10} "
              f"{g.depth_issues:<8} {g.eval_issues:<8}")

    print()


def save_json_report(report: AnalysisReport, output_path: str) -> None:
    """Save detailed analysis as JSON."""
    data = asdict(report)
    path = Path(output_path)
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
    print(f"Full report saved to: {path}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Analyze rustchess bot's losses and draws from Lichess"
    )
    parser.add_argument("--username", default="rustchess",
                        help="Lichess username (default: rustchess)")
    parser.add_argument("--max-games", type=int, default=20,
                        help="Max losses/draws to analyze (default: 20)")
    parser.add_argument("--engine-path", default="target/release/uci",
                        help="Path to custom UCI engine (default: target/release/uci)")
    parser.add_argument("--stockfish-path", default="/opt/homebrew/bin/stockfish",
                        help="Path to Stockfish (default: /opt/homebrew/bin/stockfish)")
    parser.add_argument("--analysis-depth", type=int, default=12,
                        help="Depth for custom engine analysis (default: 12)")
    parser.add_argument("--sf-deep-depth", type=int, default=20,
                        help="Deep Stockfish depth for ground truth (default: 20)")
    parser.add_argument("--mistake-threshold", type=int, default=50,
                        help="Eval swing in cp to flag as mistake (default: 50)")
    parser.add_argument("--blunder-threshold", type=int, default=150,
                        help="Eval swing in cp to flag as blunder (default: 150)")
    parser.add_argument("--resign-threshold", type=int, default=-400,
                        help="CP threshold for justified resignation (default: -400)")
    parser.add_argument("--rated-only", action="store_true", default=True,
                        help="Only analyze rated games (default: True)")
    parser.add_argument("--include-casual", action="store_true",
                        help="Include casual (unrated) games")
    parser.add_argument("--since-hours", type=float, default=None,
                        help="Only analyze games from the last N hours")
    parser.add_argument("--output", default="analysis_report.json",
                        help="JSON output path (default: analysis_report.json)")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Print per-move analysis during processing")

    args = parser.parse_args()
    rated_only = not args.include_casual

    config = {
        "analysis_depth": args.analysis_depth,
        "sf_deep_depth": args.sf_deep_depth,
        "mistake_threshold": args.mistake_threshold,
        "blunder_threshold": args.blunder_threshold,
        "resign_threshold": args.resign_threshold,
        "rated_only": rated_only,
    }

    # Fetch games
    games = fetch_games(args.username, args.max_games, rated_only, args.since_hours)
    if not games:
        print("No qualifying games found.")
        return

    # Start engines
    print()
    print("Starting engines...")
    custom_engine = open_engine(args.engine_path, "Custom engine")
    stockfish = open_engine(args.stockfish_path, "Stockfish")

    # Analyze each game
    report = AnalysisReport(
        timestamp=datetime.now(timezone.utc).isoformat(),
        bot_username=args.username,
        games_analyzed=len(games),
        config=config,
    )

    try:
        for i, game_data in enumerate(games):
            game_id = game_data["id"]
            result = game_data["_result"]
            opp_side = "black" if game_data["_bot_color"] == "white" else "white"
            opp_name = (game_data.get("players", {}).get(opp_side, {})
                        .get("user", {}).get("name", "Anonymous"))
            moves_str = game_data.get("moves", "")
            num_plies = len(moves_str.split())

            print()
            print(f"[{i+1}/{len(games)}] Analyzing game {game_id} "
                  f"({result} vs {opp_name}, {num_plies} plies)...")

            ga = analyze_game(
                game_data,
                custom_engine,
                stockfish,
                analysis_depth=args.analysis_depth,
                sf_deep_depth=args.sf_deep_depth,
                mistake_threshold=args.mistake_threshold,
                blunder_threshold=args.blunder_threshold,
                resign_threshold=args.resign_threshold,
                verbose=args.verbose,
            )
            report.games.append(ga)

    except KeyboardInterrupt:
        print("\n\nInterrupted! Saving partial results...")
    finally:
        custom_engine.quit()
        stockfish.quit()

    # Update report-level counts
    report.games_analyzed = len(report.games)
    for g in report.games:
        if g.result == "loss":
            report.total_losses += 1
            if g.termination == "resign":
                report.resignations += 1
                if g.resign_justified is True:
                    report.justified_resignations += 1
                elif g.resign_justified is False:
                    report.unjustified_resignations += 1
            elif g.termination == "mate":
                report.checkmates += 1
            elif g.termination == "outoftime":
                report.timeouts += 1
        elif g.result == "draw":
            report.total_draws += 1

        report.total_mistakes += g.total_mistakes
        report.total_blunders += g.total_blunders
        report.total_depth_issues += g.depth_issues
        report.total_eval_issues += g.eval_issues

    # Output
    print_terminal_report(report)
    save_json_report(report, args.output)


if __name__ == "__main__":
    main()
