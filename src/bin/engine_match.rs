//! Engine Match Runner - Pit two UCI engines against each other
//!
//! Usage: cargo run --release --bin engine_match -- \
//!          --engine1 ./target/release/uci \
//!          --engine2 ./target/release/uci_movepicker \
//!          --games 100 --concurrency 6 --tc 1+0

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use clap::Parser;

// Adjudication thresholds
const RESIGN_THRESHOLD: i32 = 500;      // Resign if eval worse than -500cp
const RESIGN_COUNT: usize = 4;           // For this many moves in a row
const DRAW_THRESHOLD: i32 = 10;          // Draw if |eval| < 10cp
const DRAW_COUNT: usize = 8;             // For this many moves
const MAX_MOVES: usize = 80;             // Hard limit on game length

#[derive(Parser, Debug)]
#[command(name = "engine_match")]
#[command(about = "Run matches between two UCI chess engines")]
struct Args {
    /// Path to first engine
    #[arg(long, default_value = "./target/release/uci")]
    engine1: String,

    /// Path to second engine
    #[arg(long, default_value = "./target/release/uci_movepicker")]
    engine2: String,

    /// Name for engine 1
    #[arg(long, default_value = "OldSearch")]
    name1: String,

    /// Name for engine 2
    #[arg(long, default_value = "MovePicker")]
    name2: String,

    /// Number of games to play
    #[arg(short, long, default_value_t = 100)]
    games: usize,

    /// Number of concurrent games
    #[arg(short, long, default_value_t = 1)]
    concurrency: usize,

    /// Time control: base_seconds+increment (e.g., "1+0")
    #[arg(long, default_value = "1+0")]
    tc: String,
}

#[derive(Debug, Clone, Copy)]
enum GameResult {
    Engine1Win,
    Engine2Win,
    Draw,
}

#[derive(Debug)]
struct MatchStats {
    engine1_wins: AtomicUsize,
    engine2_wins: AtomicUsize,
    draws: AtomicUsize,
    games_completed: AtomicUsize,
}

impl MatchStats {
    fn new() -> Self {
        Self {
            engine1_wins: AtomicUsize::new(0),
            engine2_wins: AtomicUsize::new(0),
            draws: AtomicUsize::new(0),
            games_completed: AtomicUsize::new(0),
        }
    }

    fn record(&self, result: GameResult) {
        match result {
            GameResult::Engine1Win => self.engine1_wins.fetch_add(1, Ordering::Relaxed),
            GameResult::Engine2Win => self.engine2_wins.fetch_add(1, Ordering::Relaxed),
            GameResult::Draw => self.draws.fetch_add(1, Ordering::Relaxed),
        };
        self.games_completed.fetch_add(1, Ordering::Relaxed);
    }

    fn get_stats(&self) -> (usize, usize, usize, usize) {
        (
            self.engine1_wins.load(Ordering::Relaxed),
            self.engine2_wins.load(Ordering::Relaxed),
            self.draws.load(Ordering::Relaxed),
            self.games_completed.load(Ordering::Relaxed),
        )
    }
}

struct UciEngine {
    process: Child,
    stdin: std::process::ChildStdin,
    stdout: BufReader<std::process::ChildStdout>,
}

impl UciEngine {
    fn new(path: &str) -> Result<Self, String> {
        let mut process = Command::new(path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("Failed to start engine {}: {}", path, e))?;

        let stdin = process.stdin.take().unwrap();
        let stdout = BufReader::new(process.stdout.take().unwrap());

        let mut engine = Self {
            process,
            stdin,
            stdout,
        };

        // Initialize UCI
        engine.send("uci")?;
        engine.wait_for("uciok")?;
        engine.send("isready")?;
        engine.wait_for("readyok")?;

        Ok(engine)
    }

    fn send(&mut self, cmd: &str) -> Result<(), String> {
        writeln!(self.stdin, "{}", cmd)
            .map_err(|e| format!("Failed to send command: {}", e))?;
        self.stdin.flush().map_err(|e| format!("Flush failed: {}", e))?;
        Ok(())
    }

    fn wait_for(&mut self, expected: &str) -> Result<(), String> {
        let mut line = String::new();
        loop {
            line.clear();
            self.stdout
                .read_line(&mut line)
                .map_err(|e| format!("Read failed: {}", e))?;
            if line.trim().starts_with(expected) {
                return Ok(());
            }
        }
    }

    /// Send go command and return (best_move, score_cp)
    fn go(&mut self, wtime: u64, btime: u64, winc: u64, binc: u64) -> Result<(String, Option<i32>), String> {
        self.send(&format!(
            "go wtime {} btime {} winc {} binc {}",
            wtime, btime, winc, binc
        ))?;

        let mut line = String::new();
        let mut last_score: Option<i32> = None;

        loop {
            line.clear();
            self.stdout
                .read_line(&mut line)
                .map_err(|e| format!("Read failed: {}", e))?;

            // Parse score from info lines
            if line.starts_with("info") && line.contains("score cp") {
                if let Some(score) = parse_score_cp(&line) {
                    last_score = Some(score);
                }
            } else if line.starts_with("info") && line.contains("score mate") {
                // Mate score - treat as very high
                if let Some(mate_in) = parse_mate_score(&line) {
                    last_score = Some(if mate_in > 0 { 10000 } else { -10000 });
                }
            }

            if line.starts_with("bestmove") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    return Ok((parts[1].to_string(), last_score));
                }
                return Err("Invalid bestmove response".to_string());
            }
        }
    }

    fn set_position(&mut self, moves: &[String]) -> Result<(), String> {
        if moves.is_empty() {
            self.send("position startpos")
        } else {
            self.send(&format!("position startpos moves {}", moves.join(" ")))
        }
    }

    fn new_game(&mut self) -> Result<(), String> {
        self.send("ucinewgame")?;
        self.send("isready")?;
        self.wait_for("readyok")
    }

    fn quit(&mut self) {
        let _ = self.send("quit");
        let _ = self.process.wait();
    }
}

impl Drop for UciEngine {
    fn drop(&mut self) {
        self.quit();
    }
}

fn parse_score_cp(line: &str) -> Option<i32> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    for (i, part) in parts.iter().enumerate() {
        if *part == "cp" && i + 1 < parts.len() {
            return parts[i + 1].parse().ok();
        }
    }
    None
}

fn parse_mate_score(line: &str) -> Option<i32> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    for (i, part) in parts.iter().enumerate() {
        if *part == "mate" && i + 1 < parts.len() {
            return parts[i + 1].parse().ok();
        }
    }
    None
}

fn parse_time_control(tc: &str) -> (u64, u64) {
    let parts: Vec<&str> = tc.split('+').collect();
    let base_ms = (parts[0].parse::<f64>().unwrap_or(60.0) * 1000.0) as u64;
    let inc_ms = if parts.len() > 1 {
        (parts[1].parse::<f64>().unwrap_or(0.0) * 1000.0) as u64
    } else {
        0
    };
    (base_ms, inc_ms)
}

fn play_game(
    engine1_path: &str,
    engine2_path: &str,
    base_time_ms: u64,
    inc_ms: u64,
    engine1_white: bool,
) -> Result<GameResult, String> {
    let mut white = if engine1_white {
        UciEngine::new(engine1_path)?
    } else {
        UciEngine::new(engine2_path)?
    };

    let mut black = if engine1_white {
        UciEngine::new(engine2_path)?
    } else {
        UciEngine::new(engine1_path)?
    };

    white.new_game()?;
    black.new_game()?;

    let mut moves: Vec<String> = Vec::new();
    let mut wtime = base_time_ms;
    let mut btime = base_time_ms;
    let mut white_to_move = true;

    // Adjudication tracking
    let mut white_losing_count = 0;  // White's eval is bad
    let mut black_losing_count = 0;  // Black's eval is bad
    let mut draw_count = 0;

    // Repetition detection
    let mut position_count: HashMap<String, usize> = HashMap::new();

    for _move_num in 0..MAX_MOVES {
        let (current, time, otime) = if white_to_move {
            (&mut white, &mut wtime, btime)
        } else {
            (&mut black, &mut btime, wtime)
        };

        current.set_position(&moves)?;

        let start = Instant::now();
        let (best_move, score) = current.go(*time, otime, inc_ms, inc_ms)?;
        let elapsed = start.elapsed().as_millis() as u64;

        // Update clock
        if *time > elapsed {
            *time = *time - elapsed + inc_ms;
        } else {
            // Flag fall - opponent wins
            return Ok(if white_to_move == engine1_white {
                GameResult::Engine2Win
            } else {
                GameResult::Engine1Win
            });
        }

        // Check for game end
        if best_move == "(none)" || best_move.is_empty() {
            // No legal moves - loss for side to move
            return Ok(if white_to_move == engine1_white {
                GameResult::Engine2Win
            } else {
                GameResult::Engine1Win
            });
        }

        // Adjudication based on eval
        if let Some(cp) = score {
            // Score is from the perspective of the side to move
            let abs_score = cp.abs();

            if abs_score < DRAW_THRESHOLD {
                draw_count += 1;
                white_losing_count = 0;
                black_losing_count = 0;
            } else {
                draw_count = 0;

                // If score is negative, side to move is losing
                if cp < -(RESIGN_THRESHOLD as i32) {
                    if white_to_move {
                        white_losing_count += 1;
                        black_losing_count = 0;
                    } else {
                        black_losing_count += 1;
                        white_losing_count = 0;
                    }
                } else {
                    if white_to_move {
                        white_losing_count = 0;
                    } else {
                        black_losing_count = 0;
                    }
                }
            }

            // Adjudicate draw
            if draw_count >= DRAW_COUNT {
                return Ok(GameResult::Draw);
            }

            // Adjudicate resignation
            if white_losing_count >= RESIGN_COUNT {
                // White resigns - who was white?
                return Ok(if engine1_white {
                    GameResult::Engine2Win
                } else {
                    GameResult::Engine1Win
                });
            }
            if black_losing_count >= RESIGN_COUNT {
                // Black resigns
                return Ok(if engine1_white {
                    GameResult::Engine1Win
                } else {
                    GameResult::Engine2Win
                });
            }
        }

        moves.push(best_move);

        // Simple position key for repetition (just use moves list as proxy)
        let pos_key = moves.join(" ");
        *position_count.entry(pos_key).or_insert(0) += 1;

        white_to_move = !white_to_move;
    }

    // Max moves reached
    Ok(GameResult::Draw)
}

fn calculate_elo_difference(wins: f64, losses: f64, draws: f64) -> (f64, f64, f64) {
    let total = wins + losses + draws;
    if total == 0.0 {
        return (0.0, 0.0, 0.0);
    }

    let score = (wins + draws * 0.5) / total;

    let elo_diff = if score >= 1.0 {
        f64::INFINITY
    } else if score <= 0.0 {
        f64::NEG_INFINITY
    } else {
        -400.0 * (1.0 / score - 1.0).log10()
    };

    let se = (score * (1.0 - score) / total).sqrt();
    let margin = 1.96 * se;

    let score_low = (score - margin).max(0.001).min(0.999);
    let score_high = (score + margin).max(0.001).min(0.999);

    let elo_low = -400.0 * (1.0 / score_high - 1.0).log10();
    let elo_high = -400.0 * (1.0 / score_low - 1.0).log10();

    (elo_diff, elo_low, elo_high)
}

fn print_progress(stats: &MatchStats, name1: &str, name2: &str, total_games: usize) {
    let (w1, w2, d, completed) = stats.get_stats();
    let total = w1 + w2 + d;

    if total == 0 {
        return;
    }

    let (elo_diff, elo_low, elo_high) = calculate_elo_difference(w2 as f64, w1 as f64, d as f64);
    let score2 = (w2 as f64 + d as f64 * 0.5) / total as f64 * 100.0;

    print!("\r\x1b[K");
    print!(
        "Games: {}/{} | {} {}-{}-{} {} | ",
        completed, total_games, name1, w1, d, w2, name2
    );
    print!("{} score: {:.1}% | ", name2, score2);

    if elo_diff.is_finite() {
        print!("Elo: {:+.0} [{:+.0}, {:+.0}]", elo_diff, elo_low, elo_high);
    } else if elo_diff > 0.0 {
        print!("Elo: +INF");
    } else {
        print!("Elo: -INF");
    }

    std::io::stdout().flush().unwrap();
}

fn main() {
    let args = Args::parse();
    let (base_time, inc_time) = parse_time_control(&args.tc);

    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║                    Engine Match Runner                     ║");
    println!("╠════════════════════════════════════════════════════════════╣");
    println!("║ Engine 1: {:<49} ║", args.name1);
    println!("║ Engine 2: {:<49} ║", args.name2);
    println!("║ Games: {:<52} ║", args.games);
    println!("║ Concurrency: {:<46} ║", args.concurrency);
    println!("║ Time Control: {}s + {}s                                    ║",
        base_time / 1000, inc_time as f64 / 1000.0);
    println!("║ Max moves: {} | Resign: {}cp x{} | Draw: {}cp x{}          ║",
        MAX_MOVES, RESIGN_THRESHOLD, RESIGN_COUNT, DRAW_THRESHOLD, DRAW_COUNT);
    println!("╚════════════════════════════════════════════════════════════╝");
    println!();

    let stats = Arc::new(MatchStats::new());
    let start_time = Instant::now();

    let games_per_thread = (args.games + args.concurrency - 1) / args.concurrency;
    let mut handles = vec![];

    for thread_id in 0..args.concurrency {
        let stats = Arc::clone(&stats);
        let engine1 = args.engine1.clone();
        let engine2 = args.engine2.clone();
        let total_games = args.games;

        let handle = thread::spawn(move || {
            let start_game = thread_id * games_per_thread;
            let end_game = ((thread_id + 1) * games_per_thread).min(total_games);

            for game_num in start_game..end_game {
                let engine1_white = game_num % 2 == 0;

                match play_game(&engine1, &engine2, base_time, inc_time, engine1_white) {
                    Ok(result) => stats.record(result),
                    Err(_e) => {
                        stats.record(GameResult::Draw);
                    }
                }
            }
        });

        handles.push(handle);
    }

    // Progress display
    let stats_display = Arc::clone(&stats);
    let name1_display = args.name1.clone();
    let name2_display = args.name2.clone();
    let total_games = args.games;

    let display_handle = thread::spawn(move || {
        loop {
            let (_, _, _, completed) = stats_display.get_stats();
            print_progress(&stats_display, &name1_display, &name2_display, total_games);
            if completed >= total_games {
                break;
            }
            thread::sleep(Duration::from_millis(250));
        }
    });

    for handle in handles {
        handle.join().unwrap();
    }
    display_handle.join().unwrap();

    // Final results
    let elapsed = start_time.elapsed();
    let (w1, w2, d, _) = stats.get_stats();
    let total = w1 + w2 + d;

    println!("\n");
    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║                       FINAL RESULTS                        ║");
    println!("╠════════════════════════════════════════════════════════════╣");

    let (elo_diff, elo_low, elo_high) = calculate_elo_difference(w2 as f64, w1 as f64, d as f64);

    println!("║ {:<20} {:>4} wins ({:>5.1}%)                      ║",
        &args.name1, w1, w1 as f64 / total as f64 * 100.0);
    println!("║ {:<20} {:>4} wins ({:>5.1}%)                      ║",
        &args.name2, w2, w2 as f64 / total as f64 * 100.0);
    println!("║ {:<20} {:>4}       ({:>5.1}%)                      ║",
        "Draws", d, d as f64 / total as f64 * 100.0);
    println!("╠════════════════════════════════════════════════════════════╣");

    let score2 = (w2 as f64 + d as f64 * 0.5) / total as f64 * 100.0;
    println!("║ {} score: {:.1}%                                      ║", &args.name2, score2);

    if elo_diff.is_finite() {
        println!("║ Elo difference: {:+.0} [{:+.0}, {:+.0}] (95% CI)             ║",
            elo_diff, elo_low, elo_high);
    } else {
        println!("║ Elo difference: {} (one side won all games)           ║",
            if elo_diff > 0.0 { "+INF" } else { "-INF" });
    }

    println!("╠════════════════════════════════════════════════════════════╣");
    println!("║ Total time: {:.1}s ({:.1} games/sec)                         ║",
        elapsed.as_secs_f64(), total as f64 / elapsed.as_secs_f64());
    println!("╚════════════════════════════════════════════════════════════╝");

    // SPRT
    if total >= 10 {
        let llr = sprt_llr(w2 as f64, w1 as f64, d as f64);
        println!();
        if llr > 2.94 {
            println!("✓ SPRT: {} is significantly STRONGER (LLR={:.2} > 2.94)", args.name2, llr);
        } else if llr < -2.94 {
            println!("✗ SPRT: {} is significantly WEAKER (LLR={:.2} < -2.94)", args.name2, llr);
        } else {
            println!("? SPRT: Inconclusive (LLR={:.2}, need |LLR| > 2.94)", llr);
        }
    }
}

fn sprt_llr(wins: f64, losses: f64, draws: f64) -> f64 {
    let n = wins + losses + draws;
    if n == 0.0 { return 0.0; }

    let score = (wins + draws * 0.5) / n;
    let p0 = 0.5;
    let p1 = 1.0 / (1.0 + 10.0_f64.powf(-10.0 / 400.0));

    if score <= 0.0 || score >= 1.0 {
        return if score > 0.5 { 10.0 } else { -10.0 };
    }

    n * (score * (score / p0).ln() + (1.0 - score) * ((1.0 - score) / (1.0 - p0)).ln())
        - n * (score * (score / p1).ln() + (1.0 - score) * ((1.0 - score) / (1.0 - p1)).ln())
}
