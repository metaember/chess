use rust_chess::board::Board;
use rust_chess::search::minimax;
use rust_chess::types::Status;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::process::Command;
use std::time::Instant;

#[derive(Debug, Serialize, Deserialize)]
struct BenchmarkResult {
    timestamp: String,
    git_commit: String,
    git_message: String,
    results: BenchmarkMetrics,
}

#[derive(Debug, Serialize, Deserialize)]
struct BenchmarkMetrics {
    // Perft results (nodes per second)
    perft_depth_4_nps: f64,
    perft_depth_5_nps: f64,

    // Search timing (ms per move)
    search_depth_4_mean_ms: f64,
    search_depth_5_mean_ms: f64,
    search_depth_6_mean_ms: f64,

    // Nodes searched
    search_depth_4_nodes: u64,
    search_depth_5_nodes: u64,
    search_depth_6_nodes: u64,

    // Moves evaluated in test games
    total_moves_benchmarked: usize,
}

const HISTORY_FILE: &str = "benchmark_history.json";

fn main() {
    println!("Chess Engine Performance Tracker");
    println!("=================================\n");

    // Get git info
    let git_commit = get_git_commit();
    let git_message = get_git_message();
    let timestamp = chrono::Utc::now().to_rfc3339();

    println!("Commit: {} - {}", &git_commit[..8.min(git_commit.len())], git_message);
    println!("Time:   {}\n", timestamp);

    // Run benchmarks
    let metrics = run_benchmarks();

    // Create result
    let result = BenchmarkResult {
        timestamp,
        git_commit,
        git_message,
        results: metrics,
    };

    // Save to history
    save_result(&result);

    // Print summary
    print_summary(&result);

    // Compare with previous if available
    compare_with_previous(&result);
}

fn get_git_commit() -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string())
}

fn get_git_message() -> String {
    Command::new("git")
        .args(["log", "-1", "--pretty=%s"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string())
}

fn run_benchmarks() -> BenchmarkMetrics {
    println!("Running benchmarks...\n");

    // Perft benchmarks
    println!("[1/3] Perft benchmarks...");
    let (perft_4_nps, perft_5_nps) = benchmark_perft();

    // Search benchmarks from starting position
    println!("[2/3] Search benchmarks (starting position)...");
    let (search_4, search_5, search_6) = benchmark_search_starting();

    // Search benchmarks from game play
    println!("[3/3] Search benchmarks (game simulation)...");
    let (game_timings, total_moves) = benchmark_game_search();

    // Calculate mean timings from game play
    let search_4_mean = calculate_mean(&game_timings.0);
    let search_5_mean = calculate_mean(&game_timings.1);
    let search_6_mean = calculate_mean(&game_timings.2);

    BenchmarkMetrics {
        perft_depth_4_nps: perft_4_nps,
        perft_depth_5_nps: perft_5_nps,
        search_depth_4_mean_ms: search_4_mean,
        search_depth_5_mean_ms: search_5_mean,
        search_depth_6_mean_ms: search_6_mean,
        search_depth_4_nodes: search_4,
        search_depth_5_nodes: search_5,
        search_depth_6_nodes: search_6,
        total_moves_benchmarked: total_moves,
    }
}

fn benchmark_perft() -> (f64, f64) {
    let mut board = Board::new();

    // Depth 4
    let start = Instant::now();
    let nodes_4 = perft(&mut board, 4);
    let time_4 = start.elapsed().as_secs_f64();
    let nps_4 = nodes_4 as f64 / time_4;
    println!("  Depth 4: {} nodes in {:.2}s ({:.0} nps)", nodes_4, time_4, nps_4);

    // Depth 5
    let start = Instant::now();
    let nodes_5 = perft(&mut board, 5);
    let time_5 = start.elapsed().as_secs_f64();
    let nps_5 = nodes_5 as f64 / time_5;
    println!("  Depth 5: {} nodes in {:.2}s ({:.0} nps)", nodes_5, time_5, nps_5);

    (nps_4, nps_5)
}

fn perft(board: &mut Board, depth: u8) -> u64 {
    if depth == 0 {
        return 1;
    }

    let color = board.get_active_color();
    let moves = match board.get_legal_moves(&color) {
        Ok(moves) => moves,
        Err(_) => return 0,
    };

    if depth == 1 {
        return moves.len() as u64;
    }

    let mut nodes = 0;
    for m in moves {
        let undo = board.make_move(&m);
        nodes += perft(board, depth - 1);
        board.unmake_move(&undo);
    }
    nodes
}

fn benchmark_search_starting() -> (u64, u64, u64) {
    let board = Board::new();

    // Depth 4
    let result_4 = minimax(4, &board).unwrap();
    println!("  Depth 4: {} nodes", result_4.nodes_searched);

    // Depth 5
    let result_5 = minimax(5, &board).unwrap();
    println!("  Depth 5: {} nodes", result_5.nodes_searched);

    // Depth 6
    let result_6 = minimax(6, &board).unwrap();
    println!("  Depth 6: {} nodes", result_6.nodes_searched);

    (result_4.nodes_searched as u64, result_5.nodes_searched as u64, result_6.nodes_searched as u64)
}

fn benchmark_game_search() -> ((Vec<f64>, Vec<f64>, Vec<f64>), usize) {
    let mut timings_4: Vec<f64> = Vec::new();
    let mut timings_5: Vec<f64> = Vec::new();
    let mut timings_6: Vec<f64> = Vec::new();

    // Play a short game, timing searches at depths 4, 5, 6
    let mut board = Board::new();
    let max_moves = 20; // 10 moves per side

    for move_num in 0..max_moves {
        let color = board.get_active_color();
        match board.get_legal_moves(&color) {
            Ok(moves) if moves.is_empty() => break,
            Err(Status::Checkmate(_)) | Err(Status::Stalemate) => break,
            Err(_) => break,
            Ok(_) => {}
        }

        if board.check_for_insufficient_material().is_some() {
            break;
        }

        // Time depth 4
        let start = Instant::now();
        let _ = minimax(4, &board);
        timings_4.push(start.elapsed().as_secs_f64() * 1000.0);

        // Time depth 5
        let start = Instant::now();
        let _ = minimax(5, &board);
        timings_5.push(start.elapsed().as_secs_f64() * 1000.0);

        // Time depth 6 and use result for the move
        let start = Instant::now();
        let result = minimax(6, &board);
        timings_6.push(start.elapsed().as_secs_f64() * 1000.0);

        // Make the best move
        if let Ok(r) = result {
            if let Some(m) = r.best_move {
                board.make_move(&m);
            } else {
                break;
            }
        } else {
            break;
        }

        if move_num % 5 == 4 {
            print!(".");
            std::io::stdout().flush().unwrap();
        }
    }
    println!(" done ({} positions)", timings_4.len());

    let total = timings_4.len();
    ((timings_4, timings_5, timings_6), total)
}

fn calculate_mean(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    values.iter().sum::<f64>() / values.len() as f64
}

fn save_result(result: &BenchmarkResult) {
    // Load existing history
    let mut history: Vec<BenchmarkResult> = load_history();

    // Check if this commit already exists
    let commit_exists = history.iter().any(|r| r.git_commit == result.git_commit);
    if commit_exists {
        println!("\nNote: Updating existing entry for commit {}", &result.git_commit[..8]);
        history.retain(|r| r.git_commit != result.git_commit);
    }

    // Add new result
    history.push(BenchmarkResult {
        timestamp: result.timestamp.clone(),
        git_commit: result.git_commit.clone(),
        git_message: result.git_message.clone(),
        results: BenchmarkMetrics {
            perft_depth_4_nps: result.results.perft_depth_4_nps,
            perft_depth_5_nps: result.results.perft_depth_5_nps,
            search_depth_4_mean_ms: result.results.search_depth_4_mean_ms,
            search_depth_5_mean_ms: result.results.search_depth_5_mean_ms,
            search_depth_6_mean_ms: result.results.search_depth_6_mean_ms,
            search_depth_4_nodes: result.results.search_depth_4_nodes,
            search_depth_5_nodes: result.results.search_depth_5_nodes,
            search_depth_6_nodes: result.results.search_depth_6_nodes,
            total_moves_benchmarked: result.results.total_moves_benchmarked,
        },
    });

    // Save
    let file = File::create(HISTORY_FILE).expect("Failed to create history file");
    let writer = BufWriter::new(file);
    serde_json::to_writer_pretty(writer, &history).expect("Failed to write history");

    println!("\nResults saved to {}", HISTORY_FILE);
}

fn load_history() -> Vec<BenchmarkResult> {
    if let Ok(file) = File::open(HISTORY_FILE) {
        let reader = BufReader::new(file);
        serde_json::from_reader(reader).unwrap_or_default()
    } else {
        Vec::new()
    }
}

fn print_summary(result: &BenchmarkResult) {
    println!("\n{}", "=".repeat(50));
    println!("BENCHMARK RESULTS");
    println!("{}", "=".repeat(50));

    println!("\nPerft (nodes/second - higher is better):");
    println!("  Depth 4: {:>12.0} nps", result.results.perft_depth_4_nps);
    println!("  Depth 5: {:>12.0} nps", result.results.perft_depth_5_nps);

    println!("\nSearch time (ms/move - lower is better):");
    println!("  Depth 4: {:>8.1} ms", result.results.search_depth_4_mean_ms);
    println!("  Depth 5: {:>8.1} ms", result.results.search_depth_5_mean_ms);
    println!("  Depth 6: {:>8.1} ms", result.results.search_depth_6_mean_ms);

    println!("\nNodes searched (starting position):");
    println!("  Depth 4: {:>12}", result.results.search_depth_4_nodes);
    println!("  Depth 5: {:>12}", result.results.search_depth_5_nodes);
    println!("  Depth 6: {:>12}", result.results.search_depth_6_nodes);
}

fn compare_with_previous(current: &BenchmarkResult) {
    let history = load_history();

    // Find previous result (not the one we just saved)
    let previous = history.iter()
        .filter(|r| r.git_commit != current.git_commit)
        .last();

    if let Some(prev) = previous {
        println!("\n{}", "=".repeat(50));
        println!("COMPARISON vs {}", &prev.git_commit[..8.min(prev.git_commit.len())]);
        println!("{}", "=".repeat(50));

        // Perft comparison
        let perft_4_change = percent_change(prev.results.perft_depth_4_nps, current.results.perft_depth_4_nps);
        let perft_5_change = percent_change(prev.results.perft_depth_5_nps, current.results.perft_depth_5_nps);

        println!("\nPerft (+ = faster):");
        println!("  Depth 4: {:>+6.1}%", perft_4_change);
        println!("  Depth 5: {:>+6.1}%", perft_5_change);

        // Search time comparison (negative is better)
        let search_4_change = -percent_change(prev.results.search_depth_4_mean_ms, current.results.search_depth_4_mean_ms);
        let search_5_change = -percent_change(prev.results.search_depth_5_mean_ms, current.results.search_depth_5_mean_ms);
        let search_6_change = -percent_change(prev.results.search_depth_6_mean_ms, current.results.search_depth_6_mean_ms);

        println!("\nSearch time (+ = faster):");
        println!("  Depth 4: {:>+6.1}%", search_4_change);
        println!("  Depth 5: {:>+6.1}%", search_5_change);
        println!("  Depth 6: {:>+6.1}%", search_6_change);

        // Node count comparison (fewer = better move ordering/pruning)
        let nodes_4_change = -percent_change(prev.results.search_depth_4_nodes as f64, current.results.search_depth_4_nodes as f64);
        let nodes_5_change = -percent_change(prev.results.search_depth_5_nodes as f64, current.results.search_depth_5_nodes as f64);
        let nodes_6_change = -percent_change(prev.results.search_depth_6_nodes as f64, current.results.search_depth_6_nodes as f64);

        println!("\nNodes searched (+ = fewer nodes, better pruning):");
        println!("  Depth 4: {:>+6.1}%", nodes_4_change);
        println!("  Depth 5: {:>+6.1}%", nodes_5_change);
        println!("  Depth 6: {:>+6.1}%", nodes_6_change);
    } else {
        println!("\nNo previous benchmark to compare against.");
    }
}

fn percent_change(old: f64, new: f64) -> f64 {
    if old == 0.0 {
        return 0.0;
    }
    ((new - old) / old) * 100.0
}
