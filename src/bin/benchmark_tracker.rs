use rust_chess::board::Board;
use rust_chess::search::iterative_deepening_movepicker;
use rust_chess::tt::TranspositionTable;
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

    // Search timing on middlegame positions (ms per move, depths 4-5)
    search_depth_4_mean_ms: f64,
    search_depth_5_mean_ms: f64,

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

    // Search benchmarks from middlegame positions (depths 4 and 5 only — d6 is too slow on complex positions)
    println!("[3/3] Search benchmarks (middlegame positions)...");
    let (mg_timings, total_moves) = benchmark_middlegame_search();

    BenchmarkMetrics {
        perft_depth_4_nps: perft_4_nps,
        perft_depth_5_nps: perft_5_nps,
        search_depth_4_mean_ms: calculate_mean(&mg_timings.0),
        search_depth_5_mean_ms: calculate_mean(&mg_timings.1),
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
    let mut tt = TranspositionTable::new(16);

    // Depth 4 (fresh TT each time for reproducibility)
    let mut board = Board::new();
    let result_4 = iterative_deepening_movepicker(&mut board, 4, &mut tt);
    println!("  Depth 4: {} nodes", result_4.nodes_searched);

    // Depth 5
    tt = TranspositionTable::new(16);
    let mut board = Board::new();
    let result_5 = iterative_deepening_movepicker(&mut board, 5, &mut tt);
    println!("  Depth 5: {} nodes", result_5.nodes_searched);

    // Depth 6
    tt = TranspositionTable::new(16);
    let mut board = Board::new();
    let result_6 = iterative_deepening_movepicker(&mut board, 6, &mut tt);
    println!("  Depth 6: {} nodes", result_6.nodes_searched);

    (result_4.nodes_searched as u64, result_5.nodes_searched as u64, result_6.nodes_searched as u64)
}

/// Well-known middlegame test positions from public test suites.
/// Sources: Bratko-Kopec, WAC (Win At Chess), Eigenmann, and standard engine benchmarks.
/// Well-known middlegame test positions from public test suites.
/// Sources: Bratko-Kopec, WAC (Win At Chess), and standard engine benchmarks.
/// 4-field FEN shorthand (omitting halfmove/fullmove) is supported.
const MIDDLEGAME_POSITIONS: &[(&str, &str)] = &[
    // Bratko-Kopec positions (complex middlegame)
    ("r1bq1rk1/pp2nppp/2n1p3/3pP3/3P4/2N2N2/PP3PPP/R1BQ1RK1 w - -", "BK: French structure"),
    ("r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -", "BK: Italian game"),
    ("2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - -", "BK: Complex middlegame"),
    ("r1bq1rk1/1pp2pbp/p1np1np1/4p3/2PPP3/2N1BP2/PP1QN1PP/R3KB1R w KQ -", "BK: King's Indian"),

    // WAC positions (tactical middlegame)
    ("r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq -", "WAC: Tactical tension"),
    ("r1b1kb1r/pppp1ppp/5n2/4p1q1/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -", "WAC: Open center"),
    ("r2q1rk1/ppp2ppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP2PPP/R2QR1K1 w - -", "WAC: Symmetrical tension"),

    // Standard engine test positions
    ("rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq -", "Sicilian structure"),
    ("r1bqr1k1/pp3pbp/2np1np1/3Pp3/2P5/2N1BP2/PP1QN1PP/R3KB1R w KQ -", "Closed center"),
    ("r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - -", "Kingside attack"),
    ("r1bq1r1k/1pp1n1pp/1p1p4/4p2n/2PP1p2/2N1PB1P/PP3PP1/R2Q1RK1 w - -", "Pawn tension"),
    ("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - -", "Rich middlegame"),

    // Endgame-ish positions (to test tapered eval)
    ("8/pp2k1pp/2p1Bp2/3p4/3P4/2P2N2/PP3PPP/4K3 w - -", "Minor piece endgame"),
    ("r1r3k1/1pq2ppp/p7/3bN3/3B4/7P/PPP2PP1/R2QR1K1 w - -", "Piece activity"),

    // Positions with clear pawn structure features
    ("r2q1rk1/pp2ppbp/2p2np1/6B1/3PP3/2N5/PPP2PPP/R2QR1K1 w - -", "Isolated d-pawn"),
    ("r1bq1rk1/pp3ppp/2n1pn2/2pp4/1bPP4/2NBPN2/PP3PPP/R1BQ1RK1 w - -", "Hanging pawns"),
];

fn benchmark_middlegame_search() -> ((Vec<f64>, Vec<f64>), usize) {
    let mut timings_4: Vec<f64> = Vec::new();
    let mut timings_5: Vec<f64> = Vec::new();

    println!("  {:>30}  {:>8}  {:>8}  {:>10}", "Position", "d4 (ms)", "d5 (ms)", "d5 nodes");
    for (_i, (fen, label)) in MIDDLEGAME_POSITIONS.iter().enumerate() {
        // Time depth 4 (fresh TT per position)
        let mut board = Board::from_fen(fen);
        let mut tt = TranspositionTable::new(16);
        let start = Instant::now();
        let _ = iterative_deepening_movepicker(&mut board, 4, &mut tt);
        let t4 = start.elapsed().as_secs_f64() * 1000.0;
        timings_4.push(t4);

        // Time depth 5 (fresh TT per position)
        let mut board = Board::from_fen(fen);
        let mut tt = TranspositionTable::new(16);
        let start = Instant::now();
        let r5 = iterative_deepening_movepicker(&mut board, 5, &mut tt);
        let t5 = start.elapsed().as_secs_f64() * 1000.0;
        timings_5.push(t5);
        let nodes5 = r5.nodes_searched;

        println!("  {:>30}  {:>8.1}  {:>8.1}  {:>10}", label, t4, t5, nodes5);
    }
    println!("  {:>30}  {:>8.1}  {:>8.1}", "MEAN", calculate_mean(&timings_4), calculate_mean(&timings_5));

    let total = timings_4.len();
    ((timings_4, timings_5), total)
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

    println!("\nSearch time on middlegame positions (ms/move - lower is better):");
    println!("  Depth 4: {:>8.1} ms", result.results.search_depth_4_mean_ms);
    println!("  Depth 5: {:>8.1} ms", result.results.search_depth_5_mean_ms);

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

        // Search time comparison on middlegame positions (negative is better)
        let search_4_change = -percent_change(prev.results.search_depth_4_mean_ms, current.results.search_depth_4_mean_ms);
        let search_5_change = -percent_change(prev.results.search_depth_5_mean_ms, current.results.search_depth_5_mean_ms);

        println!("\nSearch time on middlegame positions (+ = faster):");
        println!("  Depth 4: {:>+6.1}%", search_4_change);
        println!("  Depth 5: {:>+6.1}%", search_5_change);

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
