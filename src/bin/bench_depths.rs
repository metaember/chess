//! Benchmark search performance at depths 1-12

use rust_chess::board::Board;
use rust_chess::search::iterative_deepening_movepicker;
use rust_chess::tt::TranspositionTable;
use std::time::Instant;

fn main() {
    let positions = [
        ("Start", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
        ("Italian", "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"),
        ("Middlegame", "r2qkb1r/ppp2ppp/2n1bn2/3pp3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R w KQkq - 0 6"),
    ];

    println!("Search Depth Benchmark (MovePicker engine)");
    println!("==========================================");
    println!("{:>5} {:>12} {:>15} {:>12}", "Depth", "Time (ms)", "Nodes", "kN/s");
    println!("--------------------------------------------------");

    for depth in 1..=12 {
        let mut total_time_ms = 0u128;
        let mut total_nodes = 0u64;

        for (_name, fen) in &positions {
            let mut board = Board::from_fen(fen);
            let mut tt = TranspositionTable::new(64); // 64 MB

            let start = Instant::now();
            let result = iterative_deepening_movepicker(&mut board, depth, &mut tt);
            let elapsed = start.elapsed();

            total_time_ms += elapsed.as_millis();
            total_nodes += result.nodes_searched as u64;
        }

        let avg_time_ms = total_time_ms / 3;
        let avg_nodes = total_nodes / 3;
        let knps = if avg_time_ms > 0 {
            (avg_nodes as u128 * 1000 / avg_time_ms) / 1000
        } else {
            avg_nodes as u128
        };

        println!("{:>5} {:>12} {:>15} {:>12}", depth, avg_time_ms, avg_nodes, knps);

        // Stop if taking too long (> 60s per depth)
        if avg_time_ms > 60000 {
            println!("... stopping (search taking > 60s)");
            break;
        }
    }
}
