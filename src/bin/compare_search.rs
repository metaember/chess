//! Compare search performance across different positions and depths

use rust_chess::board::Board;
use rust_chess::search::iterative_deepening_movepicker;
use rust_chess::tt::TranspositionTable;
use std::time::Instant;

fn main() {
    println!("Search Performance");
    println!("==================\n");

    let positions = vec![
        ("Starting position", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
        ("Italian Game", "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"),
        ("Sicilian", "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"),
        ("Middlegame", "r2qkb1r/ppp2ppp/2n1bn2/3pp3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R w KQkq - 0 6"),
        ("Complex", "r1bq1rk1/ppp2ppp/2n1pn2/3p4/1bPP4/2NBPN2/PP3PPP/R1BQK2R w KQ - 0 7"),
    ];

    let depths = [4, 5];

    for (name, fen) in &positions {
        println!("Position: {}", name);
        for &depth in &depths {
            let mut board = Board::from_fen(fen);
            let mut tt = TranspositionTable::new(16);
            let start = Instant::now();
            let result = iterative_deepening_movepicker(&mut board, depth, &mut tt);
            let elapsed = start.elapsed();
            println!("  d{}: {:>7.1}ms  {:>7} nodes  {}",
                depth,
                elapsed.as_secs_f64() * 1000.0,
                result.nodes_searched,
                result.best_move.map(|m| m.to_algebraic()).unwrap_or_else(|| "none".to_string()),
            );
        }
        println!();
    }
}
