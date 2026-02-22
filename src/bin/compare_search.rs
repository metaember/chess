//! Compare old search vs new MovePicker-based search performance

use rust_chess::board::Board;
use rust_chess::search::{iterative_deepening_movepicker, minimax};
use rust_chess::tt::TranspositionTable;
use std::time::Instant;

fn main() {
    println!("Search Performance Comparison");
    println!("==============================\n");

    // Test positions
    let positions = vec![
        ("Starting position", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
        ("Italian Game", "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"),
        ("Sicilian", "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"),
        ("Middlegame", "r2qkb1r/ppp2ppp/2n1bn2/3pp3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R w KQkq - 0 6"),
        ("Complex", "r1bq1rk1/ppp2ppp/2n1pn2/3p4/1bPP4/2NBPN2/PP3PPP/R1BQK2R w KQ - 0 7"),
    ];

    let depths = [4, 5, 6];

    for (name, fen) in &positions {
        println!("Position: {}", name);
        println!("FEN: {}\n", fen);

        let board = Board::from_fen(fen);

        for &depth in &depths {
            println!("  Depth {}:", depth);

            // Old search (minimax with TT)
            let board_old = board.clone();
            let start = Instant::now();
            let old_result = minimax(depth, &board_old);
            let old_time = start.elapsed();

            let (old_nodes, old_move) = match &old_result {
                Ok(r) => (
                    r.nodes_searched,
                    r.best_move.as_ref().map(|m| m.to_algebraic()).unwrap_or_else(|| "none".to_string())
                ),
                Err(_) => (0, "error".to_string()),
            };

            // New MovePicker search
            let mut board_new = board.clone();
            let original_fen = board_new.to_fen();
            let mut tt = TranspositionTable::new(16); // 16 MB
            let start = Instant::now();
            let new_result = iterative_deepening_movepicker(&mut board_new, depth, &mut tt);
            let new_time = start.elapsed();

            // Verify board state is unchanged
            let post_fen = board_new.to_fen();
            if original_fen != post_fen {
                println!("    WARNING: Board state changed during search!");
                println!("    Before: {}", original_fen);
                println!("    After:  {}", post_fen);
            }

            let new_move = new_result.best_move
                .as_ref()
                .map(|m| format!("{} ({}->{})", m.to_algebraic(), m.from.to_algebraic(), m.to.to_algebraic()))
                .unwrap_or_else(|| "none".to_string());

            // Compare
            let speedup = old_time.as_secs_f64() / new_time.as_secs_f64();

            println!("    Old: {:>8.2}ms, {:>8} nodes, move: {}",
                old_time.as_secs_f64() * 1000.0,
                old_nodes,
                old_move
            );

            println!("    New: {:>8.2}ms, {:>8} nodes, move: {}",
                new_time.as_secs_f64() * 1000.0,
                new_result.nodes_searched,
                new_move
            );

            let speedup_pct = (speedup - 1.0) * 100.0;
            let speedup_str = if speedup > 1.0 {
                format!("+{:.1}% faster", speedup_pct)
            } else {
                format!("{:.1}% slower", -speedup_pct)
            };
            println!("    Speedup: {:.2}x ({})\n", speedup, speedup_str);
        }
        println!();
    }
}
