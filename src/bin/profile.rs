use rust_chess::board::Board;
use rust_chess::search::iterative_deepening_movepicker;
use rust_chess::tt::TranspositionTable;
use std::time::Instant;

const SEB_FEN: &str = "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1";

fn main() {
    let mut b = Board::from_fen(SEB_FEN);

    println!("=== Chess Engine Performance ===\n");

    for depth in 4..=8u8 {
        println!("Depth {} ITERATIVE DEEPENING MOVEPICKER (64MB TT):", depth);
        let mut tt = TranspositionTable::new(64);
        let start = Instant::now();
        let result = iterative_deepening_movepicker(&mut b, depth, &mut tt);
        let elapsed = start.elapsed();
        println!("  Time: {:?}", elapsed);
        println!("  Main nodes: {}", result.nodes_searched);
        println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
        println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
        println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
        println!("  {}\n", tt.info());
    }

    println!("Done!");
}
