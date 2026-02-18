use rust_chess::board::Board;
use rust_chess::search::{minimax, minimax_no_quiescence, minimax_with_tt, iterative_deepening};
use rust_chess::tt::TranspositionTable;
use std::time::Instant;

const SEB_FEN: &str = "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1";

fn main() {
    let mut b = Board::from_fen(SEB_FEN);

    println!("=== Chess Engine Performance ===\n");

    // Depth 4 without quiescence
    println!("Depth 4 WITHOUT quiescence (no TT):");
    let start = Instant::now();
    let result = minimax_no_quiescence(4, &b).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Nodes: {}", result.nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());

    // Depth 4 with quiescence
    println!("\nDepth 4 WITH quiescence (no TT):");
    let start = Instant::now();
    let result = minimax(4, &b).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());

    // Depth 4 with TT
    println!("\nDepth 4 WITH TT (64MB):");
    let mut tt = TranspositionTable::new(64);
    let start = Instant::now();
    let result = minimax_with_tt(4, &mut b, &mut tt).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
    println!("  {}", tt.info());

    // Depth 5 with TT
    println!("\nDepth 5 WITH TT (64MB):");
    tt.clear();
    let start = Instant::now();
    let result = minimax_with_tt(5, &mut b, &mut tt).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
    println!("  {}", tt.info());

    // Depth 5 with iterative deepening
    println!("\nDepth 5 ITERATIVE DEEPENING (64MB TT):");
    tt.clear();
    let start = Instant::now();
    let result = iterative_deepening(5, &mut b, &mut tt).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
    println!("  {}", tt.info());

    // Depth 6 with iterative deepening
    println!("\nDepth 6 ITERATIVE DEEPENING (64MB TT):");
    tt.clear();
    let start = Instant::now();
    let result = iterative_deepening(6, &mut b, &mut tt).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
    println!("  {}", tt.info());

    // Depth 7 with iterative deepening
    println!("\nDepth 7 ITERATIVE DEEPENING (64MB TT):");
    tt.clear();
    let start = Instant::now();
    let result = iterative_deepening(7, &mut b, &mut tt).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
    println!("  {}", tt.info());

    // Depth 8 with iterative deepening
    println!("\nDepth 8 ITERATIVE DEEPENING (64MB TT):");
    tt.clear();
    let start = Instant::now();
    let result = iterative_deepening(8, &mut b, &mut tt).unwrap();
    let elapsed = start.elapsed();
    println!("  Time: {:?}", elapsed);
    println!("  Main nodes: {}", result.nodes_searched);
    println!("  Quiescent nodes: {}", result.quiescent_nodes_searched);
    println!("  Total nodes: {}", result.nodes_searched + result.quiescent_nodes_searched);
    println!("  Best move: {}", result.best_move.unwrap().to_algebraic());
    println!("  {}", tt.info());

    println!("\nDone!");
}
