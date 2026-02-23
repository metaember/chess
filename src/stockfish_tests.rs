//! Tests that compare engine move generation against Stockfish reference data.
//!
//! These tests use pre-generated move data from Stockfish to validate that
//! our engine generates the same legal moves.

use crate::board::Board;
use crate::movegen::CompactMoveGenerator;
use crate::movelist::MoveList;
use crate::types::{Color, Move, MoveFlag, PieceType, Position};
use serde::Deserialize;
use std::collections::BTreeSet;

#[derive(Debug, Deserialize)]
struct PositionData {
    id: String,
    fen: String,
    legal_moves: Vec<String>,
    move_count: usize,
}

#[derive(Debug, Deserialize)]
struct ReferenceData {
    generated_with: String,
    positions: Vec<PositionData>,
}

/// Convert a Position to UCI format (e.g., Position{rank:1, file:1} -> "a1")
fn position_to_uci(pos: &Position) -> String {
    let file_char = (b'a' + pos.file - 1) as char;
    let rank_char = (b'0' + pos.rank) as char;
    format!("{}{}", file_char, rank_char)
}

/// Convert a Move to UCI format (e.g., "e2e4", "e7e8q" for promotion)
fn move_to_uci(m: &Move) -> String {
    let from = position_to_uci(&m.from);
    let to = position_to_uci(&m.to);

    let promo = match m.move_flag {
        MoveFlag::Promotion(PieceType::Queen) => "q",
        MoveFlag::Promotion(PieceType::Rook) => "r",
        MoveFlag::Promotion(PieceType::Bishop) => "b",
        MoveFlag::Promotion(PieceType::Knight) => "n",
        _ => "",
    };

    format!("{}{}{}", from, to, promo)
}

fn load_reference_data() -> ReferenceData {
    let json = std::fs::read_to_string("test_data/stockfish_reference.json")
        .expect("Failed to read stockfish_reference.json - run `cargo run --bin generate_stockfish_reference` first");
    serde_json::from_str(&json).expect("Failed to parse stockfish_reference.json")
}

fn get_engine_moves(board: &Board) -> BTreeSet<String> {
    let color = board.get_active_color();
    match board.get_legal_moves(&color) {
        Ok(moves) => moves.iter().map(|m| move_to_uci(m)).collect(),
        Err(_) => BTreeSet::new(), // Checkmate or stalemate
    }
}

fn get_compact_engine_moves(board: &mut Board) -> BTreeSet<String> {
    let color = board.get_active_color();
    let gen = CompactMoveGenerator::new(board, color);
    let mut list = MoveList::new();
    gen.generate_all(&mut list);

    let mut moves = BTreeSet::new();
    for i in 0..list.len() {
        let mv = list.get(i);
        if board.is_legal_compact(&mv) {
            moves.insert(mv.to_uci());
        }
    }
    moves
}

fn compare_moves(position_id: &str, fen: &str, expected: &[String]) {
    let board = Board::from_fen(fen);
    let engine_moves = get_engine_moves(&board);
    let expected_moves: BTreeSet<String> = expected.iter().cloned().collect();

    let missing: Vec<_> = expected_moves.difference(&engine_moves).collect();
    let extra: Vec<_> = engine_moves.difference(&expected_moves).collect();

    if !missing.is_empty() || !extra.is_empty() {
        eprintln!("\n=== Position: {} ===", position_id);
        eprintln!("FEN: {}", fen);
        if !missing.is_empty() {
            eprintln!("Missing moves (Stockfish has, we don't): {:?}", missing);
        }
        if !extra.is_empty() {
            eprintln!("Extra moves (we have, Stockfish doesn't): {:?}", extra);
        }
        eprintln!("Expected {} moves, got {}", expected_moves.len(), engine_moves.len());
        panic!(
            "Position '{}': {} missing, {} extra moves",
            position_id,
            missing.len(),
            extra.len()
        );
    }
}

fn compare_moves_compact(position_id: &str, fen: &str, expected: &[String]) {
    let mut board = Board::from_fen(fen);
    let engine_moves = get_compact_engine_moves(&mut board);
    let expected_moves: BTreeSet<String> = expected.iter().cloned().collect();

    let missing: Vec<_> = expected_moves.difference(&engine_moves).collect();
    let extra: Vec<_> = engine_moves.difference(&expected_moves).collect();

    if !missing.is_empty() || !extra.is_empty() {
        eprintln!("\n=== CompactMoveGen Position: {} ===", position_id);
        eprintln!("FEN: {}", fen);
        if !missing.is_empty() {
            eprintln!("Missing moves (Stockfish has, we don't): {:?}", missing);
        }
        if !extra.is_empty() {
            eprintln!("Extra moves (we have, Stockfish doesn't): {:?}", extra);
        }
        eprintln!("Expected {} moves, got {}", expected_moves.len(), engine_moves.len());
        panic!(
            "CompactMoveGen '{}': {} missing, {} extra moves",
            position_id,
            missing.len(),
            extra.len()
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_stockfish_positions() {
        let reference = load_reference_data();
        println!("Testing against: {}", reference.generated_with);
        println!("Number of positions: {}", reference.positions.len());

        for pos in &reference.positions {
            print!("Testing '{}' ({} moves)... ", pos.id, pos.move_count);
            compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
            println!("OK");
        }
    }

    // Individual position tests for easier debugging
    #[test]
    fn stockfish_start() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "start").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_kiwipete() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "kiwipete").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_position5() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "position5").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_ep_white() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "ep_white").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_ep_black() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "ep_black").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_castle_both_white() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "castle_both_white").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_castle_both_black() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "castle_both_black").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_promo_white() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "promo_white").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_promo_black() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "promo_black").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    #[test]
    fn stockfish_check_simple() {
        let reference = load_reference_data();
        let pos = reference.positions.iter().find(|p| p.id == "check_simple").unwrap();
        compare_moves(&pos.id, &pos.fen, &pos.legal_moves);
    }

    // =========================================================================
    // CompactMoveGenerator validation against all Stockfish reference positions
    // =========================================================================

    #[test]
    fn test_all_stockfish_positions_compact() {
        let reference = load_reference_data();
        println!("Testing CompactMoveGenerator against: {}", reference.generated_with);
        println!("Number of positions: {}", reference.positions.len());

        for pos in &reference.positions {
            print!("Testing compact '{}' ({} moves)... ", pos.id, pos.move_count);
            compare_moves_compact(&pos.id, &pos.fen, &pos.legal_moves);
            println!("OK");
        }
    }

    // =========================================================================
    // Cross-validate: both generators must agree on every position
    // =========================================================================

    #[test]
    fn test_both_generators_agree_all_positions() {
        let reference = load_reference_data();
        for pos in &reference.positions {
            let legacy_moves = {
                let board = Board::from_fen(&pos.fen);
                get_engine_moves(&board)
            };
            let compact_moves = {
                let mut board = Board::from_fen(&pos.fen);
                get_compact_engine_moves(&mut board)
            };

            if legacy_moves != compact_moves {
                let only_legacy: Vec<_> = legacy_moves.difference(&compact_moves).collect();
                let only_compact: Vec<_> = compact_moves.difference(&legacy_moves).collect();
                eprintln!("\n=== Generators disagree on '{}' ===", pos.id);
                eprintln!("FEN: {}", pos.fen);
                if !only_legacy.is_empty() {
                    eprintln!("Only in legacy: {:?}", only_legacy);
                }
                if !only_compact.is_empty() {
                    eprintln!("Only in compact: {:?}", only_compact);
                }
                panic!(
                    "Generators disagree on '{}': legacy={}, compact={}",
                    pos.id,
                    legacy_moves.len(),
                    compact_moves.len()
                );
            }
        }
    }
}
