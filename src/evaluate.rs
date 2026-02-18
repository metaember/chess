use crate::board::Board;
use crate::types::{Color, Move, Piece, PieceType};

/// Maximum game phase value (all major/minor pieces on board)
/// Queens = 4, Rooks = 2, Bishops = 1, Knights = 1
/// Total: 2*4 + 4*2 + 4*1 + 4*1 = 8 + 8 + 4 + 4 = 24
pub const MAX_PHASE: i32 = 24;

/// Phase weights for each piece type
pub const PHASE_WEIGHTS: [i32; 6] = [
    0, // Pawn
    2, // Rook
    1, // Knight
    1, // Bishop
    4, // Queen
    0, // King
];

pub struct Material {
    pub white_material: i32,
    pub white_pst_mg: i32, // middlegame PST score
    pub white_pst_eg: i32, // endgame PST score
    pub white_pawn_material: i32,
    pub white_piece_material: i32,

    pub black_material: i32,
    pub black_pst_mg: i32, // middlegame PST score
    pub black_pst_eg: i32, // endgame PST score
    pub black_pawn_material: i32,
    pub black_piece_material: i32,

    /// Game phase (0 = endgame, MAX_PHASE = opening/middlegame)
    pub phase: i32,
}

impl Material {
    pub fn get_material_difference(&self) -> i32 {
        self.white_material - self.black_material
    }

    /// Get tapered evaluation score (interpolates between middlegame and endgame)
    pub fn get_tapered_score(&self) -> i32 {
        let mg_score = self.white_material + self.white_pst_mg
            - self.black_material - self.black_pst_mg;
        let eg_score = self.white_material + self.white_pst_eg
            - self.black_material - self.black_pst_eg;

        // Interpolate between mg and eg based on phase
        // phase = MAX_PHASE means full middlegame, phase = 0 means full endgame
        let phase = self.phase.clamp(0, MAX_PHASE);
        (mg_score * phase + eg_score * (MAX_PHASE - phase)) / MAX_PHASE
    }

    pub fn piece_to_material(piece_type: PieceType) -> i32 {
        match piece_type {
            PieceType::Pawn => 100,
            PieceType::Rook => 500,
            PieceType::Knight => 300,
            PieceType::Bishop => 320, // Bishop is worth slightly more than a knight
            PieceType::Queen => 900,
            PieceType::King => 0,
        }
    }

    pub fn piece_to_phase(piece_type: PieceType) -> i32 {
        PHASE_WEIGHTS[piece_type_to_index(piece_type)]
    }

    pub fn compute_material(board: &Board) -> Material {
        let mut white_material = 0;
        let mut black_material = 0;
        let mut white_pst_mg = 0;
        let mut white_pst_eg = 0;
        let mut black_pst_mg = 0;
        let mut black_pst_eg = 0;
        let mut white_pawn_material = 0;
        let mut black_pawn_material = 0;
        let mut white_piece_material = 0;
        let mut black_piece_material = 0;
        let mut phase = 0;

        for piece in &board.pieces {
            let current_piece_material = Material::piece_to_material(piece.piece_type);
            phase += Material::piece_to_phase(piece.piece_type);

            match piece.color {
                Color::White => {
                    white_material += current_piece_material;
                    white_pst_mg += get_piece_pst_value(&piece, false);
                    white_pst_eg += get_piece_pst_value(&piece, true);
                    if piece.piece_type == PieceType::Pawn {
                        white_pawn_material += current_piece_material;
                    } else {
                        white_piece_material += current_piece_material
                    };
                }
                Color::Black => {
                    black_material += current_piece_material;
                    black_pst_mg += get_piece_pst_value(&piece, false);
                    black_pst_eg += get_piece_pst_value(&piece, true);
                    if piece.piece_type == PieceType::Pawn {
                        black_pawn_material += current_piece_material;
                    } else {
                        black_piece_material += current_piece_material;
                    }
                }
            };
        }
        Material {
            white_material,
            white_pst_mg,
            white_pst_eg,
            white_pawn_material,
            white_piece_material,
            black_material,
            black_pst_mg,
            black_pst_eg,
            black_pawn_material,
            black_piece_material,
            phase,
        }
    }
}

/// Convert PieceType to index for phase weights array
#[inline(always)]
fn piece_type_to_index(pt: PieceType) -> usize {
    match pt {
        PieceType::Pawn => 0,
        PieceType::Rook => 1,
        PieceType::Knight => 2,
        PieceType::Bishop => 3,
        PieceType::Queen => 4,
        PieceType::King => 5,
    }
}

pub fn evaluate_board(board: &Board) -> i32 {
    // Use incrementally maintained evaluation scores
    let unsigned_score = board.get_tapered_score();
    if board.get_active_color() == Color::White {
        unsigned_score
    } else {
        -unsigned_score
    }
}

/// Evaluate board by computing material from scratch (for verification)
pub fn evaluate_board_slow(board: &Board) -> i32 {
    let material = Material::compute_material(board);
    let unsigned_score = material.get_tapered_score();
    if board.get_active_color() == Color::White {
        unsigned_score
    } else {
        -unsigned_score
    }
}

pub fn guess_move_value(_board: &Board, mv: &Move) -> i32 {
    let mut score = 0;
    let material_difference_multiplier = 10;

    if let Some(captured_piece) = mv.captured {
        score += material_difference_multiplier
            * (Material::piece_to_material(captured_piece.piece_type)
                - Material::piece_to_material(mv.piece.piece_type));
    };

    // TODO: add promotion bonus equal to the value of the promoted piece
    // score

    // TODO: penalize moves that move a piece into a position where it can be captured by a pawn
    // thse shold be cached when we evaluate the board before the move
    score
}

/// Get PST value for a piece (used in Material::compute_material)
fn get_piece_pst_value(piece: &Piece, is_endgame: bool) -> i32 {
    get_pst_value(piece.piece_type, piece.color, piece.position.rank, piece.position.file, is_endgame)
}

/// Get PST value given piece type, color, and position
/// This is the core PST lookup function used by both evaluation and incremental updates
pub fn get_pst_value(piece_type: PieceType, color: Color, rank: u8, file: u8, is_endgame: bool) -> i32 {
    let raw_table = get_raw_pst_table(piece_type, is_endgame);
    // PST tables are stored with rank 8 at index 0-7 and rank 1 at index 56-63
    // (from White's perspective, looking at the board with rank 1 at bottom)
    let index = match color {
        // White: rank 8 -> indices 0-7, rank 1 -> indices 56-63
        Color::White => (8 - rank) * 8 + file - 1,
        // Black: mirror perspective - rank 1 -> indices 0-7, rank 8 -> indices 56-63
        // This ensures Black's home rank (8) uses same values as White's home rank (1)
        Color::Black => (rank - 1) * 8 + file - 1,
    };
    raw_table[index as usize]
}

fn get_raw_pst_table(piece_type: PieceType, is_endgame: bool) -> &'static [i32; 64] {
    match piece_type {
        PieceType::Pawn => {
            if is_endgame {
                &PAWNS_END
            } else {
                &PAWNS
            }
        }
        PieceType::Rook => &ROOKS,
        PieceType::Knight => &KNIGHTS,
        PieceType::Bishop => &BISHOPS,
        PieceType::Queen => &QUEENS,
        PieceType::King => {
            if is_endgame {
                &KING_END
            } else {
                &KING_START
            }
        }
    }
}

// This part of the code aattributes different weights to pieces in different positions
// stolen from : https://github.com/SebLague/Chess-Coding-Adventure/blob/Chess-V2-UCI/Chess-Coding-Adventure/src/Core/Evaluation/PieceSquareTable.cs

const PAWNS: [i32; 8 * 8] = [
    0, 0, 0, 0, 0, 0, 0, 0, 50, 50, 50, 50, 50, 50, 50, 50, 10, 10, 20, 30, 30, 20, 10, 10, 5, 5,
    10, 25, 25, 10, 5, 5, 0, 0, 0, 20, 20, 0, 0, 0, 5, -5, -10, 0, 0, -10, -5, 5, 5, 10, 10, -20,
    -20, 10, 10, 5, 0, 0, 0, 0, 0, 0, 0, 0,
];

const PAWNS_END: [i32; 8 * 8] = [
    0, 0, 0, 0, 0, 0, 0, 0, 80, 80, 80, 80, 80, 80, 80, 80, 50, 50, 50, 50, 50, 50, 50, 50, 30, 30,
    30, 30, 30, 30, 30, 30, 20, 20, 20, 20, 20, 20, 20, 20, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0,
];

const ROOKS: [i32; 8 * 8] = [
    0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 10, 10, 10, 10, 10, 5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0,
    0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, 0, 0,
    0, 5, 5, 0, 0, 0,
];

const KNIGHTS: [i32; 8 * 8] = [
    -50, -40, -30, -30, -30, -30, -40, -50, -40, -20, 0, 0, 0, 0, -20, -40, -30, 0, 10, 15, 15, 10,
    0, -30, -30, 5, 15, 20, 20, 15, 5, -30, -30, 0, 15, 20, 20, 15, 0, -30, -30, 5, 10, 15, 15, 10,
    5, -30, -40, -20, 0, 5, 5, 0, -20, -40, -50, -40, -30, -30, -30, -30, -40, -50,
];

const BISHOPS: [i32; 8 * 8] = [
    -20, -10, -10, -10, -10, -10, -10, -20, -10, 0, 0, 0, 0, 0, 0, -10, -10, 0, 5, 10, 10, 5, 0,
    -10, -10, 5, 5, 10, 10, 5, 5, -10, -10, 0, 10, 10, 10, 10, 0, -10, -10, 10, 10, 10, 10, 10, 10,
    -10, -10, 5, 0, 0, 0, 0, 5, -10, -20, -10, -10, -10, -10, -10, -10, -20,
];

const QUEENS: [i32; 8 * 8] = [
    -20, -10, -10, -5, -5, -10, -10, -20, -10, 0, 0, 0, 0, 0, 0, -10, -10, 0, 5, 5, 5, 5, 0, -10,
    -5, 0, 5, 5, 5, 5, 0, -5, 0, 0, 5, 5, 5, 5, 0, -5, -10, 5, 5, 5, 5, 5, 0, -10, -10, 0, 5, 0, 0,
    0, 0, -10, -20, -10, -10, -5, -5, -10, -10, -20,
];

// const KING_START: [i32; 8*8] = [
//     -80, -70, -70, -70, -70, -70, -70, -80,
//     -60, -60, -60, -60, -60, -60, -60, -60,
//     -40, -50, -50, -60, -60, -50, -50, -40,
//     -30, -40, -40, -50, -50, -40, -40, -30,
//     -20, -30, -30, -40, -40, -30, -30, -20,
//     -10, -20, -20, -20, -20, -20, -20, -10,
//     20,  20,  -5,   -5,  -5,  -5,  20,  20,
//     20,  30,  10,    0,   0,  10,  30,  20
// ];

const KING_START: [i32; 8 * 8] = [
    -80, -70, -70, -70, -70, -70, -70, -80, -60, -60, -60, -60, -60, -60, -60, -60, -40, -50, -50,
    -60, -60, -50, -50, -40, -30, -40, -40, -50, -50, -40, -40, -30, -20, -30, -30, -40, -40, -30,
    -30, -20, -10, -20, -20, -20, -20, -20, -20, -10, 20, 20, -15, -15, -15, -5, 20, 20, 20, 30,
    10, -10, 0, 10, 30, 20,
];

const KING_END: [i32; 8 * 8] = [
    -20, -10, -10, -10, -10, -10, -10, -20, -5, 0, 5, 5, 5, 5, 0, -5, -10, -5, 20, 30, 30, 20, -5,
    -10, -15, -10, 35, 45, 45, 35, -10, -15, -20, -15, 30, 40, 40, 30, -15, -20, -25, -20, 20, 25,
    25, 20, -20, -25, -30, -25, 0, 0, 0, 0, -25, -30, -50, -30, -30, -30, -30, -30, -30, -50,
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;
    use crate::types::{Color, Piece, PieceType, Position};

    /// Test that PST index calculation is correct for both colors.
    /// White pieces should use (rank-1)*8 + (file-1)
    /// Black pieces should use mirrored rank: (8-rank)*8 + (file-1)
    #[test]
    fn test_pst_index_bounds() {
        // Test all 64 squares for both colors to ensure indices are in bounds
        for rank in 1..=8 {
            for file in 1..=8 {
                let white_piece = Piece {
                    color: Color::White,
                    piece_type: PieceType::Pawn,
                    position: Position { rank, file },
                };
                let black_piece = Piece {
                    color: Color::Black,
                    piece_type: PieceType::Pawn,
                    position: Position { rank, file },
                };

                // These should not panic - if index is out of bounds, we'll crash
                let white_value = get_piece_pst_value(&white_piece, false);
                let black_value = get_piece_pst_value(&black_piece, false);

                // Values should be in reasonable range
                assert!(
                    white_value >= -100 && white_value <= 100,
                    "White pawn PST value out of range at ({}, {}): {}",
                    rank,
                    file,
                    white_value
                );
                assert!(
                    black_value >= -100 && black_value <= 100,
                    "Black pawn PST value out of range at ({}, {}): {}",
                    rank,
                    file,
                    black_value
                );
            }
        }
    }

    /// Test that mirrored positions have symmetric PST values.
    /// A white pawn on e4 should have the same PST bonus as a black pawn on e5
    /// (mirrored across the center of the board).
    #[test]
    fn test_pst_symmetry_for_mirrored_positions() {
        // White pawn on rank 4, file 5 (e4) should have same value as
        // Black pawn on rank 5, file 5 (e5) - they're both "4 ranks from home"
        let white_pawn_e4 = Piece {
            color: Color::White,
            piece_type: PieceType::Pawn,
            position: Position { rank: 4, file: 5 },
        };
        let black_pawn_e5 = Piece {
            color: Color::Black,
            piece_type: PieceType::Pawn,
            position: Position { rank: 5, file: 5 },
        };

        let white_value = get_piece_pst_value(&white_pawn_e4, false);
        let black_value = get_piece_pst_value(&black_pawn_e5, false);

        assert_eq!(
            white_value, black_value,
            "Mirrored pawn positions should have equal PST values. \
             White e4 = {}, Black e5 = {}",
            white_value, black_value
        );
    }

    /// Test that a knight in the center has higher value than on the edge.
    /// This is a sanity check that PST tables are applied correctly.
    #[test]
    fn test_knight_center_vs_edge() {
        let knight_center = Piece {
            color: Color::White,
            piece_type: PieceType::Knight,
            position: Position { rank: 4, file: 4 }, // d4
        };
        let knight_edge = Piece {
            color: Color::White,
            piece_type: PieceType::Knight,
            position: Position { rank: 1, file: 1 }, // a1
        };

        let center_value = get_piece_pst_value(&knight_center, false);
        let edge_value = get_piece_pst_value(&knight_edge, false);

        assert!(
            center_value > edge_value,
            "Knight in center should be worth more than knight on edge. \
             Center (d4) = {}, Edge (a1) = {}",
            center_value,
            edge_value
        );
    }

    /// Test that the starting position evaluates to 0 (symmetric).
    /// Both sides have identical material and identical (mirrored) piece positions.
    #[test]
    fn test_starting_position_is_equal() {
        let board = Board::new();
        let material = Material::compute_material(&board);

        // Material should be equal
        assert_eq!(
            material.white_material, material.black_material,
            "Starting position material should be equal"
        );

        // PST adjustments should also be equal (symmetric position)
        assert_eq!(
            material.white_pst_mg,
            material.black_pst_mg,
            "Starting position PST adjustments (middlegame) should be equal. \
             White = {}, Black = {}",
            material.white_pst_mg,
            material.black_pst_mg
        );
        assert_eq!(
            material.white_pst_eg,
            material.black_pst_eg,
            "Starting position PST adjustments (endgame) should be equal. \
             White = {}, Black = {}",
            material.white_pst_eg,
            material.black_pst_eg
        );

        // Overall evaluation should be 0
        let eval = evaluate_board(&board);
        assert_eq!(eval, 0, "Starting position should evaluate to 0");
    }

    /// Test that mirrored positions with equal advantages evaluate equally.
    /// evaluate_board returns score from side-to-move perspective, so:
    /// - Position where White is up, White to move -> positive
    /// - Mirrored position where Black is up, Black to move -> also positive (same magnitude)
    #[test]
    fn test_mirrored_positions_evaluate_equally() {
        // Position with white having a pawn advantage
        // White pawn on e4 (good central control)
        let white_advantage_fen = "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1";
        let white_board = Board::from_fen(white_advantage_fen);

        // Mirror: Black pawn on e5 (same relative position from Black's perspective)
        let black_advantage_fen = "4k3/8/8/4p3/8/8/8/4K3 b - - 0 1";
        let black_board = Board::from_fen(black_advantage_fen);

        let white_eval = evaluate_board(&white_board);
        let black_eval = evaluate_board(&black_board);

        // Both should be positive and equal since each side is evaluating
        // their own advantage from their own perspective
        assert_eq!(
            white_eval, black_eval,
            "Mirrored advantage positions should evaluate equally from each side's perspective. \
             White advantage (White to move) = {}, Black advantage (Black to move) = {}",
            white_eval, black_eval
        );
    }

    /// Test that the raw material difference calculation is symmetric.
    #[test]
    fn test_material_difference_symmetric() {
        // Same position, different side to move - the unsigned material diff should be same
        let fen_white_move = "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1";
        let fen_black_move = "4k3/8/8/8/4P3/8/8/4K3 b - - 0 1";

        let board_white = Board::from_fen(fen_white_move);
        let board_black = Board::from_fen(fen_black_move);

        let mat_white = Material::compute_material(&board_white);
        let mat_black = Material::compute_material(&board_black);

        // The raw material difference should be the same regardless of side to move
        assert_eq!(
            mat_white.get_tapered_score(),
            mat_black.get_tapered_score(),
            "Raw material difference should not depend on side to move"
        );

        // But evaluate_board should flip sign based on side to move
        let eval_white = evaluate_board(&board_white);
        let eval_black = evaluate_board(&board_black);
        assert_eq!(
            eval_white, -eval_black,
            "Same position with different side to move should have opposite evaluations. \
             White to move = {}, Black to move = {}",
            eval_white, eval_black
        );
    }

    /// Test that all piece types have valid PST lookups for all squares.
    #[test]
    fn test_all_piece_types_pst_valid() {
        let piece_types = [
            PieceType::Pawn,
            PieceType::Knight,
            PieceType::Bishop,
            PieceType::Rook,
            PieceType::Queen,
            PieceType::King,
        ];

        for piece_type in piece_types {
            for rank in 1..=8 {
                for file in 1..=8 {
                    // Skip invalid pawn positions (rank 1 and 8)
                    if piece_type == PieceType::Pawn && (rank == 1 || rank == 8) {
                        continue;
                    }

                    let white_piece = Piece {
                        color: Color::White,
                        piece_type,
                        position: Position { rank, file },
                    };
                    let black_piece = Piece {
                        color: Color::Black,
                        piece_type,
                        position: Position { rank, file },
                    };

                    // Should not panic
                    let _white_val = get_piece_pst_value(&white_piece, false);
                    let _black_val = get_piece_pst_value(&black_piece, false);
                    let _white_val_end = get_piece_pst_value(&white_piece, true);
                    let _black_val_end = get_piece_pst_value(&black_piece, true);
                }
            }
        }
    }

    /// Test that advanced pawns have higher PST values than pawns on starting rank.
    #[test]
    fn test_advanced_pawns_worth_more() {
        let pawn_rank_2 = Piece {
            color: Color::White,
            piece_type: PieceType::Pawn,
            position: Position { rank: 2, file: 4 }, // d2
        };
        let pawn_rank_6 = Piece {
            color: Color::White,
            piece_type: PieceType::Pawn,
            position: Position { rank: 6, file: 4 }, // d6
        };

        let rank_2_value = get_piece_pst_value(&pawn_rank_2, false);
        let rank_6_value = get_piece_pst_value(&pawn_rank_6, false);

        assert!(
            rank_6_value > rank_2_value,
            "Advanced pawn should be worth more. Rank 2 = {}, Rank 6 = {}",
            rank_2_value,
            rank_6_value
        );
    }

    /// Test that incremental evaluation matches slow evaluation after a series of moves.
    #[test]
    fn test_incremental_eval_matches_slow_eval() {
        let mut board = Board::new();

        // Play some moves and verify incremental eval matches slow eval
        let moves = [
            ("e2", "e4"),  // 1. e4
            ("e7", "e5"),  // 1... e5
            ("g1", "f3"),  // 2. Nf3
            ("b8", "c6"),  // 2... Nc6
            ("f1", "b5"),  // 3. Bb5
            ("a7", "a6"),  // 3... a6
            ("b5", "a4"),  // 4. Ba4
            ("g8", "f6"),  // 4... Nf6
        ];

        for (from, to) in moves {
            let mv = crate::types::Move::from_algebraic(&board, from, to);
            let undo = board.make_move(&mv);

            let fast_eval = evaluate_board(&board);
            let slow_eval = evaluate_board_slow(&board);

            assert_eq!(
                fast_eval, slow_eval,
                "Incremental eval {} != slow eval {} after move {} -> {}",
                fast_eval, slow_eval, from, to
            );

            // Also verify after unmake
            board.unmake_move(&undo);
            let fast_eval_after_unmake = evaluate_board(&board);
            let slow_eval_after_unmake = evaluate_board_slow(&board);
            assert_eq!(
                fast_eval_after_unmake, slow_eval_after_unmake,
                "Incremental eval {} != slow eval {} after unmake of {} -> {}",
                fast_eval_after_unmake, slow_eval_after_unmake, from, to
            );

            // Re-make the move to continue the sequence
            board.make_move(&mv);
        }
    }

    /// Test incremental evaluation with captures
    #[test]
    fn test_incremental_eval_with_captures() {
        // Start from a position with captures available
        let mut board = Board::from_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3");

        // Nf3xe5 (capture)
        let moves = board.get_legal_moves(&Color::White).unwrap();
        let capture_move = moves.iter().find(|m| m.from.to_algebraic() == "f3" && m.to.to_algebraic() == "e5").unwrap();

        let undo = board.make_move(capture_move);
        let fast_eval = evaluate_board(&board);
        let slow_eval = evaluate_board_slow(&board);

        assert_eq!(
            fast_eval, slow_eval,
            "Incremental eval {} != slow eval {} after capture",
            fast_eval, slow_eval
        );

        board.unmake_move(&undo);
        let fast_eval_unmake = evaluate_board(&board);
        let slow_eval_unmake = evaluate_board_slow(&board);

        assert_eq!(
            fast_eval_unmake, slow_eval_unmake,
            "Incremental eval {} != slow eval {} after unmake capture",
            fast_eval_unmake, slow_eval_unmake
        );
    }

    /// Test incremental evaluation with castling
    #[test]
    fn test_incremental_eval_with_castling() {
        // Position where white can castle kingside
        let mut board = Board::from_fen("r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4");

        // O-O (kingside castle)
        let moves = board.get_legal_moves(&Color::White).unwrap();
        let castle_move = moves.iter().find(|m| m.move_flag == crate::types::MoveFlag::CastleKingside).unwrap();

        let undo = board.make_move(castle_move);
        let fast_eval = evaluate_board(&board);
        let slow_eval = evaluate_board_slow(&board);

        assert_eq!(
            fast_eval, slow_eval,
            "Incremental eval {} != slow eval {} after castling",
            fast_eval, slow_eval
        );

        board.unmake_move(&undo);
        let fast_eval_unmake = evaluate_board(&board);
        let slow_eval_unmake = evaluate_board_slow(&board);

        assert_eq!(
            fast_eval_unmake, slow_eval_unmake,
            "Incremental eval {} != slow eval {} after unmake castling",
            fast_eval_unmake, slow_eval_unmake
        );
    }
}
