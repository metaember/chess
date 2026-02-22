//! MovePicker - Stockfish-style staged move generation for high-performance search.
//!
//! This module implements staged move generation:
//! 1. TT move (from transposition table) - most likely to cause cutoff
//! 2. Good captures (MVV-LVA sorted, optionally SEE filtered)
//! 3. Killer moves (quiet moves that caused cutoffs at this ply)
//! 4. Quiet moves (sorted by history heuristic)
//!
//! The key insight is that most nodes achieve beta cutoff on the first few moves,
//! so generating ALL moves and sorting them is wasteful. By generating moves in
//! stages, we often cut off before generating quiet moves at all.
//!
//! The MovePicker returns moves by VALUE (copying 4 bytes), avoiding borrow checker
//! issues during recursive search.

use crate::board::Board;
use crate::movegen::CompactMoveGenerator;
use crate::movelist::MoveList;
use crate::types::{Color, CompactMove, MoveType, PieceType};

/// Generation stages
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Stage {
    TTMove,
    GenerateCaptures,
    GoodCaptures,
    Killer1,
    Killer2,
    GenerateQuiets,
    Quiets,
    Done,
}

/// Scoring constants for move ordering
pub const TT_MOVE_SCORE: i16 = 30000;
pub const GOOD_CAPTURE_BASE: i16 = 20000;
pub const KILLER1_SCORE: i16 = 9000;
pub const KILLER2_SCORE: i16 = 8000;

/// MVV-LVA piece values for capture scoring
/// Index by piece type: Pawn, Rook, Knight, Bishop, Queen, King
const MVV_VALUES: [i16; 6] = [100, 500, 320, 330, 900, 0];
const LVA_VALUES: [i16; 6] = [1, 5, 3, 3, 9, 10];

/// MovePicker: Stockfish-style staged move generation.
/// Lives on the stack at each search node - no heap allocation!
pub struct MovePicker {
    /// Move storage - all on stack (~1.5KB)
    moves: MoveList,

    /// Current iteration state
    stage: Stage,
    current_idx: usize,

    /// TT move to try first (highest probability of cutoff)
    tt_move: CompactMove,

    /// Killer moves for this ply
    killer1: CompactMove,
    killer2: CompactMove,

    /// Side to move
    color: Color,

    /// Captures-only mode (for quiescence search)
    captures_only: bool,
}

impl MovePicker {
    /// Create a new MovePicker for the main search.
    ///
    /// # Arguments
    /// * `tt_move` - Best move from transposition table (if any)
    /// * `killers` - Killer moves for this ply [killer1, killer2]
    /// * `color` - Side to move
    #[inline]
    pub fn new(
        tt_move: Option<CompactMove>,
        killers: [Option<CompactMove>; 2],
        color: Color,
    ) -> Self {
        Self {
            moves: MoveList::new(),
            stage: if tt_move.is_some() && tt_move != Some(CompactMove::NONE) {
                Stage::TTMove
            } else {
                Stage::GenerateCaptures
            },
            current_idx: 0,
            tt_move: tt_move.unwrap_or(CompactMove::NONE),
            killer1: killers[0].unwrap_or(CompactMove::NONE),
            killer2: killers[1].unwrap_or(CompactMove::NONE),
            color,
            captures_only: false,
        }
    }

    /// Create a new MovePicker for quiescence search (captures only).
    #[inline]
    pub fn new_captures_only(color: Color) -> Self {
        Self {
            moves: MoveList::new(),
            stage: Stage::GenerateCaptures,
            current_idx: 0,
            tt_move: CompactMove::NONE,
            killer1: CompactMove::NONE,
            killer2: CompactMove::NONE,
            color,
            captures_only: true,
        }
    }

    /// Get the next move to try.
    /// Returns None when all moves have been exhausted.
    ///
    /// This is the main interface - call this in a loop during search.
    /// The move is returned by VALUE (4 bytes copied), avoiding borrow issues.
    #[inline]
    pub fn next_move(&mut self, board: &Board, history: &[[i16; 64]; 64]) -> Option<CompactMove> {
        loop {
            match self.stage {
                Stage::TTMove => {
                    self.stage = Stage::GenerateCaptures;
                    if self.tt_move != CompactMove::NONE
                        && board.is_pseudo_legal_compact(&self.tt_move)
                    {
                        return Some(self.tt_move);
                    }
                }

                Stage::GenerateCaptures => {
                    self.generate_captures(board);
                    self.score_captures();
                    self.stage = Stage::GoodCaptures;
                    self.current_idx = 0;
                }

                Stage::GoodCaptures => {
                    while self.current_idx < self.moves.len() {
                        // Selection sort: find best remaining capture
                        let mv = self.moves.pick_best(self.current_idx);
                        self.current_idx += 1;

                        // Skip TT move (already tried)
                        if mv == self.tt_move {
                            continue;
                        }

                        return Some(mv);
                    }

                    // Done with captures
                    if self.captures_only {
                        self.stage = Stage::Done;
                    } else {
                        self.stage = Stage::Killer1;
                    }
                }

                Stage::Killer1 => {
                    self.stage = Stage::Killer2;

                    if self.killer1 != CompactMove::NONE
                        && self.killer1 != self.tt_move
                        && !self.killer1.is_capture()
                        && board.is_pseudo_legal_compact(&self.killer1)
                    {
                        return Some(self.killer1);
                    }
                }

                Stage::Killer2 => {
                    self.stage = Stage::GenerateQuiets;

                    if self.killer2 != CompactMove::NONE
                        && self.killer2 != self.tt_move
                        && self.killer2 != self.killer1
                        && !self.killer2.is_capture()
                        && board.is_pseudo_legal_compact(&self.killer2)
                    {
                        return Some(self.killer2);
                    }
                }

                Stage::GenerateQuiets => {
                    self.generate_quiets(board);
                    self.score_quiets(history);
                    self.stage = Stage::Quiets;
                    self.current_idx = 0;
                }

                Stage::Quiets => {
                    while self.current_idx < self.moves.len() {
                        let mv = self.moves.pick_best(self.current_idx);
                        self.current_idx += 1;

                        // Skip already-tried moves
                        if mv == self.tt_move || mv == self.killer1 || mv == self.killer2 {
                            continue;
                        }

                        return Some(mv);
                    }
                    self.stage = Stage::Done;
                }

                Stage::Done => return None,
            }
        }
    }

    /// Generate capture moves into the internal MoveList.
    #[inline]
    fn generate_captures(&mut self, board: &Board) {
        self.moves.clear();
        let gen = CompactMoveGenerator::new(board, self.color);
        gen.generate_captures(&mut self.moves);
    }

    /// Generate quiet moves into the internal MoveList.
    #[inline]
    fn generate_quiets(&mut self, board: &Board) {
        self.moves.clear();
        let gen = CompactMoveGenerator::new(board, self.color);
        gen.generate_quiets(&mut self.moves);
    }

    /// Score captures using MVV-LVA (Most Valuable Victim - Least Valuable Attacker).
    /// Higher scores for capturing valuable pieces with less valuable attackers.
    #[inline]
    fn score_captures(&mut self) {
        for i in 0..self.moves.len() {
            let mv = self.moves.get(i);
            let score = mvv_lva_score(mv);
            self.moves.set_score(i, score);
        }
    }

    /// Score quiet moves using history heuristic.
    #[inline]
    fn score_quiets(&mut self, history: &[[i16; 64]; 64]) {
        for i in 0..self.moves.len() {
            let mv = self.moves.get(i);
            let from_sq = mv.from_sq() as usize;
            let to_sq = mv.to_sq() as usize;
            let score = history[from_sq][to_sq];
            self.moves.set_score(i, score);
        }
    }
}

/// Calculate MVV-LVA score for a capture move.
/// Higher score = better capture to try first.
/// MVV-LVA: prioritize capturing valuable pieces with less valuable attackers.
#[inline]
fn mvv_lva_score(mv: CompactMove) -> i16 {
    let victim_value = match mv.captured_type() {
        Some(pt) => MVV_VALUES[piece_type_to_idx(pt)],
        None => 0, // En passant captures a pawn
    };

    let attacker_value = LVA_VALUES[piece_type_to_idx(mv.piece_type())];

    // MVV-LVA: multiply victim by 10 to prioritize, subtract attacker
    GOOD_CAPTURE_BASE + (victim_value * 10) - attacker_value
}

/// Convert PieceType to index for MVV/LVA arrays.
#[inline]
const fn piece_type_to_idx(pt: PieceType) -> usize {
    match pt {
        PieceType::Pawn => 0,
        PieceType::Rook => 1,
        PieceType::Knight => 2,
        PieceType::Bishop => 3,
        PieceType::Queen => 4,
        PieceType::King => 5,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;

    #[test]
    fn test_movepicker_starting_position() {
        let board = Board::new();
        let history = [[0i16; 64]; 64];
        let mut picker = MovePicker::new(None, [None, None], Color::White);

        let mut move_count = 0;
        while let Some(_mv) = picker.next_move(&board, &history) {
            move_count += 1;
            if move_count > 100 {
                panic!("Too many moves generated");
            }
        }

        // Starting position has 20 legal moves for white
        assert_eq!(move_count, 20);
    }

    #[test]
    fn test_movepicker_captures_only() {
        // Position with some captures available
        let board = Board::from_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3");
        let mut picker = MovePicker::new_captures_only(Color::White);
        let history = [[0i16; 64]; 64];

        let mut captures = 0;
        while let Some(mv) = picker.next_move(&board, &history) {
            assert!(mv.is_capture(), "Non-capture move in captures_only mode");
            captures += 1;
        }

        // Nf3 can capture e5 pawn
        assert!(captures >= 1);
    }

    #[test]
    fn test_mvv_lva_ordering() {
        // Queen capture by pawn should score higher than pawn capture by queen
        let pawn_takes_queen = CompactMove::new_capture(8, 17, PieceType::Pawn, PieceType::Queen);
        let queen_takes_pawn = CompactMove::new_capture(56, 48, PieceType::Queen, PieceType::Pawn);

        let pxq_score = mvv_lva_score(pawn_takes_queen);
        let qxp_score = mvv_lva_score(queen_takes_pawn);

        assert!(
            pxq_score > qxp_score,
            "PxQ ({}) should score higher than QxP ({})",
            pxq_score,
            qxp_score
        );
    }
}
