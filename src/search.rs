// use rayon::prelude::*;

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;

use crate::board::*;
use crate::book::Book;
use crate::evaluate::*;
use crate::movepicker::MovePicker;
use crate::tt::{TranspositionTable, TTFlag};
use crate::types::{Color, CompactMove, Move, MoveFlag, MoveType, PieceType, Position, Status};

pub const MIN_SCORE: i32 = -1_000_000_000;
pub const MAX_SCORE: i32 = 1_000_000_000;
/// Maximum search depth for killer move storage
pub const MAX_PLY: usize = 64;

/// Search state containing killer moves and history heuristic
pub struct SearchState {
    /// Killer moves: 2 quiet moves per ply that caused beta cutoffs
    /// These are moves that were good in sibling nodes and might be good here too
    pub killer_moves: [[Option<Move>; 2]; MAX_PLY],

    /// Compact killer moves for MovePicker (same data, different format)
    pub killer_moves_compact: [[Option<CompactMove>; 2]; MAX_PLY],

    /// History heuristic: [color][from_square][to_square] -> score
    /// Accumulates bonuses for quiet moves that cause beta cutoffs
    pub history: [[[i32; 64]; 64]; 2],

    /// Compact history for MovePicker (i16 version for MoveList scores)
    pub history_compact: [[[i16; 64]; 64]; 2],
}

impl SearchState {
    pub fn new() -> Self {
        SearchState {
            killer_moves: [[None; 2]; MAX_PLY],
            killer_moves_compact: [[None; 2]; MAX_PLY],
            history: [[[0; 64]; 64]; 2],
            history_compact: [[[0; 64]; 64]; 2],
        }
    }

    /// Store a killer move at the given ply
    /// If the move is already the first killer, do nothing
    /// Otherwise, shift first killer to second and store new move as first
    pub fn store_killer(&mut self, ply: usize, mv: Move) {
        if ply >= MAX_PLY {
            return;
        }
        // Don't store if it's already the first killer
        if self.killer_moves[ply][0] == Some(mv) {
            return;
        }
        // Shift first to second, store new as first
        self.killer_moves[ply][1] = self.killer_moves[ply][0];
        self.killer_moves[ply][0] = Some(mv);
    }

    /// Check if a move is a killer at the given ply
    pub fn is_killer(&self, ply: usize, mv: &Move) -> bool {
        if ply >= MAX_PLY {
            return false;
        }
        self.killer_moves[ply][0] == Some(*mv) || self.killer_moves[ply][1] == Some(*mv)
    }

    /// Get killer move priority (0 = first killer, 1 = second killer, None = not a killer)
    pub fn killer_priority(&self, ply: usize, mv: &Move) -> Option<usize> {
        if ply >= MAX_PLY {
            return None;
        }
        if self.killer_moves[ply][0] == Some(*mv) {
            Some(0)
        } else if self.killer_moves[ply][1] == Some(*mv) {
            Some(1)
        } else {
            None
        }
    }

    /// Add history bonus for a quiet move that caused a beta cutoff
    /// Bonus is scaled by depth² (deeper cutoffs are more valuable)
    pub fn add_history_bonus(&mut self, color: Color, from: Position, to: Position, depth: u8) {
        let color_idx = color as usize;
        let from_sq = position_to_square(from);
        let to_sq = position_to_square(to);
        let bonus = (depth as i32) * (depth as i32);

        // Cap history values to prevent overflow
        let new_value = self.history[color_idx][from_sq][to_sq].saturating_add(bonus);
        self.history[color_idx][from_sq][to_sq] = new_value.min(10000);
    }

    /// Get history score for a move
    pub fn get_history_score(&self, color: Color, from: Position, to: Position) -> i32 {
        let color_idx = color as usize;
        let from_sq = position_to_square(from);
        let to_sq = position_to_square(to);
        self.history[color_idx][from_sq][to_sq]
    }

    /// Age history scores (call at start of new search)
    /// Divides all scores by 2 to gradually forget old information
    pub fn age_history(&mut self) {
        for color in 0..2 {
            for from in 0..64 {
                for to in 0..64 {
                    self.history[color][from][to] /= 2;
                    self.history_compact[color][from][to] /= 2;
                }
            }
        }
    }

    // =========================================================================
    // CompactMove support methods
    // =========================================================================

    /// Store a compact killer move at the given ply
    #[inline]
    pub fn store_killer_compact(&mut self, ply: usize, mv: CompactMove) {
        if ply >= MAX_PLY {
            return;
        }
        // Don't store if it's already the first killer
        if self.killer_moves_compact[ply][0] == Some(mv) {
            return;
        }
        // Shift first to second, store new as first
        self.killer_moves_compact[ply][1] = self.killer_moves_compact[ply][0];
        self.killer_moves_compact[ply][0] = Some(mv);
    }

    /// Get compact killer moves for a given ply
    #[inline]
    pub fn get_killers_compact(&self, ply: usize) -> [Option<CompactMove>; 2] {
        if ply >= MAX_PLY {
            return [None, None];
        }
        self.killer_moves_compact[ply]
    }

    /// Add history bonus for a compact move that caused a beta cutoff
    #[inline]
    pub fn add_history_bonus_compact(&mut self, color: Color, mv: CompactMove, depth: u8) {
        let color_idx = color as usize;
        let from_sq = mv.from_sq() as usize;
        let to_sq = mv.to_sq() as usize;
        let bonus = (depth as i32) * (depth as i32);

        // Update both history tables
        let new_value = self.history[color_idx][from_sq][to_sq].saturating_add(bonus);
        self.history[color_idx][from_sq][to_sq] = new_value.min(10000);

        let bonus_i16 = (depth as i16) * (depth as i16);
        let new_compact = self.history_compact[color_idx][from_sq][to_sq].saturating_add(bonus_i16);
        self.history_compact[color_idx][from_sq][to_sq] = new_compact.min(10000);
    }

    /// Get compact history table for a color
    #[inline]
    pub fn get_history_compact(&self, color: Color) -> &[[i16; 64]; 64] {
        &self.history_compact[color as usize]
    }
}

impl Default for SearchState {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert position to square index (0-63)
#[inline]
fn position_to_square(pos: Position) -> usize {
    ((pos.rank - 1) * 8 + (pos.file - 1)) as usize
}

/// Piece values for SEE (centipawns)
const SEE_PIECE_VALUES: [i32; 6] = [100, 320, 330, 500, 900, 20000]; // P, N, B, R, Q, K

/// Static Exchange Evaluation (SEE)
/// Returns the expected material gain/loss from a capture sequence on the target square.
/// Positive = winning exchange, negative = losing exchange.
/// This is a simplified SEE that doesn't account for pins or x-ray attacks.
pub fn see(board: &Board, capture_move: &Move) -> i32 {
    let target_sq = ((capture_move.to.rank - 1) * 8 + (capture_move.to.file - 1)) as u8;

    // Initial gain is the captured piece value
    let captured_value = match &capture_move.captured {
        Some(p) => SEE_PIECE_VALUES[piece_type_to_see_index(p.piece_type)],
        None => return 0, // Not a capture
    };

    // Build the initial gain list
    // gain[0] = value of captured piece
    // gain[1] = gain[0] - value of capturing piece (if opponent recaptures)
    // etc.
    let mut gain = [0i32; 32];
    let mut depth = 0;
    gain[depth] = captured_value;

    // Track attacker value
    let mut attacker_value = SEE_PIECE_VALUES[piece_type_to_see_index(capture_move.piece.piece_type)];

    // Get occupied squares, remove the attacker
    let attacker_sq = ((capture_move.from.rank - 1) * 8 + (capture_move.from.file - 1)) as u8;
    let mut occupied = board.get_occupied();
    occupied &= !(1u64 << attacker_sq);

    // Start with the side that can recapture (opponent of side that moved)
    let mut side_to_move = capture_move.piece.color.other_color();

    loop {
        depth += 1;
        gain[depth] = attacker_value - gain[depth - 1];

        // If gain is so bad that even a perfect recapture can't help, prune
        if (-gain[depth]).max(gain[depth - 1]) < 0 {
            break;
        }

        // Find the least valuable attacker for side_to_move
        if let Some((piece_type, sq)) = find_least_valuable_attacker(board, target_sq, side_to_move, occupied) {
            attacker_value = SEE_PIECE_VALUES[piece_type_to_see_index(piece_type)];
            occupied &= !(1u64 << sq);
            side_to_move = side_to_move.other_color();
        } else {
            break; // No more attackers
        }
    }

    // Negamax the gain values
    while depth > 1 {
        depth -= 1;
        gain[depth] = -(-gain[depth]).max(gain[depth + 1]);
    }

    gain[0]
}

/// Find the least valuable piece of `color` that attacks `target_sq`
/// Returns (piece_type, square_of_attacker) or None
fn find_least_valuable_attacker(
    board: &Board,
    target_sq: u8,
    color: Color,
    occupied: u64,
) -> Option<(PieceType, u8)> {
    use crate::bitboard::{ATTACK_TABLES, BitboardIter, bishop_attacks, rook_attacks, queen_attacks};

    let target_bb = 1u64 << target_sq;
    let color_idx = color as usize;

    // Check pawns first (least valuable)
    let pawns = board.get_piece_bb(color, PieceType::Pawn) & occupied;
    for sq in BitboardIter(pawns) {
        let pawn_attack = ATTACK_TABLES.pawn[color_idx][sq as usize];
        if (pawn_attack & target_bb) != 0 {
            return Some((PieceType::Pawn, sq));
        }
    }

    // Check knights
    let knights = board.get_piece_bb(color, PieceType::Knight) & occupied;
    for sq in BitboardIter(knights) {
        let attacks = ATTACK_TABLES.knight[sq as usize];
        if (attacks & target_bb) != 0 {
            return Some((PieceType::Knight, sq));
        }
    }

    // Check bishops (magic bitboards)
    let bishops = board.get_piece_bb(color, PieceType::Bishop) & occupied;
    for sq in BitboardIter(bishops) {
        let attacks = bishop_attacks(sq, occupied);
        if (attacks & target_bb) != 0 {
            return Some((PieceType::Bishop, sq));
        }
    }

    // Check rooks (magic bitboards)
    let rooks = board.get_piece_bb(color, PieceType::Rook) & occupied;
    for sq in BitboardIter(rooks) {
        let attacks = rook_attacks(sq, occupied);
        if (attacks & target_bb) != 0 {
            return Some((PieceType::Rook, sq));
        }
    }

    // Check queens (magic bitboards)
    let queens = board.get_piece_bb(color, PieceType::Queen) & occupied;
    for sq in BitboardIter(queens) {
        let attacks = queen_attacks(sq, occupied);
        if (attacks & target_bb) != 0 {
            return Some((PieceType::Queen, sq));
        }
    }

    // Check king (last resort)
    let kings = board.get_piece_bb(color, PieceType::King) & occupied;
    for sq in BitboardIter(kings) {
        let attacks = ATTACK_TABLES.king[sq as usize];
        if (attacks & target_bb) != 0 {
            return Some((PieceType::King, sq));
        }
    }

    None
}

/// Convert PieceType to SEE index (0-5)
#[inline]
fn piece_type_to_see_index(piece_type: PieceType) -> usize {
    match piece_type {
        PieceType::Pawn => 0,
        PieceType::Knight => 1,
        PieceType::Bishop => 2,
        PieceType::Rook => 3,
        PieceType::Queen => 4,
        PieceType::King => 5,
    }
}

/// Search abort error - returned when time runs out mid-search
#[derive(Debug, Clone)]
pub struct SearchAborted;

/// Controls time management and provides abort signaling for search
#[derive(Debug)]
pub struct SearchControl {
    pub start_time: Instant,
    pub soft_limit_ms: u64,  // Target time - can extend slightly if finding good move
    pub hard_limit_ms: u64,  // Absolute max - never exceed
    pub stop: Arc<AtomicBool>,  // External stop signal (e.g., from UCI "stop" command)
    pub nodes_searched: AtomicU64,  // Track nodes for periodic time checks
}

impl SearchControl {
    pub fn new(soft_limit_ms: u64, hard_limit_ms: u64) -> Self {
        Self {
            start_time: Instant::now(),
            soft_limit_ms,
            hard_limit_ms,
            stop: Arc::new(AtomicBool::new(false)),
            nodes_searched: AtomicU64::new(0),
        }
    }

    /// Create a search control that never times out (for fixed depth searches)
    pub fn infinite() -> Self {
        Self {
            start_time: Instant::now(),
            soft_limit_ms: u64::MAX,
            hard_limit_ms: u64::MAX,
            stop: Arc::new(AtomicBool::new(false)),
            nodes_searched: AtomicU64::new(0),
        }
    }

    /// Check if we should abort the search (called periodically)
    #[inline]
    pub fn should_stop(&self) -> bool {
        // Check external stop signal
        if self.stop.load(Ordering::Relaxed) {
            return true;
        }

        // Only check time every 2048 nodes to minimize overhead
        let nodes = self.nodes_searched.fetch_add(1, Ordering::Relaxed);
        if nodes & 2047 == 0 {
            let elapsed = self.start_time.elapsed().as_millis() as u64;
            if elapsed >= self.hard_limit_ms {
                return true;
            }
        }
        false
    }

    /// Check if we've exceeded soft limit (used between depths)
    pub fn exceeded_soft_limit(&self) -> bool {
        self.start_time.elapsed().as_millis() as u64 >= self.soft_limit_ms
    }

    /// Get elapsed time in milliseconds
    pub fn elapsed_ms(&self) -> u64 {
        self.start_time.elapsed().as_millis() as u64
    }

    /// Signal the search to stop
    pub fn signal_stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }
}

impl Default for SearchControl {
    fn default() -> Self {
        Self::infinite()
    }
}

/// Calculate time allocation for a move given clock state
/// Returns (soft_limit_ms, hard_limit_ms)
pub fn allocate_time(
    time_left_ms: u64,
    increment_ms: u64,
    moves_to_go: Option<u32>,
) -> (u64, u64) {
    // Estimate moves remaining in game
    let moves_to_go = moves_to_go.unwrap_or(30) as u64;

    // Base time: divide remaining time by expected moves
    let base_time = time_left_ms / moves_to_go.max(1);

    // Add most of the increment
    let with_increment = base_time + (increment_ms * 3 / 4);

    // Soft limit: target time for this move
    let soft_limit = with_increment.min(time_left_ms / 4);

    // Hard limit: absolute max (never use more than 1/3 of remaining time)
    let hard_limit = (soft_limit * 3).min(time_left_ms / 3);

    // Ensure minimums
    let soft_limit = soft_limit.max(50);  // At least 50ms
    let hard_limit = hard_limit.max(100); // At least 100ms

    (soft_limit, hard_limit)
}

#[derive(Debug)]
pub struct SearchResult {
    pub best_move: Option<Move>,
    pub best_score: i32,
    pub nodes_searched: i32,
    pub quiescent_nodes_searched: i32,
}

impl SearchResult {
    #[allow(dead_code)]
    pub fn print(&self) {
        println!(
            "Search result: [{}, nodes: {} tot, {} quies] {}: {}",
            self.best_score,
            self.nodes_searched,
            self.quiescent_nodes_searched,
            self.best_move.unwrap().to_algebraic(),
            self.best_move.unwrap().to_human()
        );
    }
}

pub fn search_with_book(
    max_depth: u8,
    board: &Board,
    book: &Book,
) -> Result<SearchResult, Vec<Move>> {
    let book_move = book.suggest_move(board, true);
    if book_move.is_some() {
        return Ok(SearchResult {
            best_move: book_move,
            best_score: 0,
            nodes_searched: 0,
            quiescent_nodes_searched: 0,
            
        });
    }
    minimax(max_depth, board)
}

pub fn search(max_depth: u8, board: &Board) -> Move {
    let result = minimax(max_depth, board);
    if result.is_err() {
        let moves = result.err().unwrap();
        panic!(
            "Error {}",
            moves
                .into_iter()
                .map(|m| m.to_human() + "\n")
                .collect::<String>()
        );
    }
    let result = result.unwrap();
    result.best_move.expect("No move found")
}

pub fn minimax(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, true, true, true)
}

pub fn minimax_no_quiescence(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, true, true, false)
}

pub fn minimax_no_ordering(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, false, false, false)
}

pub fn minimax_no_pruning(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, false, false, false)
}

/// Search with transposition table (takes mutable board to avoid clone)
pub fn minimax_with_tt(
    max_depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    let mut search_state = SearchState::new();
    negamax_with_tt_mut(max_depth, board, alpha, beta, tt, &mut search_state, 0)
}

/// Aspiration window search with time control
pub fn aspiration_search_with_control(
    depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
    prev_score: i32,
    control: &SearchControl,
) -> Result<SearchResult, SearchAborted> {
    const INITIAL_WINDOW: i32 = 25;
    let mut window = INITIAL_WINDOW;
    let mut alpha = prev_score - window;
    let mut beta = prev_score + window;

    loop {
        let result = negamax_with_control(depth, board, alpha, beta, tt, control)?;

        // Check if we got a valid result within the window
        if result.best_score > alpha && result.best_score < beta {
            return Ok(result);
        }

        // Widen window and re-search
        if result.best_score <= alpha {
            alpha = (alpha - window * 2).max(MIN_SCORE);
        }
        if result.best_score >= beta {
            beta = (beta + window * 2).min(MAX_SCORE);
        }

        window *= 2;

        // If window gets too wide, just do full search
        if window > 400 {
            return negamax_with_control(depth, board, MIN_SCORE, MAX_SCORE, tt, control);
        }
    }
}

/// Negamax with time control - can abort mid-search
pub fn negamax_with_control(
    max_depth: u8,
    board: &mut Board,
    alpha: i32,
    beta: i32,
    tt: &mut TranspositionTable,
    control: &SearchControl,
) -> Result<SearchResult, SearchAborted> {
    // Check if we should abort
    if control.should_stop() {
        return Err(SearchAborted);
    }

    let original_alpha = alpha;
    let mut alpha = alpha;

    // Probe transposition table
    if let Some((score, best_move)) = tt.probe(board.zobrist_hash, max_depth, alpha, beta) {
        return Ok(SearchResult {
            best_move: best_move.map(|m| m.to_move(board)),
            best_score: score,
            nodes_searched: 0,
            quiescent_nodes_searched: 0,

        });
    }

    if max_depth == 0 {
        // Quiescence search at leaf (doesn't need time control - fast)
        return negamax_captures_only_mut(board, alpha, beta)
            .map_err(|_| SearchAborted);
    }

    // Null move pruning
    const NULL_MOVE_REDUCTION: u8 = 2;
    let active_color = board.get_active_color();
    if max_depth >= 3 && !board.is_in_check(active_color) && beta < MAX_SCORE - 1000 {
        let undo = board.make_null_move();
        let null_result = negamax_with_control(
            max_depth - 1 - NULL_MOVE_REDUCTION,
            board,
            -beta,
            -beta + 1,
            tt,
            control,
        );
        board.unmake_null_move(&undo);

        if let Ok(result) = null_result {
            let null_score = -result.best_score;
            if null_score >= beta {
                return Ok(SearchResult {
                    best_move: None,
                    best_score: beta,
                    nodes_searched: result.nodes_searched,
                    quiescent_nodes_searched: result.quiescent_nodes_searched,
                    
                });
            }
        }
    }

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: MIN_SCORE + (100 - max_depth as i32), // Prefer shorter mates
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                
            })
        }
        _ => return Err(SearchAborted),
    };

    // Move ordering
    let tt_best_move = tt.get_best_move(board.zobrist_hash);
    let legal_moves = {
        let mut moves_and_scores: Vec<_> = legal_moves
            .iter()
            .map(|m| {
                let score = if tt_best_move.map(|tm| tm == CompactMove::from_move(m)).unwrap_or(false) {
                    1_000_000
                } else {
                    guess_move_value(board, m)
                };
                (m, score)
            })
            .collect();
        moves_and_scores.sort_by(|a, b| b.1.cmp(&a.1));
        moves_and_scores.iter().map(|(m, _)| **m).collect::<Vec<Move>>()
    };

    let mut current_best_move = legal_moves[0];
    let mut current_best_score = MIN_SCORE;
    let mut total_nodes_searched = 0;
    let mut total_quiescent_nodes_searched = 0;

    const LMR_FULL_DEPTH_MOVES: usize = 4;
    const LMR_REDUCTION_LIMIT: u8 = 3;
    let in_check = board.is_in_check(active_color);

    for (move_index, m) in legal_moves.iter().enumerate() {
        if let Some(captured) = m.captured {
            if captured.piece_type == PieceType::King {
                return Err(SearchAborted);
            }
        }
        let undo = board.make_move(m);

        // Check for check extension
        let gives_check = board.is_in_check(board.get_active_color());
        let extension = if gives_check { 1 } else { 0 };

        // LMR
        let res = if move_index >= LMR_FULL_DEPTH_MOVES
            && max_depth >= LMR_REDUCTION_LIMIT
            && !in_check
            && !gives_check
            && m.captured.is_none()
            && !matches!(m.move_flag, MoveFlag::Promotion(_))
        {
            let reduced_result = negamax_with_control(
                max_depth - 2 + extension,
                board,
                -alpha - 1,
                -alpha,
                tt,
                control,
            );

            let needs_full_search = match &reduced_result {
                Ok(r) => -r.best_score > alpha,
                Err(_) => return Err(SearchAborted),
            };

            if needs_full_search {
                negamax_with_control(max_depth - 1 + extension, board, -beta, -alpha, tt, control)
            } else {
                reduced_result
            }
        } else {
            negamax_with_control(max_depth - 1 + extension, board, -beta, -alpha, tt, control)
        };
        board.unmake_move(&undo);

        match res {
            Ok(SearchResult {
                best_move: _,
                best_score: evaluation,
                nodes_searched,
                quiescent_nodes_searched,
                
            }) => {
                total_nodes_searched += nodes_searched + 1;
                total_quiescent_nodes_searched += quiescent_nodes_searched;
                let evaluation = -evaluation;

                if evaluation > current_best_score {
                    current_best_score = evaluation;
                    current_best_move = *m;
                }

                if evaluation >= beta {
                    tt.store(
                        board.zobrist_hash,
                        max_depth,
                        evaluation,
                        TTFlag::LowerBound,
                        Some(CompactMove::from_move(m)),
                    );
                    return Ok(SearchResult {
                        best_move: Some(*m),
                        best_score: beta,
                        nodes_searched: total_nodes_searched,
                        quiescent_nodes_searched: total_quiescent_nodes_searched,

                    });
                }
                alpha = alpha.max(evaluation);
            }
            Err(SearchAborted) => {
                return Err(SearchAborted);
            }
        }
    }

    let flag = if current_best_score <= original_alpha {
        TTFlag::UpperBound
    } else {
        TTFlag::Exact
    };

    tt.store(
        board.zobrist_hash,
        max_depth,
        current_best_score,
        flag,
        Some(CompactMove::from_move(&current_best_move)),
    );


    Ok(SearchResult {
        best_move: Some(current_best_move),
        best_score: current_best_score,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        
    })
}

/// Internal negamax with TT support
fn negamax_with_tt_mut(
    max_depth: u8,
    board: &mut Board,
    alpha: i32,
    beta: i32,
    tt: &mut TranspositionTable,
    search_state: &mut SearchState,
    ply: usize,
) -> Result<SearchResult, Vec<Move>> {
    let original_alpha = alpha;
    let mut alpha = alpha;

    // Probe transposition table
    if let Some((score, best_move)) = tt.probe(board.zobrist_hash, max_depth, alpha, beta) {
        return Ok(SearchResult {
            best_move: best_move.map(|m| m.to_move(board)),
            best_score: score,
            nodes_searched: 0,
            quiescent_nodes_searched: 0,

        });
    }

    if max_depth == 0 {
        // Quiescence search at leaf
        return negamax_captures_only_mut(board, alpha, beta);
    }

    // Null move pruning: if we can give the opponent a free move and still
    // beat beta, we can prune this branch
    // Conditions: not in check, sufficient depth, not at root
    const NULL_MOVE_REDUCTION: u8 = 2;
    let active_color = board.get_active_color();
    if max_depth >= 3 && !board.is_in_check(active_color) && beta < MAX_SCORE - 1000 {
        let undo = board.make_null_move();
        // Search with reduced depth
        let null_result = negamax_with_tt_mut(
            max_depth - 1 - NULL_MOVE_REDUCTION,
            board,
            -beta,
            -beta + 1, // Null window search
            tt,
            search_state,
            ply + 1,
        );
        board.unmake_null_move(&undo);

        if let Ok(result) = null_result {
            // Negate because of negamax
            let null_score = -result.best_score;
            // If opponent can't beat beta even with a free move, prune
            if null_score >= beta {
                return Ok(SearchResult {
                    best_move: None,
                    best_score: beta,
                    nodes_searched: result.nodes_searched,
                    quiescent_nodes_searched: result.quiescent_nodes_searched,
                    
                });
            }
        }
    }

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: MIN_SCORE + (100 - max_depth as i32), // Prefer shorter mates
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                
            })
        }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    // Move ordering priority:
    // 1. TT best move (1_000_000)
    // 2. Captures with MVV-LVA scoring (100_000 + mvv_lva)
    // 3. Killer moves (90_000 for first killer, 80_000 for second)
    // 4. Quiet moves with history score
    let tt_best_move = tt.get_best_move(board.zobrist_hash);
    let legal_moves = {
        let mut moves_and_scores: Vec<_> = legal_moves
            .iter()
            .map(|m| {
                let score = if tt_best_move.map(|tm| tm == CompactMove::from_move(m)).unwrap_or(false) {
                    1_000_000 // TT move gets highest priority
                } else if m.captured.is_some() {
                    // Captures: MVV-LVA scoring + high base
                    100_000 + guess_move_value(board, m)
                } else if let Some(killer_idx) = search_state.killer_priority(ply, m) {
                    // Killer moves: first killer = 90_000, second = 80_000
                    90_000 - (killer_idx as i32 * 10_000)
                } else {
                    // Quiet moves: history score
                    search_state.get_history_score(active_color, m.from, m.to)
                };
                (m, score)
            })
            .collect();
        moves_and_scores.sort_by(|a, b| b.1.cmp(&a.1));
        moves_and_scores.iter().map(|(m, _)| **m).collect::<Vec<Move>>()
    };

    let mut current_best_move = legal_moves[0];
    let mut current_best_score = MIN_SCORE;
    let mut total_nodes_searched = 0;
    let mut total_quiescent_nodes_searched = 0;

    // Late Move Reductions (LMR) parameters
    const LMR_FULL_DEPTH_MOVES: usize = 4; // Number of moves to search at full depth
    const LMR_REDUCTION_LIMIT: u8 = 3;     // Minimum depth to apply LMR
    let in_check = board.is_in_check(active_color);

    // Futility pruning: at low depths, skip quiet moves that can't improve alpha
    // Margins: depth 1 = 200cp, depth 2 = 500cp
    const FUTILITY_MARGIN_1: i32 = 200;
    const FUTILITY_MARGIN_2: i32 = 500;
    let can_futility_prune = !in_check && max_depth <= 2;
    let futility_margin = if max_depth == 1 { FUTILITY_MARGIN_1 } else { FUTILITY_MARGIN_2 };
    let static_eval = if can_futility_prune { evaluate_board(board) } else { 0 };

    for (move_index, m) in legal_moves.iter().enumerate() {
        if let Some(captured) = m.captured {
            if captured.piece_type == PieceType::King {
                return Err(vec![*m]);
            }
        }

        // Futility pruning: skip quiet moves at low depths if static eval + margin < alpha
        // Don't prune captures, promotions, or if we haven't found any move yet
        let is_promotion = matches!(m.move_flag, MoveFlag::Promotion(_));
        if can_futility_prune
            && move_index > 0
            && m.captured.is_none()
            && !is_promotion
            && static_eval + futility_margin <= alpha
        {
            continue;
        }
        let undo = board.make_move(m);

        // Check extension: if this move gives check, extend depth by 1
        // This prevents horizon effects where tactical sequences involving checks
        // are cut off at an arbitrary point
        let opponent_color = board.get_active_color();
        let gives_check = board.is_in_check(opponent_color);
        let extension: u8 = if gives_check { 1 } else { 0 };
        let extended_depth = max_depth.saturating_sub(1).saturating_add(extension);

        // Principal Variation Search (PVS) with Late Move Reductions (LMR)
        // - First move: search with full window
        // - Later moves: search with null window first, re-search if fails high
        // - LMR: for later quiet moves, also reduce depth
        let res = if move_index == 0 {
            // First move (PV move): search with full window
            negamax_with_tt_mut(extended_depth, board, -beta, -alpha, tt, search_state, ply + 1)
        } else {
            // Late Move Reductions: search later quiet moves at reduced depth
            // Don't apply LMR if the move gives check
            let lmr_applies = move_index >= LMR_FULL_DEPTH_MOVES
                && max_depth >= LMR_REDUCTION_LIMIT
                && !in_check
                && !gives_check
                && m.captured.is_none();

            // PVS null window search (with LMR reduction if applicable)
            let search_depth = if lmr_applies {
                extended_depth.saturating_sub(1)
            } else {
                extended_depth
            };
            let null_window_result = negamax_with_tt_mut(
                search_depth,
                board,
                -alpha - 1,
                -alpha,
                tt,
                search_state,
                ply + 1,
            );

            // Check if we need to re-search with full window
            let needs_full_search = match &null_window_result {
                Ok(r) => {
                    let score = -r.best_score;
                    // Re-search if score beats alpha but doesn't beat beta
                    score > alpha && score < beta
                }
                Err(_) => true,
            };

            if needs_full_search {
                // Re-search with full window at full depth (with extension)
                negamax_with_tt_mut(extended_depth, board, -beta, -alpha, tt, search_state, ply + 1)
            } else {
                null_window_result
            }
        };
        board.unmake_move(&undo);

        match res {
            Ok(SearchResult {
                best_move: _,
                best_score: evaluation,
                nodes_searched,
                quiescent_nodes_searched,
                
            }) => {
                total_nodes_searched += nodes_searched;
                total_quiescent_nodes_searched += quiescent_nodes_searched;
                let evaluation = -evaluation;

                if evaluation > current_best_score {
                    current_best_score = evaluation;
                    current_best_move = *m;
                }

                if evaluation >= beta {
                    // Beta cutoff - store as lower bound
                    tt.store(
                        board.zobrist_hash,
                        max_depth,
                        evaluation,
                        TTFlag::LowerBound,
                        Some(CompactMove::from_move(m)),
                    );

                    // Update killer moves and history for quiet moves (non-captures)
                    if m.captured.is_none() {
                        search_state.store_killer(ply, *m);
                        search_state.add_history_bonus(active_color, m.from, m.to, max_depth);
                    }

                    return Ok(SearchResult {
                        best_move: Some(*m),
                        best_score: beta,
                        nodes_searched: total_nodes_searched,
                        quiescent_nodes_searched: total_quiescent_nodes_searched,

                    });
                }
                alpha = alpha.max(evaluation);
            }
            Err(moves) => {
                board.draw_to_terminal();
                let mut cur_move_vec = vec![*m];
                cur_move_vec.append(&mut moves.clone());
                return Err(cur_move_vec);
            }
        }
    }

    // Determine TT flag
    let flag = if current_best_score <= original_alpha {
        TTFlag::UpperBound
    } else {
        TTFlag::Exact
    };

    // Store in TT
    tt.store(
        board.zobrist_hash,
        max_depth,
        current_best_score,
        flag,
        Some(CompactMove::from_move(&current_best_move)),
    );


    Ok(SearchResult {
        best_move: Some(current_best_move),
        best_score: current_best_score,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        
    })
}

/// Main minimax function (immutable wrapper)
/// TODO: maybe discount further moves by a tiny bit to favor short mating sequences
pub fn negamax(
    max_depth: u8,
    board: &Board,
    alpha: i32,
    beta: i32,
    use_ab_pruning: bool,
    use_move_ordering: bool,
    quiescence: bool,
) -> Result<SearchResult, Vec<Move>> {
    let mut board = board.clone();
    negamax_mut(max_depth, &mut board, alpha, beta, use_ab_pruning, use_move_ordering, quiescence)
}

/// Main minimax function (mutable version using make/unmake)
fn negamax_mut(
    max_depth: u8,
    board: &mut Board,
    alpha: i32,
    beta: i32,
    use_ab_pruning: bool,
    use_move_ordering: bool,
    quiescence: bool,
) -> Result<SearchResult, Vec<Move>> {
    if max_depth == 0 {
        // If we've reached the maximum depth
        if quiescence {
            return negamax_captures_only_mut(board, alpha, beta);
        } else {
            let score = evaluate_board(board);
            return Ok(SearchResult {
                best_move: None,
                best_score: score,
                nodes_searched: 1,
                quiescent_nodes_searched: 0,
                
            });
        };
    };

    // for alpha-beta pruning
    let mut alpha = alpha;

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: MIN_SCORE + (100 - max_depth as i32), // Prefer shorter mates
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                
            })
        }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    // if using move ordering, we sort the moves by their value heuristic here
    let legal_moves = if use_move_ordering {
        let mut moves_and_scores = legal_moves
            .iter()
            .map(|m| (m, guess_move_value(board, m)))
            .collect::<Vec<(&Move, i32)>>();

        moves_and_scores.sort_by(|a, b| b.1.cmp(&a.1));
        moves_and_scores
            .iter()
            .map(|(m, _)| **m)
            .collect::<Vec<Move>>()
    } else {
        legal_moves
    };

    let mut current_best_move = legal_moves[0];
    let mut current_best_score = MIN_SCORE;
    let mut total_nodes_searched = 0;
    let mut total_quiescent_nodes_searched = 0;

    for m in &legal_moves {
        if let Some(captured) = m.captured {
            if captured.piece_type == PieceType::King {
                return Err(vec![*m]);
            }
        }
        let undo = board.make_move(m);
        let res = negamax_mut(
            max_depth - 1,
            board,
            -beta,
            -alpha,
            use_ab_pruning,
            use_move_ordering,
            quiescence,
        );
        board.unmake_move(&undo);

        match res {
            Ok(SearchResult {
                best_move: _, // best_move from prev iteration
                best_score: evaluation,
                nodes_searched,
                quiescent_nodes_searched,
                
            }) => {
                total_nodes_searched += nodes_searched;
                total_quiescent_nodes_searched += quiescent_nodes_searched;
                let evaluation = -evaluation;

                if evaluation > current_best_score {
                    current_best_score = evaluation;
                    current_best_move = *m;
                };

                if use_ab_pruning {
                    // The last move was too good, meaning the opponent would not have allowed
                    // us to get to this position by playing a different move earlier on,
                    // so we can stop searching the remaining moves
                    if evaluation >= beta {
                        current_best_score = beta;
                        break;
                    }
                    alpha = alpha.max(evaluation);
                }
            }
            Err(moves) => {
                board.draw_to_terminal();
                let mut cur_move_vec = vec![*m];
                cur_move_vec.append(&mut moves.clone());
                return Err(cur_move_vec);
            }
        }
    }


    Ok(SearchResult {
        best_move: Some(current_best_move),
        best_score: current_best_score,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        
    })
}

/// Minimax run with no depth limit for the quiescence search.
/// We consider captures and promotions. This version returns the score and some
/// metadata on the best path found.
///
/// TODO: maybe add checks here as well?
const MAX_QUIESCENCE_DEPTH: u8 = 6;

pub fn negamax_captures_only(
    board: &Board,
    alpha: i32,
    beta: i32,
) -> Result<SearchResult, Vec<Move>> {
    let mut board = board.clone();
    negamax_captures_only_mut(&mut board, alpha, beta)
}

fn negamax_captures_only_mut(
    board: &mut Board,
    alpha: i32,
    beta: i32,
) -> Result<SearchResult, Vec<Move>> {
    negamax_captures_only_mut_with_depth(board, alpha, beta, MAX_QUIESCENCE_DEPTH)
}

fn negamax_captures_only_mut_with_depth(
    board: &mut Board,
    alpha: i32,
    beta: i32,
    depth_remaining: u8,
) -> Result<SearchResult, Vec<Move>> {
    let evaluation = evaluate_board(&board);
    // Stand-pat: if current position is already good enough, we can stop
    if evaluation >= beta {
        return Ok(SearchResult {
            best_move: None,
            best_score: beta,
            nodes_searched: 1,
            quiescent_nodes_searched: 1,
            
        });
    }

    // If we've reached max quiescence depth, return static evaluation
    if depth_remaining == 0 {
        return Ok(SearchResult {
            best_move: None,
            best_score: evaluation,
            nodes_searched: 1,
            quiescent_nodes_searched: 1,
            
        });
    }

    let mut alpha = alpha.max(evaluation);

    // Use optimized captures-only generation
    let mut captures = board.get_legal_captures(&board.get_active_color());

    // MVV-LVA ordering: sort by captured piece value (descending) - attacker value (ascending)
    captures.sort_by_key(|m| {
        let victim_value = m.captured.map_or(0, |_p| guess_move_value(board, m));
        -victim_value // Negative for descending sort (best captures first)
    });

    let mut current_best_move = None;
    let mut total_nodes_searched = 0;
    let mut total_quiescent_nodes_searched = 0;

    for m in captures {
        // SEE pruning: skip losing captures
        // Only compute SEE when the attacker might be worth more than the victim
        // (otherwise the capture is likely winning or equal)
        let attacker_value = SEE_PIECE_VALUES[piece_type_to_see_index(m.piece.piece_type)];
        let victim_value = m.captured.map_or(0, |p| SEE_PIECE_VALUES[piece_type_to_see_index(p.piece_type)]);
        if attacker_value > victim_value && see(board, &m) < 0 {
            continue;
        }

        let undo = board.make_move(&m);
        let search_res = negamax_captures_only_mut_with_depth(board, -beta, -alpha, depth_remaining - 1).unwrap();
        board.unmake_move(&undo);

        let evaluation = -search_res.best_score;
        total_nodes_searched += search_res.nodes_searched;
        total_quiescent_nodes_searched += search_res.quiescent_nodes_searched;

        if evaluation >= beta {
            current_best_move = Some(m);

            return Ok(SearchResult {
                best_move: current_best_move,
                best_score: beta,
                nodes_searched: total_nodes_searched,
                quiescent_nodes_searched: total_quiescent_nodes_searched,
                
            });
        }
        alpha = alpha.max(evaluation);
    }
    return Ok(SearchResult {
        best_move: current_best_move,
        best_score: alpha,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        
    });
}

pub fn negamax_fast(depth: i32, board: &Board, alpha: i32, beta: i32, quiescence: bool) -> i32 {
    let mut board = board.clone();
    negamax_fast_mut(depth, &mut board, alpha, beta, quiescence)
}

fn negamax_fast_mut(depth: i32, board: &mut Board, alpha: i32, beta: i32, quiescence: bool) -> i32 {
    if depth == 0 {
        if quiescence {
            return negamax_captures_only_fast_mut(board, alpha, beta);
        }
        return evaluate_board(&board);
    };

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return MIN_SCORE;
        }
        Err(Status::Stalemate) => {
            return 0;
        }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    let mut alpha = alpha;

    for m in legal_moves {
        let undo = board.make_move(&m);
        let evaluation = -negamax_fast_mut(depth - 1, board, -beta, -alpha, quiescence);
        board.unmake_move(&undo);
        if evaluation >= beta {
            return beta;
        }
        alpha = alpha.max(evaluation);
    }
    return alpha;
}

/// Minimax run with no depth limit for the quiescence search.
/// We consider captures and promotions. This version returns the score of the position only.
///
/// TODO: maybe add checks here as well?
pub fn negamax_captures_only_fast(board: &Board, alpha: i32, beta: i32) -> i32 {
    let mut board = board.clone();
    negamax_captures_only_fast_mut(&mut board, alpha, beta)
}

fn negamax_captures_only_fast_mut(board: &mut Board, alpha: i32, beta: i32) -> i32 {
    negamax_captures_only_fast_mut_with_depth(board, alpha, beta, MAX_QUIESCENCE_DEPTH)
}

fn negamax_captures_only_fast_mut_with_depth(board: &mut Board, alpha: i32, beta: i32, depth_remaining: u8) -> i32 {
    let evaluation = evaluate_board(&board);
    // Stand-pat: if current position is already good enough, we can stop
    if evaluation >= beta {
        return beta;
    }

    // If we've reached max quiescence depth, return static evaluation
    if depth_remaining == 0 {
        return evaluation;
    }

    let mut alpha = alpha.max(evaluation);

    // Use optimized captures-only generation
    let mut captures = board.get_legal_captures(&board.get_active_color());

    // MVV-LVA ordering: sort by captured piece value (descending) - attacker value (ascending)
    captures.sort_by_key(|m| -guess_move_value(board, m));

    for m in captures {
        // SEE pruning: skip losing captures
        // Only compute SEE when the attacker might be worth more than the victim
        let attacker_value = SEE_PIECE_VALUES[piece_type_to_see_index(m.piece.piece_type)];
        let victim_value = m.captured.map_or(0, |p| SEE_PIECE_VALUES[piece_type_to_see_index(p.piece_type)]);
        if attacker_value > victim_value && see(board, &m) < 0 {
            continue;
        }

        let undo = board.make_move(&m);
        let evaluation = -negamax_captures_only_fast_mut_with_depth(board, -beta, -alpha, depth_remaining - 1);
        board.unmake_move(&undo);
        if evaluation >= beta {
            return beta;
        }
        alpha = alpha.max(evaluation);
    }
    return alpha;
}

// Returns a random legal move
// pub fn random_move(board: &Board) -> Move {
//     let mut rng = rand::thread_rng();

//     let legal_moves = board.get_legal_moves(&board.get_active_color()).expect(&"No legal moves");

//     *legal_moves.choose(&mut rng).expect("No moves!")
// }

// =============================================================================
// MovePicker-based search (new high-performance implementation)
// =============================================================================

/// High-performance negamax using MovePicker for staged move generation.
/// This avoids heap allocations during move generation and uses staged
/// generation to potentially cut off before generating all moves.
pub fn negamax_movepicker(
    max_depth: u8,
    board: &mut Board,
    mut alpha: i32,
    beta: i32,
    tt: &mut TranspositionTable,
    search_state: &mut SearchState,
    ply: usize,
    position_history: &mut Vec<u64>,
) -> (i32, Option<CompactMove>, i32, i32) {
    // Returns: (score, best_move, nodes_searched, quiescent_nodes)

    // Draw detection (skip at root so we always return a move)
    if ply > 0 {
        // Repetition
        if position_history.iter().any(|&h| h == board.zobrist_hash) {
            return (0, None, 0, 0);
        }
        // Fifty-move rule
        if board.check_for_fifty_move_rule().is_some() {
            return (0, None, 0, 0);
        }
        // Insufficient material
        if board.check_for_insufficient_material().is_some() {
            return (0, None, 0, 0);
        }
    }

    // Push current position onto the path for descendant repetition checks
    position_history.push(board.zobrist_hash);

    // Probe transposition table
    let tt_move = tt.get_best_move(board.zobrist_hash);
    // Never return TT cutoffs at root (ply 0) — we must always search to get a validated move.
    if ply > 0 {
        if let Some((score, _)) = tt.probe(board.zobrist_hash, max_depth, alpha, beta) {
            position_history.pop();
            return (score, tt_move, 0, 0);
        }
    }

    if max_depth == 0 {
        // Quiescence search at leaf
        let (score, qnodes) = quiescence_movepicker(board, alpha, beta, MAX_QUIESCENCE_DEPTH);
        position_history.pop();
        return (score, None, 0, qnodes);
    }

    // Null move pruning
    const NULL_MOVE_REDUCTION: u8 = 2;
    let active_color = board.get_active_color();
    if max_depth >= 3 && !board.is_in_check(active_color) && beta < MAX_SCORE - 1000 {
        let undo = board.make_null_move();
        let (null_score, _, null_nodes, null_qnodes) = negamax_movepicker(
            max_depth - 1 - NULL_MOVE_REDUCTION,
            board,
            -beta,
            -beta + 1,
            tt,
            search_state,
            ply + 1,
            position_history,
        );
        board.unmake_null_move(&undo);

        let null_score = -null_score;
        if null_score >= beta {
            position_history.pop();
            return (beta, None, null_nodes, null_qnodes);
        }
    }

    // Create MovePicker - this is on the stack, no heap allocation!
    let killers = search_state.get_killers_compact(ply);

    // Get a raw pointer to history to avoid 8KB copy per recursive call.
    // SAFETY: SearchState outlives this function, we only read through this pointer,
    // and history writes (on beta cutoff) happen after we're done picking moves.
    // Recursive calls access the opponent's history table (different color index).
    let history_ptr = search_state.get_history_compact(active_color) as *const [[i16; 64]; 64];
    let history: &[[i16; 64]; 64] = unsafe { &*history_ptr };

    let mut picker = MovePicker::new(tt_move, killers, active_color);

    let mut best_move: Option<CompactMove> = None;
    let mut best_score = MIN_SCORE;
    let mut total_nodes = 0;
    let mut total_qnodes = 0;
    let mut move_count = 0;

    let in_check = board.is_in_check(active_color);

    // Store initial board state for debugging
    #[cfg(debug_assertions)]
    let initial_fen = board.to_fen();

    // Iterate through moves in order of likelihood to cause cutoff
    while let Some(mv) = picker.next_move(board, &history) {
        #[cfg(debug_assertions)]
        let fen_before_legal_check = board.to_fen();

        // Legality check
        if !board.is_legal_compact(&mv) {
            #[cfg(debug_assertions)]
            {
                let fen_after_legal_check = board.to_fen();
                if fen_before_legal_check != fen_after_legal_check {
                    panic!("is_legal_compact corrupted board for illegal move {:?}!\n  Before: {}\n  After: {}",
                           mv, fen_before_legal_check, fen_after_legal_check);
                }
            }
            continue;
        }

        #[cfg(debug_assertions)]
        {
            let fen_after_legal_check = board.to_fen();
            if fen_before_legal_check != fen_after_legal_check {
                panic!("is_legal_compact corrupted board for legal move {:?}!\n  Before: {}\n  After: {}",
                       mv, fen_before_legal_check, fen_after_legal_check);
            }
        }

        move_count += 1;

        // Make move
        let undo = board.make_compact_move(&mv);

        // Check extension
        let gives_check = board.is_in_check(board.get_active_color());
        let extension: u8 = if gives_check { 1 } else { 0 };
        let new_depth = max_depth.saturating_sub(1).saturating_add(extension);

        // LMR for later quiet moves
        let lmr_applies = move_count > 4
            && max_depth >= 3
            && !in_check
            && !gives_check
            && !mv.is_capture();

        let search_depth = if lmr_applies {
            new_depth.saturating_sub(1)
        } else {
            new_depth
        };

        // PVS: null window for non-first moves
        let (mut score, _, nodes, qnodes) = if move_count == 1 {
            negamax_movepicker(new_depth, board, -beta, -alpha, tt, search_state, ply + 1, position_history)
        } else {
            // Null window search
            let (null_score, _, null_nodes, null_qnodes) = negamax_movepicker(
                search_depth,
                board,
                -alpha - 1,
                -alpha,
                tt,
                search_state,
                ply + 1,
                position_history,
            );
            total_nodes += null_nodes;
            total_qnodes += null_qnodes;

            let null_score = -null_score;
            if null_score > alpha && null_score < beta {
                // Re-search with full window
                negamax_movepicker(new_depth, board, -beta, -alpha, tt, search_state, ply + 1, position_history)
            } else {
                (-null_score, None, 0, 0)
            }
        };

        board.unmake_move(&undo);

        score = -score;
        total_nodes += nodes + 1;
        total_qnodes += qnodes;

        if score > best_score {
            best_score = score;
            best_move = Some(mv);

            if score > alpha {
                alpha = score;

                if score >= beta {
                    // Beta cutoff - update killers and history for quiet moves
                    if !mv.is_capture() {
                        search_state.store_killer_compact(ply, mv);
                        search_state.add_history_bonus_compact(active_color, mv, max_depth);
                    }

                    // Store in TT
                    tt.store(
                        board.zobrist_hash,
                        max_depth,
                        score,
                        TTFlag::LowerBound,
                        best_move,
                    );

                    position_history.pop();
                    return (beta, best_move, total_nodes, total_qnodes);
                }
            }
        }
    }

    // Check for checkmate/stalemate
    if move_count == 0 {
        if in_check {
            // Checkmate - prefer shorter mates
            position_history.pop();
            return (MIN_SCORE + ply as i32, None, total_nodes, total_qnodes);
        } else {
            // Stalemate
            position_history.pop();
            return (0, None, total_nodes, total_qnodes);
        }
    }

    // Store in TT
    let flag = if best_score > alpha {
        TTFlag::Exact
    } else {
        TTFlag::UpperBound
    };
    tt.store(
        board.zobrist_hash,
        max_depth,
        best_score,
        flag,
        best_move,
    );

    position_history.pop();
    (best_score, best_move, total_nodes, total_qnodes)
}

/// Quiescence search using MovePicker (captures only).
fn quiescence_movepicker(
    board: &mut Board,
    mut alpha: i32,
    beta: i32,
    depth_remaining: u8,
) -> (i32, i32) {
    // Returns: (score, nodes_searched)

    let eval = evaluate_board(board);

    // Stand-pat
    if eval >= beta {
        return (beta, 1);
    }
    if depth_remaining == 0 {
        return (eval, 1);
    }

    alpha = alpha.max(eval);
    let color = board.get_active_color();

    // Create captures-only MovePicker
    let history = [[0i16; 64]; 64]; // Not used for captures
    let mut picker = MovePicker::new_captures_only(color);

    let mut nodes = 1;

    while let Some(mv) = picker.next_move(board, &history) {
        if !board.is_legal_compact(&mv) {
            continue;
        }

        let undo = board.make_compact_move(&mv);
        let (score, child_nodes) = quiescence_movepicker(board, -beta, -alpha, depth_remaining - 1);
        board.unmake_move(&undo);

        let score = -score;
        nodes += child_nodes;

        if score >= beta {
            return (beta, nodes);
        }
        alpha = alpha.max(score);
    }

    (alpha, nodes)
}

/// Iterative deepening search using MovePicker.
/// Returns the best move found at the deepest completed depth.
pub fn iterative_deepening_movepicker(
    board: &mut Board,
    max_depth: u8,
    tt: &mut TranspositionTable,
) -> SearchResult {
    let mut search_state = SearchState::new();
    let mut position_history = Vec::new();
    let mut best_move = None;
    let mut best_score = MIN_SCORE;
    let mut total_nodes = 0;
    let mut total_qnodes = 0;

    for depth in 1..=max_depth {
        search_state.age_history();

        let (score, mv, nodes, qnodes) = negamax_movepicker(
            depth,
            board,
            MIN_SCORE,
            MAX_SCORE,
            tt,
            &mut search_state,
            0,
            &mut position_history,
        );

        total_nodes += nodes;
        total_qnodes += qnodes;

        if let Some(m) = mv {
            best_move = Some(m.to_move(board));
            best_score = score;
        }
    }

    SearchResult {
        best_move,
        best_score,
        nodes_searched: total_nodes,
        quiescent_nodes_searched: total_qnodes,
    }
}

// =============================================================================
// MovePicker search with time control (for UCI)
// =============================================================================

/// Iterative deepening search using MovePicker with time control.
/// Returns the best move found at the deepest completed depth.
pub fn iterative_deepening_movepicker_with_control(
    board: &mut Board,
    max_depth: u8,
    tt: &mut TranspositionTable,
    search_state: &mut SearchState,
    control: &SearchControl,
    position_history: &mut Vec<u64>,
) -> Result<SearchResult, SearchAborted> {
    let mut best_move = None;
    let mut best_score = MIN_SCORE;
    let mut total_nodes = 0;
    let mut total_qnodes = 0;
    let mut depth_times: Vec<u64> = Vec::with_capacity(max_depth as usize);

    for depth in 1..=max_depth {
        // Check soft limit before starting next depth
        if control.exceeded_soft_limit() {
            break;
        }

        // Predict if we have time for this depth (each depth takes ~3-4x longer)
        if depth > 2 && !depth_times.is_empty() {
            let last_time = *depth_times.last().unwrap();
            let predicted_time = last_time * 4;
            let elapsed = control.elapsed_ms();
            if elapsed + predicted_time > control.soft_limit_ms {
                break;
            }
        }

        let depth_start = std::time::Instant::now();

        search_state.age_history();

        let result = negamax_movepicker_with_control(
            depth,
            board,
            MIN_SCORE,
            MAX_SCORE,
            tt,
            search_state,
            0,
            control,
            position_history,
        );

        let depth_time = depth_start.elapsed().as_millis() as u64;
        depth_times.push(depth_time);

        match result {
            Ok((score, mv, nodes, qnodes)) => {
                total_nodes += nodes;
                total_qnodes += qnodes;

                if let Some(m) = mv {
                    best_move = Some(m.to_move(board));
                    best_score = score;
                }
            }
            Err(SearchAborted) => {
                // Search was aborted mid-depth, use best result from previous depth
                break;
            }
        }
    }

    Ok(SearchResult {
        best_move,
        best_score,
        nodes_searched: total_nodes,
        quiescent_nodes_searched: total_qnodes,
    })
}

/// Aspiration window search using MovePicker with time control.
pub fn aspiration_search_movepicker_with_control(
    depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
    search_state: &mut SearchState,
    prev_score: i32,
    control: &SearchControl,
    position_history: &mut Vec<u64>,
) -> Result<(i32, Option<CompactMove>, i32, i32), SearchAborted> {
    const INITIAL_WINDOW: i32 = 50;
    let mut delta = INITIAL_WINDOW;
    let mut alpha = prev_score - delta;
    let mut beta = prev_score + delta;

    loop {
        let result = negamax_movepicker_with_control(
            depth, board, alpha, beta, tt, search_state, 0, control,
            position_history,
        )?;

        let (score, mv, nodes, qnodes) = result;

        // Check if score is within window
        if score <= alpha {
            // Fail low - widen alpha
            alpha = (alpha - delta).max(MIN_SCORE);
            delta *= 2;
        } else if score >= beta {
            // Fail high - widen beta
            beta = (beta + delta).min(MAX_SCORE);
            delta *= 2;
        } else {
            // Score is within window
            return Ok((score, mv, nodes, qnodes));
        }

        // Prevent infinite loop with very large windows
        if delta > 1000 {
            return negamax_movepicker_with_control(
                depth, board, MIN_SCORE, MAX_SCORE, tt, search_state, 0, control,
                position_history,
            );
        }
    }
}

/// Negamax with MovePicker and time control.
fn negamax_movepicker_with_control(
    max_depth: u8,
    board: &mut Board,
    mut alpha: i32,
    beta: i32,
    tt: &mut TranspositionTable,
    search_state: &mut SearchState,
    ply: usize,
    control: &SearchControl,
    position_history: &mut Vec<u64>,
) -> Result<(i32, Option<CompactMove>, i32, i32), SearchAborted> {
    // Check if we should abort (gated by node counter inside should_stop)
    if control.should_stop() {
        return Err(SearchAborted);
    }

    // Draw detection (skip at root so we always return a move)
    if ply > 0 {
        // Repetition
        if position_history.iter().any(|&h| h == board.zobrist_hash) {
            return Ok((0, None, 0, 0));
        }
        // Fifty-move rule
        if board.check_for_fifty_move_rule().is_some() {
            return Ok((0, None, 0, 0));
        }
        // Insufficient material
        if board.check_for_insufficient_material().is_some() {
            return Ok((0, None, 0, 0));
        }
    }

    // Push current position onto the path for descendant repetition checks
    position_history.push(board.zobrist_hash);

    // Probe transposition table
    let tt_move = tt.get_best_move(board.zobrist_hash);
    // Never return TT cutoffs at root (ply 0) — we must always search to get a validated move.
    if ply > 0 {
        if let Some((score, _)) = tt.probe(board.zobrist_hash, max_depth, alpha, beta) {
            position_history.pop();
            return Ok((score, tt_move, 0, 0));
        }
    }

    // At depth 0, drop into quiescence search
    if max_depth == 0 {
        let (score, nodes) = quiescence_movepicker(board, alpha, beta, 10);
        position_history.pop();
        return Ok((score, None, 0, nodes));
    }

    let active_color = board.get_active_color();
    let in_check = board.is_in_check(active_color);

    // Null move pruning (skip when in check)
    let mut total_nodes: i32 = 0;
    let mut total_qnodes: i32 = 0;

    const NULL_MOVE_REDUCTION: u8 = 2;
    if max_depth >= 3 && !in_check && beta < MAX_SCORE - 1000 {
        let undo = board.make_null_move();
        let result = negamax_movepicker_with_control(
            max_depth.saturating_sub(1 + NULL_MOVE_REDUCTION),
            board,
            -beta,
            -beta + 1,
            tt,
            search_state,
            ply + 1,
            control,
            position_history,
        );
        board.unmake_null_move(&undo);

        if let Ok((null_score, _, null_nodes, null_qnodes)) = result {
            total_nodes += null_nodes;
            total_qnodes += null_qnodes;
            let null_score = -null_score;
            if null_score >= beta {
                position_history.pop();
                return Ok((beta, None, total_nodes, total_qnodes));
            }
        } else {
            position_history.pop();
            return Err(SearchAborted);
        }
    }

    // Get killers and TT move for move ordering
    let killers = search_state.get_killers_compact(ply);

    // Get history reference safely
    let history_ptr = search_state.get_history_compact(active_color) as *const [[i16; 64]; 64];
    let history: &[[i16; 64]; 64] = unsafe { &*history_ptr };

    let mut picker = MovePicker::new(tt_move, killers, active_color);

    let mut best_score = MIN_SCORE;
    let mut best_move: Option<CompactMove> = None;
    let mut move_count = 0;

    while let Some(mv) = picker.next_move(board, history) {
        if !board.is_legal_compact(&mv) {
            continue;
        }
        move_count += 1;

        let undo = board.make_compact_move(&mv);

        // Late move reductions
        let new_depth = if move_count > 4 && max_depth >= 3 && !mv.is_capture() && !in_check {
            max_depth - 2
        } else {
            max_depth - 1
        };

        // PVS: First move gets full window, rest get null window first
        let (mut score, _, nodes, qnodes) = if move_count == 1 {
            match negamax_movepicker_with_control(
                new_depth, board, -beta, -alpha, tt, search_state, ply + 1, control,
                position_history,
            ) {
                Ok(r) => r,
                Err(e) => {
                    board.unmake_move(&undo);
                    position_history.pop();
                    return Err(e);
                }
            }
        } else {
            // Null window search
            let (null_score, _, null_nodes, null_qnodes) = match negamax_movepicker_with_control(
                new_depth, board, -alpha - 1, -alpha, tt, search_state, ply + 1, control,
                position_history,
            ) {
                Ok(r) => r,
                Err(e) => {
                    board.unmake_move(&undo);
                    position_history.pop();
                    return Err(e);
                }
            };
            total_nodes += null_nodes;
            total_qnodes += null_qnodes;

            let null_score = -null_score;
            if null_score > alpha && null_score < beta {
                // Re-search with full window
                match negamax_movepicker_with_control(
                    new_depth, board, -beta, -alpha, tt, search_state, ply + 1, control,
                    position_history,
                ) {
                    Ok(r) => r,
                    Err(e) => {
                        board.unmake_move(&undo);
                        position_history.pop();
                        return Err(e);
                    }
                }
            } else {
                (-null_score, None, 0, 0)
            }
        };

        board.unmake_move(&undo);

        score = -score;
        total_nodes += nodes + 1;
        total_qnodes += qnodes;

        if score > best_score {
            best_score = score;
            best_move = Some(mv);

            if score > alpha {
                alpha = score;

                if score >= beta {
                    // Beta cutoff - update killers and history for quiet moves
                    if !mv.is_capture() {
                        search_state.store_killer_compact(ply, mv);
                        search_state.add_history_bonus_compact(active_color, mv, max_depth);
                    }

                    // Store in TT
                    tt.store(
                        board.zobrist_hash,
                        max_depth,
                        score,
                        TTFlag::LowerBound,
                        best_move,
                    );

                    position_history.pop();
                    return Ok((beta, best_move, total_nodes, total_qnodes));
                }
            }
        }
    }

    // Check for checkmate/stalemate
    if move_count == 0 {
        if in_check {
            position_history.pop();
            return Ok((MIN_SCORE + ply as i32, None, total_nodes, total_qnodes));
        } else {
            position_history.pop();
            return Ok((0, None, total_nodes, total_qnodes));
        }
    }

    // Store in TT
    let flag = if best_score > alpha {
        TTFlag::Exact
    } else {
        TTFlag::UpperBound
    };
    tt.store(
        board.zobrist_hash,
        max_depth,
        best_score,
        flag,
        best_move,
    );

    position_history.pop();
    Ok((best_score, best_move, total_nodes, total_qnodes))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::types::{Color, Piece, Position};
    use std::time::Instant;

    #[test]
    fn test_quiescence_search_1() {
        // . . . . . . ♖ .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . . . ♖ . .
        // . . . . . ♔ . ♚
        let b = Board::from_fen("6R1/8/8/8/8/4p3/5R2/5K1k b - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_eq!(search_result.nodes_searched, 2);
        assert_eq!(m, Move::from_algebraic(&b, "e3", "f2"));
        println!("");

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // unfortunately 1 move deep is too shallow to see the draw opportunity
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_eq!(m, Move::from_algebraic(&b, "e3", "f2"));
    }

    #[test]
    fn test_quiescence_search_2() {
        // This is an example of a position where we need to look past the first
        // capture to reject the capture

        // . . . . . . . .
        // . . . . . ♚ . .
        // . . . . . ♟︎ . .
        // . . . . ♟︎ . . .
        // . . . . ♖ . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♔ . . . .
        let b = Board::from_fen("8/5k2/5p2/4p3/4R3/8/8/3K4 w - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the pawn with the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        let eval = search_result.best_score;
        assert_eq!(m, Move::from_algebraic(&b, "e4", "e5"));

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // we find that the next move is black recapturing the rook, so we
        // don't play the move
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_ne!(m, Move::from_algebraic(&b, "e4", "e5"));
    }

    #[test]
    fn test_quiescence_search_3() {
        // This is an example of a position where looking past the first capture
        // allows us to recognize the sac is actually good

        // . . . . ♚ . . .
        // . . . . . . . .
        // . . . . ♜ . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . ♟︎ . . . .
        // . . . . . . . .
        // . . . ♖ ♕ ♔ . .
        let b = Board::from_fen("4k3/8/4r3/8/4p3/3p4/8/3RQK2 w - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the pawn with the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        let eval = search_result.best_score;
        assert!(
            m == Move::from_algebraic(&b, "d1", "d3") || m == Move::from_algebraic(&b, "e1", "e4")
        );
        println!("{} {}", eval, search_result.nodes_searched);

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // we find that the next move is black recapturing the rook, so we
        // but the move after that we recapture their rook with the queen so we
        // still play it
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_ne!(m, Move::from_algebraic(&b, "e4", "e5"));
        println!("{} {}", eval, search_result.nodes_searched);
    }

    #[test]
    fn test_quiescence_search_4() {
        // This is an example of a position where looking past the first capture
        // allows us to discard it

        // . . . . ♚ . . .
        // . . . . . . . .
        // . . . . ♜ . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . ♟︎ . . . .
        // . . . . . . . .
        // . . . ♖ . ♔ . .
        let b = Board::from_fen("4k3/8/4r3/8/4p3/3p4/8/3R1K2 w - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the pawn with the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        let eval = search_result.best_score;
        assert!(m == Move::from_algebraic(&b, "d1", "d3"));
        println!("{} {}", eval, search_result.nodes_searched);

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // we find that the next move is black recapturing the rook, so we
        // don't play the move
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_ne!(m, Move::from_algebraic(&b, "d1", "d3"));
        println!("{} {}", eval, search_result.nodes_searched);
    }

    // #[test]
    // fn test_blunders_knight_1() {
    //     // . ♞ . ♛ ♚ ♝ . ♜
    //     // ♜ ♝ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
    //     // ♟︎ ♟︎ . . . . . .
    //     // . . . . . ♟︎ . .
    //     // . . . . ♘ . . .
    //     // . . . ♕ ♙ . . .`
    //     // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
    //     // ♖ . ♗ . ♔ . ♘ ♖
    //     let fen = "1n1qkb1r/rbppp1pp/pp6/5p2/4N3/3QP3/PPPP1PPP/R1B1K1NR w KQk - 0 8";
    //     let b = Board::from_fen(fen);
    //     b.draw_to_terminal();
    //     let m = search(4, &b);
    //     print!("{} {}", m.to_human(), m.to_algebraic());
    //     assert!(m.piece == Piece::from_algebraic('N', "e4"));
    // }

    #[test]
    fn test_does_not_blunder_knight() {
        // Position where knight on e4 is attacked by pawn on f5
        // The blunder is f2f3?? which allows ...fxe4 winning the knight
        // Engine should move the knight instead
        //
        // . ♞ . ♛ ♚ ♝ . ♜
        // ♜ ♝ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
        // ♟︎ ♟︎ . . . . . .
        // . . . . . ♟︎ . .
        // . . . . ♘ . . .
        // . . . ♕ ♙ . . .
        // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
        // ♖ . ♗ . ♔ . ♘ ♖
        let mut b = Board::from_fen("1n1qkb1r/rbppp1pp/pp6/5p2/4N3/3QP3/PPPP1PPP/R1B1K1NR w KQk - 0 8");
        let mut tt = TranspositionTable::new(16);

        // Use the same search path as UCI (aspiration_search_with_control -> negamax_with_control)
        let control = SearchControl::infinite();
        let mut best_move = None;
        let mut prev_score = 0i32;

        for depth in 1..=6u8 {
            let result = if depth > 1 {
                aspiration_search_with_control(depth, &mut b, &mut tt, prev_score, &control)
            } else {
                negamax_with_control(depth, &mut b, MIN_SCORE, MAX_SCORE, &mut tt, &control)
            };

            if let Ok(r) = result {
                prev_score = r.best_score;
                best_move = r.best_move;
            }
        }

        let best_move = best_move.expect("Should find a move");

        // The blunder is f2f3 - engine should NOT play this
        let blunder = Move::from_algebraic(&b, "f2", "f3");
        assert_ne!(
            best_move, blunder,
            "Engine should not blunder f3, which loses the knight to ...fxe4"
        );

        // Best move should be a knight move (escaping the attack)
        assert_eq!(
            best_move.piece.piece_type,
            PieceType::Knight,
            "Engine should move the knight which is under attack. Got: {} {}",
            best_move.to_algebraic(),
            best_move.to_human()
        );
    }

    #[test]
    fn sebastian_lague_test_position() {
        let b =
            Board::from_fen("r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1");

        let now_minimax = Instant::now();
        let pure_minimax_4 = minimax_no_pruning(4, &b).unwrap();
        // Sebastian has 1.16 s, 3_553_501 positions
        println!(
            "pure minimax: {:.6}s evaluated {} positions",
            now_minimax.elapsed().as_secs_f32(),
            pure_minimax_4.nodes_searched
        );

        let now_pruning = Instant::now();
        let minimax_a_b_pruning = minimax_no_ordering(4, &b).unwrap();
        // Sebastian has 0.18 s, 464_795 positions
        println!(
            "minimax a b pruning: {:.6}s evaluated {} positions",
            now_pruning.elapsed().as_secs_f32(),
            minimax_a_b_pruning.nodes_searched
        );

        let now_pruning_sorting = Instant::now();
        let minimax_a_b_pruning_sorting = minimax_no_quiescence(4, &b).unwrap();
        // Sebastian has 0.025 s, 4916 positions
        println!(
            "minimax a b pruning with ordering: {:.6}s evaluated {} positions",
            now_pruning_sorting.elapsed().as_secs_f32(),
            minimax_a_b_pruning_sorting.nodes_searched
        );

        // assert things on the resulting positions being the same
        assert_eq!(pure_minimax_4.best_move, minimax_a_b_pruning.best_move);
        assert_eq!(
            pure_minimax_4.best_move,
            minimax_a_b_pruning_sorting.best_move
        );

        // let now_negamax_fast_no_q = Instant::now();
        // let res_neg_fast_no_q = negamax_fast(4, &b, MIN_SCORE, MAX_SCORE, false);
        // // Sebastian has 0.025 s, 4916 positions
        // println!("negamax fast: {:.6}s", now_negamax_fast_no_q.elapsed().as_secs_f32());

        // let now_negamax_fast = Instant::now();
        // let res_neg_fast = negamax_fast(4, &b, MIN_SCORE, MAX_SCORE, true);
        // // Sebastian has 0.025 s, 4916 positions
        // println!("negamax fast: {:.6}s", now_negamax_fast.elapsed().as_secs_f32());
    }

    // #[test]
    // fn mate_in_two_1() {
    //     // https://wtharvey.com/m8n2.txt
    //     // Magnus Carlsen vs Helgi Gretarsson, Rethymnon, 2003
    //     // r5q1/pp1b1kr1/2p2p2/2Q5/2PpB3/1P4NP/P4P2/4RK2 w - - 1 0
    //     // 1. Bg6+ Kxg6 2. Qh5#
    //     let fen = "r5q1/pp1b1kr1/2p2p2/2Q5/2PpB3/1P4NP/P4P2/4RK2 w - - 1 0";
    //     let b = Board::from_fen(fen);
    //     b.draw_to_terminal();
    //     let m = search(4, &b);
    //     print!("{} {}", m.to_human(), m.to_algebraic());
    //     assert_eq!(m.piece, Piece::from_algebraic('B', "e4"));
    //     assert_eq!(m.to, Position::from_algebraic("g6"));
    // }

    #[test]
    fn mate_in_two_2() {
        // https://wtharvey.com/m8n2.txt
        // Jon Hammer vs Magnus Carlsen, Halkidiki, 2003
        // 5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1
        // 1... Qh5+ 2. gxh5 Rh4#

        let fen = "5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1";
        let b = Board::from_fen(fen);

        let m = search(4, &b);
        assert_eq!(m.piece, Piece::from_algebraic('q', "b5"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m.captured.is_none());

        let b = b.execute_move(&m);

        let m = search(4, &b);
        assert_eq!(m.piece, Piece::from_algebraic('P', "g4"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m
            .captured
            .is_some_and(|p| p == Piece::from_algebraic('q', "h5")));

        let b = b.execute_move(&m);

        let m = search(4, &b);
        assert_eq!(m.piece, Piece::from_algebraic('r', "e4"));
        assert_eq!(m.to, Position::from_algebraic("h4"));
    }

    #[test]
    fn mate_in_two_2_m2() {
        // checking evaluation and search after first move
        // https://wtharvey.com/m8n2.txt
        // Jon Hammer vs Magnus Carlsen, Halkidiki, 2003
        // 5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1
        // 1... Qh5+ 2. gxh5 Rh4#
        let fen = "5rk1/ppp2pbp/3p2p1/7q/4r1P1/1NP1B3/PP2nPP1/R2QR2K w - - 1 2";
        let b = Board::from_fen(fen);
        let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
        assert_eq!(legal_moves.len(), 1);

        let m = search(4, &b);
        assert_eq!(m.piece, Piece::from_algebraic('P', "g4"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m
            .captured
            .is_some_and(|p| p == Piece::from_algebraic('q', "h5")));
        let b = b.execute_move(&m);

        let m = search(2, &b);
        assert_eq!(m.piece, Piece::from_algebraic('r', "e4"));
        assert_eq!(m.to, Position::from_algebraic("h4"));
    }

    #[test]
    fn mate_in_three_1() {
        // https://wtharvey.com/m8n3.txt
        // Madame de Remusat vs Napoleon I, Paris, 1802
        // r1b1kb1r/pppp1ppp/5q2/4n3/3KP3/2N3PN/PPP4P/R1BQ1B1R b kq - 0 1
        // 1... Bc5+ 2. Kxc5 Qb6+ 3. Kd5 Qd6#

        let fen = "r1b1kb1r/pppp1ppp/5q2/4n3/3KP3/2N3PN/PPP4P/R1BQ1B1R b kq - 0 1";
        let b = Board::from_fen(fen);

        // Mate in 3 requires depth 6 to see through the full line
        let m = search(6, &b);
        assert!(m.piece == Piece::from_algebraic('b', "f8"));
        assert!(m.to == Position::from_algebraic("c5"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        assert!(m.piece == Piece::from_algebraic('K', "d4"));
        assert!(m.to == Position::from_algebraic("c5"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        assert!(m.piece == Piece::from_algebraic('q', "f6"));
        assert!(m.to == Position::from_algebraic("b6"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        assert!(m.piece == Piece::from_algebraic('K', "c5"));
        assert!(m.to == Position::from_algebraic("d5"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        assert!(m.piece == Piece::from_algebraic('q', "b6"));
        assert!(m.to == Position::from_algebraic("d6"));
    }

    #[test]
    fn mate_in_three_2() {
        // https://wtharvey.com/m8n3.txt
        // Jean Netzer vs Maxime Vachier-Lagrave, Hyeres, 2002
        // 5r2/6k1/p2p4/6n1/P3p3/8/5P2/2q2QKR b - - 0 1
        // 1... Nf3+ 2. Kg2 Qg5+ 3. Kh3 Rh8#

        let fen = "5r2/6k1/p2p4/6n1/P3p3/8/5P2/2q2QKR b - - 0 1";
        let b = Board::from_fen(fen);

        // Mate in 3 requires depth 6 to see through the full line
        let m = search(6, &b);
        assert_eq!(m.piece, Piece::from_algebraic('n', "g5"));
        assert_eq!(m.to, Position::from_algebraic("f3"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        assert_eq!(m.piece, Piece::from_algebraic('K', "g1"));
        assert_eq!(m.to, Position::from_algebraic("g2"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        assert_eq!(m.piece, Piece::from_algebraic('q', "c1"));
        assert_eq!(m.to, Position::from_algebraic("g5"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        assert_eq!(m.piece, Piece::from_algebraic('K', "g2"));
        assert_eq!(m.to, Position::from_algebraic("h3"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        assert_eq!(m.piece, Piece::from_algebraic('r', "f8"));
        assert_eq!(m.to, Position::from_algebraic("h8"));
    }

    //     #[test]
    //     fn perft_1() {
    //         let b = Board::new();
    //         let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
    //         assert_eq!(legal_moves.len(), 20);

    //         let mut total_nodes = 0;
    //         for m in legal_moves {
    //             let b = b.execute_move(&m);
    //             let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
    //             total_nodes += legal_moves.len();
    //         }
    //         assert_eq!(total_nodes, 400);
    //     }

    //     #[test]
    //     fn perft_2() {
    //         let b = Board::new();
    //         assert_eq!(minimax_no_pruning(1, &b).unwrap().nodes_searched, 20);
    //         assert_eq!(minimax_no_pruning(2, &b).unwrap().nodes_searched, 400);
    //         assert_eq!(minimax_no_pruning(3, &b).unwrap().nodes_searched, 8902);
    //         assert_eq!(minimax_no_pruning(4, &b).unwrap().nodes_searched, 197281);
    //         assert_eq!(minimax_no_pruning(5, &b).unwrap().nodes_searched, 4865609);
    //         assert_eq!(minimax_no_pruning(6, &b).unwrap().nodes_searched, 119060324);
    //     }

    // ==========================================================================
    // MovePicker-based search tests
    // ==========================================================================

    #[test]
    fn test_movepicker_search_preserves_board_state_depth_1() {
        let mut board = Board::new();
        let original_fen = board.to_fen();
        let mut tt = crate::tt::TranspositionTable::new(16);

        let _ = iterative_deepening_movepicker(&mut board, 1, &mut tt);

        let after_fen = board.to_fen();
        assert_eq!(original_fen, after_fen, "Board state should be preserved after depth 1 search");
    }

    #[test]
    fn test_movepicker_search_preserves_board_state_depth_2() {
        let mut board = Board::new();
        let original_fen = board.to_fen();
        let mut tt = crate::tt::TranspositionTable::new(16);

        let _ = iterative_deepening_movepicker(&mut board, 2, &mut tt);

        let after_fen = board.to_fen();
        assert_eq!(original_fen, after_fen, "Board state should be preserved after depth 2 search");
    }

    #[test]
    fn test_movepicker_search_preserves_board_state_depth_3() {
        let mut board = Board::new();
        let original_fen = board.to_fen();
        let mut tt = crate::tt::TranspositionTable::new(16);

        let _ = iterative_deepening_movepicker(&mut board, 3, &mut tt);

        let after_fen = board.to_fen();
        assert_eq!(original_fen, after_fen, "Board state should be preserved after depth 3 search");
    }

    #[test]
    fn test_movepicker_search_preserves_board_state_depth_4() {
        let mut board = Board::new();
        let original_fen = board.to_fen();
        let mut tt = crate::tt::TranspositionTable::new(16);

        let _ = iterative_deepening_movepicker(&mut board, 4, &mut tt);

        let after_fen = board.to_fen();
        assert_eq!(original_fen, after_fen, "Board state should be preserved after depth 4 search");
    }

    #[test]
    fn test_movepicker_search_preserves_board_state_depth_5() {
        let mut board = Board::new();
        let original_fen = board.to_fen();
        let mut tt = crate::tt::TranspositionTable::new(16);

        let _ = iterative_deepening_movepicker(&mut board, 5, &mut tt);

        let after_fen = board.to_fen();
        assert_eq!(original_fen, after_fen, "Board state should be preserved after depth 5 search");
    }

    #[test]
    fn test_movepicker_search_finds_move_starting_position() {
        let mut board = Board::new();
        let mut tt = crate::tt::TranspositionTable::new(16);

        let result = iterative_deepening_movepicker(&mut board, 4, &mut tt);

        assert!(result.best_move.is_some(), "Should find a move in starting position");
    }

    #[test]
    fn test_movepicker_search_different_positions() {
        let positions = [
            "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
            "r2qkb1r/ppp2ppp/2n1bn2/3pp3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R w KQkq - 0 6",
            "r1bq1rk1/ppp2ppp/2n1pn2/3p4/1bPP4/2NBPN2/PP3PPP/R1BQK2R w KQ - 0 7",
        ];

        for fen in positions {
            let mut board = Board::from_fen(fen);
            let original_fen = board.to_fen();
            let mut tt = crate::tt::TranspositionTable::new(16);

            let result = iterative_deepening_movepicker(&mut board, 4, &mut tt);

            let after_fen = board.to_fen();
            assert_eq!(original_fen, after_fen, "Board state should be preserved for position: {}", fen);
            assert!(result.best_move.is_some(), "Should find a move for position: {}", fen);
        }
    }

    #[test]
    fn test_negamax_movepicker_preserves_board_state() {
        let mut board = Board::new();
        let original_fen = board.to_fen();
        let mut tt = crate::tt::TranspositionTable::new(16);
        let mut search_state = SearchState::new();

        for depth in 1..=4 {
            let mut test_board = Board::new();
            let before = test_board.to_fen();

            let _ = negamax_movepicker(
                depth,
                &mut test_board,
                MIN_SCORE,
                MAX_SCORE,
                &mut tt,
                &mut search_state,
                0,
                &mut Vec::new(),
            );

            let after = test_board.to_fen();
            assert_eq!(before, after, "Board state should be preserved at depth {}", depth);
        }
    }

    #[test]
    fn repetition_detection_avoids_threefold_game1() {
        // Game 1 (6N3BgiGZ): bot played g2g3 repeatedly leading to threefold
        // After 108 moves, position after g3g2 (move 108) already appeared twice before
        // Engine must NOT play g2g3 again (or if it does, score should be 0 = draw)
        let moves_str = "e2e4 e7e5 g1f3 g8f6 f3e5 d8e7 d2d4 d7d6 e5f3 f6e4 f1e2 c7c5 b1c3 c5d4 c3d5 e7d8 e1g1 b8c6 f3d4 c8e6 e2c4 c6d4 d1d4 e4f6 f1e1 f8e7 d5f6 e7f6 d4e4 f6e5 c4e6 f7e6 f2f4 e5f6 e4b7 e8g8 e1e6 f6d4 g1f1 d8h4 e6e2 g8h8 g2g3 h4h5 c2c3 d4f6 c1e3 a8d8 b7a7 h5f3 e3f2 h8g8 a7e3 f3h1 f2g1 h1b7 a1d1 g8h8 e3d3 h7h6 d3d5 b7a6 g1e3 f8g8 d1d2 g8e8 f1f2 h8h7 e3d4 e8e2 d2e2 f6d4 c3d4 a6d3 h2h4 d3d1 h4h5 h7h8 f4f5 d8f8 d5e4 d1c1 d4d5 c1g5 g3g4 g5h4 f2g2 f8c8 b2b4 c8f8 b4b5 f8b8 a2a4 h4g5 e4b4 g5d8 e2e6 b8c8 e6d6 c8c2 g2g3 c2c3 g3g2 c3c2 g2g3 c2c3 g3g2 c3c2";
        let moves: Vec<&str> = moves_str.split_whitespace().collect();

        let mut board = Board::new();
        let mut position_history: Vec<u64> = vec![board.zobrist_hash];

        for (i, move_str) in moves.iter().enumerate() {
            let legal_moves = board.get_legal_moves(&board.get_active_color()).unwrap();
            let from_file = move_str.chars().nth(0).unwrap() as u8 - b'a' + 1;
            let from_rank = move_str.chars().nth(1).unwrap().to_digit(10).unwrap() as u8;
            let to_file = move_str.chars().nth(2).unwrap() as u8 - b'a' + 1;
            let to_rank = move_str.chars().nth(3).unwrap().to_digit(10).unwrap() as u8;
            let from = crate::types::Position { rank: from_rank, file: from_file };
            let to = crate::types::Position { rank: to_rank, file: to_file };
            let mv = legal_moves.iter()
                .find(|m| m.from == from && m.to == to && !matches!(m.move_flag, crate::types::MoveFlag::Promotion(_)))
                .unwrap_or_else(|| panic!("No legal move for {} at move {}", move_str, i + 1));
            board = board.execute_move(mv);
            position_history.push(board.zobrist_hash);
        }

        // Current position: White to move after 108 moves (last was c3c2 by Black)
        // The position after g3g2 (move 108, hash at index 108) matches hashes at [100, 104]
        // So if White plays g2g3 again, the resulting position will match hashes at [102, 106]
        // The engine should detect this repetition and avoid g2g3

        let mut tt = crate::tt::TranspositionTable::new(16);
        let mut search_state = SearchState::new();
        let control = std::sync::Arc::new(SearchControl::new(30_000, 30_000));

        // Search at depth 8 (same as production)
        let result = aspiration_search_movepicker_with_control(
            8, &mut board, &mut tt, &mut search_state, 0, &control,
            &mut position_history,
        );

        match result {
            Ok((_score, mv, _, _)) => {
                let mv_uci = mv.as_ref().map(|m| m.to_uci()).unwrap_or("none".to_string());

                // The engine must not play g2g3 (the repeating move)
                assert_ne!(mv_uci, "g2g3", "Engine must not play the repeating move g2g3");
            }
            Err(_) => panic!("Search was aborted"),
        }
    }

    #[test]
    fn repetition_detection_avoids_threefold_game2() {
        // Game 2 (OFj0tHpW): bot (White) kept playing c3b3/b3c3 rook shuffle
        // Test at move 37 (index 72): White should not play c3b3
        let moves_str = "d2d4 e7e6 e2e4 d7d5 b1c3 f8b4 e4e5 c7c5 a2a3 b4c3 b2c3 g8e7 d1g4 e7f5 f1d3 h7h5 g4f4 d8c7 c1d2 g7g6 g1f3 b8c6 d4c5 c8d7 e1g1 e8c8 c3c4 d5c4 f4c4 c6e5 f3e5 c7e5 d2c3 e5d5 c3h8 d8h8 c4c3 h8g8 a1e1 d7c6 d3e4 d5d4 c3d4 f5d4 e4c6 d4c6 e1e3 g8d8 f1b1 d8d5 e3c3 c8c7 f2f4 c6d4 c3c4 d4e2 g1f2 e2d4 b1b2 d4c6 f2e3 c6a5 c4c3 a5c6 b2b5 a7a6 b5b2 d5d1 c3b3 c6a5 b3c3 a5c6";
        let moves: Vec<&str> = moves_str.split_whitespace().collect();

        let mut board = Board::new();
        let mut position_history: Vec<u64> = vec![board.zobrist_hash];

        for (i, move_str) in moves.iter().enumerate() {
            let legal_moves = board.get_legal_moves(&board.get_active_color()).unwrap();
            let from_file = move_str.chars().nth(0).unwrap() as u8 - b'a' + 1;
            let from_rank = move_str.chars().nth(1).unwrap().to_digit(10).unwrap() as u8;
            let to_file = move_str.chars().nth(2).unwrap() as u8 - b'a' + 1;
            let to_rank = move_str.chars().nth(3).unwrap().to_digit(10).unwrap() as u8;
            let from = crate::types::Position { rank: from_rank, file: from_file };
            let to = crate::types::Position { rank: to_rank, file: to_file };
            let mv = legal_moves.iter()
                .find(|m| m.from == from && m.to == to && !matches!(m.move_flag, crate::types::MoveFlag::Promotion(_)))
                .unwrap_or_else(|| panic!("No legal move for {} at move {}", move_str, i + 1));
            board = board.execute_move(mv);
            position_history.push(board.zobrist_hash);
        }

        // White to move — should NOT play c3b3 (the repeating move)
        let mut tt = crate::tt::TranspositionTable::new(16);
        let mut search_state = SearchState::new();
        let control = std::sync::Arc::new(SearchControl::new(30_000, 30_000));

        let result = aspiration_search_movepicker_with_control(
            8, &mut board, &mut tt, &mut search_state, 0, &control,
            &mut position_history,
        );

        match result {
            Ok((_score, mv, _, _)) => {
                let mv_uci = mv.as_ref().map(|m| m.to_uci()).unwrap_or("none".to_string());
                assert_ne!(mv_uci, "c3b3", "Engine must not play the repeating rook move c3b3");
            }
            Err(_) => panic!("Search was aborted"),
        }
    }

    #[test]
    fn repetition_detection_avoids_threefold_game3() {
        // Game 3 (LknqEJkx): bot (Black) kept playing d2d4/d4d2 rook shuffle
        // Test after White plays e2f4 (index 88): Black should not play d2d4
        let moves_str = "d2d4 g8f6 b1c3 d7d5 c1f4 a7a6 e2e3 e7e6 f1d3 b8d7 e1d2 f8b4 a2a3 b4c3 b2c3 c7c5 g2g4 c5c4 g4g5 c4d3 g5f6 d3c2 d1c2 d7f6 g1f3 f6h5 h1g1 h5f4 e3f4 e8g8 d2e3 b7b5 a3a4 b5a4 c2a4 c8b7 a1b1 a8a7 a4a3 b7c8 b1b8 a7c7 g1b1 d8f6 a3a5 c7e7 a5c5 e7e8 b1b2 f6f5 f3d2 f5h3 f2f3 h3h2 d2e4 h2g1 e4f2 g7g5 f4g5 g1g5 e3e2 e6e5 e2f1 g5c1 f1g2 e5d4 c3d4 c8h3 f2h3 c1c5 d4c5 e8b8 b2a2 f8c8 a2a6 c8c5 a6a4 b8b2 g2g3 b2d2 a4a6 g8g7 a6a1 c5c4 h3f4 d2d4 f4e2 d4d2 e2f4 d2d4 f4e2 d4d2 e2f4";
        let moves: Vec<&str> = moves_str.split_whitespace().collect();

        let mut board = Board::new();
        let mut position_history: Vec<u64> = vec![board.zobrist_hash];

        for (i, move_str) in moves.iter().enumerate() {
            let legal_moves = board.get_legal_moves(&board.get_active_color()).unwrap();
            let from_file = move_str.chars().nth(0).unwrap() as u8 - b'a' + 1;
            let from_rank = move_str.chars().nth(1).unwrap().to_digit(10).unwrap() as u8;
            let to_file = move_str.chars().nth(2).unwrap() as u8 - b'a' + 1;
            let to_rank = move_str.chars().nth(3).unwrap().to_digit(10).unwrap() as u8;
            let from = crate::types::Position { rank: from_rank, file: from_file };
            let to = crate::types::Position { rank: to_rank, file: to_file };
            let mv = legal_moves.iter()
                .find(|m| m.from == from && m.to == to && !matches!(m.move_flag, crate::types::MoveFlag::Promotion(_)))
                .unwrap_or_else(|| panic!("No legal move for {} at move {}", move_str, i + 1));
            board = board.execute_move(mv);
            position_history.push(board.zobrist_hash);
        }

        // Black to move — should NOT play d2d4 (the repeating move)
        let mut tt = crate::tt::TranspositionTable::new(16);
        let mut search_state = SearchState::new();
        let control = std::sync::Arc::new(SearchControl::new(30_000, 30_000));

        let result = aspiration_search_movepicker_with_control(
            8, &mut board, &mut tt, &mut search_state, 0, &control,
            &mut position_history,
        );

        match result {
            Ok((_score, mv, _, _)) => {
                let mv_uci = mv.as_ref().map(|m| m.to_uci()).unwrap_or("none".to_string());
                assert_ne!(mv_uci, "d2d4", "Engine must not play the repeating rook move d2d4");
            }
            Err(_) => panic!("Search was aborted"),
        }
    }

    #[test]
    fn fifty_move_rule_returns_draw() {
        // FEN with halfmove_clock = 100 (fifty-move rule triggered)
        let mut board = Board::from_fen("8/8/4k3/8/8/4K3/8/8 w - - 100 80");
        let mut tt = crate::tt::TranspositionTable::new(16);
        let mut search_state = SearchState::new();

        let (score, _, _, _) = negamax_movepicker(
            6, &mut board, MIN_SCORE, MAX_SCORE, &mut tt, &mut search_state, 1, &mut Vec::new(),
        );
        assert_eq!(score, 0, "Position with halfmove_clock=100 should be drawn, got {}", score);
    }

    #[test]
    fn insufficient_material_returns_draw() {
        // King vs King — clearly insufficient
        let mut board = Board::from_fen("8/8/4k3/8/8/4K3/8/8 w - - 0 1");
        let mut tt = crate::tt::TranspositionTable::new(16);
        let mut search_state = SearchState::new();

        let (score, _, _, _) = negamax_movepicker(
            6, &mut board, MIN_SCORE, MAX_SCORE, &mut tt, &mut search_state, 1, &mut Vec::new(),
        );
        assert_eq!(score, 0, "K vs K should be drawn, got {}", score);
    }

    #[test]
    fn test_quiescence_movepicker_preserves_board_state() {
        let positions = [
            "r1bqkb1r/pppp1ppp/2n2n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
            "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
        ];

        for fen in positions {
            let mut board = Board::from_fen(fen);
            let original_fen = board.to_fen();

            let _ = quiescence_movepicker(&mut board, MIN_SCORE, MAX_SCORE, 10);

            let after_fen = board.to_fen();
            assert_eq!(original_fen, after_fen, "Quiescence should preserve board for: {}", fen);
        }
    }
}
