// use rayon::prelude::*;

use crate::board::*;
use crate::book::Book;
use crate::evaluate::*;
use crate::tt::{TranspositionTable, TTFlag};
use crate::types::{Color, Move, MoveFlag, PieceType, Position, Status};

pub const MIN_SCORE: i32 = -1_000_000_000;
pub const MAX_SCORE: i32 = 1_000_000_000;

/// Maximum search depth for killer move storage
pub const MAX_PLY: usize = 64;

/// Search state containing killer moves and history heuristic
pub struct SearchState {
    /// Killer moves: 2 quiet moves per ply that caused beta cutoffs
    /// These are moves that were good in sibling nodes and might be good here too
    pub killer_moves: [[Option<Move>; 2]; MAX_PLY],

    /// History heuristic: [color][from_square][to_square] -> score
    /// Accumulates bonuses for quiet moves that cause beta cutoffs
    pub history: [[[i32; 64]; 64]; 2],
}

impl SearchState {
    pub fn new() -> Self {
        SearchState {
            killer_moves: [[None; 2]; MAX_PLY],
            history: [[[0; 64]; 64]; 2],
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
                }
            }
        }
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

#[derive(Debug)]
pub struct SearchResult {
    pub best_move: Option<Move>,
    pub best_score: i32,
    pub nodes_searched: i32,
    pub quiescent_nodes_searched: i32,
    // TODO remove these two fields in prod for speed reasons
    pub moves: Vec<Move>, // path of moves down the tree
}

impl SearchResult {
    pub fn print(&self) {
        println!(
            "Search result: [{}, nodes: {} tot, {} quies] {}: {}",
            self.best_score,
            self.nodes_searched,
            self.quiescent_nodes_searched,
            self.best_move.unwrap().to_algebraic(),
            self.best_move.unwrap().to_human()
        );
        println!("Move path:");
        self.moves
            .iter()
            .rev()
            .for_each(|m| println!("  • {}: {}", m.to_algebraic(), m.to_human()));
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
            moves: vec![],
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

/// Iterative deepening search with transposition table and aspiration windows
/// Searches at depths 1, 2, 3, ... up to max_depth
/// Each iteration benefits from TT entries from previous iterations
/// Uses aspiration windows: narrow search window centered on previous score
/// Takes mutable board reference to avoid cloning - board state is preserved after search
pub fn iterative_deepening(
    max_depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
) -> Result<SearchResult, Vec<Move>> {
    let mut best_result: Option<SearchResult> = None;
    let mut total_nodes = 0;
    let mut total_quiescent_nodes = 0;
    let mut search_state = SearchState::new();
    let mut prev_score = 0;

    // Aspiration window parameters
    const INITIAL_WINDOW: i32 = 25; // ±25 centipawns

    for depth in 1..=max_depth {
        // Age history at the start of each new depth iteration
        search_state.age_history();

        // Use aspiration windows after depth 1
        let result = if depth > 1 {
            aspiration_search(depth, board, tt, &mut search_state, prev_score, INITIAL_WINDOW)?
        } else {
            negamax_with_tt_mut(depth, board, MIN_SCORE, MAX_SCORE, tt, &mut search_state, 0)?
        };

        total_nodes += result.nodes_searched;
        total_quiescent_nodes += result.quiescent_nodes_searched;
        prev_score = result.best_score;

        best_result = Some(SearchResult {
            best_move: result.best_move,
            best_score: result.best_score,
            nodes_searched: total_nodes,
            quiescent_nodes_searched: total_quiescent_nodes,
            moves: result.moves,
        });
    }

    Ok(best_result.unwrap())
}

/// Aspiration window search: start with narrow window, widen on fail
fn aspiration_search(
    depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
    search_state: &mut SearchState,
    prev_score: i32,
    initial_window: i32,
) -> Result<SearchResult, Vec<Move>> {
    let mut alpha = prev_score - initial_window;
    let mut beta = prev_score + initial_window;
    let mut window = initial_window;

    loop {
        let result = negamax_with_tt_mut(depth, board, alpha, beta, tt, search_state, 0)?;

        // Check if score is within the window (not a fail-low or fail-high)
        if result.best_score > alpha && result.best_score < beta {
            return Ok(result);
        }

        // Fail-low: score <= alpha, need to widen lower bound
        if result.best_score <= alpha {
            alpha = if window >= 200 { MIN_SCORE } else { alpha - window };
        }

        // Fail-high: score >= beta, need to widen upper bound
        if result.best_score >= beta {
            beta = if window >= 200 { MAX_SCORE } else { beta + window };
        }

        // Double the window for next iteration
        window *= 2;

        // If window is huge, just do full search
        if alpha <= MIN_SCORE + 1000 && beta >= MAX_SCORE - 1000 {
            return negamax_with_tt_mut(depth, board, MIN_SCORE, MAX_SCORE, tt, search_state, 0);
        }
    }
}

/// Iterative deepening with time limit (in milliseconds) and aspiration windows
/// Returns the best result found within the time limit
pub fn iterative_deepening_timed(
    max_depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
    max_time_ms: u64,
) -> Result<SearchResult, Vec<Move>> {
    use std::time::Instant;

    let mut best_result: Option<SearchResult> = None;
    let mut total_nodes = 0;
    let mut total_quiescent_nodes = 0;
    let mut search_state = SearchState::new();
    let mut prev_score = 0;
    let start = Instant::now();

    const INITIAL_WINDOW: i32 = 25;

    for depth in 1..=max_depth {
        // Check if we've exceeded time limit
        if start.elapsed().as_millis() as u64 > max_time_ms {
            break;
        }

        // Age history at the start of each new depth iteration
        search_state.age_history();

        // Use aspiration windows after depth 1
        let result = if depth > 1 {
            aspiration_search(depth, board, tt, &mut search_state, prev_score, INITIAL_WINDOW)?
        } else {
            negamax_with_tt_mut(depth, board, MIN_SCORE, MAX_SCORE, tt, &mut search_state, 0)?
        };

        total_nodes += result.nodes_searched;
        total_quiescent_nodes += result.quiescent_nodes_searched;
        prev_score = result.best_score;

        best_result = Some(SearchResult {
            best_move: result.best_move,
            best_score: result.best_score,
            nodes_searched: total_nodes,
            quiescent_nodes_searched: total_quiescent_nodes,
            moves: result.moves,
        });
    }

    Ok(best_result.unwrap_or(SearchResult {
        best_move: None,
        best_score: 0,
        nodes_searched: total_nodes,
        quiescent_nodes_searched: total_quiescent_nodes,
        moves: vec![],
    }))
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
            best_move,
            best_score: score,
            nodes_searched: 0,
            quiescent_nodes_searched: 0,
            moves: vec![],
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
                    moves: vec![],
                });
            }
        }
    }

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: MIN_SCORE,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
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
                let score = if Some(*m) == tt_best_move {
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
    let mut current_moves = vec![];

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
                moves,
            }) => {
                total_nodes_searched += nodes_searched;
                total_quiescent_nodes_searched += quiescent_nodes_searched;
                let evaluation = -evaluation;

                if evaluation > current_best_score {
                    current_best_score = evaluation;
                    current_best_move = *m;
                    current_moves = moves;
                }

                if evaluation >= beta {
                    // Beta cutoff - store as lower bound
                    tt.store(
                        board.zobrist_hash,
                        max_depth,
                        evaluation,
                        TTFlag::LowerBound,
                        Some(*m),
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
                        moves: current_moves,
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
        Some(current_best_move),
    );

    current_moves.push(current_best_move);

    Ok(SearchResult {
        best_move: Some(current_best_move),
        best_score: current_best_score,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        moves: current_moves,
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
                moves: vec![],
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
                best_score: MIN_SCORE,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
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
    let mut current_moves = vec![];

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
                moves,
            }) => {
                total_nodes_searched += nodes_searched;
                total_quiescent_nodes_searched += quiescent_nodes_searched;
                let evaluation = -evaluation;

                if evaluation > current_best_score {
                    current_best_score = evaluation;
                    current_best_move = *m;
                    current_moves = moves;
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

    current_moves.push(current_best_move);

    Ok(SearchResult {
        best_move: Some(current_best_move),
        best_score: current_best_score,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        moves: current_moves,
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
            moves: vec![],
        });
    }

    // If we've reached max quiescence depth, return static evaluation
    if depth_remaining == 0 {
        return Ok(SearchResult {
            best_move: None,
            best_score: evaluation,
            nodes_searched: 1,
            quiescent_nodes_searched: 1,
            moves: vec![],
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
    let mut current_moves = vec![];

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
            current_moves = search_res.moves;
            current_moves.push(m);

            return Ok(SearchResult {
                best_move: current_best_move,
                best_score: beta,
                nodes_searched: total_nodes_searched,
                quiescent_nodes_searched: total_quiescent_nodes_searched,
                moves: current_moves,
            });
        }
        alpha = alpha.max(evaluation);
    }
    return Ok(SearchResult {
        best_move: current_best_move,
        best_score: alpha,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        moves: current_moves,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::types::{Color, Piece};
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
    #[ignore] // Test incomplete (has todo!()) and behavior changed after adding promotions to quiescence
    fn test_blunders_knight_2() {
        // . ♞ . ♛ ♚ ♝ . ♜
        // ♜ ♝ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
        // ♟︎ ♟︎ . . . . . .
        // . . . . . ♟︎ . .
        // . . . . ♘ . . .
        // . . . ♕ ♙ . . .
        // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
        // ♖ . ♗ . ♔ . ♘ ♖

        // 1n1qkb1r/rbppp1pp/pp6/5p2/4N3/3QP3/PPPP1PPP/R1B1K1NR w KQk - 0 8

        // 1. Nb1c3 Ng8f6 2. e2e3 a7a6 3. Bf1d3 Ra8a7 4. Qd1e2 b7b6 5. Bd3e4 Nf6xe4 6. Nc3xe4 Bc8b7
        // 7. Qe2d3 f7f5 8. f2f3?? f5xe4
        // this series of moves should get us to the position from the previous test. When running the test above it
        // passes but when playing the game it still blunders f3
        let b = Board::new();
        b.draw_to_terminal();

        // 1
        let b = b.execute_move(&Move::from_algebraic(&b, "b1", "c3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "g8", "f6"));
        // 2
        let b = b.execute_move(&Move::from_algebraic(&b, "e2", "e3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "a7", "a6"));
        // 3
        let b = b.execute_move(&Move::from_algebraic(&b, "f1", "d3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "a8", "a7"));
        // 4
        let b = b.execute_move(&Move::from_algebraic(&b, "d1", "e2"));
        let b = b.execute_move(&Move::from_algebraic(&b, "b7", "b6"));
        // 5
        let b = b.execute_move(&Move::from_algebraic(&b, "d3", "e4"));
        let b = b.execute_move(&Move::from_algebraic(&b, "f6", "e4"));
        b.draw_to_terminal();

        // 6
        let b = b.execute_move(&Move::from_algebraic(&b, "c3", "e4"));
        let b = b.execute_move(&Move::from_algebraic(&b, "c8", "b7"));
        b.draw_to_terminal();

        // 7
        let b = b.execute_move(&Move::from_algebraic(&b, "e2", "d3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "f7", "f5"));
        b.draw_to_terminal();

        // make sure the Knight has legal moves
        let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
        let knight_moves = legal_moves
            .iter()
            .filter(|m| m.piece.piece_type == PieceType::Knight && m.piece.position.rank == 4)
            .collect::<Vec<&Move>>();
        assert!(knight_moves.len() > 5);
        // knight_moves
        //     .iter()
        //     .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(knight_moves.contains(&&Move::from_algebraic(&b, "e4", "c3")));
        // ok the knight has good squares to move to, that's not the issue
        // also it's not pinned or it would not show up in the legal moves

        let blunder_move = Move::from_algebraic(&b, "f2", "f3");
        let b_after_blunder = b.execute_move(&blunder_move);
        println!("After blunder:");
        b_after_blunder.draw_to_terminal();

        let black_pseudo_moves =
            b_after_blunder.get_all_pseudo_moves(b_after_blunder.get_active_color(), true);
        let capture_knight_move = Move::from_algebraic(&b_after_blunder, "f5", "e4");
        assert!(black_pseudo_moves.contains(&&capture_knight_move));
        assert!(capture_knight_move
            .captured
            .is_some_and(|p| p == Piece::from_algebraic('N', "e4")));
        // ok so white should know black can capture the knight

        // check the eval before and after the blunder
        let eval_before_blunder = evaluate_board(&b);
        let eval_after_blunder = evaluate_board(&b_after_blunder);
        println!("Eval before blunder: {}", eval_before_blunder);
        println!("Eval after blunder: {}\n", eval_after_blunder);
        assert!(eval_before_blunder - 150 < eval_before_blunder);

        let after_recapture = b_after_blunder.execute_move(&capture_knight_move);
        println!("After recapture:");
        after_recapture.draw_to_terminal();
        let eval_after_recapture = evaluate_board(&after_recapture);
        println!("Eval after recapture: {}\n", eval_after_recapture);

        // ok so going back to before the blunder
        println!("Search before blunder: 4 moves deep");
        assert!(b.get_active_color() == Color::White);
        let search_result = minimax(4, &b).unwrap();
        search_result.print();

        // search 2 moves deep
        println!("Search before blunder: 2 moves deep");
        assert!(b.get_active_color() == Color::White);
        let search_result = minimax(2, &b).unwrap();
        search_result.print();
        assert!(search_result.best_move.unwrap().piece == Piece::from_algebraic('N', "e4"));

        // search 5 moves deep
        println!("Search before blunder: 5 moves deep");
        assert!(b.get_active_color() == Color::White);
        let search_result = minimax(5, &b).unwrap();
        println!(
            "search result: {} {}",
            search_result.best_move.unwrap().to_human(),
            search_result.best_score
        );
        search_result
            .moves
            .iter()
            .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(search_result.best_move.unwrap().piece == Piece::from_algebraic('N', "e4"));

        let m = search(4, &b);
        print!("best move: {} {}", m.to_human(), m.to_algebraic());
        // Move the knight, it's under attacj by a pawn!
        assert!(m.piece == Piece::from_algebraic('N', "e4"));

        todo!("Implement");
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
}
