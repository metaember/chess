//! Unified chess engine interface
//!
//! This module provides a single ChessEngine struct that encapsulates
//! all engine components: opening book, transposition table, search state.
//! Both UCI and web server binaries use this for consistent behavior.

use std::sync::Arc;
use std::time::Instant;

use crate::board::Board;
use crate::book::Book;
use crate::search::{
    allocate_time, aspiration_search_movepicker_with_control,
    iterative_deepening_movepicker, iterative_deepening_movepicker_with_control,
    SearchControl, SearchResult, SearchState, MAX_SCORE, MIN_SCORE,
};
use crate::tt::TranspositionTable;
use crate::types::{Color, CompactMove, Move};

/// Search options for pick_move
#[derive(Clone)]
pub struct SearchOptions {
    /// Maximum search depth (ply)
    pub depth: Option<u8>,
    /// Time remaining for our side (ms)
    pub time_left: Option<u64>,
    /// Increment per move (ms)
    pub increment: u64,
    /// Moves until next time control
    pub movestogo: Option<u32>,
    /// Exact time to search (ms)
    pub movetime: Option<u64>,
    /// Search indefinitely until stopped
    pub infinite: bool,
}

impl Default for SearchOptions {
    fn default() -> Self {
        Self {
            depth: Some(8),
            time_left: None,
            increment: 0,
            movestogo: None,
            movetime: None,
            infinite: false,
        }
    }
}

impl SearchOptions {
    pub fn with_depth(depth: u8) -> Self {
        Self {
            depth: Some(depth),
            ..Default::default()
        }
    }

    pub fn with_time(time_left: u64, increment: u64) -> Self {
        Self {
            depth: None,
            time_left: Some(time_left),
            increment,
            ..Default::default()
        }
    }

    pub fn with_movetime(ms: u64) -> Self {
        Self {
            depth: None,
            movetime: Some(ms),
            ..Default::default()
        }
    }
}

/// Result from engine search/book lookup
pub struct EngineResult {
    pub best_move: Move,
    pub score: i32,
    pub from_book: bool,
    pub depth_reached: u8,
    pub nodes_searched: u64,
    pub time_ms: u64,
}

/// Callback for UCI-style info output during search
pub type InfoCallback = Box<dyn Fn(SearchInfo) + Send + Sync>;

/// Information about search progress
#[derive(Clone)]
pub struct SearchInfo {
    pub depth: u8,
    pub score: i32,
    pub nodes: u64,
    pub time_ms: u64,
    pub nps: u64,
    pub pv: String,
}

impl SearchInfo {
    /// Format score as UCI string (cp or mate)
    pub fn format_score(&self) -> String {
        const MATE_THRESHOLD: i32 = 900_000_000;

        if self.score > MATE_THRESHOLD {
            let plies_to_mate = (MAX_SCORE - self.score) / 100;
            let moves_to_mate = (plies_to_mate + 1) / 2;
            format!("mate {}", moves_to_mate.max(1))
        } else if self.score < -MATE_THRESHOLD {
            let plies_to_mate = (self.score - MIN_SCORE) / 100;
            let moves_to_mate = (plies_to_mate + 1) / 2;
            format!("mate -{}", moves_to_mate.max(1))
        } else {
            format!("cp {}", self.score)
        }
    }

    /// Format as UCI info string
    pub fn to_uci(&self) -> String {
        format!(
            "info depth {} score {} nodes {} nps {} time {} pv {}",
            self.depth,
            self.format_score(),
            self.nodes,
            self.nps,
            self.time_ms,
            self.pv
        )
    }
}

/// Unified chess engine
pub struct ChessEngine {
    book: Arc<Book>,
    tt: TranspositionTable,
    search_state: SearchState,
    tt_size_mb: usize,
}

impl ChessEngine {
    /// Create a new engine with default settings
    pub fn new() -> Self {
        Self::with_tt_size(64)
    }

    /// Create a new engine with specified transposition table size
    pub fn with_tt_size(tt_size_mb: usize) -> Self {
        let book = Arc::new(Book::new());
        let tt = TranspositionTable::new(tt_size_mb);
        let search_state = SearchState::new();

        Self {
            book,
            tt,
            search_state,
            tt_size_mb,
        }
    }

    /// Clear transposition table (call on new game)
    pub fn new_game(&mut self) {
        self.tt = TranspositionTable::new(self.tt_size_mb);
        self.search_state = SearchState::new();
    }

    /// Try to get a move from the opening book
    pub fn book_move(&self, board: &Board) -> Option<Move> {
        self.book.suggest_move(board, true)
    }

    /// Pick the best move for the current position
    ///
    /// Tries opening book first, then falls back to search.
    pub fn pick_move(&mut self, board: &mut Board, options: &SearchOptions) -> Option<EngineResult> {
        // Try opening book first
        if let Some(book_move) = self.book.suggest_move(board, true) {
            return Some(EngineResult {
                best_move: book_move,
                score: 0,
                from_book: true,
                depth_reached: 0,
                nodes_searched: 0,
                time_ms: 0,
            });
        }

        // Fall back to search
        self.search(board, options)
    }

    /// Search for the best move (bypasses opening book)
    pub fn search(&mut self, board: &mut Board, options: &SearchOptions) -> Option<EngineResult> {
        let start = Instant::now();

        // Simple depth-only search
        if let Some(depth) = options.depth {
            if options.time_left.is_none() && options.movetime.is_none() {
                let result = iterative_deepening_movepicker(board, depth, &mut self.tt);
                let elapsed = start.elapsed().as_millis() as u64;

                return result.best_move.map(|mv| EngineResult {
                    best_move: mv,
                    score: result.best_score,
                    from_book: false,
                    depth_reached: depth,
                    nodes_searched: (result.nodes_searched + result.quiescent_nodes_searched) as u64,
                    time_ms: elapsed,
                });
            }
        }

        // Time-controlled search
        let control = if let Some(mt) = options.movetime {
            Arc::new(SearchControl::new(mt, mt))
        } else if options.infinite {
            Arc::new(SearchControl::infinite())
        } else if let Some(time_left) = options.time_left {
            let (soft, hard) = allocate_time(time_left, options.increment, options.movestogo);
            Arc::new(SearchControl::new(soft, hard))
        } else {
            // Default: search to depth with no time limit
            let depth = options.depth.unwrap_or(8);
            let result = iterative_deepening_movepicker(board, depth, &mut self.tt);
            let elapsed = start.elapsed().as_millis() as u64;

            return result.best_move.map(|mv| EngineResult {
                best_move: mv,
                score: result.best_score,
                from_book: false,
                depth_reached: depth,
                nodes_searched: (result.nodes_searched + result.quiescent_nodes_searched) as u64,
                time_ms: elapsed,
            });
        };

        let max_depth = options.depth.unwrap_or(64);

        // Iterative deepening with time control
        let mut best_result: Option<SearchResult> = None;
        let mut total_nodes: u64 = 0;
        let mut prev_score = 0i32;
        let mut depth_times: Vec<u64> = Vec::with_capacity(max_depth as usize);
        let mut final_depth = 0u8;

        for depth in 1..=max_depth {
            if control.exceeded_soft_limit() {
                break;
            }

            // Predict if we have time for this depth
            if depth > 2 && !depth_times.is_empty() {
                let last_time = *depth_times.last().unwrap();
                let predicted_time = last_time * 4;
                let elapsed = control.elapsed_ms();
                if elapsed + predicted_time > control.soft_limit_ms {
                    break;
                }
            }

            let depth_start = Instant::now();

            // Use aspiration windows after depth 3
            let result = if depth > 3 {
                aspiration_search_movepicker_with_control(
                    depth,
                    board,
                    &mut self.tt,
                    &mut self.search_state,
                    prev_score,
                    &control,
                )
            } else {
                iterative_deepening_movepicker_with_control(
                    board,
                    depth,
                    &mut self.tt,
                    &mut self.search_state,
                    &control,
                )
                .map(|r| {
                    let compact_move = r.best_move.as_ref().map(|m| CompactMove::from_move(m));
                    (
                        r.best_score,
                        compact_move,
                        r.nodes_searched,
                        r.quiescent_nodes_searched,
                    )
                })
            };

            let depth_time = depth_start.elapsed().as_millis() as u64;
            depth_times.push(depth_time);

            match result {
                Ok((score, mv, nodes, qnodes)) => {
                    total_nodes += (nodes + qnodes) as u64;
                    prev_score = score;
                    final_depth = depth;

                    best_result = Some(SearchResult {
                        best_move: mv.map(|m| m.to_move(board)),
                        best_score: score,
                        nodes_searched: nodes,
                        quiescent_nodes_searched: qnodes,
                    });
                }
                Err(_) => {
                    break;
                }
            }
        }

        let elapsed = start.elapsed().as_millis() as u64;

        best_result.and_then(|r| {
            r.best_move.map(|mv| EngineResult {
                best_move: mv,
                score: r.best_score,
                from_book: false,
                depth_reached: final_depth,
                nodes_searched: total_nodes,
                time_ms: elapsed,
            })
        })
    }

    /// Search with UCI-style info callbacks
    pub fn search_with_info<F>(
        &mut self,
        board: &mut Board,
        options: &SearchOptions,
        info_callback: F,
    ) -> Option<EngineResult>
    where
        F: Fn(SearchInfo),
    {
        let start = Instant::now();

        // Time control setup
        let control = if let Some(mt) = options.movetime {
            Arc::new(SearchControl::new(mt, mt))
        } else if options.infinite {
            Arc::new(SearchControl::infinite())
        } else if let Some(time_left) = options.time_left {
            let (soft, hard) = allocate_time(time_left, options.increment, options.movestogo);
            Arc::new(SearchControl::new(soft, hard))
        } else {
            // Depth-only: use large time limit
            Arc::new(SearchControl::new(3600_000, 3600_000))
        };

        let max_depth = options.depth.unwrap_or(64);

        let mut best_result: Option<SearchResult> = None;
        let mut total_nodes: u64 = 0;
        let mut prev_score = 0i32;
        let mut depth_times: Vec<u64> = Vec::with_capacity(max_depth as usize);
        let mut final_depth = 0u8;

        for depth in 1..=max_depth {
            if control.exceeded_soft_limit() {
                break;
            }

            // Predict if we have time for this depth
            if depth > 2 && !depth_times.is_empty() {
                let last_time = *depth_times.last().unwrap();
                let predicted_time = last_time * 4;
                let elapsed = control.elapsed_ms();
                if elapsed + predicted_time > control.soft_limit_ms {
                    break;
                }
            }

            let depth_start = Instant::now();

            let result = if depth > 3 {
                aspiration_search_movepicker_with_control(
                    depth,
                    board,
                    &mut self.tt,
                    &mut self.search_state,
                    prev_score,
                    &control,
                )
            } else {
                iterative_deepening_movepicker_with_control(
                    board,
                    depth,
                    &mut self.tt,
                    &mut self.search_state,
                    &control,
                )
                .map(|r| {
                    let compact_move = r.best_move.as_ref().map(|m| CompactMove::from_move(m));
                    (
                        r.best_score,
                        compact_move,
                        r.nodes_searched,
                        r.quiescent_nodes_searched,
                    )
                })
            };

            let depth_time = depth_start.elapsed().as_millis() as u64;
            depth_times.push(depth_time);

            match result {
                Ok((score, mv, nodes, qnodes)) => {
                    total_nodes += (nodes + qnodes) as u64;
                    prev_score = score;
                    final_depth = depth;

                    let elapsed_ms = start.elapsed().as_millis() as u64;
                    let nps = if elapsed_ms > 0 {
                        total_nodes * 1000 / elapsed_ms
                    } else {
                        0
                    };

                    let pv = mv.as_ref().map(|m| m.to_uci()).unwrap_or_default();

                    info_callback(SearchInfo {
                        depth,
                        score,
                        nodes: total_nodes,
                        time_ms: elapsed_ms,
                        nps,
                        pv,
                    });

                    best_result = Some(SearchResult {
                        best_move: mv.map(|m| m.to_move(board)),
                        best_score: score,
                        nodes_searched: nodes,
                        quiescent_nodes_searched: qnodes,
                    });
                }
                Err(_) => {
                    break;
                }
            }
        }

        let elapsed = start.elapsed().as_millis() as u64;

        best_result.and_then(|r| {
            r.best_move.map(|mv| EngineResult {
                best_move: mv,
                score: r.best_score,
                from_book: false,
                depth_reached: final_depth,
                nodes_searched: total_nodes,
                time_ms: elapsed,
            })
        })
    }
}

impl Default for ChessEngine {
    fn default() -> Self {
        Self::new()
    }
}
