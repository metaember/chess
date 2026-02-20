//! UCI (Universal Chess Interface) protocol implementation
//!
//! This binary provides a UCI-compatible interface for the chess engine,
//! allowing it to communicate with chess GUIs and services like Lichess.
//!
//! Usage:
//!   cargo run --release --bin uci
//!
//! Then send UCI commands via stdin. Example session:
//!   > uci
//!   < id name RustChess
//!   < id author Your Name
//!   < uciok
//!   > isready
//!   < readyok
//!   > position startpos moves e2e4 e7e5
//!   > go wtime 300000 btime 300000 winc 0 binc 0
//!   < info depth 1 score cp 35 nodes 20 pv e2e4
//!   < info depth 2 score cp 10 nodes 150 pv e2e4 e7e5
//!   < bestmove g1f3

use std::io::{self, BufRead, Write};
use std::time::Instant;

use rust_chess::board::Board;
use rust_chess::search::{
    allocate_time, aspiration_search_with_control, negamax_with_control,
    SearchControl, SearchResult, MIN_SCORE, MAX_SCORE,
};
use rust_chess::tt::TranspositionTable;
use rust_chess::types::{Move, MoveFlag};

const ENGINE_NAME: &str = "RustChess";
const ENGINE_AUTHOR: &str = "Chess Engine";

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut board = Board::new();
    let mut tt = TranspositionTable::new(64); // 64 MB transposition table

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };

        let tokens: Vec<&str> = line.split_whitespace().collect();
        if tokens.is_empty() {
            continue;
        }

        match tokens[0] {
            "uci" => {
                println!("id name {}", ENGINE_NAME);
                println!("id author {}", ENGINE_AUTHOR);
                // Options could be added here
                // println!("option name Hash type spin default 64 min 1 max 1024");
                println!("uciok");
                stdout.flush().unwrap();
            }

            "isready" => {
                println!("readyok");
                stdout.flush().unwrap();
            }

            "ucinewgame" => {
                board = Board::new();
                tt.clear();
            }

            "position" => {
                parse_position(&tokens, &mut board);
            }

            "go" => {
                let best_move = parse_go(&tokens, &mut board, &mut tt);
                if let Some(mv) = best_move {
                    println!("bestmove {}", move_to_uci(&mv));
                    stdout.flush().unwrap();
                }
            }

            "stop" => {
                // In a threaded implementation, this would signal the search to stop
                // For now, we run searches synchronously
            }

            "quit" => {
                break;
            }

            "d" | "display" => {
                // Debug: display the current board
                board.draw_to_terminal();
            }

            _ => {
                // Unknown command, ignore
            }
        }
    }
}

fn parse_position(tokens: &[&str], board: &mut Board) {
    let mut idx = 1;

    // Parse starting position
    if idx < tokens.len() && tokens[idx] == "startpos" {
        *board = Board::new();
        idx += 1;
    } else if idx < tokens.len() && tokens[idx] == "fen" {
        idx += 1;
        // Collect FEN string (up to 6 parts)
        let mut fen_parts = Vec::new();
        while idx < tokens.len() && tokens[idx] != "moves" {
            fen_parts.push(tokens[idx]);
            idx += 1;
        }
        let fen = fen_parts.join(" ");
        *board = Board::from_fen(&fen);
    }

    // Parse moves
    if idx < tokens.len() && tokens[idx] == "moves" {
        idx += 1;
        while idx < tokens.len() {
            let move_str = tokens[idx];
            if let Some(mv) = parse_uci_move(board, move_str) {
                *board = board.execute_move(&mv);
            }
            idx += 1;
        }
    }
}

fn parse_go(tokens: &[&str], board: &mut Board, tt: &mut TranspositionTable) -> Option<Move> {
    let mut wtime: Option<u64> = None;
    let mut btime: Option<u64> = None;
    let mut winc: u64 = 0;
    let mut binc: u64 = 0;
    let mut movestogo: Option<u32> = None;
    let mut depth: Option<u8> = None;
    let mut movetime: Option<u64> = None;
    let mut infinite = false;

    let mut idx = 1;
    while idx < tokens.len() {
        match tokens[idx] {
            "wtime" => {
                idx += 1;
                wtime = tokens.get(idx).and_then(|s| s.parse().ok());
            }
            "btime" => {
                idx += 1;
                btime = tokens.get(idx).and_then(|s| s.parse().ok());
            }
            "winc" => {
                idx += 1;
                winc = tokens.get(idx).and_then(|s| s.parse().ok()).unwrap_or(0);
            }
            "binc" => {
                idx += 1;
                binc = tokens.get(idx).and_then(|s| s.parse().ok()).unwrap_or(0);
            }
            "movestogo" => {
                idx += 1;
                movestogo = tokens.get(idx).and_then(|s| s.parse().ok());
            }
            "depth" => {
                idx += 1;
                depth = tokens.get(idx).and_then(|s| s.parse().ok());
            }
            "movetime" => {
                idx += 1;
                movetime = tokens.get(idx).and_then(|s| s.parse().ok());
            }
            "infinite" => {
                infinite = true;
            }
            _ => {}
        }
        idx += 1;
    }

    // Determine time allocation
    let is_white = board.get_active_color() == rust_chess::types::Color::White;
    let (time_left, increment) = if is_white {
        (wtime.unwrap_or(60000), winc)
    } else {
        (btime.unwrap_or(60000), binc)
    };

    let control = if let Some(mt) = movetime {
        // Fixed time per move
        SearchControl::new(mt, mt)
    } else if infinite {
        // Infinite search (until "stop" command)
        SearchControl::infinite()
    } else {
        // Time-based search
        let (soft, hard) = allocate_time(time_left, increment, movestogo);
        SearchControl::new(soft, hard)
    };

    // Run iterative deepening with UCI info output
    let max_depth = depth.unwrap_or(64);
    iterative_deepening_with_info(max_depth, board, tt, &control)
}

/// Iterative deepening with UCI info output after each depth
fn iterative_deepening_with_info(
    max_depth: u8,
    board: &mut Board,
    tt: &mut TranspositionTable,
    control: &SearchControl,
) -> Option<Move> {
    let mut best_result: Option<SearchResult> = None;
    let mut total_nodes: u64 = 0;
    let mut prev_score = 0i32;
    let mut depth_times: Vec<u64> = Vec::with_capacity(max_depth as usize);
    let search_start = Instant::now();

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

        let depth_start = Instant::now();

        // Use aspiration windows after depth 1
        let result = if depth > 1 {
            aspiration_search_with_control(depth, board, tt, prev_score, control)
        } else {
            negamax_with_control(depth, board, MIN_SCORE, MAX_SCORE, tt, control)
        };

        let depth_time = depth_start.elapsed().as_millis() as u64;
        depth_times.push(depth_time);

        match result {
            Ok(search_result) => {
                total_nodes += (search_result.nodes_searched + search_result.quiescent_nodes_searched) as u64;
                prev_score = search_result.best_score;

                // Output UCI info line
                let elapsed_ms = search_start.elapsed().as_millis() as u64;
                let nps = if elapsed_ms > 0 { total_nodes * 1000 / elapsed_ms } else { 0 };

                // Format score (handle mate scores)
                let score_str = format_score(search_result.best_score);

                // Format PV (principal variation)
                let pv_str = if let Some(ref mv) = search_result.best_move {
                    move_to_uci(mv)
                } else {
                    String::new()
                };

                println!(
                    "info depth {} score {} nodes {} nps {} time {} pv {}",
                    depth, score_str, total_nodes, nps, elapsed_ms, pv_str
                );
                let _ = io::stdout().flush();

                best_result = Some(search_result);
            }
            Err(_) => {
                // Search was aborted mid-depth, use best result from previous depth
                break;
            }
        }
    }

    best_result.and_then(|r| r.best_move)
}

/// Format score for UCI output (handles mate scores)
fn format_score(score: i32) -> String {
    const MATE_THRESHOLD: i32 = 900_000_000;

    if score > MATE_THRESHOLD {
        // Positive mate score: we're giving mate
        let plies_to_mate = (MAX_SCORE - score) / 100;
        let moves_to_mate = (plies_to_mate + 1) / 2;
        format!("mate {}", moves_to_mate.max(1))
    } else if score < -MATE_THRESHOLD {
        // Negative mate score: we're getting mated
        let plies_to_mate = (score - MIN_SCORE) / 100;
        let moves_to_mate = (plies_to_mate + 1) / 2;
        format!("mate -{}", moves_to_mate.max(1))
    } else {
        // Regular centipawn score
        format!("cp {}", score)
    }
}

fn parse_uci_move(board: &Board, move_str: &str) -> Option<rust_chess::types::Move> {
    if move_str.len() < 4 {
        return None;
    }

    let from_file = move_str.chars().nth(0)? as u8 - b'a' + 1;
    let from_rank = move_str.chars().nth(1)?.to_digit(10)? as u8;
    let to_file = move_str.chars().nth(2)? as u8 - b'a' + 1;
    let to_rank = move_str.chars().nth(3)?.to_digit(10)? as u8;

    let from = rust_chess::types::Position {
        rank: from_rank,
        file: from_file,
    };
    let to = rust_chess::types::Position {
        rank: to_rank,
        file: to_file,
    };

    // Handle promotion
    let promotion = if move_str.len() > 4 {
        match move_str.chars().nth(4)? {
            'q' => Some(rust_chess::types::PieceType::Queen),
            'r' => Some(rust_chess::types::PieceType::Rook),
            'b' => Some(rust_chess::types::PieceType::Bishop),
            'n' => Some(rust_chess::types::PieceType::Knight),
            _ => None,
        }
    } else {
        None
    };

    // Find matching legal move
    let legal_moves = board.get_legal_moves(&board.get_active_color()).ok()?;
    for mv in legal_moves {
        if mv.from == from && mv.to == to {
            // Check promotion matches
            if let Some(promo_type) = promotion {
                if let MoveFlag::Promotion(pt) = mv.move_flag {
                    if pt == promo_type {
                        return Some(mv);
                    }
                }
            } else if !matches!(mv.move_flag, MoveFlag::Promotion(_)) {
                return Some(mv);
            }
        }
    }

    None
}

fn move_to_uci(mv: &rust_chess::types::Move) -> String {
    let from_file = (mv.from.file - 1 + b'a') as char;
    let from_rank = mv.from.rank;
    let to_file = (mv.to.file - 1 + b'a') as char;
    let to_rank = mv.to.rank;

    let mut result = format!("{}{}{}{}", from_file, from_rank, to_file, to_rank);

    // Add promotion piece
    if let MoveFlag::Promotion(piece_type) = mv.move_flag {
        let promo_char = match piece_type {
            rust_chess::types::PieceType::Queen => 'q',
            rust_chess::types::PieceType::Rook => 'r',
            rust_chess::types::PieceType::Bishop => 'b',
            rust_chess::types::PieceType::Knight => 'n',
            _ => 'q',
        };
        result.push(promo_char);
    }

    result
}
