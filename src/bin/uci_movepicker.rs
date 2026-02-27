//! UCI (Universal Chess Interface) protocol implementation
//!
//! Uses the unified ChessEngine for consistent behavior with web server.
//!
//! Usage:
//!   cargo run --release --bin uci_movepicker

use std::io::{self, BufRead, Write};
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

use rust_chess::board::Board;
use rust_chess::engine::{ChessEngine, SearchInfo, SearchOptions};
use rust_chess::types::{Move, MoveFlag, PieceType, Position};

const ENGINE_NAME: &str = "RustChess";
const ENGINE_AUTHOR: &str = "Chess Engine";

/// Commands sent from main thread to search thread
#[derive(Debug, Clone)]
enum UciCommand {
    Go {
        wtime: Option<u64>,
        btime: Option<u64>,
        winc: u64,
        binc: u64,
        movestogo: Option<u32>,
        depth: Option<u8>,
        movetime: Option<u64>,
        infinite: bool,
    },
    Stop,
    PonderHit,
    Quit,
}

/// Results sent from search thread to main thread
#[derive(Debug)]
enum SearchThreadResult {
    BestMove {
        best_move: Option<Move>,
        ponder_move: Option<Move>,
    },
    Info(String),
}

/// Shared state between threads
struct SharedState {
    board: Arc<Mutex<Board>>,
    engine: Arc<Mutex<ChessEngine>>,
    position_history: Arc<Mutex<Vec<u64>>>,
}

impl SharedState {
    fn new() -> Self {
        eprintln!("Initializing chess engine...");
        let engine = ChessEngine::new();
        eprintln!("Chess engine ready.");

        Self {
            board: Arc::new(Mutex::new(Board::new())),
            engine: Arc::new(Mutex::new(engine)),
            position_history: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn clone_refs(&self) -> Self {
        Self {
            board: Arc::clone(&self.board),
            engine: Arc::clone(&self.engine),
            position_history: Arc::clone(&self.position_history),
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let state = SharedState::new();

    // Create channels for communication
    let (cmd_tx, cmd_rx) = mpsc::channel::<UciCommand>();
    let (result_tx, result_rx) = mpsc::channel::<SearchThreadResult>();

    // Spawn search thread
    let search_state = state.clone_refs();
    let search_handle = thread::spawn(move || {
        search_thread(search_state, cmd_rx, result_tx);
    });

    // Main thread handles stdin and stdout
    let cmd_tx_clone = cmd_tx.clone();
    let stdin_handle = thread::spawn(move || {
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
                    println!("uciok");
                    let _ = io::stdout().flush();
                }

                "isready" => {
                    println!("readyok");
                    let _ = io::stdout().flush();
                }

                "ucinewgame" => {
                    let mut engine = state.engine.lock().unwrap();
                    engine.new_game();
                }

                "position" => {
                    let mut board = state.board.lock().unwrap();
                    let mut history = state.position_history.lock().unwrap();
                    parse_position(&tokens, &mut board, &mut history);
                }

                "go" => {
                    let cmd = parse_go_command(&tokens);
                    if cmd_tx_clone.send(cmd).is_err() {
                        break;
                    }
                }

                "stop" => {
                    if cmd_tx_clone.send(UciCommand::Stop).is_err() {
                        break;
                    }
                }

                "ponderhit" => {
                    if cmd_tx_clone.send(UciCommand::PonderHit).is_err() {
                        break;
                    }
                }

                "quit" => {
                    let _ = cmd_tx_clone.send(UciCommand::Quit);
                    break;
                }

                "d" | "display" => {
                    let board = state.board.lock().unwrap();
                    board.draw_to_terminal();
                }

                _ => {
                    // Unknown command, ignore
                }
            }
        }
    });

    // Handle results from search thread
    while let Ok(result) = result_rx.recv() {
        match result {
            SearchThreadResult::Info(info) => {
                println!("{}", info);
                stdout.flush().unwrap();
            }
            SearchThreadResult::BestMove {
                best_move,
                ponder_move,
            } => {
                match best_move {
                    None => {
                        println!("bestmove (none)");
                    }
                    Some(ref mv) => {
                        if let Some(ponder) = ponder_move {
                            println!(
                                "bestmove {} ponder {}",
                                move_to_uci(mv),
                                move_to_uci(&ponder)
                            );
                        } else {
                            println!("bestmove {}", move_to_uci(mv));
                        }
                    }
                }
                stdout.flush().unwrap();
            }
        }
    }

    // Wait for threads to finish
    let _ = stdin_handle.join();
    let _ = search_handle.join();
}

/// Search thread main loop
fn search_thread(
    state: SharedState,
    cmd_rx: mpsc::Receiver<UciCommand>,
    result_tx: mpsc::Sender<SearchThreadResult>,
) {
    loop {
        let cmd = match cmd_rx.recv() {
            Ok(cmd) => cmd,
            Err(_) => break,
        };

        match cmd {
            UciCommand::Quit => break,

            UciCommand::Stop => {
                // Stop command without active search, ignore
            }

            UciCommand::PonderHit => {
                // Ponderhit without active ponder, ignore
            }

            UciCommand::Go {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
                depth,
                movetime,
                infinite,
            } => {
                let mut board = state.board.lock().unwrap().clone();

                // Try opening book first
                let mut engine = state.engine.lock().unwrap();
                if let Some(book_move) = engine.book_move(&board) {
                    let _ = result_tx.send(SearchThreadResult::BestMove {
                        best_move: Some(book_move),
                        ponder_move: None,
                    });
                    continue;
                }

                // Build search options
                let is_white = board.get_active_color() == rust_chess::types::Color::White;
                let (time_left, increment) = if is_white {
                    (wtime, winc)
                } else {
                    (btime, binc)
                };

                let options = if let Some(mt) = movetime {
                    SearchOptions {
                        depth,
                        time_left: None,
                        increment: 0,
                        movestogo: None,
                        movetime: Some(mt),
                        infinite: false,
                    }
                } else if infinite {
                    SearchOptions {
                        depth,
                        time_left: None,
                        increment: 0,
                        movestogo: None,
                        movetime: None,
                        infinite: true,
                    }
                } else {
                    SearchOptions {
                        depth,
                        time_left,
                        increment,
                        movestogo,
                        movetime: None,
                        infinite: false,
                    }
                };

                // Search with info callbacks
                let result_tx_clone = result_tx.clone();
                let mut history = state.position_history.lock().unwrap().clone();
                let result = engine.search_with_info(&mut board, &options, move |info: SearchInfo| {
                    let _ = result_tx_clone.send(SearchThreadResult::Info(info.to_uci()));
                }, &mut history);

                // Always send bestmove — UCI requires it even on checkmate/stalemate.
                // If the search returned nothing (no legal moves), send None so the
                // output loop emits "bestmove (none)".
                let _ = result_tx.send(SearchThreadResult::BestMove {
                    best_move: result.map(|r| r.best_move),
                    ponder_move: None,
                });
            }
        }
    }
}

fn parse_position(tokens: &[&str], board: &mut Board, position_history: &mut Vec<u64>) {
    let mut idx = 1;

    if idx < tokens.len() && tokens[idx] == "startpos" {
        *board = Board::new();
        idx += 1;
    } else if idx < tokens.len() && tokens[idx] == "fen" {
        idx += 1;
        let mut fen_parts = Vec::new();
        while idx < tokens.len() && tokens[idx] != "moves" {
            fen_parts.push(tokens[idx]);
            idx += 1;
        }
        let fen = fen_parts.join(" ");
        *board = Board::from_fen(&fen);
    }

    // Build position history from the starting position
    position_history.clear();
    position_history.push(board.zobrist_hash);

    if idx < tokens.len() && tokens[idx] == "moves" {
        idx += 1;
        while idx < tokens.len() {
            let move_str = tokens[idx];
            if let Some(mv) = parse_uci_move(board, move_str) {
                *board = board.execute_move(&mv);
                position_history.push(board.zobrist_hash);
            }
            idx += 1;
        }
    }
}

fn parse_go_command(tokens: &[&str]) -> UciCommand {
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

    UciCommand::Go {
        wtime,
        btime,
        winc,
        binc,
        movestogo,
        depth,
        movetime,
        infinite,
    }
}

fn parse_uci_move(board: &Board, move_str: &str) -> Option<Move> {
    if move_str.len() < 4 {
        return None;
    }

    let from_file = move_str.chars().nth(0)? as u8 - b'a' + 1;
    let from_rank = move_str.chars().nth(1)?.to_digit(10)? as u8;
    let to_file = move_str.chars().nth(2)? as u8 - b'a' + 1;
    let to_rank = move_str.chars().nth(3)?.to_digit(10)? as u8;

    let from = Position {
        rank: from_rank,
        file: from_file,
    };
    let to = Position {
        rank: to_rank,
        file: to_file,
    };

    let promotion = if move_str.len() > 4 {
        match move_str.chars().nth(4)? {
            'q' => Some(PieceType::Queen),
            'r' => Some(PieceType::Rook),
            'b' => Some(PieceType::Bishop),
            'n' => Some(PieceType::Knight),
            _ => None,
        }
    } else {
        None
    };

    let legal_moves = board.get_legal_moves(&board.get_active_color()).ok()?;
    for mv in legal_moves {
        if mv.from == from && mv.to == to {
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

fn move_to_uci(mv: &Move) -> String {
    let from_file = (mv.from.file - 1 + b'a') as char;
    let from_rank = mv.from.rank;
    let to_file = (mv.to.file - 1 + b'a') as char;
    let to_rank = mv.to.rank;

    let mut result = format!("{}{}{}{}", from_file, from_rank, to_file, to_rank);

    if let MoveFlag::Promotion(piece_type) = mv.move_flag {
        let promo_char = match piece_type {
            PieceType::Queen => 'q',
            PieceType::Rook => 'r',
            PieceType::Bishop => 'b',
            PieceType::Knight => 'n',
            _ => 'q',
        };
        result.push(promo_char);
    }

    result
}
