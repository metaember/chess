use axum::{
    extract::State,
    http::StatusCode,
    routing::post,
    Json, Router,
};
use rand::Rng;
use rust_chess::board::Board;
use rust_chess::search::minimax;
use rust_chess::types::{Color, MoveFlag, PieceType, Position, Status};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant, SystemTime};
use tower_http::cors::CorsLayer;
use tower_http::services::ServeDir;

const MAX_GAMES: usize = 1000;
const GAME_TTL: Duration = Duration::from_secs(48 * 60 * 60); // 48 hours
const MAX_DEPTH: u8 = 8;
const DEFAULT_DEPTH: u8 = 5; // Expert difficulty

// Individual game state
struct GameState {
    board: Board,
    player_color: Color,
    game_over: Option<String>,
    depth: u8,
    created_at: SystemTime,
}

// Shared app state with multiple games
#[derive(Clone)]
struct AppState {
    games: Arc<RwLock<HashMap<String, GameState>>>,
}

#[derive(Serialize)]
struct BoardResponse {
    game_id: String,
    fen: String,
    board_display: String,
    active_color: String,
    is_check: bool,
    game_over: Option<String>,
    legal_moves: Vec<MoveResponse>,
    eval_score: Option<i32>,
}

#[derive(Serialize)]
struct MoveResponse {
    from: String,
    to: String,
    promotion: Option<String>,
}

#[derive(Deserialize)]
struct NewGameRequest {
    player_color: Option<String>,
    depth: Option<u8>,
}

#[derive(Deserialize)]
struct MakeMoveRequest {
    game_id: String,
    from: String,
    to: String,
    promotion: Option<String>,
}

#[derive(Serialize)]
struct MakeMoveResponse {
    success: bool,
    message: String,
    board: BoardResponse,
    engine_move: Option<EngineMoveInfo>,
    player_move_algebraic: Option<String>,
    eval_score: Option<i32>,
    engine_stats: Option<EngineStats>,
}

#[derive(Serialize)]
struct EngineMoveInfo {
    from: String,
    to: String,
    algebraic: String,
    human: String,
}

#[derive(Serialize)]
struct EngineStats {
    time_ms: u64,
    nodes_searched: i32,
    quiescent_nodes: i32,
    nodes_per_second: u64,
    depth: u8,
}

fn generate_game_id() -> String {
    let bytes: [u8; 16] = rand::thread_rng().gen();
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

fn cleanup_games(games: &mut HashMap<String, GameState>) {
    let now = SystemTime::now();

    // Remove expired games (older than TTL)
    games.retain(|_, game| {
        now.duration_since(game.created_at)
            .map(|age| age < GAME_TTL)
            .unwrap_or(true)
    });

    // If still over limit, remove oldest games
    while games.len() > MAX_GAMES {
        if let Some(oldest_id) = games
            .iter()
            .min_by_key(|(_, g)| g.created_at)
            .map(|(id, _)| id.clone())
        {
            games.remove(&oldest_id);
        } else {
            break;
        }
    }
}

fn get_legal_moves_response(board: &Board) -> Vec<MoveResponse> {
    let color = board.get_active_color();
    match board.get_legal_moves(&color) {
        Ok(moves) => moves
            .iter()
            .map(|m| MoveResponse {
                from: m.from.to_algebraic(),
                to: m.to.to_algebraic(),
                promotion: match m.move_flag {
                    MoveFlag::Promotion(pt) => Some(
                        match pt {
                            PieceType::Queen => "q",
                            PieceType::Rook => "r",
                            PieceType::Bishop => "b",
                            PieceType::Knight => "n",
                            _ => "q",
                        }
                        .to_string(),
                    ),
                    _ => None,
                },
            })
            .collect(),
        Err(_) => vec![],
    }
}

fn get_board_response(
    game_id: &str,
    board: &Board,
    game_over: &Option<String>,
    eval_score: Option<i32>,
) -> BoardResponse {
    let active_color = board.get_active_color();
    BoardResponse {
        game_id: game_id.to_string(),
        fen: board.to_fen(),
        board_display: board.draw_board(),
        active_color: active_color.to_human().to_string(),
        is_check: board.is_in_check(active_color),
        game_over: game_over.clone(),
        legal_moves: get_legal_moves_response(board),
        eval_score,
    }
}

fn check_game_status(board: &Board) -> Option<String> {
    if let Some(Status::InsufficientMaterial) = board.check_for_insufficient_material() {
        return Some("Draw by insufficient material".to_string());
    }

    if let Some(Status::FiftyMoveRule) = board.check_for_fifty_move_rule() {
        return Some("Draw by fifty-move rule".to_string());
    }

    let color = board.get_active_color();
    match board.get_legal_moves(&color) {
        Err(Status::Checkmate(loser)) => {
            let winner = loser.other_color();
            Some(format!("Checkmate! {} wins!", winner.to_human()))
        }
        Err(Status::Stalemate) => Some("Stalemate! It's a draw.".to_string()),
        _ => None,
    }
}

async fn new_game(
    State(state): State<AppState>,
    Json(req): Json<NewGameRequest>,
) -> Json<BoardResponse> {
    let player_color = match req.player_color.as_deref() {
        Some("black") => Color::Black,
        _ => Color::White,
    };

    let depth = req.depth.unwrap_or(DEFAULT_DEPTH).min(MAX_DEPTH);
    let game_id = generate_game_id();

    let mut board = Board::new();
    let mut eval_score = None;

    // If player is black, engine makes the first move
    if player_color == Color::Black {
        let result = minimax(depth, &board).unwrap();
        eval_score = Some(result.best_score);
        let engine_move = result.best_move.unwrap();
        board = board.execute_move(&engine_move);
    }

    let game_state = GameState {
        board: board.clone(),
        player_color,
        game_over: None,
        depth,
        created_at: SystemTime::now(),
    };

    {
        let mut games = state.games.write().unwrap_or_else(|e| e.into_inner());
        cleanup_games(&mut games);
        games.insert(game_id.clone(), game_state);
    }

    Json(get_board_response(&game_id, &board, &None, eval_score))
}

async fn make_move(
    State(state): State<AppState>,
    Json(req): Json<MakeMoveRequest>,
) -> Result<Json<MakeMoveResponse>, (StatusCode, String)> {
    let mut games = state.games.write().unwrap_or_else(|e| e.into_inner());

    let game = match games.get_mut(&req.game_id) {
        Some(g) => g,
        None => {
            return Err((StatusCode::NOT_FOUND, "Game not found".to_string()));
        }
    };

    // Check if game is already over
    if game.game_over.is_some() {
        return Ok(Json(MakeMoveResponse {
            success: false,
            message: "Game is already over".to_string(),
            board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
            engine_move: None,
            player_move_algebraic: None,
            eval_score: None,
            engine_stats: None,
        }));
    }

    // Check if it's the player's turn
    if game.board.get_active_color() != game.player_color {
        return Ok(Json(MakeMoveResponse {
            success: false,
            message: "It's not your turn".to_string(),
            board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
            engine_move: None,
            player_move_algebraic: None,
            eval_score: None,
            engine_stats: None,
        }));
    }

    let from = Position::from_algebraic(&req.from);
    let to = Position::from_algebraic(&req.to);

    let legal_moves = match game.board.get_legal_moves(&game.player_color) {
        Ok(moves) => moves,
        Err(status) => {
            let msg = match status {
                Status::Checkmate(_) => "Checkmate!",
                Status::Stalemate => "Stalemate!",
                _ => "Game over",
            };
            game.game_over = Some(msg.to_string());
            return Ok(Json(MakeMoveResponse {
                success: false,
                message: msg.to_string(),
                board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
                engine_move: None,
                player_move_algebraic: None,
                eval_score: None,
                engine_stats: None,
            }));
        }
    };

    let matching_move = legal_moves.iter().find(|m| {
        m.from == from
            && m.to == to
            && {
                match (&m.move_flag, &req.promotion) {
                    (MoveFlag::Promotion(pt), Some(promo)) => {
                        let requested_pt = match promo.to_lowercase().as_str() {
                            "q" => PieceType::Queen,
                            "r" => PieceType::Rook,
                            "b" => PieceType::Bishop,
                            "n" => PieceType::Knight,
                            _ => PieceType::Queen,
                        };
                        *pt == requested_pt
                    }
                    (MoveFlag::Promotion(_), None) => false,
                    (_, _) => true,
                }
            }
    });

    let player_move = match matching_move {
        Some(m) => m.clone(),
        None => {
            let is_promotion = legal_moves
                .iter()
                .any(|m| m.from == from && m.to == to && matches!(m.move_flag, MoveFlag::Promotion(_)));

            if is_promotion && req.promotion.is_none() {
                return Ok(Json(MakeMoveResponse {
                    success: false,
                    message: "Promotion piece required. Specify 'q', 'r', 'b', or 'n'".to_string(),
                    board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
                    engine_move: None,
                    player_move_algebraic: None,
                    eval_score: None,
                    engine_stats: None,
                }));
            }

            return Ok(Json(MakeMoveResponse {
                success: false,
                message: format!("Invalid move: {} to {}", req.from, req.to),
                board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
                engine_move: None,
                player_move_algebraic: None,
                eval_score: None,
                engine_stats: None,
            }));
        }
    };

    let player_move_algebraic = player_move.to_algebraic();
    game.board = game.board.execute_move(&player_move);

    if let Some(status) = check_game_status(&game.board) {
        game.game_over = Some(status.clone());
        return Ok(Json(MakeMoveResponse {
            success: true,
            message: status,
            board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
            engine_move: None,
            player_move_algebraic: Some(player_move_algebraic),
            eval_score: None,
            engine_stats: None,
        }));
    }

    // Engine's turn
    let start_time = Instant::now();
    let engine_result = minimax(game.depth, &game.board);
    let elapsed = start_time.elapsed();
    let time_ms = elapsed.as_millis() as u64;

    let (engine_move, eval_score, engine_stats) = match engine_result {
        Ok(result) => {
            let score = result.best_score;
            let total_nodes = result.nodes_searched + result.quiescent_nodes_searched;
            let nodes_per_second = if time_ms > 0 {
                (total_nodes as u64 * 1000) / time_ms
            } else {
                total_nodes as u64 * 1000
            };

            let stats = EngineStats {
                time_ms,
                nodes_searched: result.nodes_searched,
                quiescent_nodes: result.quiescent_nodes_searched,
                nodes_per_second,
                depth: game.depth,
            };

            match result.best_move {
                Some(m) => (m, Some(score), Some(stats)),
                None => {
                    game.game_over = Some("Engine has no moves".to_string());
                    return Ok(Json(MakeMoveResponse {
                        success: true,
                        message: "Engine has no moves".to_string(),
                        board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
                        engine_move: None,
                        player_move_algebraic: Some(player_move_algebraic),
                        eval_score: None,
                        engine_stats: Some(stats),
                    }));
                }
            }
        }
        Err(_) => {
            if let Some(status) = check_game_status(&game.board) {
                game.game_over = Some(status.clone());
                return Ok(Json(MakeMoveResponse {
                    success: true,
                    message: status,
                    board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
                    engine_move: None,
                    player_move_algebraic: Some(player_move_algebraic),
                    eval_score: None,
                    engine_stats: None,
                }));
            }
            return Ok(Json(MakeMoveResponse {
                success: true,
                message: "Game over".to_string(),
                board: get_board_response(&req.game_id, &game.board, &game.game_over, None),
                engine_move: None,
                player_move_algebraic: Some(player_move_algebraic),
                eval_score: None,
                engine_stats: None,
            }));
        }
    };

    let engine_move_info = EngineMoveInfo {
        from: engine_move.from.to_algebraic(),
        to: engine_move.to.to_algebraic(),
        algebraic: engine_move.to_algebraic(),
        human: engine_move.to_human(),
    };

    game.board = game.board.execute_move(&engine_move);

    if let Some(status) = check_game_status(&game.board) {
        game.game_over = Some(status.clone());
    }

    Ok(Json(MakeMoveResponse {
        success: true,
        message: "Move executed".to_string(),
        board: get_board_response(&req.game_id, &game.board, &game.game_over, eval_score),
        engine_move: Some(engine_move_info),
        player_move_algebraic: Some(player_move_algebraic),
        eval_score,
        engine_stats,
    }))
}

#[tokio::main]
async fn main() {
    let state = AppState {
        games: Arc::new(RwLock::new(HashMap::new())),
    };

    let cors = CorsLayer::new();

    let app = Router::new()
        .route("/api/new-game", post(new_game))
        .route("/api/move", post(make_move))
        .nest_service("/", ServeDir::new("web"))
        .layer(cors)
        .with_state(state);

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000")
        .await
        .unwrap();

    println!("Chess server running at http://localhost:3000");
    println!("Open your browser to play against the engine!");

    axum::serve(listener, app).await.unwrap();
}
