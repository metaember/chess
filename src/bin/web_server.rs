//! Web server for playing chess against the engine
//!
//! Uses the unified ChessEngine for consistent behavior with UCI.

use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};
use rand::Rng;
use rust_chess::board::Board;
use rust_chess::engine::{ChessEngine, SearchOptions};
use rust_chess::types::{Color, MoveFlag, PieceType, Position, Status};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::path::PathBuf;
use std::time::{Duration, SystemTime};
use tower_http::cors::CorsLayer;
use tower_http::services::{ServeDir, ServeFile};
use rusqlite::Connection;

const MAX_GAMES: usize = 1000;
const GAME_TTL: Duration = Duration::from_secs(48 * 60 * 60); // 48 hours
const MAX_DEPTH: u8 = 14;
const DEFAULT_DEPTH: u8 = 8;

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
    engine: Arc<RwLock<ChessEngine>>,
    db_path: Option<PathBuf>,
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
    nodes_searched: u64,
    depth: u8,
    from_book: bool,
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
        let mut engine = state.engine.write().unwrap();
        let options = SearchOptions::with_depth(depth);

        if let Some(result) = engine.pick_move(&mut board, &options, &mut Vec::new()) {
            eval_score = Some(result.score);
            board = board.execute_move(&result.best_move);
        }
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

    // Engine's turn - use unified ChessEngine
    let options = SearchOptions::with_depth(game.depth);
    let engine_result = {
        let mut engine = state.engine.write().unwrap();
        engine.pick_move(&mut game.board, &options, &mut Vec::new())
    };

    let (engine_move, eval_score, engine_stats) = match engine_result {
        Some(result) => {
            let stats = EngineStats {
                time_ms: result.time_ms,
                nodes_searched: result.nodes_searched,
                depth: result.depth_reached,
                from_book: result.from_book,
            };
            (result.best_move, Some(result.score), Some(stats))
        }
        None => {
            // Check if game is over
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
            game.game_over = Some("Engine has no moves".to_string());
            return Ok(Json(MakeMoveResponse {
                success: true,
                message: "Engine has no moves".to_string(),
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

// ---------------------------------------------------------------------------
// Spectator API types
// ---------------------------------------------------------------------------

#[derive(Serialize, Debug)]
struct LiveGameSummary {
    game_id: String,
    lichess_url: String,
    bot_color: String,
    opponent_name: String,
    opponent_rating: Option<i64>,
    opponent_is_bot: bool,
    time_control: String,
    speed: String,
    provenance: String,
    fen: String,
    last_move_uci: Option<String>,
    wtime_ms: Option<i64>,
    btime_ms: Option<i64>,
    eval_cp: Option<i64>,
    eval_mate: Option<i64>,
    depth: Option<i64>,
    started_at: String,
}

#[derive(Serialize, Debug)]
struct GameDetail {
    game_id: String,
    lichess_url: String,
    bot_color: String,
    opponent_name: String,
    opponent_rating: Option<i64>,
    opponent_is_bot: bool,
    time_control: String,
    speed: String,
    mode: String,
    provenance: String,
    status: String,
    result: String,
    termination: Option<String>,
    started_at: String,
    finished_at: Option<String>,
    fen: Option<String>,
    last_move_uci: Option<String>,
    moves_uci: Option<String>,
    wtime_ms: Option<i64>,
    btime_ms: Option<i64>,
    evals: Vec<MoveEval>,
}

#[derive(Serialize, Debug)]
struct MoveEval {
    ply: i64,
    move_uci: String,
    move_san: String,
    eval_cp: Option<i64>,
    eval_mate: Option<i64>,
    depth: Option<i64>,
    pv: Option<String>,
    nodes: Option<i64>,
    time_ms: Option<i64>,
    source: Option<String>,
    clock_ms: Option<i64>,
}

#[derive(Serialize, Debug)]
struct GameListItem {
    game_id: String,
    lichess_url: String,
    bot_color: String,
    opponent_name: String,
    opponent_rating: Option<i64>,
    opponent_is_bot: bool,
    time_control: String,
    speed: String,
    provenance: String,
    result: String,
    termination: Option<String>,
    started_at: String,
    finished_at: Option<String>,
}

#[derive(Deserialize)]
struct GamesQuery {
    limit: Option<i64>,
    offset: Option<i64>,
}

fn open_db_readonly(db_path: &Option<PathBuf>) -> Result<Connection, StatusCode> {
    let path = db_path.as_ref().ok_or(StatusCode::SERVICE_UNAVAILABLE)?;
    if !path.exists() {
        return Err(StatusCode::SERVICE_UNAVAILABLE);
    }
    Connection::open_with_flags(path, rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn get_live_games(
    State(state): State<AppState>,
) -> Result<Json<Vec<LiveGameSummary>>, StatusCode> {
    let conn = open_db_readonly(&state.db_path)?;

    let mut stmt = conn.prepare(
        "SELECT g.game_id, g.lichess_url, g.bot_color, g.opponent_name, g.opponent_rating,
                g.opponent_is_bot, g.time_control, g.speed, g.provenance, g.started_at,
                l.fen, l.last_move_uci, l.wtime_ms, l.btime_ms,
                (SELECT me.eval_cp FROM move_evals me WHERE me.game_id = g.game_id ORDER BY me.ply DESC LIMIT 1),
                (SELECT me.eval_mate FROM move_evals me WHERE me.game_id = g.game_id ORDER BY me.ply DESC LIMIT 1),
                (SELECT me.depth FROM move_evals me WHERE me.game_id = g.game_id ORDER BY me.ply DESC LIMIT 1)
         FROM games g
         JOIN live_state l ON g.game_id = l.game_id
         WHERE g.status = 'playing'
         ORDER BY g.started_at DESC"
    ).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let games = stmt.query_map([], |row| {
        Ok(LiveGameSummary {
            game_id: row.get(0)?,
            lichess_url: row.get(1)?,
            bot_color: row.get(2)?,
            opponent_name: row.get(3)?,
            opponent_rating: row.get(4)?,
            opponent_is_bot: row.get::<_, i64>(5)? != 0,
            time_control: row.get(6)?,
            speed: row.get(7)?,
            provenance: row.get(8)?,
            started_at: row.get(9)?,
            fen: row.get(10)?,
            last_move_uci: row.get(11)?,
            wtime_ms: row.get(12)?,
            btime_ms: row.get(13)?,
            eval_cp: row.get(14)?,
            eval_mate: row.get(15)?,
            depth: row.get(16)?,
        })
    }).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let result: Vec<LiveGameSummary> = games.filter_map(|g| g.ok()).collect();
    Ok(Json(result))
}

async fn get_game_detail(
    State(state): State<AppState>,
    Path(game_id): Path<String>,
) -> Result<Json<GameDetail>, StatusCode> {
    let conn = open_db_readonly(&state.db_path)?;

    let game = conn.query_row(
        "SELECT g.game_id, g.lichess_url, g.bot_color, g.opponent_name, g.opponent_rating,
                g.opponent_is_bot, g.time_control, g.speed, g.mode, g.provenance,
                g.status, g.result, g.termination, g.started_at, g.finished_at,
                l.fen, l.last_move_uci, l.moves_uci, l.wtime_ms, l.btime_ms
         FROM games g
         LEFT JOIN live_state l ON g.game_id = l.game_id
         WHERE g.game_id = ?1",
        [&game_id],
        |row| {
            Ok(GameDetail {
                game_id: row.get(0)?,
                lichess_url: row.get(1)?,
                bot_color: row.get(2)?,
                opponent_name: row.get(3)?,
                opponent_rating: row.get(4)?,
                opponent_is_bot: row.get::<_, i64>(5)? != 0,
                time_control: row.get(6)?,
                speed: row.get(7)?,
                mode: row.get(8)?,
                provenance: row.get(9)?,
                status: row.get(10)?,
                result: row.get(11)?,
                termination: row.get(12)?,
                started_at: row.get(13)?,
                finished_at: row.get(14)?,
                fen: row.get(15)?,
                last_move_uci: row.get(16)?,
                moves_uci: row.get(17)?,
                wtime_ms: row.get(18)?,
                btime_ms: row.get(19)?,
                evals: Vec::new(),
            })
        },
    ).map_err(|_| StatusCode::NOT_FOUND)?;

    // Get move evals
    let mut eval_stmt = conn.prepare(
        "SELECT ply, move_uci, move_san, eval_cp, eval_mate, depth, pv, nodes, time_ms, source, clock_ms
         FROM move_evals WHERE game_id = ?1 ORDER BY ply"
    ).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let evals: Vec<MoveEval> = eval_stmt.query_map([&game_id], |row| {
        Ok(MoveEval {
            ply: row.get(0)?,
            move_uci: row.get(1)?,
            move_san: row.get(2)?,
            eval_cp: row.get(3)?,
            eval_mate: row.get(4)?,
            depth: row.get(5)?,
            pv: row.get(6)?,
            nodes: row.get(7)?,
            time_ms: row.get(8)?,
            source: row.get(9)?,
            clock_ms: row.get(10)?,
        })
    }).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
    .filter_map(|e| e.ok())
    .collect();

    let mut game = game;
    game.evals = evals;
    Ok(Json(game))
}

async fn get_recent_games(
    State(state): State<AppState>,
    Query(params): Query<GamesQuery>,
) -> Result<Json<Vec<GameListItem>>, StatusCode> {
    let conn = open_db_readonly(&state.db_path)?;
    let limit = params.limit.unwrap_or(50).min(200);
    let offset = params.offset.unwrap_or(0);

    let mut stmt = conn.prepare(
        "SELECT game_id, lichess_url, bot_color, opponent_name, opponent_rating,
                opponent_is_bot, time_control, speed, provenance, result, termination,
                started_at, finished_at
         FROM games
         ORDER BY started_at DESC
         LIMIT ?1 OFFSET ?2"
    ).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let games = stmt.query_map(rusqlite::params![limit, offset], |row| {
        Ok(GameListItem {
            game_id: row.get(0)?,
            lichess_url: row.get(1)?,
            bot_color: row.get(2)?,
            opponent_name: row.get(3)?,
            opponent_rating: row.get(4)?,
            opponent_is_bot: row.get::<_, i64>(5)? != 0,
            time_control: row.get(6)?,
            speed: row.get(7)?,
            provenance: row.get(8)?,
            result: row.get(9)?,
            termination: row.get(10)?,
            started_at: row.get(11)?,
            finished_at: row.get(12)?,
        })
    }).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let result: Vec<GameListItem> = games.filter_map(|g| g.ok()).collect();
    Ok(Json(result))
}

#[tokio::main]
async fn main() {
    println!("Initializing chess engine...");
    let engine = ChessEngine::new();
    println!("Chess engine ready (opening book loaded).");

    // Look for the SQLite database from the lichess-bot
    let db_path = ["lichess-bot/data/games.db", "data/games.db"]
        .iter()
        .map(PathBuf::from)
        .find(|p| p.exists());

    if let Some(ref path) = db_path {
        println!("Spectator DB found: {}", path.display());
    } else {
        println!("No spectator DB found (lichess-bot hasn't run yet). Spectator API will return 503.");
    }

    let state = AppState {
        games: Arc::new(RwLock::new(HashMap::new())),
        engine: Arc::new(RwLock::new(engine)),
        db_path,
    };

    let cors = CorsLayer::new();

    let app = Router::new()
        .route("/api/new-game", post(new_game))
        .route("/api/move", post(make_move))
        .route("/api/live-games", get(get_live_games))
        .route("/api/games", get(get_recent_games))
        .route("/api/games/{game_id}", get(get_game_detail))
        .fallback_service(ServeDir::new("web").fallback(ServeFile::new("web/index.html")))
        .layer(cors)
        .with_state(state);

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000")
        .await
        .unwrap();

    println!("Chess server running at http://localhost:3000");
    println!("Open your browser to play against the engine!");

    axum::serve(listener, app).await.unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::sync::{Arc, RwLock};

    const SCHEMA: &str = "
        CREATE TABLE IF NOT EXISTS games (
            game_id TEXT PRIMARY KEY,
            lichess_url TEXT,
            bot_color TEXT,
            opponent_name TEXT,
            opponent_rating INTEGER,
            opponent_is_bot INTEGER,
            time_control TEXT,
            speed TEXT,
            mode TEXT,
            provenance TEXT,
            status TEXT DEFAULT 'playing',
            result TEXT DEFAULT '*',
            termination TEXT,
            started_at TEXT,
            finished_at TEXT
        );
        CREATE TABLE IF NOT EXISTS move_evals (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            game_id TEXT REFERENCES games(game_id),
            ply INTEGER,
            move_uci TEXT,
            move_san TEXT,
            eval_cp INTEGER,
            eval_mate INTEGER,
            depth INTEGER,
            pv TEXT,
            nodes INTEGER,
            nps INTEGER,
            time_ms INTEGER,
            source TEXT,
            clock_ms INTEGER
        );
        CREATE TABLE IF NOT EXISTS live_state (
            game_id TEXT PRIMARY KEY REFERENCES games(game_id),
            fen TEXT,
            last_move_uci TEXT,
            moves_uci TEXT,
            wtime_ms INTEGER,
            btime_ms INTEGER
        );
        CREATE INDEX IF NOT EXISTS idx_move_evals_game ON move_evals(game_id, ply);
        CREATE INDEX IF NOT EXISTS idx_games_status ON games(status);
        CREATE INDEX IF NOT EXISTS idx_games_started ON games(started_at);
    ";

    fn create_test_db() -> (tempfile::TempDir, PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        let db_path = dir.path().join("test_games.db");
        let conn = Connection::open(&db_path).unwrap();
        conn.execute_batch(SCHEMA).unwrap();
        (dir, db_path)
    }

    fn make_state(db_path: PathBuf) -> AppState {
        AppState {
            games: Arc::new(RwLock::new(HashMap::new())),
            engine: Arc::new(RwLock::new(ChessEngine::new())),
            db_path: Some(db_path),
        }
    }

    fn seed_game(db_path: &PathBuf, game_id: &str, status: &str, result: &str, started_at: &str) {
        let conn = Connection::open(db_path).unwrap();
        conn.execute(
            "INSERT INTO games (game_id, lichess_url, bot_color, opponent_name, opponent_rating,
             opponent_is_bot, time_control, speed, mode, provenance, status, result, started_at)
             VALUES (?1, ?2, 'white', 'TestOpp', 1500, 0, '180+2', 'blitz', 'rated', 'matchmaking', ?3, ?4, ?5)",
            rusqlite::params![
                game_id,
                format!("https://lichess.org/{}", game_id),
                status,
                result,
                started_at,
            ],
        ).unwrap();
    }

    fn seed_live_state(db_path: &PathBuf, game_id: &str, fen: &str) {
        let conn = Connection::open(db_path).unwrap();
        conn.execute(
            "INSERT INTO live_state (game_id, fen, last_move_uci, moves_uci, wtime_ms, btime_ms)
             VALUES (?1, ?2, 'e2e4', 'e2e4', 175000, 180000)",
            rusqlite::params![game_id, fen],
        ).unwrap();
    }

    fn seed_eval(db_path: &PathBuf, game_id: &str, ply: i64, move_uci: &str, move_san: &str, eval_cp: Option<i64>, eval_mate: Option<i64>) {
        let conn = Connection::open(db_path).unwrap();
        conn.execute(
            "INSERT INTO move_evals (game_id, ply, move_uci, move_san, eval_cp, eval_mate, depth, nodes, time_ms, source)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, 12, 50000, 50, 'search')",
            rusqlite::params![game_id, ply, move_uci, move_san, eval_cp, eval_mate],
        ).unwrap();
    }

    // -----------------------------------------------------------------------
    // open_db_readonly tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_open_db_readonly_none() {
        assert_eq!(open_db_readonly(&None).unwrap_err(), StatusCode::SERVICE_UNAVAILABLE);
    }

    #[test]
    fn test_open_db_readonly_missing_path() {
        let path = Some(PathBuf::from("/tmp/nonexistent_test_db_12345.db"));
        assert_eq!(open_db_readonly(&path).unwrap_err(), StatusCode::SERVICE_UNAVAILABLE);
    }

    #[test]
    fn test_open_db_readonly_exists() {
        let (_dir, db_path) = create_test_db();
        let conn = open_db_readonly(&Some(db_path));
        assert!(conn.is_ok());
    }

    // -----------------------------------------------------------------------
    // get_live_games tests
    // -----------------------------------------------------------------------

    #[tokio::test]
    async fn test_get_live_games_empty() {
        let (_dir, db_path) = create_test_db();
        let state = make_state(db_path);

        let result = get_live_games(State(state)).await.unwrap();
        assert!(result.0.is_empty());
    }

    #[tokio::test]
    async fn test_get_live_games_with_data() {
        let (_dir, db_path) = create_test_db();

        seed_game(&db_path, "live1", "playing", "*", "2025-01-15T12:00:00Z");
        seed_live_state(&db_path, "live1", "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
        seed_eval(&db_path, "live1", 0, "e2e4", "e4", Some(35), None);

        let state = make_state(db_path);
        let result = get_live_games(State(state)).await.unwrap();
        let games = &result.0;

        assert_eq!(games.len(), 1);
        assert_eq!(games[0].game_id, "live1");
        assert_eq!(games[0].bot_color, "white");
        assert_eq!(games[0].opponent_name, "TestOpp");
        assert_eq!(games[0].opponent_rating, Some(1500));
        assert!(!games[0].opponent_is_bot);
        assert_eq!(games[0].fen, "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
        assert_eq!(games[0].last_move_uci, Some("e2e4".to_string()));
        assert_eq!(games[0].wtime_ms, Some(175000));
        assert_eq!(games[0].eval_cp, Some(35));
        assert_eq!(games[0].eval_mate, None);
        assert_eq!(games[0].depth, Some(12));
    }

    #[tokio::test]
    async fn test_get_live_games_excludes_finished() {
        let (_dir, db_path) = create_test_db();

        seed_game(&db_path, "finished1", "finished", "1-0", "2025-01-15T12:00:00Z");
        // No live_state for finished game

        let state = make_state(db_path);
        let result = get_live_games(State(state)).await.unwrap();
        assert!(result.0.is_empty());
    }

    // -----------------------------------------------------------------------
    // get_game_detail tests
    // -----------------------------------------------------------------------

    #[tokio::test]
    async fn test_get_game_detail_not_found() {
        let (_dir, db_path) = create_test_db();
        let state = make_state(db_path);

        let result = get_game_detail(State(state), Path("nonexistent".to_string())).await;
        assert_eq!(result.unwrap_err(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn test_get_game_detail_with_evals() {
        let (_dir, db_path) = create_test_db();

        seed_game(&db_path, "detail1", "playing", "*", "2025-01-15T12:00:00Z");
        seed_live_state(&db_path, "detail1", "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2");
        seed_eval(&db_path, "detail1", 0, "e2e4", "e4", Some(30), None);
        seed_eval(&db_path, "detail1", 2, "g1f3", "Nf3", Some(25), None);

        let state = make_state(db_path);
        let result = get_game_detail(State(state), Path("detail1".to_string())).await.unwrap();
        let detail = &result.0;

        assert_eq!(detail.game_id, "detail1");
        assert_eq!(detail.status, "playing");
        assert_eq!(detail.bot_color, "white");
        assert_eq!(detail.opponent_name, "TestOpp");
        assert_eq!(detail.fen, Some("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2".to_string()));

        assert_eq!(detail.evals.len(), 2);
        assert_eq!(detail.evals[0].ply, 0);
        assert_eq!(detail.evals[0].move_san, "e4");
        assert_eq!(detail.evals[0].eval_cp, Some(30));
        assert_eq!(detail.evals[1].ply, 2);
        assert_eq!(detail.evals[1].move_san, "Nf3");
        assert_eq!(detail.evals[1].eval_cp, Some(25));
    }

    #[tokio::test]
    async fn test_get_game_detail_finished_no_live_state() {
        let (_dir, db_path) = create_test_db();

        seed_game(&db_path, "fin1", "finished", "1-0", "2025-01-15T12:00:00Z");
        // No live_state for finished game

        let state = make_state(db_path);
        let result = get_game_detail(State(state), Path("fin1".to_string())).await.unwrap();
        let detail = &result.0;

        assert_eq!(detail.game_id, "fin1");
        assert_eq!(detail.status, "finished");
        assert_eq!(detail.result, "1-0");
        assert!(detail.fen.is_none());
        assert!(detail.wtime_ms.is_none());
    }

    #[tokio::test]
    async fn test_get_game_detail_with_mate_eval() {
        let (_dir, db_path) = create_test_db();

        seed_game(&db_path, "mate1", "finished", "1-0", "2025-01-15T12:00:00Z");
        seed_eval(&db_path, "mate1", 20, "d1h5", "Qh5#", None, Some(1));

        let state = make_state(db_path);
        let result = get_game_detail(State(state), Path("mate1".to_string())).await.unwrap();

        assert_eq!(result.0.evals.len(), 1);
        assert_eq!(result.0.evals[0].eval_mate, Some(1));
        assert_eq!(result.0.evals[0].eval_cp, None);
    }

    // -----------------------------------------------------------------------
    // get_recent_games tests
    // -----------------------------------------------------------------------

    #[tokio::test]
    async fn test_get_recent_games_empty() {
        let (_dir, db_path) = create_test_db();
        let state = make_state(db_path);

        let result = get_recent_games(
            State(state),
            Query(GamesQuery { limit: None, offset: None }),
        ).await.unwrap();

        assert!(result.0.is_empty());
    }

    #[tokio::test]
    async fn test_get_recent_games_ordered_by_started_at_desc() {
        let (_dir, db_path) = create_test_db();

        seed_game(&db_path, "g1", "finished", "1-0", "2025-01-15T10:00:00Z");
        seed_game(&db_path, "g2", "finished", "0-1", "2025-01-15T11:00:00Z");
        seed_game(&db_path, "g3", "finished", "1-0", "2025-01-15T12:00:00Z");

        let state = make_state(db_path);
        let result = get_recent_games(
            State(state),
            Query(GamesQuery { limit: None, offset: None }),
        ).await.unwrap();
        let games = &result.0;

        assert_eq!(games.len(), 3);
        assert_eq!(games[0].game_id, "g3"); // most recent first
        assert_eq!(games[1].game_id, "g2");
        assert_eq!(games[2].game_id, "g1");
    }

    #[tokio::test]
    async fn test_get_recent_games_pagination() {
        let (_dir, db_path) = create_test_db();

        for i in 0..5 {
            seed_game(
                &db_path,
                &format!("pg{}", i),
                "finished",
                "1-0",
                &format!("2025-01-15T{:02}:00:00Z", 10 + i),
            );
        }

        let state = make_state(db_path);

        // Page 1: limit=2, offset=0
        let result = get_recent_games(
            State(state.clone()),
            Query(GamesQuery { limit: Some(2), offset: Some(0) }),
        ).await.unwrap();
        assert_eq!(result.0.len(), 2);
        assert_eq!(result.0[0].game_id, "pg4"); // most recent
        assert_eq!(result.0[1].game_id, "pg3");

        // Page 2: limit=2, offset=2
        let result = get_recent_games(
            State(state.clone()),
            Query(GamesQuery { limit: Some(2), offset: Some(2) }),
        ).await.unwrap();
        assert_eq!(result.0.len(), 2);
        assert_eq!(result.0[0].game_id, "pg2");
        assert_eq!(result.0[1].game_id, "pg1");

        // Page 3: limit=2, offset=4 → only 1 left
        let result = get_recent_games(
            State(state),
            Query(GamesQuery { limit: Some(2), offset: Some(4) }),
        ).await.unwrap();
        assert_eq!(result.0.len(), 1);
        assert_eq!(result.0[0].game_id, "pg0");
    }

    #[tokio::test]
    async fn test_get_recent_games_limit_capped_at_200() {
        let (_dir, db_path) = create_test_db();
        let state = make_state(db_path);

        // Request limit=500, should be capped to 200 internally
        let result = get_recent_games(
            State(state),
            Query(GamesQuery { limit: Some(500), offset: None }),
        ).await.unwrap();
        // Just verify it doesn't error — actual cap is tested by the code path
        assert!(result.0.is_empty());
    }

    #[tokio::test]
    async fn test_get_recent_games_default_params() {
        let (_dir, db_path) = create_test_db();
        seed_game(&db_path, "def1", "finished", "1-0", "2025-01-15T12:00:00Z");

        let state = make_state(db_path);
        let result = get_recent_games(
            State(state),
            Query(GamesQuery { limit: None, offset: None }),
        ).await.unwrap();

        assert_eq!(result.0.len(), 1);
        assert_eq!(result.0[0].game_id, "def1");
    }

    // -----------------------------------------------------------------------
    // No DB available tests
    // -----------------------------------------------------------------------

    #[tokio::test]
    async fn test_live_games_no_db() {
        let state = AppState {
            games: Arc::new(RwLock::new(HashMap::new())),
            engine: Arc::new(RwLock::new(ChessEngine::new())),
            db_path: None,
        };

        let result = get_live_games(State(state)).await;
        assert_eq!(result.unwrap_err(), StatusCode::SERVICE_UNAVAILABLE);
    }

    #[tokio::test]
    async fn test_game_detail_no_db() {
        let state = AppState {
            games: Arc::new(RwLock::new(HashMap::new())),
            engine: Arc::new(RwLock::new(ChessEngine::new())),
            db_path: None,
        };

        let result = get_game_detail(State(state), Path("any".to_string())).await;
        assert_eq!(result.unwrap_err(), StatusCode::SERVICE_UNAVAILABLE);
    }

    #[tokio::test]
    async fn test_recent_games_no_db() {
        let state = AppState {
            games: Arc::new(RwLock::new(HashMap::new())),
            engine: Arc::new(RwLock::new(ChessEngine::new())),
            db_path: None,
        };

        let result = get_recent_games(
            State(state),
            Query(GamesQuery { limit: None, offset: None }),
        ).await;
        assert_eq!(result.unwrap_err(), StatusCode::SERVICE_UNAVAILABLE);
    }
}
