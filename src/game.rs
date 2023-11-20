use chrono::prelude::*;
use std::time::Instant;

use crate::board::*;
use crate::search::*;

pub struct Game {
    board: Board,
    moves: Vec<Move>,
    max_depth: u8,
    start_fen: String,
    game_start_time: DateTime<Local>,
}

impl Game {
    pub fn new_from_fen(max_depth: u8, start_fen: String) -> Self {
        Self {
            board: Board::from_fen(start_fen.as_str()),
            moves: Vec::new(),
            max_depth,
            start_fen,
            game_start_time: Local::now(),
        }
    }
    pub fn new(max_depth: u8) -> Self {
        Game::new_from_fen(max_depth, STARTING_POSITION_FEN.to_string())
    }

    pub fn play(&mut self, max_moves: i32) {
        let max_moves_in_ply = 2 * max_moves;

        for i in 1..=max_moves_in_ply {
            let now = Instant::now();
            let selected_move = search(self.max_depth, &self.board);
            let elaped = now.elapsed().as_secs_f32();
            println!("move {}: {} ({} - elapsed: {:.6}s)", (i + 1) / 2, selected_move.to_human(), selected_move.to_algebraic(), elaped);
            self.board = self.board.execute_move(&selected_move);
            self.moves.push(selected_move);
            self.board.draw_to_terminal();
            println!();
        }
    }

    pub fn to_pgn(&self) -> String {
        let mut pgn = String::new();
        pgn.push_str("[Event \"Charles's chess bot game\"]\n");
        pgn.push_str("[Site \"Charles's Computer\"]\n");
        pgn.push_str(format!("[Date \"{}\"]\n", self.game_start_time.format("%Y.%m.%d")).as_str());
        pgn.push_str("[Round \"1\"]\n");
        pgn.push_str("[White \"Charles's Bot\"]\n");
        pgn.push_str("[Black \"Charles's Bot\"]\n");
        pgn.push_str("[Black \"*\"]\n");
        pgn.push_str("\n");

        for (i, m) in self.moves.iter().enumerate() {
            if i % 2 == 0 {
                pgn.push_str(&format!("{}. ", (i + 2) / 2));
            }
            pgn.push_str(&m.to_algebraic());
            pgn.push_str(" ");
        }
        pgn
    }
}


mod tests {
    use super::*;

    #[test]
    fn test_game() {
        let mut game = Game::new(1);
        game.play(3);
        game.to_pgn();
    }
}