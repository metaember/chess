use std::fs;
use std::collections::HashMap;
use rand::prelude::*;
use crate::board::{Board, Move};


const NEW_POS_PREFIX: &str = "pos ";
const BOOK_PATH: &str = "/Users/charlesbine/Documents/prog/rust_chess/book/book.txt";

pub struct Book {
    book: HashMap<String, HashMap<String, i32>>,
}

impl Book {
    pub fn new() -> Self {
        let book = Book::read_book();
        Self {
            book
        }
    }

    /// Read the book flat file, returning a hash map of fen string
    /// encoding the board positiong to a hashmap of position(str) and their
    /// occurances in the game sample.
    fn read_book() -> HashMap<String, HashMap<String, i32>> {
        let mut book: HashMap<String, HashMap<String, i32>> = HashMap::new();
        let mut current_position_map: HashMap<String, i32> = HashMap::new();
        let contents = fs::read_to_string(BOOK_PATH).expect("Something went wrong reading the file");
        let mut last_fen: Option<String> = None;

        for line in contents.lines() {
            if line.starts_with(NEW_POS_PREFIX) {
                // maybe insert the previous matches
                if last_fen.is_some() {
                    book.insert(last_fen.unwrap(), current_position_map);
                };
                // start a new position
                current_position_map = HashMap::new();
                last_fen = Some(line.trim_start_matches(NEW_POS_PREFIX).to_string());
            } else {
                let mut split = line.split_whitespace();
                let move_str = split.next().unwrap();
                let count = split.next().unwrap().parse::<i32>().unwrap();
                current_position_map.insert(move_str.to_string(), count);
            }
        }
        book
    }

    /// Given a position, return the best moves from the book.
    /// if a position is not in the book, return None
    pub fn get_book_moves(&self, current_fen: String) -> Option<&HashMap<String, i32>>{
        self.book.get(&current_fen)
    }

    pub fn get_uniformly_selected_move(&self, current_fen: String) -> Option<String> {
        let book_moves = self.get_book_moves(current_fen);
        if book_moves.is_none() {
            return None;
        }
        let book_moves = book_moves.unwrap();
        let total_moves = book_moves.values().len();

        let mut rng = rand::thread_rng();
        let mut cumulative_move_count = 0;
        let random_number = rng.gen_range(0..total_moves);

        for (move_str, _) in book_moves {
            cumulative_move_count += 1;
            if random_number < cumulative_move_count {
                return Some(move_str.clone());
            }
        }
        panic!("Should not get here");
    }

    pub fn get_weighted_selected_move(&self, current_fen: String) -> Option<String> {
        let book_moves = self.get_book_moves(current_fen);
        if book_moves.is_none() {
            return None;
        }
        let book_moves = book_moves.unwrap();
        let total_moves = book_moves.values().sum::<i32>();

        let mut rng = rand::thread_rng();
        let mut cumulative_move_count = 0;
        let random_number = rng.gen_range(0..total_moves);

        for (move_str, count) in book_moves {
            cumulative_move_count += *count;
            if random_number < cumulative_move_count {
                return Some(move_str.clone());
            }
        }
        panic!("Should not get here");
    }

    pub fn suggest_move(&self, board: &Board, weighted: bool) -> Option<Move> {
        let move_str = if weighted {
            self.get_weighted_selected_move(board.to_fen_no_moves().to_string())
        } else {
            self.get_uniformly_selected_move(board.to_fen_no_moves().to_string())
        }?;

        let m = Move::from_algebraic(board, move_str.get(0..2).unwrap(), move_str.get(2..4).unwrap());
        Some(m)
    }
}



#[cfg(test)]
mod tests {
    use std::time::Instant;
    use super::Book;
    use crate::board::{Board, Move, PieceType, MoveFlag};
    use pretty_assertions::assert_eq;

    #[test]
    fn load_book() {
        let now = Instant::now();
        let book = Book::new();
        let elapsed = now.elapsed().as_secs_f32();
        println!("Loaded book in {:.6}s", elapsed);
        println!("Book has {} positions", book.book.len());

        for (pos_fen, moves) in &book.book {
            let b = Board::from_fen_no_moves(pos_fen.as_str());
            assert_eq!(b.to_fen_no_moves(), *pos_fen);

            book.suggest_move(&b, true);
            book.suggest_move(&b, false);

            // try to make the moves
            for (move_str, count) in moves {
                // test move
                let m = Move::from_algebraic(&b, move_str.get(0..2).unwrap(), move_str.get(2..4).unwrap());
                if move_str == "e1g1" {
                    assert_eq!(m.piece.piece_type, PieceType::King);
                    assert_eq!(m.move_flag, MoveFlag::CastleKingside);
                } else if move_str == "e1c1" {
                    assert_eq!(m.piece.piece_type, PieceType::King);
                    assert_eq!(m.move_flag, MoveFlag::CastleQueenside);
                } else if move_str == "e8g8" {
                    assert_eq!(m.piece.piece_type, PieceType::King);
                    assert_eq!(m.move_flag, MoveFlag::CastleKingside);
                } else if move_str == "e8c8" {
                    assert_eq!(m.piece.piece_type, PieceType::King);
                    assert_eq!(m.move_flag, MoveFlag::CastleQueenside);
                };
                b.execute_move(&m);

                assert!(*count >= 0)
            }
        }
    }

    #[test]
    fn suggest_move_from_start() {
        let b = Board::new();
        let book = Book::new();
        let m = book.suggest_move(&b, true);
        assert!(m.is_some());
    }
}