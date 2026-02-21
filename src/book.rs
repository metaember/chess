use crate::board::Board;
use crate::types::{Move, Position, PieceType};
use rand::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::io::Read;

const NEW_POS_PREFIX: &str = "pos ";
const BOOK_PATH: &str = "/Users/charlesbine/Documents/prog/rust_chess/book/book.txt";
const POLYGLOT_BOOK_PATH: &str = "/Users/charlesbine/Documents/prog/rust_chess/book/Cerebellum3Merge.bin";

#[derive(Debug, Clone)]
struct PolyglotEntry {
    key: u64,      // Zobrist hash
    move_: u16,    // Encoded move
    weight: u16,   // Move weight
    learn: u32,    // Learning value (unused)
}

pub struct Book {
    book: HashMap<String, HashMap<String, i32>>,
    polyglot_entries: Vec<PolyglotEntry>,
}

impl Book {
    pub fn new() -> Self {
        let book = Book::read_book();
        let polyglot_entries = Book::read_polyglot_book();
        Self { book, polyglot_entries }
    }

    /// Read Polyglot .bin format opening book
    fn read_polyglot_book() -> Vec<PolyglotEntry> {
        let mut entries = Vec::new();

        let Ok(mut file) = fs::File::open(POLYGLOT_BOOK_PATH) else {
            eprintln!("Warning: Could not open Polyglot book at {}", POLYGLOT_BOOK_PATH);
            return entries;
        };

        let mut buffer = Vec::new();
        if file.read_to_end(&mut buffer).is_err() {
            eprintln!("Warning: Could not read Polyglot book");
            return entries;
        }

        // Each entry is 16 bytes: 8 (key) + 2 (move) + 2 (weight) + 4 (learn)
        for chunk in buffer.chunks_exact(16) {
            let key = u64::from_be_bytes([
                chunk[0], chunk[1], chunk[2], chunk[3],
                chunk[4], chunk[5], chunk[6], chunk[7],
            ]);
            let move_ = u16::from_be_bytes([chunk[8], chunk[9]]);
            let weight = u16::from_be_bytes([chunk[10], chunk[11]]);
            let learn = u32::from_be_bytes([chunk[12], chunk[13], chunk[14], chunk[15]]);

            entries.push(PolyglotEntry { key, move_, weight, learn });
        }

        entries
    }

    /// Read the book flat file, returning a hash map of fen string
    /// encoding the board positiong to a hashmap of position(str) and their
    /// occurances in the game sample.
    fn read_book() -> HashMap<String, HashMap<String, i32>> {
        let mut book: HashMap<String, HashMap<String, i32>> = HashMap::new();
        let mut current_position_map: HashMap<String, i32> = HashMap::new();
        let contents =
            fs::read_to_string(BOOK_PATH).expect("Something went wrong reading the file");
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
    pub fn get_book_moves(&self, current_fen: String) -> Option<&HashMap<String, i32>> {
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

    /// Decode Polyglot move encoding to algebraic positions
    /// Polyglot format: bits 0-5=from, bits 6-11=to, bits 12-14=promotion
    fn decode_polyglot_move(&self, encoded: u16) -> (Position, Position, Option<PieceType>) {
        let from_square = (encoded & 0x3F) as u8; // bits 0-5
        let to_square = ((encoded >> 6) & 0x3F) as u8; // bits 6-11
        let promo_bits = (encoded >> 12) & 0x7; // bits 12-14

        // Convert square index (0-63) to rank/file (1-8)
        // Polyglot uses 0=a1, 7=h1, 8=a2, ..., 63=h8
        let from_file = (from_square % 8) + 1;
        let from_rank = (from_square / 8) + 1;
        let to_file = (to_square % 8) + 1;
        let to_rank = (to_square / 8) + 1;

        let promotion = match promo_bits {
            1 => Some(PieceType::Knight),
            2 => Some(PieceType::Bishop),
            3 => Some(PieceType::Rook),
            4 => Some(PieceType::Queen),
            _ => None,
        };

        (
            Position { rank: from_rank, file: from_file },
            Position { rank: to_rank, file: to_file },
            promotion,
        )
    }

    /// Look up moves for a position in Polyglot book
    fn get_polyglot_moves(&self, zobrist: u64) -> Vec<(u16, u16)> {
        // Binary search for entries with matching zobrist hash
        let mut moves = Vec::new();

        for entry in &self.polyglot_entries {
            if entry.key == zobrist {
                moves.push((entry.move_, entry.weight));
            }
        }

        moves
    }

    pub fn suggest_move(&self, board: &Board, weighted: bool) -> Option<Move> {
        // Try Polyglot book first (stronger) - use Polyglot-compatible hash
        let polyglot_hash = board.polyglot_hash();
        let polyglot_moves = self.get_polyglot_moves(polyglot_hash);

        if !polyglot_moves.is_empty() {
            // Weighted random selection
            let total_weight: u32 = polyglot_moves.iter().map(|(_, w)| *w as u32).sum();

            if total_weight > 0 {
                let mut rng = rand::thread_rng();
                let mut random = rng.gen_range(0..total_weight);

                for (encoded_move, weight) in polyglot_moves {
                    if random < weight as u32 {
                        let (from, to, promotion) = self.decode_polyglot_move(encoded_move);

                        // Find matching legal move
                        let legal_moves = board.get_legal_moves(&board.get_active_color()).ok()?;
                        for mv in legal_moves {
                            if mv.from == from && mv.to == to {
                                // Check promotion matches if applicable
                                if let Some(promo_type) = promotion {
                                    if let crate::types::MoveFlag::Promotion(pt) = mv.move_flag {
                                        if pt == promo_type {
                                            return Some(mv);
                                        }
                                    }
                                } else if !matches!(mv.move_flag, crate::types::MoveFlag::Promotion(_)) {
                                    return Some(mv);
                                }
                            }
                        }
                    }
                    random -= weight as u32;
                }
            }
        }

        // Fallback to old text-based book
        let move_str = if weighted {
            self.get_weighted_selected_move(board.to_fen_no_moves().to_string())
        } else {
            self.get_uniformly_selected_move(board.to_fen_no_moves().to_string())
        }?;

        let m = Move::from_algebraic(
            board,
            move_str.get(0..2).unwrap(),
            move_str.get(2..4).unwrap(),
        );
        Some(m)
    }
}

#[cfg(test)]
mod tests {
    use super::Book;
    use crate::board::Board;
    use crate::types::{Move, MoveFlag, PieceType};
    use pretty_assertions::assert_eq;
    use std::time::Instant;

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
                let m = Move::from_algebraic(
                    &b,
                    move_str.get(0..2).unwrap(),
                    move_str.get(2..4).unwrap(),
                );
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
