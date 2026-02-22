use crate::board::Board;
use crate::types::{Move, Position, PieceType};
use rand::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::io::Read;

const NEW_POS_PREFIX: &str = "pos ";

/// Get the book directory path - checks multiple locations
fn get_book_dir() -> Option<std::path::PathBuf> {
    // Try relative path first (for Docker/deployment)
    let paths = [
        std::path::PathBuf::from("book"),
        std::path::PathBuf::from("./book"),
        // Fallback to absolute path for local development
        std::path::PathBuf::from("/Users/charlesbine/Documents/prog/rust_chess/book"),
    ];

    for path in paths {
        if path.exists() {
            return Some(path);
        }
    }
    None
}

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

        let Some(book_dir) = get_book_dir() else {
            eprintln!("Warning: Could not find book directory");
            return entries;
        };

        let polyglot_path = book_dir.join("Cerebellum3Merge.bin");
        let Ok(mut file) = fs::File::open(&polyglot_path) else {
            eprintln!("Warning: Could not open Polyglot book at {:?}", polyglot_path);
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

        let Some(book_dir) = get_book_dir() else {
            eprintln!("Warning: Could not find book directory for text book");
            return book;
        };

        let book_path = book_dir.join("book.txt");
        let contents = match fs::read_to_string(&book_path) {
            Ok(c) => c,
            Err(_) => {
                eprintln!("Warning: Could not read text book at {:?}", book_path);
                return book;
            }
        };
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
    /// Polyglot format: bits 0-2=to_file, 3-5=to_rank, 6-8=from_file, 9-11=from_rank, 12-14=promotion
    fn decode_polyglot_move(&self, encoded: u16) -> (Position, Position, Option<PieceType>) {
        let to_file = (encoded & 0x7) as u8 + 1;
        let to_rank = ((encoded >> 3) & 0x7) as u8 + 1;
        let from_file = ((encoded >> 6) & 0x7) as u8 + 1;
        let from_rank = ((encoded >> 9) & 0x7) as u8 + 1;
        let promo_bits = (encoded >> 12) & 0x7;

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

    /// Look up moves for a position in Polyglot book using binary search.
    /// Polyglot .bin files are sorted by key, so we find any match then
    /// scan left/right to collect all entries with the same key.
    fn get_polyglot_moves(&self, zobrist: u64) -> Vec<(u16, u16)> {
        let mut moves = Vec::new();

        let idx = self.polyglot_entries
            .binary_search_by_key(&zobrist, |e| e.key);
        let Ok(found) = idx else { return moves; };

        // Scan left to find first entry with this key
        let mut start = found;
        while start > 0 && self.polyglot_entries[start - 1].key == zobrist {
            start -= 1;
        }

        // Collect all entries with this key
        for entry in &self.polyglot_entries[start..] {
            if entry.key != zobrist { break; }
            moves.push((entry.move_, entry.weight));
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
    use crate::types::{Move, MoveFlag, PieceType, Position};
    use pretty_assertions::assert_eq;
    use std::collections::HashSet;
    use std::time::Instant;

    // Well-known Polyglot Zobrist hashes (from the specification)
    const STARTING_POSITION_HASH: u64 = 0x463b96181691fc9c;
    // After 1.e4
    const AFTER_1E4_HASH: u64 = 0x823c9b50fd114196;
    // After 1.e4 d5
    const AFTER_1E4_D5_HASH: u64 = 0x0756b94461c50fb0;
    // After 1.e4 d5 2.e5 (French-like)
    const AFTER_1E4_D5_2E5_HASH: u64 = 0x662fafb965db29d4;

    #[test]
    fn polyglot_hash_starting_position() {
        let board = Board::new();
        assert_eq!(
            board.polyglot_hash(), STARTING_POSITION_HASH,
            "Starting position Polyglot hash doesn't match the spec"
        );
    }

    #[test]
    fn polyglot_hash_after_1e4() {
        let board = Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1");
        assert_eq!(
            board.polyglot_hash(), AFTER_1E4_HASH,
            "Position after 1.e4 Polyglot hash doesn't match the spec"
        );
    }

    #[test]
    fn polyglot_hash_after_1e4_d5() {
        let board = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2");
        assert_eq!(
            board.polyglot_hash(), AFTER_1E4_D5_HASH,
            "Position after 1.e4 d5 Polyglot hash doesn't match the spec"
        );
    }

    #[test]
    fn polyglot_hash_after_1e4_d5_2e5() {
        let board = Board::from_fen("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2");
        assert_eq!(
            board.polyglot_hash(), AFTER_1E4_D5_2E5_HASH,
            "Position after 1.e4 d5 2.e5 Polyglot hash doesn't match the spec"
        );
    }

    #[test]
    fn decode_polyglot_move_e2e4() {
        let book = Book::new();
        // e2e4: from=(e,2) to=(e,4)
        // from_file=4(e), from_rank=1(rank2), to_file=4(e), to_rank=3(rank4)
        // encoded = (from_rank << 9) | (from_file << 6) | (to_rank << 3) | to_file
        //         = (1 << 9) | (4 << 6) | (3 << 3) | 4 = 512 + 256 + 24 + 4 = 796
        let (from, to, promo) = book.decode_polyglot_move(796);
        assert_eq!(from, Position { rank: 2, file: 5 }, "e2e4 from should be e2");
        assert_eq!(to, Position { rank: 4, file: 5 }, "e2e4 to should be e4");
        assert!(promo.is_none());
    }

    #[test]
    fn decode_polyglot_move_with_promotion() {
        let book = Book::new();
        // a7a8=Q: from=(a,7) to=(a,8)
        // from_file=0(a), from_rank=6(rank7), to_file=0(a), to_rank=7(rank8)
        // promo=4(Queen)
        // encoded = (4 << 12) | (6 << 9) | (0 << 6) | (7 << 3) | 0
        //         = 16384 + 3072 + 0 + 56 + 0 = 19512
        let (from, to, promo) = book.decode_polyglot_move(19512);
        assert_eq!(from, Position { rank: 7, file: 1 });
        assert_eq!(to, Position { rank: 8, file: 1 });
        assert_eq!(promo, Some(PieceType::Queen));
    }

    #[test]
    fn polyglot_lookup_finds_starting_moves() {
        let book = Book::new();
        let moves = book.get_polyglot_moves(STARTING_POSITION_HASH);
        assert!(!moves.is_empty(), "Polyglot book should have moves for the starting position");

        // Decode all moves and collect as strings
        let move_strs: Vec<String> = moves.iter().map(|(encoded, _weight)| {
            let (from, to, _) = book.decode_polyglot_move(*encoded);
            format!("{}{}", from.to_algebraic(), to.to_algebraic())
        }).collect();

        assert!(move_strs.contains(&"e2e4".to_string()), "Starting moves should include e2e4, got: {:?}", move_strs);
        assert!(move_strs.contains(&"d2d4".to_string()), "Starting moves should include d2d4, got: {:?}", move_strs);
    }

    #[test]
    fn suggest_move_starting_position_is_sane() {
        let board = Board::new();
        let book = Book::new();

        // Run 50 trials — every suggested move should be e4 or d4 (from Cerebellum)
        let mut seen = HashSet::new();
        for _ in 0..50 {
            let mv = book.suggest_move(&board, true).expect("should find a book move");
            let move_str = format!("{}{}", mv.from.to_algebraic(), mv.to.to_algebraic());
            seen.insert(move_str.clone());
            assert!(
                move_str == "e2e4" || move_str == "d2d4",
                "Starting position book move should be e2e4 or d2d4, got {}",
                move_str
            );
        }
        // With 50 trials at 67/33 split we should see both
        assert!(seen.len() == 2, "Expected both e2e4 and d2d4 in 50 trials, only saw: {:?}", seen);
    }

    #[test]
    fn suggest_move_after_1e4_finds_book_move() {
        let board = Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1");
        let book = Book::new();
        let mv = book.suggest_move(&board, true);
        assert!(mv.is_some(), "Polyglot book should have a response to 1.e4");
    }

    #[test]
    fn polyglot_book_produces_moves_deep_into_opening() {
        // Play an actual opening (Italian Game) and verify the book has moves
        // at every step. This catches hash bugs that only manifest after several moves.
        let book = Book::new();
        let positions = [
            // Starting position
            ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "move 0 (start)"),
            // After 1.e4
            ("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", "after 1.e4"),
            // After 1.e4 e5
            ("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2", "after 1.e4 e5"),
            // After 1.e4 e5 2.Nf3
            ("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2", "after 2.Nf3"),
            // After 1.e4 e5 2.Nf3 Nc6
            ("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3", "after 2...Nc6"),
            // After 1.e4 e5 2.Nf3 Nc6 3.Bb5 (Ruy Lopez)
            ("r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3", "after 3.Bb5"),
        ];

        for (fen, label) in &positions {
            let board = Board::from_fen(fen);
            let hash = board.polyglot_hash();
            let moves = book.get_polyglot_moves(hash);
            assert!(
                !moves.is_empty(),
                "Polyglot book should have moves {label} (hash=0x{hash:016X})"
            );
            // Also verify suggest_move returns a legal move
            let mv = book.suggest_move(&board, true);
            assert!(mv.is_some(), "suggest_move should return a move {label}");
        }
    }

    #[test]
    fn polyglot_hash_with_castling_changes() {
        // After 1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 5.O-O (white castled kingside)
        // White has lost both castling rights, black still has both
        let board = Board::from_fen(
            "r1bqkb1r/1ppp1ppp/p1n2n2/4p3/B3P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 5"
        );
        let book = Book::new();
        let hash = board.polyglot_hash();
        let moves = book.get_polyglot_moves(hash);
        // This is a very common Ruy Lopez position — Cerebellum should have it
        assert!(
            !moves.is_empty(),
            "Polyglot book should have moves in the Ruy Lopez after 5.O-O (hash=0x{hash:016X})"
        );
    }

    #[test]
    fn load_text_book() {
        let now = Instant::now();
        let book = Book::new();
        let elapsed = now.elapsed().as_secs_f32();
        println!("Loaded book in {:.6}s", elapsed);
        println!("Text book has {} positions", book.book.len());

        for (pos_fen, moves) in &book.book {
            let b = Board::from_fen_no_moves(pos_fen.as_str());
            assert_eq!(b.to_fen_no_moves(), *pos_fen);

            for (move_str, count) in moves {
                let m = Move::from_algebraic(
                    &b,
                    move_str.get(0..2).unwrap(),
                    move_str.get(2..4).unwrap(),
                );
                if move_str == "e1g1" || move_str == "e8g8" {
                    assert_eq!(m.move_flag, MoveFlag::CastleKingside);
                } else if move_str == "e1c1" || move_str == "e8c8" {
                    assert_eq!(m.move_flag, MoveFlag::CastleQueenside);
                }
                b.execute_move(&m);
                assert!(*count >= 0);
            }
        }
    }
}
