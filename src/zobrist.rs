use crate::types::{Color, PieceType, Position};

/// Zobrist hashing keys for chess positions.
/// These are pseudo-random u64 values XORed together to create a unique hash for each position.
pub struct ZobristKeys {
    /// Keys for each (color, piece_type, square) combination: 2 * 6 * 64 = 768 keys
    /// Indexed as: pieces[color][piece_type][square]
    pub pieces: [[[u64; 64]; 6]; 2],
    /// Key for side to move (XORed when it's black's turn)
    pub side_to_move: u64,
    /// Keys for castling rights (4 bits = 16 combinations, but we use 4 individual keys)
    pub castle_kingside_white: u64,
    pub castle_queenside_white: u64,
    pub castle_kingside_black: u64,
    pub castle_queenside_black: u64,
    /// Keys for en passant file (0-7 for files a-h)
    pub en_passant: [u64; 8],
}

impl ZobristKeys {
    /// Initialize Zobrist keys with deterministic pseudo-random values.
    /// Uses a simple PRNG seeded with a fixed value for reproducibility.
    pub fn new() -> Self {
        let mut rng = XorShift64::new(0x1234567890ABCDEF);

        let mut pieces = [[[0u64; 64]; 6]; 2];
        for color in 0..2 {
            for piece in 0..6 {
                for square in 0..64 {
                    pieces[color][piece][square] = rng.next();
                }
            }
        }

        let side_to_move = rng.next();

        let castle_kingside_white = rng.next();
        let castle_queenside_white = rng.next();
        let castle_kingside_black = rng.next();
        let castle_queenside_black = rng.next();

        let mut en_passant = [0u64; 8];
        for file in 0..8 {
            en_passant[file] = rng.next();
        }

        ZobristKeys {
            pieces,
            side_to_move,
            castle_kingside_white,
            castle_queenside_white,
            castle_kingside_black,
            castle_queenside_black,
            en_passant,
        }
    }

    /// Get the key for a piece at a position
    #[inline]
    pub fn piece_key(&self, color: Color, piece_type: PieceType, pos: &Position) -> u64 {
        let color_idx = match color {
            Color::White => 0,
            Color::Black => 1,
        };
        let piece_idx = match piece_type {
            PieceType::Pawn => 0,
            PieceType::Knight => 1,
            PieceType::Bishop => 2,
            PieceType::Rook => 3,
            PieceType::Queen => 4,
            PieceType::King => 5,
        };
        let square_idx = ((pos.rank - 1) * 8 + (pos.file - 1)) as usize;
        self.pieces[color_idx][piece_idx][square_idx]
    }
}

/// Simple XorShift64 PRNG for deterministic key generation
struct XorShift64 {
    state: u64,
}

impl XorShift64 {
    fn new(seed: u64) -> Self {
        XorShift64 { state: seed }
    }

    fn next(&mut self) -> u64 {
        let mut x = self.state;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.state = x;
        x
    }
}

/// Global static Zobrist keys (initialized once)
use once_cell::sync::Lazy;
pub static ZOBRIST_KEYS: Lazy<ZobristKeys> = Lazy::new(|| ZobristKeys::new());

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zobrist_keys_unique() {
        let keys = ZobristKeys::new();

        // Check that side_to_move key is non-zero
        assert_ne!(keys.side_to_move, 0);

        // Check that piece keys are unique
        let mut all_keys: Vec<u64> = Vec::new();
        for color in 0..2 {
            for piece in 0..6 {
                for square in 0..64 {
                    all_keys.push(keys.pieces[color][piece][square]);
                }
            }
        }
        all_keys.push(keys.side_to_move);
        all_keys.push(keys.castle_kingside_white);
        all_keys.push(keys.castle_queenside_white);
        all_keys.push(keys.castle_kingside_black);
        all_keys.push(keys.castle_queenside_black);
        for ep in &keys.en_passant {
            all_keys.push(*ep);
        }

        // Check for uniqueness (no duplicates)
        all_keys.sort();
        for i in 1..all_keys.len() {
            assert_ne!(all_keys[i - 1], all_keys[i], "Duplicate Zobrist key found");
        }
    }

    #[test]
    fn test_zobrist_deterministic() {
        // Keys should be the same on repeated initialization
        let keys1 = ZobristKeys::new();
        let keys2 = ZobristKeys::new();

        assert_eq!(keys1.side_to_move, keys2.side_to_move);
        assert_eq!(keys1.castle_kingside_white, keys2.castle_kingside_white);

        for color in 0..2 {
            for piece in 0..6 {
                for square in 0..64 {
                    assert_eq!(
                        keys1.pieces[color][piece][square],
                        keys2.pieces[color][piece][square]
                    );
                }
            }
        }
    }
}
