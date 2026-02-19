//! Bitboard representation and precomputed attack tables for fast move generation.
//!
//! A bitboard is a 64-bit integer where each bit represents a square on the chess board.
//! Square indexing: a1 = 0, b1 = 1, ..., h1 = 7, a2 = 8, ..., h8 = 63
//! This corresponds to: square_index = (rank - 1) * 8 + (file - 1) where rank/file are 1-indexed.
//!
//! Magic bitboards are used for O(1) sliding piece attack lookups. Each square has a
//! precomputed "magic number" that maps occupancy patterns to attack sets via a hash table.

use crate::types::Position;
use std::sync::OnceLock;

/// Convert a 1-indexed position to a square index (0-63)
#[inline(always)]
pub const fn pos_to_sq(rank: u8, file: u8) -> u8 {
    (rank - 1) * 8 + (file - 1)
}

/// Convert a Position to a square index (0-63)
#[inline(always)]
pub fn position_to_sq(pos: &Position) -> u8 {
    pos_to_sq(pos.rank, pos.file)
}

/// Convert a square index to a bitboard with that single bit set
#[inline(always)]
pub const fn sq_to_bb(sq: u8) -> u64 {
    1u64 << sq
}

/// Convert a Position to a bitboard with that single bit set
#[inline(always)]
pub fn position_to_bb(pos: &Position) -> u64 {
    sq_to_bb(position_to_sq(pos))
}

/// Get the rank (1-8) from a square index
#[inline(always)]
pub const fn sq_rank(sq: u8) -> u8 {
    (sq >> 3) + 1
}

/// Get the file (1-8) from a square index
#[inline(always)]
pub const fn sq_file(sq: u8) -> u8 {
    (sq & 7) + 1
}

/// Convert a square index to a Position
#[inline(always)]
pub fn sq_to_position(sq: u8) -> crate::types::Position {
    crate::types::Position {
        rank: sq_rank(sq),
        file: sq_file(sq),
    }
}

/// Iterate over set bits in a bitboard, returning square indices
pub struct BitboardIter(pub u64);

impl Iterator for BitboardIter {
    type Item = u8;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            None
        } else {
            let sq = self.0.trailing_zeros() as u8;
            self.0 &= self.0 - 1; // Clear the lowest set bit
            Some(sq)
        }
    }
}

/// Precomputed attack tables
pub struct AttackTables {
    /// Knight attacks for each square
    pub knight: [u64; 64],
    /// King attacks for each square
    pub king: [u64; 64],
    /// Pawn attacks for each color and square: pawn_attacks[color as usize][square]
    pub pawn: [[u64; 64]; 2],
    /// Ray attacks in each direction from each square (for sliding pieces)
    /// Directions: 0=N, 1=NE, 2=E, 3=SE, 4=S, 5=SW, 6=W, 7=NW
    pub rays: [[u64; 64]; 8],
}

impl AttackTables {
    /// Initialize all attack tables at compile time
    pub const fn new() -> Self {
        let mut knight = [0u64; 64];
        let mut king = [0u64; 64];
        let mut pawn = [[0u64; 64]; 2];
        let mut rays = [[0u64; 64]; 8];

        // Knight move deltas: (rank_delta, file_delta)
        const KNIGHT_DELTAS: [(i8, i8); 8] = [
            (-2, -1), (-2, 1), (-1, -2), (-1, 2),
            (1, -2), (1, 2), (2, -1), (2, 1),
        ];

        // King move deltas
        const KING_DELTAS: [(i8, i8); 8] = [
            (-1, -1), (-1, 0), (-1, 1),
            (0, -1),           (0, 1),
            (1, -1),  (1, 0),  (1, 1),
        ];

        // Generate tables for each square
        let mut sq: u8 = 0;
        while sq < 64 {
            let rank = (sq >> 3) as i8 + 1; // 1-8
            let file = (sq & 7) as i8 + 1;  // 1-8

            // Knight attacks
            let mut i = 0;
            while i < 8 {
                let (dr, df) = KNIGHT_DELTAS[i];
                let nr = rank + dr;
                let nf = file + df;
                if nr >= 1 && nr <= 8 && nf >= 1 && nf <= 8 {
                    let target_sq = ((nr - 1) * 8 + (nf - 1)) as u8;
                    knight[sq as usize] |= 1u64 << target_sq;
                }
                i += 1;
            }

            // King attacks
            i = 0;
            while i < 8 {
                let (dr, df) = KING_DELTAS[i];
                let nr = rank + dr;
                let nf = file + df;
                if nr >= 1 && nr <= 8 && nf >= 1 && nf <= 8 {
                    let target_sq = ((nr - 1) * 8 + (nf - 1)) as u8;
                    king[sq as usize] |= 1u64 << target_sq;
                }
                i += 1;
            }

            // Pawn attacks (White = 0, Black = 1)
            // White pawns attack diagonally upward
            if rank < 8 {
                if file > 1 {
                    let target_sq = ((rank) * 8 + (file - 2)) as u8;
                    pawn[0][sq as usize] |= 1u64 << target_sq;
                }
                if file < 8 {
                    let target_sq = ((rank) * 8 + (file)) as u8;
                    pawn[0][sq as usize] |= 1u64 << target_sq;
                }
            }
            // Black pawns attack diagonally downward
            if rank > 1 {
                if file > 1 {
                    let target_sq = ((rank - 2) * 8 + (file - 2)) as u8;
                    pawn[1][sq as usize] |= 1u64 << target_sq;
                }
                if file < 8 {
                    let target_sq = ((rank - 2) * 8 + (file)) as u8;
                    pawn[1][sq as usize] |= 1u64 << target_sq;
                }
            }

            // Ray attacks (directions: N, NE, E, SE, S, SW, W, NW)
            // Direction 0: North (+rank)
            {
                let mut r = rank + 1;
                while r <= 8 {
                    let target_sq = ((r - 1) * 8 + (file - 1)) as u8;
                    rays[0][sq as usize] |= 1u64 << target_sq;
                    r += 1;
                }
            }
            // Direction 1: NorthEast (+rank, +file)
            {
                let mut r = rank + 1;
                let mut f = file + 1;
                while r <= 8 && f <= 8 {
                    let target_sq = ((r - 1) * 8 + (f - 1)) as u8;
                    rays[1][sq as usize] |= 1u64 << target_sq;
                    r += 1;
                    f += 1;
                }
            }
            // Direction 2: East (+file)
            {
                let mut f = file + 1;
                while f <= 8 {
                    let target_sq = ((rank - 1) * 8 + (f - 1)) as u8;
                    rays[2][sq as usize] |= 1u64 << target_sq;
                    f += 1;
                }
            }
            // Direction 3: SouthEast (-rank, +file)
            {
                let mut r = rank - 1;
                let mut f = file + 1;
                while r >= 1 && f <= 8 {
                    let target_sq = ((r - 1) * 8 + (f - 1)) as u8;
                    rays[3][sq as usize] |= 1u64 << target_sq;
                    r -= 1;
                    f += 1;
                }
            }
            // Direction 4: South (-rank)
            {
                let mut r = rank - 1;
                while r >= 1 {
                    let target_sq = ((r - 1) * 8 + (file - 1)) as u8;
                    rays[4][sq as usize] |= 1u64 << target_sq;
                    r -= 1;
                }
            }
            // Direction 5: SouthWest (-rank, -file)
            {
                let mut r = rank - 1;
                let mut f = file - 1;
                while r >= 1 && f >= 1 {
                    let target_sq = ((r - 1) * 8 + (f - 1)) as u8;
                    rays[5][sq as usize] |= 1u64 << target_sq;
                    r -= 1;
                    f -= 1;
                }
            }
            // Direction 6: West (-file)
            {
                let mut f = file - 1;
                while f >= 1 {
                    let target_sq = ((rank - 1) * 8 + (f - 1)) as u8;
                    rays[6][sq as usize] |= 1u64 << target_sq;
                    f -= 1;
                }
            }
            // Direction 7: NorthWest (+rank, -file)
            {
                let mut r = rank + 1;
                let mut f = file - 1;
                while r <= 8 && f >= 1 {
                    let target_sq = ((r - 1) * 8 + (f - 1)) as u8;
                    rays[7][sq as usize] |= 1u64 << target_sq;
                    r += 1;
                    f -= 1;
                }
            }

            sq += 1;
        }

        AttackTables { knight, king, pawn, rays }
    }

    /// Get rook attacks from a square given occupied squares
    #[inline]
    pub fn rook_attacks(&self, sq: u8, occupied: u64) -> u64 {
        self.ray_attacks(sq, occupied, 0) | // North
        self.ray_attacks(sq, occupied, 2) | // East
        self.ray_attacks(sq, occupied, 4) | // South
        self.ray_attacks(sq, occupied, 6)   // West
    }

    /// Get bishop attacks from a square given occupied squares
    #[inline]
    pub fn bishop_attacks(&self, sq: u8, occupied: u64) -> u64 {
        self.ray_attacks(sq, occupied, 1) | // NorthEast
        self.ray_attacks(sq, occupied, 3) | // SouthEast
        self.ray_attacks(sq, occupied, 5) | // SouthWest
        self.ray_attacks(sq, occupied, 7)   // NorthWest
    }

    /// Get queen attacks from a square given occupied squares
    #[inline]
    pub fn queen_attacks(&self, sq: u8, occupied: u64) -> u64 {
        self.rook_attacks(sq, occupied) | self.bishop_attacks(sq, occupied)
    }

    /// Get attacks along a ray, stopping at the first occupied square
    /// We need to find the blocker closest to the origin square.
    /// For rays going towards higher square indices: use trailing_zeros (lowest bit = closest)
    /// For rays going towards lower square indices: use leading_zeros (highest bit = closest)
    #[inline]
    fn ray_attacks(&self, sq: u8, occupied: u64, direction: usize) -> u64 {
        let ray = self.rays[direction][sq as usize];
        let blockers = ray & occupied;

        if blockers == 0 {
            return ray;
        }

        // Directions that go towards higher square indices: N(0), NE(1), E(2), NW(7)
        // Directions that go towards lower square indices: SE(3), S(4), SW(5), W(6)
        let goes_to_higher = matches!(direction, 0 | 1 | 2 | 7);
        let blocker_sq = if goes_to_higher {
            blockers.trailing_zeros() as u8
        } else {
            63 - blockers.leading_zeros() as u8
        };

        // Return the ray up to and including the blocker, minus squares beyond it
        ray ^ self.rays[direction][blocker_sq as usize]
    }
}

/// Global precomputed attack tables
pub static ATTACK_TABLES: AttackTables = AttackTables::new();

// ============================================================================
// MAGIC BITBOARDS
// ============================================================================

/// Precomputed magic numbers for bishop attacks.
/// These are well-known magic numbers that produce collision-free hashing.
const BISHOP_MAGICS: [u64; 64] = [
    0x0002020202020200, 0x0002020202020000, 0x0004010202000000, 0x0004040080000000,
    0x0001104000000000, 0x0000821040000000, 0x0000410410400000, 0x0000104104104000,
    0x0000040404040400, 0x0000020202020200, 0x0000040102020000, 0x0000040400800000,
    0x0000011040000000, 0x0000008210400000, 0x0000004104104000, 0x0000002082082000,
    0x0004000808080800, 0x0002000404040400, 0x0001000202020200, 0x0000800802004000,
    0x0000800400A00000, 0x0000200100884000, 0x0000400082082000, 0x0000200041041000,
    0x0002080010101000, 0x0001040008080800, 0x0000208004010400, 0x0000404004010200,
    0x0000840000802000, 0x0000404002011000, 0x0000808001041000, 0x0000404000820800,
    0x0001041000202000, 0x0000820800101000, 0x0000104400080800, 0x0000020080080080,
    0x0000404040040100, 0x0000808100020100, 0x0001010100020800, 0x0000808080010400,
    0x0000820820004000, 0x0000410410002000, 0x0000082088001000, 0x0000002011000800,
    0x0000080100400400, 0x0001010101000200, 0x0002020202000400, 0x0001010101000200,
    0x0000410410400000, 0x0000208208200000, 0x0000002084100000, 0x0000000020880000,
    0x0000001002020000, 0x0000040408020000, 0x0004040404040000, 0x0002020202020000,
    0x0000104104104000, 0x0000002082082000, 0x0000000020841000, 0x0000000000208800,
    0x0000000010020200, 0x0000000404080200, 0x0000040404040400, 0x0002020202020200,
];

/// Precomputed magic numbers for rook attacks.
const ROOK_MAGICS: [u64; 64] = [
    0x0080001020400080, 0x0040001000200040, 0x0080081000200080, 0x0080040800100080,
    0x0080020400080080, 0x0080010200040080, 0x0080008001000200, 0x0080002040800100,
    0x0000800020400080, 0x0000400020005000, 0x0000801000200080, 0x0000800800100080,
    0x0000800400080080, 0x0000800200040080, 0x0000800100020080, 0x0000800040800100,
    0x0000208000400080, 0x0000404000201000, 0x0000808010002000, 0x0000808008001000,
    0x0000808004000800, 0x0000808002000400, 0x0000010100020004, 0x0000020000408104,
    0x0000208080004000, 0x0000200040005000, 0x0000100080200080, 0x0000080080100080,
    0x0000040080080080, 0x0000020080040080, 0x0000010080800200, 0x0000800080004100,
    0x0000204000800080, 0x0000200040401000, 0x0000100080802000, 0x0000080080801000,
    0x0000040080800800, 0x0000020080800400, 0x0000020001010004, 0x0000800040800100,
    0x0000204000808000, 0x0000200040008080, 0x0000100020008080, 0x0000080010008080,
    0x0000040008008080, 0x0000020004008080, 0x0000010002008080, 0x0000004081020004,
    0x0000204000800080, 0x0000200040008080, 0x0000100020008080, 0x0000080010008080,
    0x0000040008008080, 0x0000020004008080, 0x0000800100020080, 0x0000800041000080,
    0x00FFFCDDFCED714A, 0x007FFCDDFCED714A, 0x003FFFCDFFD88096, 0x0000040810002101,
    0x0001000204080011, 0x0001000204000801, 0x0001000082000401, 0x0001FFFAABFAD1A2,
];

/// Number of bits in the bishop occupancy mask for each square
const BISHOP_BITS: [u8; 64] = [
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6,
];

/// Number of bits in the rook occupancy mask for each square
const ROOK_BITS: [u8; 64] = [
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12,
];

/// Magic bitboard attack lookup tables
pub struct MagicTables {
    /// Bishop attack tables. Indexed by: bishop_attacks[square][magic_index]
    /// Uses "fancy" magic bitboards with per-square table sizes
    pub bishop_attacks: Vec<Vec<u64>>,
    /// Rook attack tables
    pub rook_attacks: Vec<Vec<u64>>,
    /// Bishop occupancy masks (relevant squares that can block)
    pub bishop_masks: [u64; 64],
    /// Rook occupancy masks
    pub rook_masks: [u64; 64],
}

impl MagicTables {
    /// Initialize magic bitboard tables
    pub fn new() -> Self {
        let mut bishop_masks = [0u64; 64];
        let mut rook_masks = [0u64; 64];

        // Compute occupancy masks (excluding edges since blockers on edges don't affect anything beyond)
        for sq in 0..64 {
            bishop_masks[sq] = Self::bishop_mask(sq as u8);
            rook_masks[sq] = Self::rook_mask(sq as u8);
        }

        // Build attack tables
        let mut bishop_attacks = Vec::with_capacity(64);
        let mut rook_attacks = Vec::with_capacity(64);

        for sq in 0..64 {
            bishop_attacks.push(Self::init_bishop_attacks(sq as u8, bishop_masks[sq]));
            rook_attacks.push(Self::init_rook_attacks(sq as u8, rook_masks[sq]));
        }

        MagicTables {
            bishop_attacks,
            rook_attacks,
            bishop_masks,
            rook_masks,
        }
    }

    /// Compute bishop occupancy mask (excluding edges)
    fn bishop_mask(sq: u8) -> u64 {
        let rank = (sq >> 3) as i32;
        let file = (sq & 7) as i32;
        let mut mask = 0u64;

        // Generate diagonal rays, excluding edge squares
        for (dr, df) in [(1, 1), (1, -1), (-1, 1), (-1, -1)] {
            let mut r = rank + dr;
            let mut f = file + df;
            while r > 0 && r < 7 && f > 0 && f < 7 {
                mask |= 1u64 << (r * 8 + f);
                r += dr;
                f += df;
            }
        }
        mask
    }

    /// Compute rook occupancy mask (excluding edges)
    fn rook_mask(sq: u8) -> u64 {
        let rank = (sq >> 3) as i32;
        let file = (sq & 7) as i32;
        let mut mask = 0u64;

        // Rank ray (exclude edges)
        for f in 1..7 {
            if f != file {
                mask |= 1u64 << (rank * 8 + f);
            }
        }

        // File ray (exclude edges)
        for r in 1..7 {
            if r != rank {
                mask |= 1u64 << (r * 8 + file);
            }
        }
        mask
    }

    /// Compute actual bishop attacks given occupancy
    fn bishop_attacks_slow(sq: u8, occupied: u64) -> u64 {
        let rank = (sq >> 3) as i32;
        let file = (sq & 7) as i32;
        let mut attacks = 0u64;

        for (dr, df) in [(1, 1), (1, -1), (-1, 1), (-1, -1)] {
            let mut r = rank + dr;
            let mut f = file + df;
            while r >= 0 && r < 8 && f >= 0 && f < 8 {
                let target = 1u64 << (r * 8 + f);
                attacks |= target;
                if occupied & target != 0 {
                    break;
                }
                r += dr;
                f += df;
            }
        }
        attacks
    }

    /// Compute actual rook attacks given occupancy
    fn rook_attacks_slow(sq: u8, occupied: u64) -> u64 {
        let rank = (sq >> 3) as i32;
        let file = (sq & 7) as i32;
        let mut attacks = 0u64;

        // Horizontal
        for f in (file + 1)..8 {
            let target = 1u64 << (rank * 8 + f);
            attacks |= target;
            if occupied & target != 0 { break; }
        }
        for f in (0..file).rev() {
            let target = 1u64 << (rank * 8 + f);
            attacks |= target;
            if occupied & target != 0 { break; }
        }

        // Vertical
        for r in (rank + 1)..8 {
            let target = 1u64 << (r * 8 + file);
            attacks |= target;
            if occupied & target != 0 { break; }
        }
        for r in (0..rank).rev() {
            let target = 1u64 << (r * 8 + file);
            attacks |= target;
            if occupied & target != 0 { break; }
        }
        attacks
    }

    /// Generate all subsets of a mask using Carry-Rippler technique
    fn all_subsets(mask: u64) -> impl Iterator<Item = u64> {
        let mut subset = 0u64;
        let mut done = false;
        std::iter::from_fn(move || {
            if done { return None; }
            let current = subset;
            // Carry-Rippler: next subset
            subset = subset.wrapping_sub(mask) & mask;
            if subset == 0 { done = true; }
            Some(current)
        })
    }

    /// Initialize bishop attack table for a square
    fn init_bishop_attacks(sq: u8, mask: u64) -> Vec<u64> {
        let bits = BISHOP_BITS[sq as usize];
        let size = 1 << bits;
        let mut attacks = vec![0u64; size];
        let magic = BISHOP_MAGICS[sq as usize];

        for occupied in Self::all_subsets(mask) {
            let index = ((occupied.wrapping_mul(magic)) >> (64 - bits)) as usize;
            attacks[index] = Self::bishop_attacks_slow(sq, occupied);
        }
        attacks
    }

    /// Initialize rook attack table for a square
    fn init_rook_attacks(sq: u8, mask: u64) -> Vec<u64> {
        let bits = ROOK_BITS[sq as usize];
        let size = 1 << bits;
        let mut attacks = vec![0u64; size];
        let magic = ROOK_MAGICS[sq as usize];

        for occupied in Self::all_subsets(mask) {
            let index = ((occupied.wrapping_mul(magic)) >> (64 - bits)) as usize;
            attacks[index] = Self::rook_attacks_slow(sq, occupied);
        }
        attacks
    }

    /// Get bishop attacks using magic lookup
    #[inline(always)]
    pub fn bishop_attacks(&self, sq: u8, occupied: u64) -> u64 {
        let mask = self.bishop_masks[sq as usize];
        let magic = BISHOP_MAGICS[sq as usize];
        let bits = BISHOP_BITS[sq as usize];
        let index = (((occupied & mask).wrapping_mul(magic)) >> (64 - bits)) as usize;
        self.bishop_attacks[sq as usize][index]
    }

    /// Get rook attacks using magic lookup
    #[inline(always)]
    pub fn rook_attacks(&self, sq: u8, occupied: u64) -> u64 {
        let mask = self.rook_masks[sq as usize];
        let magic = ROOK_MAGICS[sq as usize];
        let bits = ROOK_BITS[sq as usize];
        let index = (((occupied & mask).wrapping_mul(magic)) >> (64 - bits)) as usize;
        self.rook_attacks[sq as usize][index]
    }

    /// Get queen attacks (combines bishop and rook)
    #[inline(always)]
    pub fn queen_attacks(&self, sq: u8, occupied: u64) -> u64 {
        self.bishop_attacks(sq, occupied) | self.rook_attacks(sq, occupied)
    }
}

/// Global magic bitboard tables (lazily initialized at runtime)
static MAGIC_TABLES: OnceLock<MagicTables> = OnceLock::new();

/// Get the global magic tables, initializing if needed
#[inline]
pub fn magic_tables() -> &'static MagicTables {
    MAGIC_TABLES.get_or_init(MagicTables::new)
}

/// Get bishop attacks using magic bitboards
#[inline(always)]
pub fn bishop_attacks(sq: u8, occupied: u64) -> u64 {
    magic_tables().bishop_attacks(sq, occupied)
}

/// Get rook attacks using magic bitboards
#[inline(always)]
pub fn rook_attacks(sq: u8, occupied: u64) -> u64 {
    magic_tables().rook_attacks(sq, occupied)
}

/// Get queen attacks using magic bitboards
#[inline(always)]
pub fn queen_attacks(sq: u8, occupied: u64) -> u64 {
    magic_tables().queen_attacks(sq, occupied)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_knight_attacks() {
        // Knight on e4 (sq = 28) should attack d2, f2, c3, g3, c5, g5, d6, f6
        let sq = pos_to_sq(4, 5); // e4
        let attacks = ATTACK_TABLES.knight[sq as usize];

        // Check expected squares
        assert!(attacks & sq_to_bb(pos_to_sq(2, 4)) != 0); // d2
        assert!(attacks & sq_to_bb(pos_to_sq(2, 6)) != 0); // f2
        assert!(attacks & sq_to_bb(pos_to_sq(3, 3)) != 0); // c3
        assert!(attacks & sq_to_bb(pos_to_sq(3, 7)) != 0); // g3
        assert!(attacks & sq_to_bb(pos_to_sq(5, 3)) != 0); // c5
        assert!(attacks & sq_to_bb(pos_to_sq(5, 7)) != 0); // g5
        assert!(attacks & sq_to_bb(pos_to_sq(6, 4)) != 0); // d6
        assert!(attacks & sq_to_bb(pos_to_sq(6, 6)) != 0); // f6

        // Should have exactly 8 attacks
        assert_eq!(attacks.count_ones(), 8);
    }

    #[test]
    fn test_knight_corner() {
        // Knight on a1 (sq = 0) should only attack b3 and c2
        let sq = pos_to_sq(1, 1); // a1
        let attacks = ATTACK_TABLES.knight[sq as usize];

        assert!(attacks & sq_to_bb(pos_to_sq(2, 3)) != 0); // c2
        assert!(attacks & sq_to_bb(pos_to_sq(3, 2)) != 0); // b3
        assert_eq!(attacks.count_ones(), 2);
    }

    #[test]
    fn test_king_attacks() {
        // King on e4 should attack 8 squares
        let sq = pos_to_sq(4, 5); // e4
        let attacks = ATTACK_TABLES.king[sq as usize];
        assert_eq!(attacks.count_ones(), 8);

        // King on a1 should attack 3 squares
        let sq = pos_to_sq(1, 1); // a1
        let attacks = ATTACK_TABLES.king[sq as usize];
        assert_eq!(attacks.count_ones(), 3);
    }

    #[test]
    fn test_pawn_attacks() {
        // White pawn on e4 attacks d5 and f5
        let sq = pos_to_sq(4, 5); // e4
        let attacks = ATTACK_TABLES.pawn[0][sq as usize]; // White
        assert!(attacks & sq_to_bb(pos_to_sq(5, 4)) != 0); // d5
        assert!(attacks & sq_to_bb(pos_to_sq(5, 6)) != 0); // f5
        assert_eq!(attacks.count_ones(), 2);

        // Black pawn on e5 attacks d4 and f4
        let sq = pos_to_sq(5, 5); // e5
        let attacks = ATTACK_TABLES.pawn[1][sq as usize]; // Black
        assert!(attacks & sq_to_bb(pos_to_sq(4, 4)) != 0); // d4
        assert!(attacks & sq_to_bb(pos_to_sq(4, 6)) != 0); // f4
        assert_eq!(attacks.count_ones(), 2);
    }

    #[test]
    fn test_rook_attacks_empty_board() {
        // Rook on e4 with no blockers
        let sq = pos_to_sq(4, 5); // e4
        let attacks = ATTACK_TABLES.rook_attacks(sq, 0);
        // Should attack 14 squares (7 on rank, 7 on file)
        assert_eq!(attacks.count_ones(), 14);
    }

    #[test]
    fn test_rook_attacks_with_blockers() {
        // Rook on e4, with pieces on e2 and g4
        let sq = pos_to_sq(4, 5); // e4
        let occupied = sq_to_bb(pos_to_sq(2, 5)) | sq_to_bb(pos_to_sq(4, 7)); // e2, g4
        let attacks = ATTACK_TABLES.rook_attacks(sq, occupied);

        // Should include e2 and g4 (can capture), but not squares beyond
        assert!(attacks & sq_to_bb(pos_to_sq(2, 5)) != 0); // e2 (blocker)
        assert!(attacks & sq_to_bb(pos_to_sq(4, 7)) != 0); // g4 (blocker)
        assert!(attacks & sq_to_bb(pos_to_sq(1, 5)) == 0); // e1 (blocked)
        assert!(attacks & sq_to_bb(pos_to_sq(4, 8)) == 0); // h4 (blocked)
    }

    #[test]
    fn test_bishop_attacks_empty_board() {
        // Bishop on e4 with no blockers
        let sq = pos_to_sq(4, 5); // e4
        let attacks = ATTACK_TABLES.bishop_attacks(sq, 0);
        // Should attack 13 squares on diagonals
        assert_eq!(attacks.count_ones(), 13);
    }

    #[test]
    fn test_bitboard_iter() {
        let bb = sq_to_bb(0) | sq_to_bb(7) | sq_to_bb(63); // a1, h1, h8
        let squares: Vec<u8> = BitboardIter(bb).collect();
        assert_eq!(squares, vec![0, 7, 63]);
    }

    // Magic bitboard tests

    #[test]
    fn test_magic_rook_attacks_empty_board() {
        // Rook on e4 with no blockers
        let sq = pos_to_sq(4, 5); // e4
        let attacks = rook_attacks(sq, 0);
        // Should attack 14 squares (7 on rank, 7 on file)
        assert_eq!(attacks.count_ones(), 14);
    }

    #[test]
    fn test_magic_rook_attacks_with_blockers() {
        // Rook on e4, with pieces on e2 and g4
        let sq = pos_to_sq(4, 5); // e4
        let occupied = sq_to_bb(pos_to_sq(2, 5)) | sq_to_bb(pos_to_sq(4, 7)); // e2, g4
        let attacks = rook_attacks(sq, occupied);

        // Should include e2 and g4 (can capture), but not squares beyond
        assert!(attacks & sq_to_bb(pos_to_sq(2, 5)) != 0); // e2 (blocker)
        assert!(attacks & sq_to_bb(pos_to_sq(4, 7)) != 0); // g4 (blocker)
        assert!(attacks & sq_to_bb(pos_to_sq(1, 5)) == 0); // e1 (blocked)
        assert!(attacks & sq_to_bb(pos_to_sq(4, 8)) == 0); // h4 (blocked)
    }

    #[test]
    fn test_magic_bishop_attacks_empty_board() {
        // Bishop on e4 with no blockers
        let sq = pos_to_sq(4, 5); // e4
        let attacks = bishop_attacks(sq, 0);
        // Should attack 13 squares on diagonals
        assert_eq!(attacks.count_ones(), 13);
    }

    #[test]
    fn test_magic_bishop_attacks_with_blockers() {
        // Bishop on e4, with pieces on c2 and g6
        let sq = pos_to_sq(4, 5); // e4
        let occupied = sq_to_bb(pos_to_sq(2, 3)) | sq_to_bb(pos_to_sq(6, 7)); // c2, g6
        let attacks = bishop_attacks(sq, occupied);

        // Should include blockers, but not squares beyond
        assert!(attacks & sq_to_bb(pos_to_sq(2, 3)) != 0); // c2 (blocker)
        assert!(attacks & sq_to_bb(pos_to_sq(6, 7)) != 0); // g6 (blocker)
        assert!(attacks & sq_to_bb(pos_to_sq(1, 2)) == 0); // b1 (blocked)
        assert!(attacks & sq_to_bb(pos_to_sq(7, 8)) == 0); // h7 (blocked)
    }

    #[test]
    fn test_magic_queen_attacks() {
        // Queen on e4 with no blockers
        let sq = pos_to_sq(4, 5); // e4
        let attacks = queen_attacks(sq, 0);
        // Should attack 27 squares (14 rook + 13 bishop)
        assert_eq!(attacks.count_ones(), 27);
    }

    #[test]
    fn test_magic_matches_classical() {
        // Verify magic bitboards produce same results as classical ray attacks
        let test_positions = [
            (pos_to_sq(1, 1), 0u64),                    // a1, empty
            (pos_to_sq(4, 5), 0u64),                    // e4, empty
            (pos_to_sq(8, 8), 0u64),                    // h8, empty
            (pos_to_sq(4, 5), sq_to_bb(pos_to_sq(2, 5)) | sq_to_bb(pos_to_sq(6, 5))), // e4 with e2,e6
            (pos_to_sq(4, 5), sq_to_bb(pos_to_sq(2, 3)) | sq_to_bb(pos_to_sq(6, 7))), // e4 with c2,g6
        ];

        for (sq, occupied) in test_positions {
            // Compare rook attacks
            let magic_rook = rook_attacks(sq, occupied);
            let classical_rook = ATTACK_TABLES.rook_attacks(sq, occupied);
            assert_eq!(magic_rook, classical_rook, "Rook mismatch at sq={}, occ={}", sq, occupied);

            // Compare bishop attacks
            let magic_bishop = bishop_attacks(sq, occupied);
            let classical_bishop = ATTACK_TABLES.bishop_attacks(sq, occupied);
            assert_eq!(magic_bishop, classical_bishop, "Bishop mismatch at sq={}, occ={}", sq, occupied);

            // Compare queen attacks
            let magic_queen = queen_attacks(sq, occupied);
            let classical_queen = ATTACK_TABLES.queen_attacks(sq, occupied);
            assert_eq!(magic_queen, classical_queen, "Queen mismatch at sq={}, occ={}", sq, occupied);
        }
    }
}
