//! Bitboard representation and precomputed attack tables for fast move generation.
//!
//! A bitboard is a 64-bit integer where each bit represents a square on the chess board.
//! Square indexing: a1 = 0, b1 = 1, ..., h1 = 7, a2 = 8, ..., h8 = 63
//! This corresponds to: square_index = (rank - 1) * 8 + (file - 1) where rank/file are 1-indexed.

use crate::types::Position;

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
}
