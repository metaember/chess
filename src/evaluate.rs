use std::cell::UnsafeCell;

use crate::bitboard::{
    BitboardIter, FILE_MASKS, ADJACENT_FILE_MASKS, PASSED_PAWN_MASKS,
    ATTACK_TABLES, bishop_attacks, rook_attacks,
};
use crate::board::Board;
use crate::types::{Color, Move, MoveFlag, Piece, PieceType};

/// Maximum game phase value (all major/minor pieces on board)
/// Queens = 4, Rooks = 2, Bishops = 1, Knights = 1
/// Total: 2*4 + 4*2 + 4*1 + 4*1 = 8 + 8 + 4 + 4 = 24
pub const MAX_PHASE: i32 = 24;

/// Phase weights for each piece type
pub const PHASE_WEIGHTS: [i32; 6] = [
    0, // Pawn
    2, // Rook
    1, // Knight
    1, // Bishop
    4, // Queen
    0, // King
];

pub struct Material {
    pub white_material: i32,
    pub white_pst_mg: i32, // middlegame PST score
    pub white_pst_eg: i32, // endgame PST score
    pub white_pawn_material: i32,
    pub white_piece_material: i32,

    pub black_material: i32,
    pub black_pst_mg: i32, // middlegame PST score
    pub black_pst_eg: i32, // endgame PST score
    pub black_pawn_material: i32,
    pub black_piece_material: i32,

    /// Game phase (0 = endgame, MAX_PHASE = opening/middlegame)
    pub phase: i32,
}

impl Material {
    pub fn get_material_difference(&self) -> i32 {
        self.white_material - self.black_material
    }

    /// Get tapered evaluation score (interpolates between middlegame and endgame)
    pub fn get_tapered_score(&self) -> i32 {
        let mg_score = self.white_material + self.white_pst_mg
            - self.black_material - self.black_pst_mg;
        let eg_score = self.white_material + self.white_pst_eg
            - self.black_material - self.black_pst_eg;

        // Interpolate between mg and eg based on phase
        // phase = MAX_PHASE means full middlegame, phase = 0 means full endgame
        let phase = self.phase.clamp(0, MAX_PHASE);
        (mg_score * phase + eg_score * (MAX_PHASE - phase)) / MAX_PHASE
    }

    pub fn piece_to_material(piece_type: PieceType) -> i32 {
        match piece_type {
            PieceType::Pawn => 100,
            PieceType::Rook => 500,
            PieceType::Knight => 300,
            PieceType::Bishop => 320,
            PieceType::Queen => 900,
            PieceType::King => 0,
        }
    }

    pub fn piece_to_phase(piece_type: PieceType) -> i32 {
        PHASE_WEIGHTS[piece_type_to_index(piece_type)]
    }

    pub fn compute_material(board: &Board) -> Material {
        let mut white_material = 0;
        let mut black_material = 0;
        let mut white_pst_mg = 0;
        let mut white_pst_eg = 0;
        let mut black_pst_mg = 0;
        let mut black_pst_eg = 0;
        let mut white_pawn_material = 0;
        let mut black_pawn_material = 0;
        let mut white_piece_material = 0;
        let mut black_piece_material = 0;
        let mut phase = 0;

        for piece in board.iter_pieces() {
            let current_piece_material = Material::piece_to_material(piece.piece_type);
            phase += Material::piece_to_phase(piece.piece_type);

            match piece.color {
                Color::White => {
                    white_material += current_piece_material;
                    white_pst_mg += get_piece_pst_value(&piece, false);
                    white_pst_eg += get_piece_pst_value(&piece, true);
                    if piece.piece_type == PieceType::Pawn {
                        white_pawn_material += current_piece_material;
                    } else {
                        white_piece_material += current_piece_material
                    };
                }
                Color::Black => {
                    black_material += current_piece_material;
                    black_pst_mg += get_piece_pst_value(&piece, false);
                    black_pst_eg += get_piece_pst_value(&piece, true);
                    if piece.piece_type == PieceType::Pawn {
                        black_pawn_material += current_piece_material;
                    } else {
                        black_piece_material += current_piece_material;
                    }
                }
            };
        }
        Material {
            white_material,
            white_pst_mg,
            white_pst_eg,
            white_pawn_material,
            white_piece_material,
            black_material,
            black_pst_mg,
            black_pst_eg,
            black_pawn_material,
            black_piece_material,
            phase,
        }
    }
}

/// Convert PieceType to index for phase weights array
#[inline(always)]
fn piece_type_to_index(pt: PieceType) -> usize {
    match pt {
        PieceType::Pawn => 0,
        PieceType::Rook => 1,
        PieceType::Knight => 2,
        PieceType::Bishop => 3,
        PieceType::Queen => 4,
        PieceType::King => 5,
    }
}


// =============================================================================
// Pawn Hash Table - caches pawn structure evaluation
// =============================================================================

const PAWN_HASH_SIZE: usize = 16384; // 16K entries

#[derive(Clone, Copy)]
struct PawnEntry {
    key: u64,
    mg_score: i16,
    eg_score: i16,
    white_passed: u64,
    black_passed: u64,
}

impl Default for PawnEntry {
    fn default() -> Self {
        PawnEntry { key: 0, mg_score: 0, eg_score: 0, white_passed: 0, black_passed: 0 }
    }
}

struct PawnHashTable {
    entries: Vec<PawnEntry>,
}

impl PawnHashTable {
    fn new() -> Self {
        PawnHashTable {
            entries: vec![PawnEntry::default(); PAWN_HASH_SIZE],
        }
    }

    #[inline]
    fn probe(&self, key: u64) -> Option<&PawnEntry> {
        let idx = (key as usize) % PAWN_HASH_SIZE;
        let entry = &self.entries[idx];
        if entry.key == key {
            Some(entry)
        } else {
            None
        }
    }

    #[inline]
    fn store(&mut self, key: u64, mg_score: i16, eg_score: i16, white_passed: u64, black_passed: u64) {
        let idx = (key as usize) % PAWN_HASH_SIZE;
        self.entries[idx] = PawnEntry { key, mg_score, eg_score, white_passed, black_passed };
    }
}

thread_local! {
    static PAWN_HASH: UnsafeCell<PawnHashTable> = UnsafeCell::new(PawnHashTable::new());
}

// =============================================================================
// Evaluation constants
// =============================================================================

/// Bishop pair bonus (centipawns)
const BISHOP_PAIR_MG: i32 = 30;
const BISHOP_PAIR_EG: i32 = 50;

/// Doubled pawn penalty per extra pawn on same file
const DOUBLED_PAWN_MG: i32 = -10;
const DOUBLED_PAWN_EG: i32 = -50;

/// Isolated pawn penalty (no friendly pawns on adjacent files)
const ISOLATED_PAWN_MG: i32 = -5;
const ISOLATED_PAWN_EG: i32 = -15;
/// Extra penalty when isolated pawn is on an open file (no enemy pawn blocking)
const ISOLATED_PAWN_OPEN_MG: i32 = -20;
const ISOLATED_PAWN_OPEN_EG: i32 = -40;

/// Backward pawn penalty (can't advance, no friendly pawns behind on adjacent files)
const BACKWARD_PAWN_MG: i32 = -8;
const BACKWARD_PAWN_EG: i32 = -25;

/// Passed pawn bonus by rank advancement (index 0=rank2, 5=rank7 from pawn's perspective)
const PASSED_PAWN_MG: [i32; 6] = [10, 15, 20, 60, 150, 250];
const PASSED_PAWN_EG: [i32; 6] = [25, 30, 40, 70, 170, 260];

/// Connected pawn bonus by rank (phalanx or supported)
/// Index 0=rank2, 5=rank7 from pawn's perspective
const CONNECTED_PAWN_MG: [i32; 6] = [3, 4, 6, 10, 20, 30];
const CONNECTED_PAWN_EG: [i32; 6] = [4, 5, 8, 12, 25, 35];

/// King safety: pawn shield bonus per pawn
const PAWN_SHIELD_BONUS: i32 = 10;
/// King safety: penalty per open file near king
const KING_OPEN_FILE_PENALTY: i32 = -20;
/// King attack zone weights: penalty based on number of enemy attack squares in king zone
/// Indexed by attack count (0..=14), quadratic-ish growth
const KING_ATTACK_PENALTY: [i32; 16] = [0, 0, 2, 6, 12, 20, 30, 42, 56, 70, 82, 92, 100, 106, 110, 112];

/// Outpost bonus for knight/bishop on an advanced square:
/// - defended by a friendly pawn
/// - cannot be attacked by enemy pawns (no enemy pawn on adjacent files ahead)
const KNIGHT_OUTPOST_MG: i32 = 20;
const KNIGHT_OUTPOST_EG: i32 = 12;
const BISHOP_OUTPOST_MG: i32 = 10;
const BISHOP_OUTPOST_EG: i32 = 6;

/// Rook on open/semi-open file (MG/EG split)
const ROOK_OPEN_FILE_MG: i32 = 40;
const ROOK_OPEN_FILE_EG: i32 = 20;
const ROOK_SEMI_OPEN_FILE_MG: i32 = 18;
const ROOK_SEMI_OPEN_FILE_EG: i32 = 7;

/// Chebyshev (king) distance between two squares (0-indexed, 0-63)
#[inline]
fn chebyshev_distance(sq1: u8, sq2: u8) -> u8 {
    let file1 = (sq1 & 7) as i8;
    let rank1 = (sq1 >> 3) as i8;
    let file2 = (sq2 & 7) as i8;
    let rank2 = (sq2 >> 3) as i8;
    ((file1 - file2).abs()).max((rank1 - rank2).abs()) as u8
}

// =============================================================================
// Pawn structure evaluation (cached via pawn hash table)
// =============================================================================

/// Evaluate pawn structure for both colors.
/// Returns (mg_score, eg_score, white_passed_bb, black_passed_bb) from white's perspective.
pub fn evaluate_pawn_structure(white_pawns: u64, black_pawns: u64) -> (i32, i32, u64, u64) {

    let mut mg = 0i32;
    let mut eg = 0i32;
    let mut white_passed = 0u64;
    let mut black_passed = 0u64;

    // Pawn attack maps for backward pawn detection and connected pawn detection
    let white_pawn_attacks = ((white_pawns & !FILE_MASKS[0]) << 7) | ((white_pawns & !FILE_MASKS[7]) << 9);
    let black_pawn_attacks = ((black_pawns & !FILE_MASKS[7]) >> 7) | ((black_pawns & !FILE_MASKS[0]) >> 9);

    // Phalanx: pawn with a neighbor on the same rank (adjacent file)
    let white_phalanx = white_pawns & (((white_pawns & !FILE_MASKS[7]) << 1) | ((white_pawns & !FILE_MASKS[0]) >> 1));
    let black_phalanx = black_pawns & (((black_pawns & !FILE_MASKS[7]) << 1) | ((black_pawns & !FILE_MASKS[0]) >> 1));

    // Supported: pawn that is defended by a friendly pawn
    let white_supported = white_pawns & white_pawn_attacks;
    let black_supported = black_pawns & black_pawn_attacks;

    // Connected = phalanx OR supported
    let white_connected = white_phalanx | white_supported;
    let black_connected = black_phalanx | black_supported;

    // --- White pawns ---
    for sq in BitboardIter(white_pawns) {
        let file = (sq & 7) as usize; // 0-indexed file
        let rank = (sq >> 3) as usize; // 0-indexed rank

        // Connected pawn bonus
        if white_connected & (1u64 << sq) != 0 {
            let adv = (rank as i32 - 1).clamp(0, 5) as usize;
            mg += CONNECTED_PAWN_MG[adv];
            eg += CONNECTED_PAWN_EG[adv];
        } else {
            // Isolated pawn: no friendly pawns on adjacent files
            let isolated = white_pawns & ADJACENT_FILE_MASKS[file] == 0;
            if isolated {
                if black_pawns & FILE_MASKS[file] == 0 {
                    mg += ISOLATED_PAWN_OPEN_MG;
                    eg += ISOLATED_PAWN_OPEN_EG;
                } else {
                    mg += ISOLATED_PAWN_MG;
                    eg += ISOLATED_PAWN_EG;
                }
            } else if rank < 6 {
                // Backward pawn: not isolated, stop square attacked by enemy pawn,
                // and no friendly pawn on adjacent files at same rank or behind to support
                let stop_sq = sq + 8;
                if black_pawn_attacks & (1u64 << stop_sq) != 0 {
                    // Mask of adjacent file squares at ranks 0..=rank
                    let behind_mask = ADJACENT_FILE_MASKS[file] & ((1u64 << ((rank + 1) * 8)) - 1);
                    if white_pawns & behind_mask == 0 {
                        mg += BACKWARD_PAWN_MG;
                        eg += BACKWARD_PAWN_EG;
                    }
                }
            }
        }

        // Passed pawn: no enemy pawns ahead on same + adjacent files
        if black_pawns & PASSED_PAWN_MASKS[0][sq as usize] == 0 {
            white_passed |= 1u64 << sq;
            // rank is 0-indexed; pawns on rank 1(idx 1) to rank 6(idx 6) can be passed
            // advancement index: rank-1 (rank2=idx0, rank7=idx5)
            let adv = (rank as i32 - 1).clamp(0, 5) as usize;
            mg += PASSED_PAWN_MG[adv];
            eg += PASSED_PAWN_EG[adv];
        }
    }

    // Doubled pawns (white)
    for file in 0..8 {
        let count = (white_pawns & FILE_MASKS[file]).count_ones() as i32;
        if count > 1 {
            mg += DOUBLED_PAWN_MG * (count - 1);
            eg += DOUBLED_PAWN_EG * (count - 1);
        }
    }

    // --- Black pawns ---
    for sq in BitboardIter(black_pawns) {
        let file = (sq & 7) as usize;
        let rank = (sq >> 3) as usize;

        // Connected pawn bonus
        if black_connected & (1u64 << sq) != 0 {
            let adv = (6 - rank as i32).clamp(0, 5) as usize;
            mg -= CONNECTED_PAWN_MG[adv];
            eg -= CONNECTED_PAWN_EG[adv];
        } else {
            // Isolated pawn
            let isolated = black_pawns & ADJACENT_FILE_MASKS[file] == 0;
            if isolated {
                if white_pawns & FILE_MASKS[file] == 0 {
                    mg -= ISOLATED_PAWN_OPEN_MG;
                    eg -= ISOLATED_PAWN_OPEN_EG;
                } else {
                    mg -= ISOLATED_PAWN_MG;
                    eg -= ISOLATED_PAWN_EG;
                }
            } else if rank > 1 {
                // Backward pawn (black): stop square is sq-8
                let stop_sq = sq - 8;
                if white_pawn_attacks & (1u64 << stop_sq) != 0 {
                    // Mask of adjacent file squares at ranks rank..=7
                    let behind_mask = ADJACENT_FILE_MASKS[file] & !((1u64 << (rank * 8)) - 1);
                    if black_pawns & behind_mask == 0 {
                        mg -= BACKWARD_PAWN_MG;
                        eg -= BACKWARD_PAWN_EG;
                    }
                }
            }
        }

        // Passed pawn (from black's perspective)
        if white_pawns & PASSED_PAWN_MASKS[1][sq as usize] == 0 {
            black_passed |= 1u64 << sq;
            // For black, advancement = (6 - rank), rank7(idx6)->0, rank2(idx1)->5
            let adv = (6 - rank as i32).clamp(0, 5) as usize;
            mg -= PASSED_PAWN_MG[adv];
            eg -= PASSED_PAWN_EG[adv];
        }
    }

    // Doubled pawns (black)
    for file in 0..8 {
        let count = (black_pawns & FILE_MASKS[file]).count_ones() as i32;
        if count > 1 {
            mg -= DOUBLED_PAWN_MG * (count - 1);
            eg -= DOUBLED_PAWN_EG * (count - 1);
        }
    }

    (mg, eg, white_passed, black_passed)
}

// =============================================================================
// King safety evaluation
// =============================================================================

/// Evaluate king safety (pawn shield + open files + attack zones). Returns score from white's perspective.
/// Weighted by phase (middlegame only).
#[inline]
fn evaluate_king_safety(
    phase: i32,
    white_pawns: u64, black_pawns: u64,
    wk: u8, bk: u8,
    white_attack_map: u64, black_attack_map: u64,
) -> i32 {
    if phase == 0 {
        return 0; // Pure endgame, king safety irrelevant
    }

    let white_safety = king_safety_for_color(wk, white_pawns, Color::White, black_attack_map);
    let black_safety = king_safety_for_color(bk, black_pawns, Color::Black, white_attack_map);

    // Scale by phase (full weight in middlegame, zero in endgame)
    ((white_safety - black_safety) * phase) / MAX_PHASE
}

fn king_safety_for_color(king_sq: u8, friendly_pawns: u64, color: Color, enemy_attack_map: u64) -> i32 {
    let king_file = (king_sq & 7) as usize;
    let king_rank = (king_sq >> 3) as usize;
    let mut score = 0i32;

    // Pawn shield: count friendly pawns on king's file and adjacent files,
    // on the 1-2 ranks in front of the king
    let shield_ranks: u64 = match color {
        Color::White => {
            // Ranks above the king (1-2 ranks forward)
            let mut mask = 0u64;
            if king_rank < 7 { mask |= 0xFFu64 << ((king_rank + 1) * 8); }
            if king_rank < 6 { mask |= 0xFFu64 << ((king_rank + 2) * 8); }
            mask
        }
        Color::Black => {
            let mut mask = 0u64;
            if king_rank > 0 { mask |= 0xFFu64 << ((king_rank - 1) * 8); }
            if king_rank > 1 { mask |= 0xFFu64 << ((king_rank - 2) * 8); }
            mask
        }
    };

    // Files around the king (king file + adjacent)
    let mut file_mask = FILE_MASKS[king_file];
    if king_file > 0 { file_mask |= FILE_MASKS[king_file - 1]; }
    if king_file < 7 { file_mask |= FILE_MASKS[king_file + 1]; }

    let shield_zone = shield_ranks & file_mask;
    let shield_pawns = (friendly_pawns & shield_zone).count_ones() as i32;
    score += shield_pawns * PAWN_SHIELD_BONUS;

    // Open file penalty: check if king's file or adjacent files have no friendly pawns
    let start_file = if king_file > 0 { king_file - 1 } else { 0 };
    let end_file = if king_file < 7 { king_file + 1 } else { 7 };
    for f in start_file..=end_file {
        if friendly_pawns & FILE_MASKS[f] == 0 {
            score += KING_OPEN_FILE_PENALTY;
        }
    }

    // King attack zone: count enemy attacks into the king's 3x3 neighborhood
    let king_zone = ATTACK_TABLES.king[king_sq as usize] | (1u64 << king_sq);
    let attack_count = (enemy_attack_map & king_zone).count_ones() as usize;
    score -= KING_ATTACK_PENALTY[attack_count.min(15)];

    score
}

// =============================================================================
// Outpost evaluation
// =============================================================================

/// Evaluate knight/bishop outposts. Returns (mg, eg) from white's perspective.
#[inline]
pub fn evaluate_outposts(board: &Board, white_pawns: u64, black_pawns: u64) -> (i32, i32) {
    let white_pawn_attacks = ((white_pawns & !FILE_MASKS[0]) << 7) | ((white_pawns & !FILE_MASKS[7]) << 9);
    let black_pawn_attacks = ((black_pawns & !FILE_MASKS[7]) >> 7) | ((black_pawns & !FILE_MASKS[0]) >> 9);

    let mut mg = 0i32;
    let mut eg = 0i32;

    // White knights/bishops: outpost = defended by white pawn + no black pawn on adjacent files ahead
    for sq in BitboardIter(board.get_piece_bb(Color::White, PieceType::Knight)) {
        if white_pawn_attacks & (1u64 << sq) != 0 {
            let file = (sq & 7) as usize;
            let rank = (sq >> 3) as usize;
            // No black pawn on adjacent files at ranks > this piece's rank
            let ranks_ahead: u64 = if rank < 7 { !((1u64 << ((rank + 1) * 8)) - 1) } else { 0 };
            if black_pawns & ADJACENT_FILE_MASKS[file] & ranks_ahead == 0 {
                mg += KNIGHT_OUTPOST_MG;
                eg += KNIGHT_OUTPOST_EG;
            }
        }
    }

    for sq in BitboardIter(board.get_piece_bb(Color::White, PieceType::Bishop)) {
        if white_pawn_attacks & (1u64 << sq) != 0 {
            let file = (sq & 7) as usize;
            let rank = (sq >> 3) as usize;
            let ranks_ahead: u64 = if rank < 7 { !((1u64 << ((rank + 1) * 8)) - 1) } else { 0 };
            if black_pawns & ADJACENT_FILE_MASKS[file] & ranks_ahead == 0 {
                mg += BISHOP_OUTPOST_MG;
                eg += BISHOP_OUTPOST_EG;
            }
        }
    }

    // Black knights/bishops: outpost = defended by black pawn + no white pawn on adjacent files ahead (toward rank 1)
    for sq in BitboardIter(board.get_piece_bb(Color::Black, PieceType::Knight)) {
        if black_pawn_attacks & (1u64 << sq) != 0 {
            let file = (sq & 7) as usize;
            let rank = (sq >> 3) as usize;
            // No white pawn on adjacent files at ranks < this piece's rank (toward rank 0)
            let ranks_ahead: u64 = if rank > 0 { (1u64 << (rank * 8)) - 1 } else { 0 };
            if white_pawns & ADJACENT_FILE_MASKS[file] & ranks_ahead == 0 {
                mg -= KNIGHT_OUTPOST_MG;
                eg -= KNIGHT_OUTPOST_EG;
            }
        }
    }

    for sq in BitboardIter(board.get_piece_bb(Color::Black, PieceType::Bishop)) {
        if black_pawn_attacks & (1u64 << sq) != 0 {
            let file = (sq & 7) as usize;
            let rank = (sq >> 3) as usize;
            let ranks_ahead: u64 = if rank > 0 { (1u64 << (rank * 8)) - 1 } else { 0 };
            if white_pawns & ADJACENT_FILE_MASKS[file] & ranks_ahead == 0 {
                mg -= BISHOP_OUTPOST_MG;
                eg -= BISHOP_OUTPOST_EG;
            }
        }
    }

    (mg, eg)
}

// =============================================================================
// Rook on open/semi-open file evaluation
// =============================================================================

/// Evaluate rooks on open/semi-open files. Returns (mg, eg) from white's perspective.
#[inline]
pub fn evaluate_rooks(board: &Board, white_pawns: u64, black_pawns: u64) -> (i32, i32) {
    let all_pawns = white_pawns | black_pawns;

    let mut mg = 0i32;
    let mut eg = 0i32;

    // White rooks
    for sq in BitboardIter(board.get_piece_bb(Color::White, PieceType::Rook)) {
        let file = (sq & 7) as usize;
        if all_pawns & FILE_MASKS[file] == 0 {
            mg += ROOK_OPEN_FILE_MG;
            eg += ROOK_OPEN_FILE_EG;
        } else if white_pawns & FILE_MASKS[file] == 0 {
            mg += ROOK_SEMI_OPEN_FILE_MG;
            eg += ROOK_SEMI_OPEN_FILE_EG;
        }
    }

    // Black rooks
    for sq in BitboardIter(board.get_piece_bb(Color::Black, PieceType::Rook)) {
        let file = (sq & 7) as usize;
        if all_pawns & FILE_MASKS[file] == 0 {
            mg -= ROOK_OPEN_FILE_MG;
            eg -= ROOK_OPEN_FILE_EG;
        } else if black_pawns & FILE_MASKS[file] == 0 {
            mg -= ROOK_SEMI_OPEN_FILE_MG;
            eg -= ROOK_SEMI_OPEN_FILE_EG;
        }
    }

    (mg, eg)
}

// =============================================================================
// Mobility evaluation (lookup tables from Stockfish 14 classical eval)
// =============================================================================

/// Knight mobility bonus by number of accessible squares (0-8)
/// Stockfish 14 classical values scaled to our 100cp pawn (SF pawn ~126cp MG / 208cp EG)
const KNIGHT_MOB_MG: [i32; 9] = [-49, -42, -10, -3, 2, 10, 17, 22, 26];
const KNIGHT_MOB_EG: [i32; 9] = [-39, -27, -14, -7, 4, 7, 11, 13, 16];

/// Bishop mobility bonus by number of accessible squares (0-13)
const BISHOP_MOB_MG: [i32; 14] = [-38, -16, 13, 21, 30, 40, 43, 50, 50, 54, 64, 64, 72, 77];
const BISHOP_MOB_EG: [i32; 14] = [-28, -11, -1, 6, 12, 20, 26, 27, 31, 35, 37, 41, 42, 47];

/// Rook mobility bonus by number of accessible squares (0-14)
const ROOK_MOB_MG: [i32; 15] = [-47, -16, 2, 2, 2, 9, 17, 24, 32, 32, 32, 32, 38, 45, 45];
const ROOK_MOB_EG: [i32; 15] = [-37, -8, 11, 19, 34, 47, 49, 58, 64, 67, 76, 79, 81, 81, 83];

/// Evaluate piece mobility for both colors.
/// Returns (mg_score, eg_score) from white's perspective.
#[inline]
pub fn evaluate_mobility(board: &Board, white_pawns: u64, black_pawns: u64) -> (i32, i32) {
    let white_pieces = board.get_pieces_bb(Color::White);
    let black_pieces = board.get_pieces_bb(Color::Black);
    let occupied = board.get_occupied();

    // Compute enemy pawn attack masks
    let white_pawn_attacks = ((white_pawns & !FILE_MASKS[0]) << 7) | ((white_pawns & !FILE_MASKS[7]) << 9);
    let black_pawn_attacks = ((black_pawns & !FILE_MASKS[7]) >> 7) | ((black_pawns & !FILE_MASKS[0]) >> 9);

    // Mobility areas: not friendly pieces, not attacked by enemy pawns
    let white_mob_area = !white_pieces & !black_pawn_attacks;
    let black_mob_area = !black_pieces & !white_pawn_attacks;

    let mut mg = 0i32;
    let mut eg = 0i32;

    // White knights
    for sq in BitboardIter(board.get_piece_bb(Color::White, PieceType::Knight)) {
        let count = (ATTACK_TABLES.knight[sq as usize] & white_mob_area).count_ones() as usize;
        mg += KNIGHT_MOB_MG[count.min(8)];
        eg += KNIGHT_MOB_EG[count.min(8)];
    }

    // Black knights
    for sq in BitboardIter(board.get_piece_bb(Color::Black, PieceType::Knight)) {
        let count = (ATTACK_TABLES.knight[sq as usize] & black_mob_area).count_ones() as usize;
        mg -= KNIGHT_MOB_MG[count.min(8)];
        eg -= KNIGHT_MOB_EG[count.min(8)];
    }

    // White bishops
    for sq in BitboardIter(board.get_piece_bb(Color::White, PieceType::Bishop)) {
        let count = (bishop_attacks(sq, occupied) & white_mob_area).count_ones() as usize;
        mg += BISHOP_MOB_MG[count.min(13)];
        eg += BISHOP_MOB_EG[count.min(13)];
    }

    // Black bishops
    for sq in BitboardIter(board.get_piece_bb(Color::Black, PieceType::Bishop)) {
        let count = (bishop_attacks(sq, occupied) & black_mob_area).count_ones() as usize;
        mg -= BISHOP_MOB_MG[count.min(13)];
        eg -= BISHOP_MOB_EG[count.min(13)];
    }

    // White rooks
    for sq in BitboardIter(board.get_piece_bb(Color::White, PieceType::Rook)) {
        let count = (rook_attacks(sq, occupied) & white_mob_area).count_ones() as usize;
        mg += ROOK_MOB_MG[count.min(14)];
        eg += ROOK_MOB_EG[count.min(14)];
    }

    // Black rooks
    for sq in BitboardIter(board.get_piece_bb(Color::Black, PieceType::Rook)) {
        let count = (rook_attacks(sq, occupied) & black_mob_area).count_ones() as usize;
        mg -= ROOK_MOB_MG[count.min(14)];
        eg -= ROOK_MOB_EG[count.min(14)];
    }

    (mg, eg)
}

// =============================================================================
// Main evaluation function
// =============================================================================

pub fn evaluate_board(board: &Board) -> i32 {
    // Use incrementally maintained evaluation scores (material + PST)
    let phase = board.get_phase().clamp(0, MAX_PHASE);

    // Start with tapered material + PST score
    let mut mg_score = board.get_material(Color::White) + board.get_pst_mg(Color::White)
        - board.get_material(Color::Black) - board.get_pst_mg(Color::Black);
    let mut eg_score = board.get_material(Color::White) + board.get_pst_eg(Color::White)
        - board.get_material(Color::Black) - board.get_pst_eg(Color::Black);

    // Bishop pair bonus (incremental data, O(1))
    if board.get_bishop_count(Color::White) >= 2 {
        mg_score += BISHOP_PAIR_MG;
        eg_score += BISHOP_PAIR_EG;
    }
    if board.get_bishop_count(Color::Black) >= 2 {
        mg_score -= BISHOP_PAIR_MG;
        eg_score -= BISHOP_PAIR_EG;
    }

    // Hoist common lookups used by multiple sub-functions
    let white_pawns = board.get_piece_bb(Color::White, PieceType::Pawn);
    let black_pawns = board.get_piece_bb(Color::Black, PieceType::Pawn);
    let wk = board.get_king_square(Color::White);
    let bk = board.get_king_square(Color::Black);

    // Pawn structure (cached via pawn hash table)
    let pawn_hash = board.get_pawn_hash();
    // SAFETY: single-threaded access within thread_local, no re-entrant calls
    let (pawn_mg, pawn_eg, white_passed, black_passed) = PAWN_HASH.with(|ph| {
        let table = unsafe { &mut *ph.get() };
        if let Some(entry) = table.probe(pawn_hash) {
            (entry.mg_score as i32, entry.eg_score as i32, entry.white_passed, entry.black_passed)
        } else {
            let (mg, eg, wp, bp) = evaluate_pawn_structure(white_pawns, black_pawns);
            table.store(pawn_hash, mg as i16, eg as i16, wp, bp);
            (mg, eg, wp, bp)
        }
    });
    mg_score += pawn_mg;
    eg_score += pawn_eg;

    // King proximity to passed pawns (endgame bonus, not cached — depends on king positions)
    for sq in BitboardIter(white_passed) {
        let rank = (sq >> 3) as i32; // 0-indexed
        let adv = (rank - 1).clamp(0, 5);
        if adv >= 3 { // only rank 5+ (adv 3 = rank 4 in 0-indexed = rank 5 in chess)
            let promo_sq = (sq & 7) | 56; // promotion square (rank 8)
            let enemy_dist = chebyshev_distance(bk, promo_sq as u8);
            let friendly_dist = chebyshev_distance(wk, promo_sq as u8);
            let weight = 5 * adv - 13;
            eg_score += (enemy_dist as i32 * 5 - friendly_dist as i32 * 2) * weight;
        }
    }
    for sq in BitboardIter(black_passed) {
        let rank = (sq >> 3) as i32;
        let adv = (6 - rank).clamp(0, 5);
        if adv >= 3 {
            let promo_sq = sq & 7; // promotion square (rank 1)
            let enemy_dist = chebyshev_distance(wk, promo_sq as u8);
            let friendly_dist = chebyshev_distance(bk, promo_sq as u8);
            let weight = 5 * adv - 13;
            eg_score -= (enemy_dist as i32 * 5 - friendly_dist as i32 * 2) * weight;
        }
    }

    // Piece mobility (computed each call — depends on all piece positions)
    let (mob_mg, mob_eg) = evaluate_mobility(board, white_pawns, black_pawns);
    mg_score += mob_mg;
    eg_score += mob_eg;

    // Outposts
    let (out_mg, out_eg) = evaluate_outposts(board, white_pawns, black_pawns);
    mg_score += out_mg;
    eg_score += out_eg;

    // Rook on open/semi-open files
    let (rook_mg, rook_eg) = evaluate_rooks(board, white_pawns, black_pawns);
    mg_score += rook_mg;
    eg_score += rook_eg;

    // Taper the score
    let mut unsigned_score = (mg_score * phase + eg_score * (MAX_PHASE - phase)) / MAX_PHASE;

    // King safety (middlegame-weighted, computed each call)
    let white_attack_map = board.get_attack_map(Color::White);
    let black_attack_map = board.get_attack_map(Color::Black);
    unsigned_score += evaluate_king_safety(phase, white_pawns, black_pawns, wk, bk, white_attack_map, black_attack_map);

    if board.get_active_color() == Color::White {
        unsigned_score
    } else {
        -unsigned_score
    }
}

/// Evaluate board by computing material from scratch (for test verification).
/// Includes all eval terms (same as evaluate_board) but recomputes material+PST from scratch.
#[cfg(test)]
fn debug_slow_evaluate_board(board: &Board) -> i32 {
    let material = Material::compute_material(board);
    let phase = material.phase.clamp(0, MAX_PHASE);

    let mut mg_score = material.white_material + material.white_pst_mg
        - material.black_material - material.black_pst_mg;
    let mut eg_score = material.white_material + material.white_pst_eg
        - material.black_material - material.black_pst_eg;

    // Bishop pair (recount from bitboards)
    if board.get_piece_bb(Color::White, PieceType::Bishop).count_ones() >= 2 {
        mg_score += BISHOP_PAIR_MG;
        eg_score += BISHOP_PAIR_EG;
    }
    if board.get_piece_bb(Color::Black, PieceType::Bishop).count_ones() >= 2 {
        mg_score -= BISHOP_PAIR_MG;
        eg_score -= BISHOP_PAIR_EG;
    }

    // Hoist common lookups
    let white_pawns = board.get_piece_bb(Color::White, PieceType::Pawn);
    let black_pawns = board.get_piece_bb(Color::Black, PieceType::Pawn);
    let wk = board.get_king_square(Color::White);
    let bk = board.get_king_square(Color::Black);

    // Pawn structure (recompute from scratch, no hash table)
    let (pawn_mg, pawn_eg, white_passed, black_passed) = evaluate_pawn_structure(white_pawns, black_pawns);
    mg_score += pawn_mg;
    eg_score += pawn_eg;

    // King proximity to passed pawns
    for sq in BitboardIter(white_passed) {
        let rank = (sq >> 3) as i32;
        let adv = (rank - 1).clamp(0, 5);
        if adv >= 3 {
            let promo_sq = (sq & 7) | 56;
            let enemy_dist = chebyshev_distance(bk, promo_sq as u8);
            let friendly_dist = chebyshev_distance(wk, promo_sq as u8);
            let weight = 5 * adv - 13;
            eg_score += (enemy_dist as i32 * 5 - friendly_dist as i32 * 2) * weight;
        }
    }
    for sq in BitboardIter(black_passed) {
        let rank = (sq >> 3) as i32;
        let adv = (6 - rank).clamp(0, 5);
        if adv >= 3 {
            let promo_sq = sq & 7;
            let enemy_dist = chebyshev_distance(wk, promo_sq as u8);
            let friendly_dist = chebyshev_distance(bk, promo_sq as u8);
            let weight = 5 * adv - 13;
            eg_score -= (enemy_dist as i32 * 5 - friendly_dist as i32 * 2) * weight;
        }
    }

    // Piece mobility
    let (mob_mg, mob_eg) = evaluate_mobility(board, white_pawns, black_pawns);
    mg_score += mob_mg;
    eg_score += mob_eg;

    // Outposts
    let (out_mg, out_eg) = evaluate_outposts(board, white_pawns, black_pawns);
    mg_score += out_mg;
    eg_score += out_eg;

    // Rook on open/semi-open files
    let (rook_mg, rook_eg) = evaluate_rooks(board, white_pawns, black_pawns);
    mg_score += rook_mg;
    eg_score += rook_eg;

    let mut unsigned_score = (mg_score * phase + eg_score * (MAX_PHASE - phase)) / MAX_PHASE;

    // King safety
    let white_attack_map = board.get_attack_map(Color::White);
    let black_attack_map = board.get_attack_map(Color::Black);
    unsigned_score += evaluate_king_safety(phase, white_pawns, black_pawns, wk, bk, white_attack_map, black_attack_map);

    if board.get_active_color() == Color::White {
        unsigned_score
    } else {
        -unsigned_score
    }
}

pub fn guess_move_value(_board: &Board, mv: &Move) -> i32 {
    let mut score = 0;
    let material_difference_multiplier = 10;

    if let Some(captured_piece) = mv.captured {
        score += material_difference_multiplier
            * (Material::piece_to_material(captured_piece.piece_type)
                - Material::piece_to_material(mv.piece.piece_type));
    };

    // Add promotion bonus equal to the value gained (promoted piece - pawn)
    if let MoveFlag::Promotion(promo_type) = mv.move_flag {
        let promo_value = Material::piece_to_material(promo_type);
        let pawn_value = Material::piece_to_material(PieceType::Pawn);
        score += promo_value - pawn_value; // e.g., Queen = +800
    }

    // TODO: penalize moves that move a piece into a position where it can be captured by a pawn
    // these should be cached when we evaluate the board before the move
    score
}

/// Get PST value for a piece (used in Material::compute_material)
fn get_piece_pst_value(piece: &Piece, is_endgame: bool) -> i32 {
    get_pst_value(piece.piece_type, piece.color, piece.position.rank, piece.position.file, is_endgame)
}

/// Get PST value given piece type, color, and position
/// This is the core PST lookup function used by both evaluation and incremental updates
pub fn get_pst_value(piece_type: PieceType, color: Color, rank: u8, file: u8, is_endgame: bool) -> i32 {
    let raw_table = get_raw_pst_table(piece_type, is_endgame);
    // PST tables are stored with rank 8 at index 0-7 and rank 1 at index 56-63
    // (from White's perspective, looking at the board with rank 1 at bottom)
    let index = match color {
        // White: rank 8 -> indices 0-7, rank 1 -> indices 56-63
        Color::White => (8 - rank) * 8 + file - 1,
        // Black: mirror perspective - rank 1 -> indices 0-7, rank 8 -> indices 56-63
        // This ensures Black's home rank (8) uses same values as White's home rank (1)
        Color::Black => (rank - 1) * 8 + file - 1,
    };
    raw_table[index as usize]
}

fn get_raw_pst_table(piece_type: PieceType, is_endgame: bool) -> &'static [i32; 64] {
    match piece_type {
        PieceType::Pawn => {
            if is_endgame {
                &PAWNS_END
            } else {
                &PAWNS
            }
        }
        PieceType::Rook => &ROOKS,
        PieceType::Knight => &KNIGHTS,
        PieceType::Bishop => &BISHOPS,
        PieceType::Queen => &QUEENS,
        PieceType::King => {
            if is_endgame {
                &KING_END
            } else {
                &KING_START
            }
        }
    }
}

// This part of the code aattributes different weights to pieces in different positions
// stolen from : https://github.com/SebLague/Chess-Coding-Adventure/blob/Chess-V2-UCI/Chess-Coding-Adventure/src/Core/Evaluation/PieceSquareTable.cs

const PAWNS: [i32; 8 * 8] = [
    0, 0, 0, 0, 0, 0, 0, 0, 50, 50, 50, 50, 50, 50, 50, 50, 10, 10, 20, 30, 30, 20, 10, 10, 5, 5,
    10, 25, 25, 10, 5, 5, 0, 0, 0, 20, 20, 0, 0, 0, 5, -5, -10, 0, 0, -10, -5, 5, 5, 10, 10, -20,
    -20, 10, 10, 5, 0, 0, 0, 0, 0, 0, 0, 0,
];

const PAWNS_END: [i32; 8 * 8] = [
    0, 0, 0, 0, 0, 0, 0, 0, 80, 80, 80, 80, 80, 80, 80, 80, 50, 50, 50, 50, 50, 50, 50, 50, 30, 30,
    30, 30, 30, 30, 30, 30, 20, 20, 20, 20, 20, 20, 20, 20, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0,
];

const ROOKS: [i32; 8 * 8] = [
    0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 10, 10, 10, 10, 10, 5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0,
    0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, 0, 0,
    0, 5, 5, 0, 0, 0,
];

const KNIGHTS: [i32; 8 * 8] = [
    -50, -40, -30, -30, -30, -30, -40, -50, -40, -20, 0, 0, 0, 0, -20, -40, -30, 0, 10, 15, 15, 10,
    0, -30, -30, 5, 15, 20, 20, 15, 5, -30, -30, 0, 15, 20, 20, 15, 0, -30, -30, 5, 10, 15, 15, 10,
    5, -30, -40, -20, 0, 5, 5, 0, -20, -40, -50, -40, -30, -30, -30, -30, -40, -50,
];

const BISHOPS: [i32; 8 * 8] = [
    -20, -10, -10, -10, -10, -10, -10, -20, -10, 0, 0, 0, 0, 0, 0, -10, -10, 0, 5, 10, 10, 5, 0,
    -10, -10, 5, 5, 10, 10, 5, 5, -10, -10, 0, 10, 10, 10, 10, 0, -10, -10, 10, 10, 10, 10, 10, 10,
    -10, -10, 5, 0, 0, 0, 0, 5, -10, -20, -10, -10, -10, -10, -10, -10, -20,
];

const QUEENS: [i32; 8 * 8] = [
    -20, -10, -10, -5, -5, -10, -10, -20, -10, 0, 0, 0, 0, 0, 0, -10, -10, 0, 5, 5, 5, 5, 0, -10,
    -5, 0, 5, 5, 5, 5, 0, -5, 0, 0, 5, 5, 5, 5, 0, -5, -10, 5, 5, 5, 5, 5, 0, -10, -10, 0, 5, 0, 0,
    0, 0, -10, -20, -10, -10, -5, -5, -10, -10, -20,
];

// const KING_START: [i32; 8*8] = [
//     -80, -70, -70, -70, -70, -70, -70, -80,
//     -60, -60, -60, -60, -60, -60, -60, -60,
//     -40, -50, -50, -60, -60, -50, -50, -40,
//     -30, -40, -40, -50, -50, -40, -40, -30,
//     -20, -30, -30, -40, -40, -30, -30, -20,
//     -10, -20, -20, -20, -20, -20, -20, -10,
//     20,  20,  -5,   -5,  -5,  -5,  20,  20,
//     20,  30,  10,    0,   0,  10,  30,  20
// ];

const KING_START: [i32; 8 * 8] = [
    -80, -70, -70, -70, -70, -70, -70, -80, -60, -60, -60, -60, -60, -60, -60, -60, -40, -50, -50,
    -60, -60, -50, -50, -40, -30, -40, -40, -50, -50, -40, -40, -30, -20, -30, -30, -40, -40, -30,
    -30, -20, -10, -20, -20, -20, -20, -20, -20, -10, 20, 20, -15, -15, -15, -5, 20, 20, 20, 30,
    10, -10, 0, 10, 30, 20,
];

const KING_END: [i32; 8 * 8] = [
    -20, -10, -10, -10, -10, -10, -10, -20, -5, 0, 5, 5, 5, 5, 0, -5, -10, -5, 20, 30, 30, 20, -5,
    -10, -15, -10, 35, 45, 45, 35, -10, -15, -20, -15, 30, 40, 40, 30, -15, -20, -25, -20, 20, 25,
    25, 20, -20, -25, -30, -25, 0, 0, 0, 0, -25, -30, -50, -30, -30, -30, -30, -30, -30, -50,
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;
    use crate::types::{Color, Piece, PieceType, Position};

    /// Test that PST index calculation is correct for both colors.
    /// White pieces should use (rank-1)*8 + (file-1)
    /// Black pieces should use mirrored rank: (8-rank)*8 + (file-1)
    #[test]
    fn test_pst_index_bounds() {
        // Test all 64 squares for both colors to ensure indices are in bounds
        for rank in 1..=8 {
            for file in 1..=8 {
                let white_piece = Piece {
                    color: Color::White,
                    piece_type: PieceType::Pawn,
                    position: Position { rank, file },
                };
                let black_piece = Piece {
                    color: Color::Black,
                    piece_type: PieceType::Pawn,
                    position: Position { rank, file },
                };

                // These should not panic - if index is out of bounds, we'll crash
                let white_value = get_piece_pst_value(&white_piece, false);
                let black_value = get_piece_pst_value(&black_piece, false);

                // Values should be in reasonable range
                assert!(
                    white_value >= -100 && white_value <= 100,
                    "White pawn PST value out of range at ({}, {}): {}",
                    rank,
                    file,
                    white_value
                );
                assert!(
                    black_value >= -100 && black_value <= 100,
                    "Black pawn PST value out of range at ({}, {}): {}",
                    rank,
                    file,
                    black_value
                );
            }
        }
    }

    /// Test that mirrored positions have symmetric PST values.
    /// A white pawn on e4 should have the same PST bonus as a black pawn on e5
    /// (mirrored across the center of the board).
    #[test]
    fn test_pst_symmetry_for_mirrored_positions() {
        // White pawn on rank 4, file 5 (e4) should have same value as
        // Black pawn on rank 5, file 5 (e5) - they're both "4 ranks from home"
        let white_pawn_e4 = Piece {
            color: Color::White,
            piece_type: PieceType::Pawn,
            position: Position { rank: 4, file: 5 },
        };
        let black_pawn_e5 = Piece {
            color: Color::Black,
            piece_type: PieceType::Pawn,
            position: Position { rank: 5, file: 5 },
        };

        let white_value = get_piece_pst_value(&white_pawn_e4, false);
        let black_value = get_piece_pst_value(&black_pawn_e5, false);

        assert_eq!(
            white_value, black_value,
            "Mirrored pawn positions should have equal PST values. \
             White e4 = {}, Black e5 = {}",
            white_value, black_value
        );
    }

    /// Test that a knight in the center has higher value than on the edge.
    /// This is a sanity check that PST tables are applied correctly.
    #[test]
    fn test_knight_center_vs_edge() {
        let knight_center = Piece {
            color: Color::White,
            piece_type: PieceType::Knight,
            position: Position { rank: 4, file: 4 }, // d4
        };
        let knight_edge = Piece {
            color: Color::White,
            piece_type: PieceType::Knight,
            position: Position { rank: 1, file: 1 }, // a1
        };

        let center_value = get_piece_pst_value(&knight_center, false);
        let edge_value = get_piece_pst_value(&knight_edge, false);

        assert!(
            center_value > edge_value,
            "Knight in center should be worth more than knight on edge. \
             Center (d4) = {}, Edge (a1) = {}",
            center_value,
            edge_value
        );
    }

    /// Test that the starting position evaluates to 0 (symmetric).
    /// Both sides have identical material and identical (mirrored) piece positions.
    #[test]
    fn test_starting_position_is_equal() {
        let board = Board::new();
        let material = Material::compute_material(&board);

        // Material should be equal
        assert_eq!(
            material.white_material, material.black_material,
            "Starting position material should be equal"
        );

        // PST adjustments should also be equal (symmetric position)
        assert_eq!(
            material.white_pst_mg,
            material.black_pst_mg,
            "Starting position PST adjustments (middlegame) should be equal. \
             White = {}, Black = {}",
            material.white_pst_mg,
            material.black_pst_mg
        );
        assert_eq!(
            material.white_pst_eg,
            material.black_pst_eg,
            "Starting position PST adjustments (endgame) should be equal. \
             White = {}, Black = {}",
            material.white_pst_eg,
            material.black_pst_eg
        );

        // Overall evaluation should be 0
        let eval = evaluate_board(&board);
        assert_eq!(eval, 0, "Starting position should evaluate to 0");
    }

    /// Test that mirrored positions with equal advantages evaluate equally.
    /// evaluate_board returns score from side-to-move perspective, so:
    /// - Position where White is up, White to move -> positive
    /// - Mirrored position where Black is up, Black to move -> also positive (same magnitude)
    #[test]
    fn test_mirrored_positions_evaluate_equally() {
        // Position with white having a pawn advantage
        // White pawn on e4 (good central control)
        let white_advantage_fen = "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1";
        let white_board = Board::from_fen(white_advantage_fen);

        // Mirror: Black pawn on e5 (same relative position from Black's perspective)
        let black_advantage_fen = "4k3/8/8/4p3/8/8/8/4K3 b - - 0 1";
        let black_board = Board::from_fen(black_advantage_fen);

        let white_eval = evaluate_board(&white_board);
        let black_eval = evaluate_board(&black_board);

        // Both should be positive and equal since each side is evaluating
        // their own advantage from their own perspective
        assert_eq!(
            white_eval, black_eval,
            "Mirrored advantage positions should evaluate equally from each side's perspective. \
             White advantage (White to move) = {}, Black advantage (Black to move) = {}",
            white_eval, black_eval
        );
    }

    /// Test that the raw material difference calculation is symmetric.
    #[test]
    fn test_material_difference_symmetric() {
        // Same position, different side to move - the unsigned material diff should be same
        let fen_white_move = "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1";
        let fen_black_move = "4k3/8/8/8/4P3/8/8/4K3 b - - 0 1";

        let board_white = Board::from_fen(fen_white_move);
        let board_black = Board::from_fen(fen_black_move);

        let mat_white = Material::compute_material(&board_white);
        let mat_black = Material::compute_material(&board_black);

        // The raw material difference should be the same regardless of side to move
        assert_eq!(
            mat_white.get_tapered_score(),
            mat_black.get_tapered_score(),
            "Raw material difference should not depend on side to move"
        );

        // But evaluate_board should flip sign based on side to move
        let eval_white = evaluate_board(&board_white);
        let eval_black = evaluate_board(&board_black);
        assert_eq!(
            eval_white, -eval_black,
            "Same position with different side to move should have opposite evaluations. \
             White to move = {}, Black to move = {}",
            eval_white, eval_black
        );
    }

    /// Test that all piece types have valid PST lookups for all squares.
    #[test]
    fn test_all_piece_types_pst_valid() {
        let piece_types = [
            PieceType::Pawn,
            PieceType::Knight,
            PieceType::Bishop,
            PieceType::Rook,
            PieceType::Queen,
            PieceType::King,
        ];

        for piece_type in piece_types {
            for rank in 1..=8 {
                for file in 1..=8 {
                    // Skip invalid pawn positions (rank 1 and 8)
                    if piece_type == PieceType::Pawn && (rank == 1 || rank == 8) {
                        continue;
                    }

                    let white_piece = Piece {
                        color: Color::White,
                        piece_type,
                        position: Position { rank, file },
                    };
                    let black_piece = Piece {
                        color: Color::Black,
                        piece_type,
                        position: Position { rank, file },
                    };

                    // Should not panic
                    let _white_val = get_piece_pst_value(&white_piece, false);
                    let _black_val = get_piece_pst_value(&black_piece, false);
                    let _white_val_end = get_piece_pst_value(&white_piece, true);
                    let _black_val_end = get_piece_pst_value(&black_piece, true);
                }
            }
        }
    }

    /// Test that advanced pawns have higher PST values than pawns on starting rank.
    #[test]
    fn test_advanced_pawns_worth_more() {
        let pawn_rank_2 = Piece {
            color: Color::White,
            piece_type: PieceType::Pawn,
            position: Position { rank: 2, file: 4 }, // d2
        };
        let pawn_rank_6 = Piece {
            color: Color::White,
            piece_type: PieceType::Pawn,
            position: Position { rank: 6, file: 4 }, // d6
        };

        let rank_2_value = get_piece_pst_value(&pawn_rank_2, false);
        let rank_6_value = get_piece_pst_value(&pawn_rank_6, false);

        assert!(
            rank_6_value > rank_2_value,
            "Advanced pawn should be worth more. Rank 2 = {}, Rank 6 = {}",
            rank_2_value,
            rank_6_value
        );
    }

    /// Test that incremental evaluation matches slow evaluation after a series of moves.
    #[test]
    fn test_incremental_eval_matches_slow_eval() {
        let mut board = Board::new();

        // Play some moves and verify incremental eval matches slow eval
        let moves = [
            ("e2", "e4"),  // 1. e4
            ("e7", "e5"),  // 1... e5
            ("g1", "f3"),  // 2. Nf3
            ("b8", "c6"),  // 2... Nc6
            ("f1", "b5"),  // 3. Bb5
            ("a7", "a6"),  // 3... a6
            ("b5", "a4"),  // 4. Ba4
            ("g8", "f6"),  // 4... Nf6
        ];

        for (from, to) in moves {
            let mv = crate::types::Move::from_algebraic(&board, from, to);
            let undo = board.make_move(&mv);

            let fast_eval = evaluate_board(&board);
            let slow_eval = debug_slow_evaluate_board(&board);

            assert_eq!(
                fast_eval, slow_eval,
                "Incremental eval {} != slow eval {} after move {} -> {}",
                fast_eval, slow_eval, from, to
            );

            // Also verify after unmake
            board.unmake_move(&undo);
            let fast_eval_after_unmake = evaluate_board(&board);
            let slow_eval_after_unmake = debug_slow_evaluate_board(&board);
            assert_eq!(
                fast_eval_after_unmake, slow_eval_after_unmake,
                "Incremental eval {} != slow eval {} after unmake of {} -> {}",
                fast_eval_after_unmake, slow_eval_after_unmake, from, to
            );

            // Re-make the move to continue the sequence
            board.make_move(&mv);
        }
    }

    /// Test incremental evaluation with captures
    #[test]
    fn test_incremental_eval_with_captures() {
        // Start from a position with captures available
        let mut board = Board::from_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3");

        // Nf3xe5 (capture)
        let moves = board.get_legal_moves(&Color::White).unwrap();
        let capture_move = moves.iter().find(|m| m.from.to_algebraic() == "f3" && m.to.to_algebraic() == "e5").unwrap();

        let undo = board.make_move(capture_move);
        let fast_eval = evaluate_board(&board);
        let slow_eval = debug_slow_evaluate_board(&board);

        assert_eq!(
            fast_eval, slow_eval,
            "Incremental eval {} != slow eval {} after capture",
            fast_eval, slow_eval
        );

        board.unmake_move(&undo);
        let fast_eval_unmake = evaluate_board(&board);
        let slow_eval_unmake = debug_slow_evaluate_board(&board);

        assert_eq!(
            fast_eval_unmake, slow_eval_unmake,
            "Incremental eval {} != slow eval {} after unmake capture",
            fast_eval_unmake, slow_eval_unmake
        );
    }

    /// Test incremental evaluation with castling
    #[test]
    fn test_incremental_eval_with_castling() {
        // Position where white can castle kingside
        let mut board = Board::from_fen("r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4");

        // O-O (kingside castle)
        let moves = board.get_legal_moves(&Color::White).unwrap();
        let castle_move = moves.iter().find(|m| m.move_flag == crate::types::MoveFlag::CastleKingside).unwrap();

        let undo = board.make_move(castle_move);
        let fast_eval = evaluate_board(&board);
        let slow_eval = debug_slow_evaluate_board(&board);

        assert_eq!(
            fast_eval, slow_eval,
            "Incremental eval {} != slow eval {} after castling",
            fast_eval, slow_eval
        );

        board.unmake_move(&undo);
        let fast_eval_unmake = evaluate_board(&board);
        let slow_eval_unmake = debug_slow_evaluate_board(&board);

        assert_eq!(
            fast_eval_unmake, slow_eval_unmake,
            "Incremental eval {} != slow eval {} after unmake castling",
            fast_eval_unmake, slow_eval_unmake
        );
    }
}
