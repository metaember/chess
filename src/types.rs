use crate::board::Board;
use std::cmp::{max, min};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn from_char(c: char) -> Color {
        if c == 'w' {
            Color::White
        } else if c == 'b' {
            Color::Black
        } else {
            panic!("Color string must be either `b` or `w`.")
        }
    }
    pub fn from_case(c: char) -> Color {
        if c.is_uppercase() {
            Color::White
        } else if c.is_lowercase() {
            Color::Black
        } else {
            panic!("Color char must be either upper or lowercase.")
        }
    }

    pub fn other_color(&self) -> Color {
        if *self == Color::White {
            Color::Black
        } else {
            Color::White
        }
    }

    pub fn to_human(&self) -> &str {
        match self {
            Self::White => "white",
            Self::Black => "black",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl PieceType {
    pub fn from_char(c: char) -> PieceType {
        match c.to_lowercase().next().unwrap() {
            'p' => PieceType::Pawn,
            'r' => PieceType::Rook,
            'n' => PieceType::Knight,
            'b' => PieceType::Bishop,
            'q' => PieceType::Queen,
            'k' => PieceType::King,
            other => panic!("Unrecognized piece type {other}."),
        }
    }

    /// Is the piece a sliding piece (one which can move multiple square in a given direction)
    pub fn is_sliding(&self) -> bool {
        match *self {
            PieceType::Rook | PieceType::Bishop | PieceType::Queen => true,
            _ => false,
        }
    }

    pub fn to_human(&self) -> &str {
        match self {
            Self::Pawn => "pawn",
            Self::Rook => "rook",
            Self::Knight => "knight",
            Self::Bishop => "bishop",
            Self::Queen => "queen",
            Self::King => "king",
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            Self::Pawn => 'P',
            Self::Rook => 'R',
            Self::Knight => 'N',
            Self::Bishop => 'B',
            Self::Queen => 'Q',
            Self::King => 'K',
        }
    }
}

pub const PIECES_CAN_PROMOTE_TO: [PieceType; 4] = [
    PieceType::Queen,
    PieceType::Rook,
    PieceType::Bishop,
    PieceType::Knight,
];

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Position {
    pub rank: u8, // row, 1 is start
    pub file: u8, // col, 1 is start
}

impl Position {
    pub fn from_algebraic(s: &str) -> Position {
        if s.len() != 2 {
            panic!("Algebraic notation must be of length 2.")
        }

        let mut char_iter = s.chars();
        let file_char = char_iter.next().unwrap();
        let rank_char = char_iter.next().unwrap();

        let file = 1 + file_char as u8 - 'a' as u8;
        let rank = rank_char as u8 - '0' as u8;

        Position { rank, file }
    }

    pub fn to_algebraic(&self) -> String {
        format!(
            "{}{}",
            (self.file - 1 + 'a' as u8) as char,
            (self.rank + '0' as u8) as char
        )
    }

    /// Ray of all the positions between self (inclusive) and the other piece (exclusive)
    ///
    /// TODO: Should this be implemented as ray in direction (cardinal direction?)
    pub fn ray_to(&self, other: &Self) -> Option<Vec<Position>> {
        let mut result: Vec<Position> = vec![];
        if self == other {
            return Some(result);
        }
        if self.rank == other.rank {
            for file in range(self.file, other.file) {
                result.push(Self {
                    rank: self.rank,
                    file,
                });
            }
            return Some(result);
        }
        if self.file == other.file {
            for rank in range(self.rank, other.rank) {
                result.push(Self {
                    rank,
                    file: self.file,
                })
            }
            return Some(result);
        }
        if max(self.file, other.file) - min(self.file, other.file)
            == max(self.rank, other.rank) - min(self.rank, other.rank)
        {
            for (rank, file) in
                std::iter::zip(range(self.rank, other.rank), range(self.file, other.file))
            {
                result.push(Self { rank, file })
            }
            return Some(result);
        }
        None
    }
}

pub fn range(x1: u8, x2: u8) -> Vec<u8> {
    if x1 <= x2 {
        (x1..x2).collect::<Vec<u8>>()
    } else {
        (x2 + 1..x1 + 1).rev().collect::<Vec<u8>>()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Piece {
    pub color: Color,
    pub piece_type: PieceType,
    pub position: Position,
}

impl Piece {
    pub fn from_algebraic(piece: char, position: &str) -> Piece {
        Piece {
            color: Color::from_case(piece),
            piece_type: PieceType::from_char(piece),
            position: Position::from_algebraic(position),
        }
    }
    pub fn from_rank_file(piece: char, rank: u8, file: u8) -> Piece {
        Piece {
            color: Color::from_case(piece),
            piece_type: PieceType::from_char(piece),
            position: Position { rank, file },
        }
    }
    pub fn to_symbol(&self) -> &str {
        let is_white = self.color == Color::White;
        match self.piece_type {
            PieceType::Pawn => {
                if is_white {
                    "♙"
                } else {
                    "♟︎"
                }
            }
            PieceType::Rook => {
                if is_white {
                    "♖"
                } else {
                    "♜"
                }
            }
            PieceType::Knight => {
                if is_white {
                    "♘"
                } else {
                    "♞"
                }
            }
            PieceType::Bishop => {
                if is_white {
                    "♗"
                } else {
                    "♝"
                }
            }
            PieceType::Queen => {
                if is_white {
                    "♕"
                } else {
                    "♛"
                }
            }
            PieceType::King => {
                if is_white {
                    "♔"
                } else {
                    "♚"
                }
            }
        }
    }
    pub fn to_algebraic(&self) -> String {
        let mut res = String::new();
        match self.piece_type {
            PieceType::Pawn => {}
            PieceType::Rook => res.push('r'),
            PieceType::Knight => res.push('n'),
            PieceType::Bishop => res.push('b'),
            PieceType::Queen => res.push('q'),
            PieceType::King => res.push('k'),
        }
        if self.color == Color::White {
            res.to_uppercase()
        } else {
            res.to_lowercase()
        }
    }
    pub fn to_algebraic_pgn(&self) -> String {
        let mut res = String::new();
        match self.piece_type {
            PieceType::Pawn => res.push('p'),
            PieceType::Rook => res.push('r'),
            PieceType::Knight => res.push('n'),
            PieceType::Bishop => res.push('b'),
            PieceType::Queen => res.push('q'),
            PieceType::King => res.push('k'),
        }
        if self.color == Color::White {
            res.to_uppercase()
        } else {
            res.to_lowercase()
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Status {
    Checkmate(Color),
    Stalemate,
    ThreefoldRepetition,
    FiftyMoveRule,
    InsufficientMaterial,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveFlag {
    Regular,
    CastleKingside,
    CastleQueenside,
    /// Pawn promotion. Contains the piece promoted to
    Promotion(PieceType),
    /// Double pawn push at first pawn move. Contains the en passant square
    DoublePawnPush(Position),
    EnPassantCapture,
}

impl MoveFlag {
    pub fn is_castle(&self) -> bool {
        match self {
            MoveFlag::CastleKingside | MoveFlag::CastleQueenside => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub piece: Piece,
    pub from: Position,
    pub to: Position,
    pub captured: Option<Piece>,
    pub move_flag: MoveFlag,
    // TODO: remove piece from this, since we can get it from the board in O(1) now
}

impl Move {
    pub fn new(piece: Piece, to: Position, captured: Option<Piece>) -> Self {
        Self {
            piece,
            from: piece.position,
            to,
            captured,
            move_flag: MoveFlag::Regular,
        }
    }

    pub fn to_human(&self) -> String {
        let maybe_capture_str = match self.captured {
            Some(p) => format!(
                " capturing {} {} at {}",
                p.color.to_human(),
                p.piece_type.to_human(),
                p.position.to_algebraic()
            ),
            None => "".to_string(),
        };
        let maybe_promotion_str = match self.move_flag {
            MoveFlag::Promotion(promoted_to_type) => {
                format!(" promoting to {}", promoted_to_type.to_human())
            }
            _ => "".to_string(),
        };
        if let MoveFlag::CastleKingside = self.move_flag {
            return format!("{} castles kingside", self.piece.color.to_human());
        }
        if let MoveFlag::CastleQueenside = self.move_flag {
            return format!("{} castles queenside", self.piece.color.to_human());
        }
        format!(
            "{} moves {} from {} to {}{}{}",
            self.piece.color.to_human(),
            self.piece.piece_type.to_human(),
            self.from.to_algebraic(),
            self.to.to_algebraic(),
            maybe_capture_str,
            maybe_promotion_str,
        )
    }
    pub fn to_algebraic(&self) -> String {
        if let MoveFlag::CastleKingside = self.move_flag {
            return "O-O".to_string();
        }
        if let MoveFlag::CastleQueenside = self.move_flag {
            return "O-O-O".to_string();
        }
        format!(
            "{}{}{}{}",
            self.piece.to_algebraic().to_uppercase(),
            self.from.to_algebraic(),
            match self.captured {
                Some(p) => format!("x{}", p.position.to_algebraic()),
                None => self.to.to_algebraic(),
            },
            if let MoveFlag::Promotion(promotion_to) = self.move_flag {
                format!("={}", promotion_to.to_char())
            } else {
                "".to_string()
            }
        )
    }

    pub fn from_algebraic(board: &Board, from_str: &str, to_str: &str) -> Self {
        let from = Position::from_algebraic(from_str);
        let to = Position::from_algebraic(to_str);
        let piece = board.piece_at_position(&from).unwrap().clone();

        if piece.piece_type == PieceType::King && from.file == 5 && to.file == 7 {
            return Self {
                piece,
                from,
                to,
                captured: None,
                move_flag: MoveFlag::CastleKingside,
            };
        } else if piece.piece_type == PieceType::King && from.file == 5 && to.file == 3 {
            return Self {
                piece,
                from,
                to,
                captured: None,
                move_flag: MoveFlag::CastleQueenside,
            };
        } else if piece.piece_type == PieceType::Pawn
            && (from.rank as i8 - to.rank as i8).abs() == 2
        {
            return Self {
                piece,
                from,
                to,
                captured: None,
                move_flag: MoveFlag::DoublePawnPush(Position {
                    rank: match piece.color {
                        Color::White => 3,
                        Color::Black => 6,
                    },
                    file: from.file,
                }),
            };
        }
        Move {
            piece: board.piece_at_position(&from).unwrap().clone(),
            from,
            to,
            captured: board.piece_at_position(&to).cloned().to_owned(),
            move_flag: MoveFlag::Regular,
        }
    }
}

/// Struct that caches how far you can move in as given direction from a given square

pub struct RaysFromPosition {
    pub north: Vec<Position>,
    pub south: Vec<Position>,
    pub east: Vec<Position>,
    pub west: Vec<Position>,
    pub northeast: Vec<Position>,
    pub northwest: Vec<Position>,
    pub southeast: Vec<Position>,
    pub southwest: Vec<Position>,
}

impl RaysFromPosition {
    pub fn new_from_position(pos: Position) -> Self {
        let north = (pos.rank + 1..=8)
            .map(|r| Position {
                rank: r,
                file: pos.file,
            })
            .collect();
        let south = (1..pos.rank)
            .rev()
            .map(|r| Position {
                rank: r,
                file: pos.file,
            })
            .collect();
        let east = (pos.file + 1..=8)
            .map(|f| Position {
                rank: pos.rank,
                file: f,
            })
            .collect();
        let west = (1..pos.file)
            .rev()
            .map(|f| Position {
                rank: pos.rank,
                file: f,
            })
            .collect();
        let northeast = (1..=min(8 - pos.rank, 8 - pos.file))
            .map(|i| Position {
                rank: pos.rank + i,
                file: pos.file + i,
            })
            .collect();
        let northwest = (1..=min(8 - pos.rank, pos.file - 1))
            .map(|i| Position {
                rank: pos.rank + i,
                file: pos.file - i,
            })
            .collect();
        let southeast = (1..=min(pos.rank - 1, 8 - pos.file))
            .map(|i| Position {
                rank: pos.rank - i,
                file: pos.file + i,
            })
            .collect();
        let southwest = (1..=min(pos.rank - 1, pos.file - 1))
            .map(|i| Position {
                rank: pos.rank - i,
                file: pos.file - i,
            })
            .collect();
        Self {
            north,
            south,
            east,
            west,
            northeast,
            northwest,
            southeast,
            southwest,
        }
    }
}

pub struct RaysForBoard {
    pub positions: [[RaysFromPosition; 8]; 8],
}

impl RaysForBoard {
    pub fn new() -> Self {
        // to avoid annotating the whole struct as copy, we initialize form a const
        const RAY: RaysFromPosition = RaysFromPosition {
            north: vec![],
            south: vec![],
            east: vec![],
            west: vec![],
            northeast: vec![],
            northwest: vec![],
            southeast: vec![],
            southwest: vec![],
        };
        const RAY_RANK: [RaysFromPosition; 8] = [RAY; 8];
        let mut positions = [RAY_RANK; 8];

        // then we can fill in the values
        for rank in 1..=8 {
            for file in 1..=8 {
                positions[(rank - 1) as usize][(file - 1) as usize] =
                    RaysFromPosition::new_from_position(Position { rank, file });
            }
        }
        RaysForBoard { positions }
    }
}

// static RAYS_FOR_BOARD: RaysForBoard = RaysForBoard::new();

/// Information needed to undo a move. Stored during make_move, used in unmake_move.
#[derive(Debug, Clone)]
pub struct UndoInfo {
    /// The move that was made
    pub mv: Move,
    /// Previous castling rights
    pub castle_kingside_white: bool,
    pub castle_queenside_white: bool,
    pub castle_kingside_black: bool,
    pub castle_queenside_black: bool,
    /// Previous en passant target
    pub en_passant_target: Option<Position>,
    /// Previous halfmove clock
    pub halfmove_clock: u32,
    /// Original piece type (for undoing promotion)
    pub original_piece_type: PieceType,
    /// For castling: the rook's original position (to restore it during unmake)
    pub rook_old_position: Option<Position>,
    /// Previous Zobrist hash
    pub zobrist_hash: u64,
    /// Previous incremental evaluation state
    pub material: [i32; 2],
    pub pst_mg: [i32; 2],
    pub pst_eg: [i32; 2],
    pub phase: i32,
}

/// Information needed to undo a null move (pass to opponent without moving)
#[derive(Debug, Clone)]
pub struct NullMoveUndo {
    /// Previous en passant target
    pub en_passant_target: Option<Position>,
    /// Previous Zobrist hash
    pub zobrist_hash: u64,
}

// =============================================================================
// CompactMove - 4-byte move representation for high-performance search
// =============================================================================

/// Move type flags for CompactMove encoding
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MoveType {
    Quiet = 0,
    DoublePawnPush = 1,
    CastleKingside = 2,
    CastleQueenside = 3,
    Capture = 4,
    EnPassantCapture = 5,
    // 6, 7 reserved
    PromoQueen = 8,
    PromoRook = 9,
    PromoBishop = 10,
    PromoKnight = 11,
    PromoCaptureQueen = 12,
    PromoCaptureRook = 13,
    PromoCaptureBishop = 14,
    PromoCaptureKnight = 15,
}

impl MoveType {
    #[inline(always)]
    pub fn is_capture(self) -> bool {
        matches!(
            self,
            MoveType::Capture
                | MoveType::EnPassantCapture
                | MoveType::PromoCaptureQueen
                | MoveType::PromoCaptureRook
                | MoveType::PromoCaptureBishop
                | MoveType::PromoCaptureKnight
        )
    }

    #[inline(always)]
    pub fn is_promotion(self) -> bool {
        (self as u8) >= 8
    }

    #[inline(always)]
    pub fn promotion_piece_type(self) -> Option<PieceType> {
        match self {
            MoveType::PromoQueen | MoveType::PromoCaptureQueen => Some(PieceType::Queen),
            MoveType::PromoRook | MoveType::PromoCaptureRook => Some(PieceType::Rook),
            MoveType::PromoBishop | MoveType::PromoCaptureBishop => Some(PieceType::Bishop),
            MoveType::PromoKnight | MoveType::PromoCaptureKnight => Some(PieceType::Knight),
            _ => None,
        }
    }

    #[inline(always)]
    fn from_u8(val: u8) -> Self {
        match val {
            0 => MoveType::Quiet,
            1 => MoveType::DoublePawnPush,
            2 => MoveType::CastleKingside,
            3 => MoveType::CastleQueenside,
            4 => MoveType::Capture,
            5 => MoveType::EnPassantCapture,
            8 => MoveType::PromoQueen,
            9 => MoveType::PromoRook,
            10 => MoveType::PromoBishop,
            11 => MoveType::PromoKnight,
            12 => MoveType::PromoCaptureQueen,
            13 => MoveType::PromoCaptureRook,
            14 => MoveType::PromoCaptureBishop,
            15 => MoveType::PromoCaptureKnight,
            _ => MoveType::Quiet, // fallback
        }
    }
}

/// Compact 4-byte move representation for high-performance search.
///
/// Bit layout (32 bits total):
/// - bits 0-5:   from square (0-63)
/// - bits 6-11:  to square (0-63)
/// - bits 12-15: move type (MoveType enum)
/// - bits 16-18: moving piece type (0-5)
/// - bits 19-21: captured piece type (0-5, or 7=none)
/// - bits 22-31: unused (reserved)
///
/// This is designed to fit in a register and avoid heap allocations.
/// The search copies these by value (4 bytes) instead of passing references.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompactMove(u32);

impl CompactMove {
    /// A null/invalid move (used as sentinel)
    pub const NONE: Self = Self(0xFFFFFFFF);

    // Bit masks and shifts
    const FROM_MASK: u32 = 0x3F;         // bits 0-5
    const TO_SHIFT: u32 = 6;
    const TO_MASK: u32 = 0x3F << 6;      // bits 6-11
    const TYPE_SHIFT: u32 = 12;
    const TYPE_MASK: u32 = 0xF << 12;    // bits 12-15
    const PIECE_SHIFT: u32 = 16;
    const PIECE_MASK: u32 = 0x7 << 16;   // bits 16-18
    const CAPTURED_SHIFT: u32 = 19;
    const CAPTURED_MASK: u32 = 0x7 << 19; // bits 19-21
    const NO_CAPTURE: u32 = 7;

    /// Create a new compact move
    #[inline(always)]
    pub fn new(
        from_sq: u8,
        to_sq: u8,
        piece_type: PieceType,
        move_type: MoveType,
        captured: Option<PieceType>,
    ) -> Self {
        let captured_bits = match captured {
            Some(pt) => pt as u32,
            None => Self::NO_CAPTURE,
        };
        Self(
            (from_sq as u32 & Self::FROM_MASK)
                | ((to_sq as u32) << Self::TO_SHIFT)
                | ((move_type as u32) << Self::TYPE_SHIFT)
                | ((piece_type as u32) << Self::PIECE_SHIFT)
                | (captured_bits << Self::CAPTURED_SHIFT),
        )
    }

    /// Create a quiet move (no capture, no special flags)
    #[inline(always)]
    pub fn new_quiet(from_sq: u8, to_sq: u8, piece_type: PieceType) -> Self {
        Self::new(from_sq, to_sq, piece_type, MoveType::Quiet, None)
    }

    /// Create a capture move
    #[inline(always)]
    pub fn new_capture(
        from_sq: u8,
        to_sq: u8,
        piece_type: PieceType,
        captured: PieceType,
    ) -> Self {
        Self::new(from_sq, to_sq, piece_type, MoveType::Capture, Some(captured))
    }

    /// Get the source square (0-63)
    #[inline(always)]
    pub fn from_sq(self) -> u8 {
        (self.0 & Self::FROM_MASK) as u8
    }

    /// Get the destination square (0-63)
    #[inline(always)]
    pub fn to_sq(self) -> u8 {
        ((self.0 & Self::TO_MASK) >> Self::TO_SHIFT) as u8
    }

    /// Get the move type
    #[inline(always)]
    pub fn move_type(self) -> MoveType {
        MoveType::from_u8(((self.0 & Self::TYPE_MASK) >> Self::TYPE_SHIFT) as u8)
    }

    /// Get the moving piece type
    #[inline(always)]
    pub fn piece_type(self) -> PieceType {
        match ((self.0 & Self::PIECE_MASK) >> Self::PIECE_SHIFT) as u8 {
            0 => PieceType::Pawn,
            1 => PieceType::Rook,
            2 => PieceType::Knight,
            3 => PieceType::Bishop,
            4 => PieceType::Queen,
            _ => PieceType::King,
        }
    }

    /// Check if this is a capture
    #[inline(always)]
    pub fn is_capture(self) -> bool {
        self.move_type().is_capture()
    }

    /// Get the captured piece type (if any)
    #[inline(always)]
    pub fn captured_type(self) -> Option<PieceType> {
        let bits = ((self.0 & Self::CAPTURED_MASK) >> Self::CAPTURED_SHIFT) as u8;
        if bits >= Self::NO_CAPTURE as u8 {
            None
        } else {
            Some(match bits {
                0 => PieceType::Pawn,
                1 => PieceType::Rook,
                2 => PieceType::Knight,
                3 => PieceType::Bishop,
                4 => PieceType::Queen,
                _ => PieceType::King,
            })
        }
    }

    /// Check if this is a promotion
    #[inline(always)]
    pub fn is_promotion(self) -> bool {
        self.move_type().is_promotion()
    }

    /// Get the promotion piece type (if any)
    #[inline(always)]
    pub fn promotion_type(self) -> Option<PieceType> {
        self.move_type().promotion_piece_type()
    }

    /// Check if this is a castling move
    #[inline(always)]
    pub fn is_castle(self) -> bool {
        matches!(
            self.move_type(),
            MoveType::CastleKingside | MoveType::CastleQueenside
        )
    }

    /// Check if this is an en passant capture
    #[inline(always)]
    pub fn is_en_passant(self) -> bool {
        self.move_type() == MoveType::EnPassantCapture
    }

    /// Convert a Position (rank 1-8, file 1-8) to a square index (0-63)
    #[inline(always)]
    pub fn pos_to_sq(pos: &Position) -> u8 {
        ((pos.rank - 1) * 8 + (pos.file - 1)) as u8
    }

    /// Convert a square index (0-63) to a Position (rank 1-8, file 1-8)
    #[inline(always)]
    pub fn sq_to_pos(sq: u8) -> Position {
        Position {
            rank: (sq / 8) + 1,
            file: (sq % 8) + 1,
        }
    }

    /// Convert from the existing Move type (for compatibility)
    pub fn from_move(mv: &Move) -> Self {
        let from_sq = Self::pos_to_sq(&mv.from);
        let to_sq = Self::pos_to_sq(&mv.to);
        let piece_type = mv.piece.piece_type;
        let captured = mv.captured.map(|p| p.piece_type);

        let move_type = match mv.move_flag {
            MoveFlag::Regular => {
                if captured.is_some() {
                    MoveType::Capture
                } else {
                    MoveType::Quiet
                }
            }
            MoveFlag::CastleKingside => MoveType::CastleKingside,
            MoveFlag::CastleQueenside => MoveType::CastleQueenside,
            MoveFlag::DoublePawnPush(_) => MoveType::DoublePawnPush,
            MoveFlag::EnPassantCapture => MoveType::EnPassantCapture,
            MoveFlag::Promotion(promo) => {
                let is_capture = captured.is_some();
                match (promo, is_capture) {
                    (PieceType::Queen, false) => MoveType::PromoQueen,
                    (PieceType::Rook, false) => MoveType::PromoRook,
                    (PieceType::Bishop, false) => MoveType::PromoBishop,
                    (PieceType::Knight, false) => MoveType::PromoKnight,
                    (PieceType::Queen, true) => MoveType::PromoCaptureQueen,
                    (PieceType::Rook, true) => MoveType::PromoCaptureRook,
                    (PieceType::Bishop, true) => MoveType::PromoCaptureBishop,
                    (PieceType::Knight, true) => MoveType::PromoCaptureKnight,
                    _ => MoveType::PromoQueen, // fallback
                }
            }
        };

        Self::new(from_sq, to_sq, piece_type, move_type, captured)
    }

    /// Convert to the existing Move type (for UCI output, display, etc.)
    /// Requires the board to reconstruct full piece information
    pub fn to_move(self, board: &Board) -> Move {
        let from_pos = Self::sq_to_pos(self.from_sq());
        let to_pos = Self::sq_to_pos(self.to_sq());

        let piece = board
            .piece_at_position(&from_pos)
            .cloned()
            .unwrap_or(Piece {
                color: Color::White,
                piece_type: self.piece_type(),
                position: from_pos,
            });

        let captured = match self.captured_type() {
            Some(cap_type) => board.piece_at_position(&to_pos).cloned().or_else(|| {
                // For en passant, the captured piece is not at the destination
                if self.is_en_passant() {
                    let cap_pos = Position {
                        rank: from_pos.rank,
                        file: to_pos.file,
                    };
                    board.piece_at_position(&cap_pos).cloned()
                } else {
                    Some(Piece {
                        color: piece.color.other_color(),
                        piece_type: cap_type,
                        position: to_pos,
                    })
                }
            }),
            None => None,
        };

        let move_flag = match self.move_type() {
            MoveType::Quiet => MoveFlag::Regular,
            MoveType::DoublePawnPush => MoveFlag::DoublePawnPush(Position {
                rank: match piece.color {
                    Color::White => 3,
                    Color::Black => 6,
                },
                file: from_pos.file,
            }),
            MoveType::CastleKingside => MoveFlag::CastleKingside,
            MoveType::CastleQueenside => MoveFlag::CastleQueenside,
            MoveType::Capture => MoveFlag::Regular,
            MoveType::EnPassantCapture => MoveFlag::EnPassantCapture,
            MoveType::PromoQueen | MoveType::PromoCaptureQueen => {
                MoveFlag::Promotion(PieceType::Queen)
            }
            MoveType::PromoRook | MoveType::PromoCaptureRook => {
                MoveFlag::Promotion(PieceType::Rook)
            }
            MoveType::PromoBishop | MoveType::PromoCaptureBishop => {
                MoveFlag::Promotion(PieceType::Bishop)
            }
            MoveType::PromoKnight | MoveType::PromoCaptureKnight => {
                MoveFlag::Promotion(PieceType::Knight)
            }
        };

        Move {
            piece,
            from: from_pos,
            to: to_pos,
            captured,
            move_flag,
        }
    }

    /// Convert to UCI notation (e.g., "e2e4", "e7e8q")
    pub fn to_uci(self) -> String {
        let from = Self::sq_to_pos(self.from_sq());
        let to = Self::sq_to_pos(self.to_sq());
        let mut s = format!("{}{}", from.to_algebraic(), to.to_algebraic());
        if let Some(promo) = self.promotion_type() {
            s.push(match promo {
                PieceType::Queen => 'q',
                PieceType::Rook => 'r',
                PieceType::Bishop => 'b',
                PieceType::Knight => 'n',
                _ => 'q',
            });
        }
        s
    }
}

impl std::fmt::Debug for CompactMove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::NONE {
            write!(f, "CompactMove::NONE")
        } else {
            write!(
                f,
                "CompactMove({} {:?} {:?}->{})",
                self.to_uci(),
                self.piece_type(),
                self.from_sq(),
                self.to_sq()
            )
        }
    }
}
