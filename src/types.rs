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
