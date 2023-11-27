use std::cmp::{max, min};
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};


pub const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

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
    fn from_case(c: char) -> Color {
        if c.is_uppercase() {
            Color::White
        } else if c.is_lowercase() {
            Color::Black
        } else {
            panic!("Color char must be either upper or lowercase.")
        }
    }

    fn other_color(&self) -> Color {
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
    fn from_char(c: char) -> PieceType {
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
    fn is_sliding(&self) -> bool {
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

    fn to_char(&self) -> char {
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

const PIECES_CAN_PROMOTE_TO: [PieceType; 4] = [PieceType::Queen, PieceType::Rook, PieceType::Bishop, PieceType::Knight];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    fn ray_to(&self, other: &Self) -> Option<Vec<Position>> {
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

fn range(x1: u8, x2: u8) -> Vec<u8> {
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
    fn from_rank_file(piece: char, rank: u8, file: u8) -> Piece {
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
            PieceType::Pawn => {},
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


/// Starting rank for pawns of given color
fn pawn_starting_rank(color: &Color) -> u8 {
    match color {
        Color::White => 2,
        Color::Black => 7,
    }
}

/// Direction of pawn movement for given color
fn pawn_step_forward(rank: u8, color: &Color) -> u8 {
    match color {
        Color::White => rank + 1,
        Color::Black => rank -1,
    }
}


/// Potential move, either valid, in which case has a bool inicating
/// if it's a capture
enum PotentialMove {
    Valid(Option<Piece>),
    Invalid,
}

impl PotentialMove {
    fn continue_search_in_direction(&self) -> bool {
        match self {
            PotentialMove::Invalid => false,
            PotentialMove::Valid(Some(_)) => false,
            PotentialMove::Valid(None) => true,
        }
    }
}

/// This represents a pinned piece (to the king), as well as the valid moves it can have
/// whilst staying between the king and the attacking piece.
struct PinnedPiece {
    /// The pinned piece
    piece: Piece,
    /// The positions in the ray between the attacking piece (inclusive) and the king (exclusive)
    valid_responses: Vec<Position>,
}

impl PinnedPiece {
    fn new(piece: Piece, valid_responses: Vec<Position>) -> Self {
        Self {
            piece,
            valid_responses: valid_responses.into_iter().filter(|p| p != &piece.position).collect(),
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
    // TODO: remove piece from this, since we can get it from the board
    // in O(1) now
    // TODO: add a flag for promotion
    // pub promotion: Option<PieceType>,
    // perhaps also en passant flag or check flag
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
            Some(p) => format!(" capturing {} {} at {}", p.color.to_human(),
                p.piece_type.to_human(), p.position.to_algebraic()),
            None => "".to_string(),
        };
        let maybe_promotion_str = match self.move_flag {
            MoveFlag::Promotion(promoted_to_type) => format!(" promoting to {}", promoted_to_type.to_human()),
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
        format!("{}{}{}{}", self.piece.to_algebraic().to_uppercase(), self.from.to_algebraic(),
            match self.captured {
                Some(p) => format!("x{}", p.position.to_algebraic()),
                None => self.to.to_algebraic()
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
        Move {
            piece: board.piece_at_position(&from).unwrap().clone(),
            from,
            to,
            captured: board.piece_at_position(&to).cloned().to_owned(),
            move_flag: MoveFlag::Regular
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
        let north = (pos.rank + 1..=(8- pos.rank)).map(|r| Position { rank: r, file: pos.file }).collect();
        let south = (1..pos.rank).rev().map(|r| Position { rank: r, file: pos.file }).collect();
        let east = (pos.file + 1..=(8 - pos.file)).map(|f| Position { rank: pos.rank, file: f }).collect();
        let west = (1..pos.file).rev().map(|f| Position { rank: pos.rank, file: f }).collect();
        let northeast = (1..=min(8 - pos.rank, 8 - pos.file)).map(|i| Position { rank: pos.rank + i, file: pos.file + i }).collect();
        let northwest = (1..=min(8 - pos.rank, pos.file - 1)).map(|i| Position { rank: pos.rank + i, file: pos.file - i }).collect();
        let southeast = (1..=min(pos.rank - 1, 8 - pos.file)).map(|i| Position { rank: pos.rank - i, file: pos.file + i }).collect();
        let southwest = (1..=min(pos.rank - 1, pos.file - 1)).map(|i| Position { rank: pos.rank - i, file: pos.file - i }).collect();
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
                positions[(rank - 1) as usize][(file - 1) as usize] = RaysFromPosition::new_from_position(Position { rank, file });
            }
        }
        RaysForBoard { positions }
    }
}

// static RAYS_FOR_BOARD: RaysForBoard = RaysForBoard::new();



pub struct Board {
    pub pieces: Vec<Piece>,
    // who's move it is
    active_color: Color,
    castle_kingside_white: bool,
    castle_queenside_white: bool,
    castle_kingside_black: bool,
    castle_queenside_black: bool,
    en_passant_target: Option<Position>,
    // number of half moves since last capture or pawn advance
    halfmove_clock: u32,
    // number of full moves. Starts at 1, and gets incremented after every black move
    fullmove_clock: u32,
    board_to_piece: [[Option<Piece>; 8]; 8],
    white_king_position: Position,
    black_king_position: Position,
}

impl Board {
    pub fn from_fen(fen_string: &str) -> Board {
        if fen_string.chars().filter(|c| *c == ' ').count() != 5 {
            panic!("Fen string must have 6 fields, space delimited")
        };
        let parts: Vec<&str> = fen_string.splitn(6, " ").collect();

        assert!(parts.len() == 6);
        let mut board_to_piece: [[Option<Piece>; 8]; 8] = [[None; 8]; 8];

        let piece_data = parts[0];
        let mut pieces: Vec<Piece> = vec![];
        let mut rank = 8;
        let mut file = 1;
        for piece_char in piece_data.chars() {
            if piece_char.is_alphabetic() {
                let p = Piece::from_rank_file(piece_char, rank, file);
                pieces.push(p);
                board_to_piece[(rank - 1) as usize][(file - 1) as usize] = Some(p);
                file += 1;
            } else if piece_char.is_numeric() {
                file += piece_char as u8 - '0' as u8;
            } else if piece_char == '/' {
                rank -= 1;
                file = 1;
            } else {
                panic!("Unexpected char {piece_char} in position string.");
            }
        }

        let white_king_position = pieces
            .iter()
            .find(|p| p.color == Color::White && p.piece_type == PieceType::King)
            .unwrap()
            .position;

        let black_king_position = pieces
            .iter()
            .find(|p| p.color == Color::Black && p.piece_type == PieceType::King)
            .unwrap()
            .position;

        let active_color = Color::from_char(parts[1].chars().next().unwrap());

        let castling = parts[2];
        let castle_kingside_white = castling.contains("K");
        let castle_kingside_black = castling.contains("k");
        let castle_queenside_white = castling.contains("Q");
        let castle_queenside_black = castling.contains("q");

        let en_passant_target = if parts[3] == "-" {
            None
        } else {
            Some(Position::from_algebraic(parts[3].get(0..3).unwrap()))
        };

        let halfmove_clock: u32 = parts[4].parse().expect("Halfmove clock should be an u32");

        let fullmove_clock: u32 = parts[5].parse().expect("Fullmove clock should be a u32");

        Board {
            pieces,
            active_color,
            castle_kingside_white,
            castle_queenside_white,
            castle_kingside_black,
            castle_queenside_black,
            en_passant_target,
            halfmove_clock,
            fullmove_clock,
            board_to_piece,
            white_king_position,
            black_king_position,
        }
    }

    pub fn new() -> Board {
        Board::from_fen(STARTING_POSITION_FEN)
    }

    fn piece_at(&self, rank: u8, file: u8) -> Option<&Piece> {
        // TODO: speed this up by storing a board representation as well
        self.board_to_piece[(rank - 1) as usize][(file - 1) as usize].as_ref()
    }

    fn piece_at_position(&self, pos: &Position) -> Option<&Piece> {
        // self.piece_at(pos.rank, pos.file)
        self.board_to_piece[(pos.rank - 1) as usize][(pos.file - 1) as usize].as_ref()
    }

    fn piece_at_algebraic(&self, pos: &str) -> Option<&Piece> {
        let pos = Position::from_algebraic(pos);
        self.piece_at_position(&pos)
    }

    /// Get the color of the side to move
    pub fn get_active_color(&self) -> Color {
        self.active_color
    }

    fn check_for_insufficient_material(&self) -> Option<Status> {
        let white_pieces: Vec<_> = self.pieces.iter().filter(|p| p.color == Color::White).collect();
        let black_pieces: Vec<_> = self.pieces.iter().filter(|p| p.color == Color::Black).collect();

        if white_pieces.len() >= 3 || black_pieces.len() >= 3 {
            return None;
        }

        let white_bishop_count = white_pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::Bishop)
            .count();

        let black_bishop_count = black_pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::Bishop)
            .count();

        let white_knight_count = white_pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::Knight)
            .count();

        let black_knight_count = black_pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::Knight)
            .count();

        let black_insufficient  = black_pieces.len() == 1 || black_pieces.len() == 2 && {
            black_knight_count == 1 || black_bishop_count == 1
        };

        let white_insufficient  = white_pieces.len() == 1 || white_pieces.len() == 2 && {
            white_knight_count == 1 || white_bishop_count == 1
        };

        if black_insufficient && white_insufficient {
            Some(Status::InsufficientMaterial)
        } else {
            None
        }
    }

    pub fn execute_move(&self, selected_move: &Move) -> Board {
        if selected_move.captured.is_some_and(|p| p.piece_type == PieceType::King) {
            self.draw_to_terminal();
            panic!("King cannot be captured, something is amiss. Move was {}", selected_move.to_human());
        };

        let mut pieces = self.pieces.clone();
        let mut board_to_piece = self.board_to_piece.clone();

        let mut new_piece = Piece {
            position: selected_move.to,
            ..selected_move.piece
        };

        // Handle promotion
        if let MoveFlag::Promotion(promoted_to_type) = selected_move.move_flag {
            if promoted_to_type == PieceType::Pawn || promoted_to_type == PieceType::King {
                panic!("Cannot promote pawn to a king or a pawn");
            }
            new_piece = Piece {
                position: selected_move.to,
                piece_type: promoted_to_type,
                ..selected_move.piece
            };
        };

        match selected_move.move_flag {
            MoveFlag::CastleKingside | MoveFlag::CastleQueenside => {
                if selected_move.piece.piece_type != PieceType::King {
                    panic!("Only kings can castle");
                };
                let castle_rank = match selected_move.piece.color {
                    Color::White => 1,
                    Color::Black => 8,
                };
                if selected_move.piece.position.rank != castle_rank {
                    panic!("Kings can only castle from the first rank");
                };

                let old_rook_file = match selected_move.move_flag {
                    MoveFlag::CastleKingside => 8,
                    MoveFlag::CastleQueenside => 1,
                    _ => panic!("Only castling moves can have the castle flag"),
                };
                let new_rook_file = match selected_move.move_flag {
                    MoveFlag::CastleKingside => 6,
                    MoveFlag::CastleQueenside => 4,
                    _ => panic!("Only castling moves can have the castle flag"),
                };
                let new_king_file = match selected_move.move_flag {
                    MoveFlag::CastleKingside => 7,
                    MoveFlag::CastleQueenside => 3,
                    _ => panic!("Only castling moves can have the castle flag"),
                };
                let new_rook = Piece {
                    color: selected_move.piece.color,
                    piece_type: PieceType::Rook,
                    position: Position {
                        rank: castle_rank,
                        file: new_rook_file,
                    },
                };
                let new_king = Piece {
                    color: selected_move.piece.color,
                    piece_type: PieceType::King,
                    position: Position {
                        rank: castle_rank,
                        file: new_king_file,
                    },
                };
                let old_rook = Piece {
                    color: selected_move.piece.color,
                    piece_type: PieceType::Rook,
                    position: Position {
                        rank: castle_rank,
                        file: old_rook_file,
                    },
                };

                // remove old pieces
                pieces = pieces
                    .into_iter()
                    .filter(|p| p != &old_rook
                                && p != &selected_move.piece
                                && match selected_move.captured {
                                    Some(cap_piece) => p != &cap_piece,  // .. maybe remove the captured piece ..
                                    None => true,
                    })
                    .collect();

                // .. add the new pieces
                pieces.push(new_king);
                pieces.push(new_rook);

                // update the board map
                board_to_piece[(selected_move.from.rank - 1) as usize][(selected_move.from.file - 1) as usize] = None;
                board_to_piece[(old_rook.position.rank - 1) as usize][(old_rook.position.file - 1) as usize] = None;
                if let Some(captured) = selected_move.captured {
                    // In the case of en passant, the captures sqaure is not the target square of the move
                    board_to_piece[(captured.position.rank - 1) as usize][(captured.position.file - 1) as usize] = None;
                }
                board_to_piece[(selected_move.to.rank - 1) as usize][(selected_move.to.file - 1) as usize] = Some(new_king);
                board_to_piece[(new_rook.position.rank - 1) as usize][(new_rook.position.file - 1) as usize] = Some(new_rook);

            },
            _ => {
                // not a castle
                pieces = pieces
                .into_iter()
                .filter(|p| {
                    // remove the piece that moved ...
                    p != &selected_move.piece && match selected_move.captured {
                        Some(cap_piece) => p != &cap_piece,  // .. maybe remove the captured piece ..
                        None => true,
                    }
                })
                .collect();

                pieces.push(new_piece);

                // update the board map
                board_to_piece[(selected_move.from.rank - 1) as usize][(selected_move.from.file - 1) as usize] = None;
                if let Some(captured) = selected_move.captured {
                    // In the case of en passant, the captures sqaure is not the target square of the move
                    board_to_piece[(captured.position.rank - 1) as usize][(captured.position.file - 1) as usize] = None;
                }
                board_to_piece[(selected_move.to.rank - 1) as usize][(selected_move.to.file - 1) as usize] = Some(new_piece);
            }
        };

        // update castling rights
        let mut castle_kingside_white = true;
        let mut castle_queenside_white = true;
        let mut castle_kingside_black = true;
        let mut castle_queenside_black = true;

        if selected_move.piece.piece_type == PieceType::King{
            match selected_move.piece.color {
                Color::White => {
                    castle_kingside_white = false;
                    castle_queenside_white = false;
                },
                Color::Black => {
                    castle_kingside_black = false;
                    castle_queenside_black = false;
                }
            }
        }
        else if selected_move.piece.piece_type == PieceType::Rook {
            match selected_move.piece.color {
                Color::White => {
                    if selected_move.from.rank == 1 && selected_move.from.file == 1 {
                        castle_queenside_white = false;
                    }
                    if selected_move.from.rank == 1 && selected_move.from.file == 8 {
                        castle_kingside_white = false;
                    }
                },
                Color::Black => {
                    if selected_move.from.rank == 8 && selected_move.from.file == 1 {
                        castle_queenside_black = false;
                    }
                    if selected_move.from.rank == 8 && selected_move.from.file == 8 {
                        castle_kingside_black = false;
                    }
                }
            }
        }

        // maybe update king positions
        let mut white_king_position = self.white_king_position;
        let mut black_king_position = self.black_king_position;
        if selected_move.piece.piece_type == PieceType::King {
            match selected_move.piece.color {
                Color::White => {white_king_position = selected_move.to},
                Color::Black => {black_king_position = selected_move.to},
            };
        };

        // self.check_for_insufficient_material();

        let move_is_irriversible = selected_move.captured.is_some() || selected_move.piece.piece_type == PieceType::Pawn;
        // TODO: update the other states too
        Board {
            pieces,
            active_color: self.active_color.other_color(),
            halfmove_clock: if move_is_irriversible {0} else {self.halfmove_clock + 1},
            fullmove_clock: self.fullmove_clock + if self.active_color == Color::Black {1} else {0},
            castle_kingside_white: self.castle_kingside_white && castle_kingside_white,
            castle_queenside_white: self.castle_queenside_white && castle_queenside_white,
            castle_kingside_black: self.castle_kingside_black && castle_kingside_black,
            castle_queenside_black: self.castle_queenside_black && castle_queenside_black,
            en_passant_target: if let MoveFlag::DoublePawnPush(ep_target) = selected_move.move_flag {
                Some(ep_target)
            } else {
                None
            },
            board_to_piece,
            white_king_position,
            black_king_position,
            ..*self
        }
    }

    /// Get the vector of all legal moves for side `color`. This accounts for checks, pins, etc.
    ///
    /// stored state required
    ///      - king positions for both sides (remember to update if the king moves)
    ///      -
    ///
    ///
    /// 1. Check for checks.
    ///      - naive version:
    ///              - find all opponenet moves from the current position, filter for
    ///                those captureing the king (only valid moves, no need to compute the legal
    ///                moves)
    ///              - Filter out moves that cannot capture, like pawn pushes.
    ///              - we can also use this step to extend the search of sliding pieces
    ///                past the first occurance of one of our own pieces. If there is at most
    ///                one of our pieces between their sliding piece and our king, then this
    ///                piece of ours is pinned. Save the pinning piece and the pinned piece.
    ///              - we also keep a record of all the observed squares by the opponent
    ///      - potentially more efficient:
    ///              - keep all the possible moves the opponent had last turn, tracking pinned
    ///                pieces and checks in that evaluation
    ///              - we prune that list by removing the old position of the piece that did in
    ///                fact move, and remove any observed squares that are now blocked by the
    ///                new position of the piece that just moved
    ///              - use that to find the checks and observed squares for the king moves and
    ///                to determine the pins
    ///
    /// 2. If in check, we can narrow down the subset of pieces we can legally move
    ///      - if double check, king must move
    ///      - if not, either:
    ///              - king moves
    ///              - checking piece is captured
    ///              - own piece blocks the checking ray if its a sliding piece
    ///      - if not in check, any of our pieces can potentially have valid moves.
    ///
    ///
    /// 3. For each piece we can legally move, we compute the possible moves
    ///      - if the piece is not our king
    ///              - check if the piece is pinned (we have the list of pinning pieces from
    ///                step 1). If this is the case, we can only move the that piece along
    ///                the pinning piece's ray, up to and including capturing the pinning piece
    ///              - if it's not pinned, all valid moves are legal.
    ///      - if the piece is our king
    ///              - king cannot move into check, so we filter out any moves that would
    ///                land us on a square observed by our opponent.
    ///
    /// 4. If there are no valid moves, the game ends:
    ///      - if we're in check, it's checkmate
    ///      - if we're not, it's stalemate
    ///
    ///
    /// 5. Make a move using minimax with alpha beta pruning
    ///      - Check for no progress stalemate
    ///          - if it's a reversible move, inrement the halfmove_clock
    ///          - if not, reset the half move clock
    ///          - if the halfmove_clock is over 50, game ends in stalemate
    ///
    ///      - Check for threefold repetition
    ///          - if this is not a reversible move, clear the position cache and short circuit
    ///            the rest of this block
    ///          - otherwise, hash the current position
    ///          - compare the hash against the list of hashes of all previous positions since
    ///            last halfmove_clock half moves.
    ///          - if the same hash appears twice in that list, it's a draw by threefold
    ///            repetition
    ///          - if not, append the current hash and continue
    ///
    ///     - update caslting rights
    ///         - if the king moved, both casling options are dissallowed
    ///         - if either rooks has moved, then that side is dissallowed.
    pub fn get_legal_moves(&self, color: &Color) -> Result<Vec<Move>, Status> {
        // 1: get all valid opponent moves and compute pins
        // We set `observed_mode` here because we need to check if an oppoenent piece is defended
        // to know if we can capture it with out king. This also does not include the opponent king (
        // that is our king in this context) because or sliding pieces, squares behind our king also
        // constiture sqaures they could attack and therefore are invlaid moves for our king
        let opponent_potential_moves = self.get_all_pseudo_moves(&color.other_color(), true);

        // 2: Compute checks, this subsets the possible pieces to move
        let my_king = self.get_king(*color);
        let current_checks: Vec<&Move> = opponent_potential_moves
            .iter()
            .filter(|m| m.to == my_king.position)
            .collect();

        let is_in_check = current_checks.len() >= 1;

        let my_pieces: Vec<&Piece> = self.pieces.iter().filter(|p| p.color == *color).collect();
        let pieces_to_compute_moves_for = match current_checks.len() {
            0 => my_pieces,
            // but only onto squares that block or capture the attacking piece, or king move
            1 => my_pieces,
            // double check, only the king can move
            _ => vec![&my_king],
        };

        // 3: check all legal moves from the pieces we can legally move
        // first get all valid moves
        let mut my_possible_moves: Vec<_> = pieces_to_compute_moves_for
            .into_iter()
            .map(|p| self.get_pseudo_moves(&p, false))
            .flatten()
            // castling out of check is not allowed
            .filter(|m| !(is_in_check && (m.move_flag == MoveFlag::CastleKingside || m.move_flag == MoveFlag::CastleQueenside)))
            .collect();

        // if in single check, we have three options:
        // move the king, capture the checking piece, or block the checking ray if the checker is sliding
        // compute pins
        if current_checks.len() == 1 {
            let checking_piece = current_checks[0].piece;
            let checking_piece_position = current_checks[0].from;

            // filter out moves that don't block, capture, or move the king
            my_possible_moves = my_possible_moves
                .into_iter()
                .filter(|m| {
                    // if the move is a capture of the checking piece, it's valid
                    if m.captured.is_some_and(|p| p == checking_piece) {
                        return true;
                    }
                    // if the move is a king move, it's valid
                    if m.piece.piece_type == PieceType::King {
                        // we filter out king moves that land on a square observed by the opponent
                        // later
                        return true;
                    }
                    // if the move is a block, it's valid. We check if the checking piece is a sliding piece
                    // because if it's not, it cannot be blocked
                    if checking_piece.piece_type.is_sliding() {
                        // if it is, we check if the move target lands on the ray between us and the piece.
                        let checking_piece_ray = checking_piece_position.ray_to(&my_king.position);
                        return checking_piece_ray.as_ref().is_some_and(|ray| ray.contains(&m.to));
                    }
                    false
                })
                .collect();
        }

        let pins = self.get_pins(color);

        let mut castle_kingside_obstructed = false;
        let mut castle_queenside_obstructed = false;

        let castle_rank = if *color == Color::White {1} else {8};

        for Move { piece: _, from: _, to, captured: _, move_flag: _ } in opponent_potential_moves.iter() {
            if !castle_queenside_obstructed && to.rank == castle_rank && (to.file == 3 || to.file == 4) {
                castle_queenside_obstructed = true;
            }
            if !castle_kingside_obstructed && to.rank == castle_rank && (to.file == 7 || to.file == 6) {
                castle_kingside_obstructed = true;
            }
            if castle_kingside_obstructed && castle_queenside_obstructed {
                break;
            }
        }

        // 4: further filter moves
        my_possible_moves = my_possible_moves
            .into_iter()
            // 4.A Filter out moves that put the king in check
            .filter(|m| match m.piece.piece_type {
                // King cannot move into check
                PieceType::King => !opponent_potential_moves.iter().any(|om| om.to == m.to),
                _ => true,
            })
            // 4.B. Filter out invalid moves of a pinned piece
            .filter(|m| {
                let mut can_move_to = true;
                // here handle the pins
                // we iterate over all the pinnied pieces
                for PinnedPiece {
                    piece: pinned_piece,
                    valid_responses,
                } in pins.iter()
                {
                    // if the pinned piece is the one we're currently considering
                    if *pinned_piece == m.piece {
                        // check if the move is allowed by the pin
                        // TODO OPTIMISE: this is a nested loop, can we rewrite another way?
                        can_move_to = valid_responses.iter().find(|&&x| x == m.to).is_some();
                    }
                }
                can_move_to
            })
            // Filter out castles that go through observed squares
            .filter(|m| match m.move_flag {
                MoveFlag::CastleKingside => !castle_kingside_obstructed && match color {
                    Color::White => self.castle_kingside_white,
                    Color::Black => self.castle_kingside_black,
                },
                MoveFlag::CastleQueenside => !castle_queenside_obstructed && match color {
                    Color::White => self.castle_queenside_white,
                    Color::Black => self.castle_queenside_black,
                },
                _ => true,
            })
            .collect();


        // 5: check for checkmate or stalemate
        if my_possible_moves.len() == 0 {
            if current_checks.len() > 0 {
                return Err(Status::Checkmate(color.other_color()));
            } else {
                return Err(Status::Stalemate);
            }
        };
       Ok(my_possible_moves)
    }

    /// Get the current position of the king
    fn get_king_position(&self, color: Color) -> Position {
        match color {
            Color::White => return self.white_king_position,
            Color::Black => return self.black_king_position,
        }
    }

    /// get the king piece.
    /// TODO: deprecate this in favor of get_king_position
    fn get_king(&self, color: Color) -> Piece {
        let king_pos = self.get_king_position(color);
        self.piece_at(king_pos.rank, king_pos.file).expect("King should be here!").clone()
    }

    /// Get a vector of all pseudo valid moves for the side `color`.
    ///
    ///  We define a pseudo valid move to be a move that obeys the piece move directions,
    /// stays on the board, does not skip over pieces, etc, but does not check for
    /// checks, pins etc.
    pub fn get_all_pseudo_moves(
        &self,
        color: &Color,
        observed_mode: bool
    ) -> Vec<Move> {
        self.pieces
            .iter()
            .filter(|p| p.color == *color)
            .map(|p| self.get_pseudo_moves(p, observed_mode))
            .flatten()
            .collect()
    }

    /// Get a vector of pseudo valid moves for the piece `piece`.
    fn get_pseudo_moves(
        &self,
        piece: &Piece,
        observed_mode: bool,
    ) -> Vec<Move> {
        match piece.piece_type {
            PieceType::Pawn => {
                if observed_mode {
                    self.get_pawn_observed_squares(piece)
                } else {
                    let mut pushes =
                        self.get_pseudo_pawn_pushes(piece);
                    let mut captures =
                        self.get_pseudo_pawn_captures(piece);
                    pushes.append(&mut captures);
                    pushes
                }
            },
            PieceType::Rook => self.get_pseudo_rook_moves(piece, observed_mode),
            PieceType::Knight => self.get_pseudo_knight_moves(piece, observed_mode),
            PieceType::Bishop => self.get_pseudo_bishop_moves(piece, observed_mode),
            PieceType::Queen => {
                let mut bishop =
                    self.get_pseudo_bishop_moves(piece, observed_mode);
                let mut rook =
                    self.get_pseudo_rook_moves(piece, observed_mode);
                bishop.append(&mut rook);
                bishop
            }
            PieceType::King => self.get_pseudo_king_moves(piece, observed_mode),
        }
    }

    /// Squares the pawn *could* capture if there is a piece there
    fn get_pawn_observed_squares(&self, piece: &Piece) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;

        if (piece.color == Color::White && piece.position.rank >= 8)
            || (piece.color == Color::Black && piece.position.rank <= 1)
        {
            panic!("Pawn is on a promotion square, should not move from here.")
        }

        let next_rank = pawn_step_forward(pos.rank, &piece.color);
        let promotion_rank = if piece.color == Color::White {8} else {1};
        let ep_rank = if piece.color == Color::White {6} else {3};

        if pos.file >= 2 {
            let maybe_other_piece = self.piece_at(next_rank, pos.file - 1).map(|p| *p);

            if next_rank != promotion_rank {
                if next_rank == ep_rank
                        && maybe_other_piece.is_none()
                        && self.en_passant_target == Some(Position { rank: next_rank, file: pos.file - 1 }) {
                    // En passant capture
                    let captured_piece = self.piece_at(pos.rank, pos.file - 1).expect("Should have piece at the en passant target");
                    if captured_piece.piece_type != PieceType::Pawn {
                        panic!("En passant capture should be a pawn")
                    }

                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: next_rank,
                            file: pos.file - 1,
                        },
                        captured: Some(*captured_piece),
                        move_flag: MoveFlag::EnPassantCapture,
                    });
                } else {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: next_rank,
                            file: pos.file - 1,
                        },
                        captured: None,
                        move_flag: MoveFlag::Regular,
                    });
                }
            } else {
                // promotion
                for piece_type in PIECES_CAN_PROMOTE_TO {
                    moves.push(Move{
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: next_rank,
                            file: pos.file - 1,
                        },
                        captured: maybe_other_piece,
                        move_flag: MoveFlag::Promotion(piece_type)
                    })
                }
            }
        }
        if pos.file <= 7 {
            let maybe_other_piece = self.piece_at(next_rank, pos.file + 1).map(|p| *p);

            if next_rank != promotion_rank {
                if next_rank == ep_rank
                        && maybe_other_piece.is_none()
                        && self.en_passant_target == Some(Position { rank: next_rank, file: pos.file + 1 }) {
                    // En passant capture
                    println!("{:?}, {}, {:?}", piece.position, piece.color.to_human(), piece);
                    println!(
                        "En passant capture: ep_target: {:?}, ep_captured_pawn: {:?}:",
                        self.en_passant_target.unwrap(),
                        (pos.rank, pos.file + 1));

                    let captured_piece = self.piece_at(pos.rank, pos.file + 1).expect("Should have piece at the en passant target");
                    if captured_piece.piece_type != PieceType::Pawn {
                        panic!("En passant capture should be a pawn")
                    }
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: piece.position.rank,
                            file: pos.file + 1,
                        },
                        captured: Some(*captured_piece),
                        move_flag: MoveFlag::EnPassantCapture,
                    });
                } else {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: next_rank,
                            file: pos.file + 1,
                        },
                        captured: None,
                        move_flag: MoveFlag::Regular,
                    });
                }
            } else {
                // promotion
                for piece_type in PIECES_CAN_PROMOTE_TO {
                    moves.push(Move{
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: next_rank,
                            file: pos.file + 1,
                        },
                        captured: maybe_other_piece,
                        move_flag: MoveFlag::Promotion(piece_type)
                    })
                }
            }
        }
        moves
    }

    /// Get the valid captures for a pawn
    fn get_pseudo_pawn_captures(&self, piece: &Piece) -> Vec<Move> {
        self.get_pawn_observed_squares(piece).into_iter()
            .filter(|m| m.captured.map(|cp| cp.color != piece.color).unwrap_or(false))
            .collect()
    }

    /// get the valid pushes for a pawn
    fn get_pseudo_pawn_pushes(&self, piece: &Piece) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;

        if (piece.color == Color::White && piece.position.rank >= 8)
            || (piece.color == Color::Black && piece.position.rank <= 1)
        {
            panic!("Pawn is on a promotion square, should not move from here.")
        }

        // move one square forward, requires no piece there
        let one_step = pawn_step_forward(pos.rank, &piece.color);
        let promotion_rank = if piece.color == Color::White {8} else {1};

        if self.piece_at(one_step, pos.file).is_none() {
            if one_step != promotion_rank {
                moves.push(Move::new(
                    *piece,
                    Position {
                        rank: one_step,
                        file: pos.file,
                    },
                    None
                ))
            }
            else {
                // promotion
                for piece_type in PIECES_CAN_PROMOTE_TO {
                    moves.push(Move{
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: one_step,
                            file: pos.file,
                        },
                        captured: None,
                        move_flag: MoveFlag::Promotion(piece_type)
                    })
                }
            }

            // if starting position for white and both squares in front free
            let start_rank = pawn_starting_rank(&piece.color);
            let two_step = pawn_step_forward(pawn_step_forward(pos.rank, &piece.color), &piece.color);
            if pos.rank == start_rank && self.piece_at(two_step, pos.file).is_none() {
                moves.push(Move {
                    piece: *piece,
                    from: piece.position,
                    to: Position {
                        rank: two_step,
                        file: pos.file,
                    },
                    captured: None,
                    move_flag: MoveFlag::DoublePawnPush(Position { rank: one_step, file: pos.file }),
                });
            }
        };
        moves
    }

    fn get_pseudo_rook_moves(
        &self,
        piece: &Piece,
        observed_mode: bool
    ) -> Vec<Move> {
        // vector of possible move, is_capture
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;

        // up
        for rank in (pos.rank + 1)..=8 {
            let candidate = Position {
                rank,
                file: pos.file,
            };
            let potential_move =
                self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: candidate,
                        captured: maybe_other,
                        move_flag: MoveFlag::Regular,
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        }
        // down
        for rank in (1..pos.rank).rev() {
            let candidate = Position {
                rank,
                file: pos.file,
            };
            let potential_move =
                self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: candidate,
                        captured: maybe_other,
                        move_flag: MoveFlag::Regular,
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        }

        // right
        for file in (pos.file + 1)..=8 {
            let candidate = Position {
                rank: pos.rank,
                file,
            };
            let potential_move =
                self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: candidate,
                        captured: maybe_other,
                        move_flag: MoveFlag::Regular,
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        }
        // left
        for file in (1..pos.file).rev() {
            let candidate = Position {
                rank: pos.rank,
                file,
            };
            let potential_move =
                self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: candidate,
                        captured: maybe_other,
                        move_flag: MoveFlag::Regular,
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        }
        moves
    }

    fn get_pseudo_bishop_moves(
        &self,
        piece: &Piece,
        observed_mode: bool
    ) -> Vec<Move> {
        // vector of possible move, is_capture
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;

        let directions = [
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ];

        for direction in directions {
            let mut cur_rank = pos.rank as i8 + direction.0;
            let mut cur_file = pos.file as i8 + direction.1;

            while 1 <= cur_rank && cur_rank <= 8 && 1 <= cur_file && cur_file <= 8 {
                let candidate = Position {
                    rank: cur_rank as u8,
                    file: cur_file as u8,
                };
                let potential_move =
                    self.check_move_target(piece, &candidate, observed_mode);
                match potential_move {
                    PotentialMove::Invalid => break,
                    PotentialMove::Valid(maybe_other) => {
                        moves.push(Move {
                            piece: *piece,
                            from: piece.position,
                            to: candidate,
                            captured: maybe_other,
                            move_flag: MoveFlag::Regular,
                        });
                        if maybe_other.is_some() {
                            break;
                        }
                    }
                }
                cur_rank += direction.0;
                cur_file += direction.1;
            }
        }
        moves
    }

    // This funciton will check if the `piece` can move to `candidate_pos`. If so, it returns
    // a bool indicating if it's valid, if it's a capture, and a bool indicating if further search along
    // this axis is required.
    fn check_move_target(
        &self,
        piece: &Piece,
        candidate_pos: &Position,
        observed_mode: bool,
    ) -> PotentialMove {
        match self.piece_at_position(candidate_pos) {
            Some(other_piece) => {
                if piece.color == other_piece.color {
                    // this is not a valid move and no need to search further in this direction
                    if observed_mode {
                        // this is not a legal move, we return it when `observed_mode` is true
                        // to allow us to check if a friendly piece is protected.
                        return PotentialMove::Valid(Some(*other_piece));
                    } else {
                        return PotentialMove::Invalid;
                    }
                } else {
                    // this is a valid move, it's a capture! no need to look any further
                    if observed_mode && other_piece.piece_type == PieceType::King {
                        // except if we don't want to include the opponent king
                        return PotentialMove::Valid(None);
                    }
                    return PotentialMove::Valid(Some(*other_piece));
                }
            }
            // no piece here, so it's a valid move (not a capture)
            // and we can keep searching in this direction
            None => return PotentialMove::Valid(None),
        };
    }

    // TODO: add type annotations to make this only take knight pieces
    // TODO: same for the other functions above
    fn get_pseudo_knight_moves(&self, piece: &Piece, observed_mode: bool) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];

        for (rank_delta, file_delta) in
            std::iter::zip([-2, -2, -1, -1, 1, 1, 2, 2], [-1, 1, -2, 2, -2, 2, -1, 1])
        {
            let future_rank: i8 = piece.position.rank as i8 + rank_delta;
            let future_file: i8 = piece.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                let future_rank = future_rank as u8;
                let future_file = future_file as u8;
                let maybe_other_piece = self.piece_at(future_rank, future_file);
                if maybe_other_piece.is_some_and(|p| p.color == piece.color && !observed_mode) {
                    continue;
                }
                else {
                    moves.push(Move {
                        piece: *piece,
                        from: piece.position,
                        to: Position {
                            rank: future_rank,
                            file: future_file,
                        },
                        captured: maybe_other_piece.map(|p| *p),
                        move_flag: MoveFlag::Regular,
                    })
                }
            }
        }
        moves
    }

    fn get_pseudo_king_moves(&self, king: &Piece, observed_mode: bool) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];

        for (rank_delta, file_delta) in std::iter::zip(
            [-1, -1, -1, 0, 0, 0, 1, 1, 1],
            [-1, 0, 1, -1, 0, 1, -1, 0, 1],
        ) {
            let future_rank: i8 = king.position.rank as i8 + rank_delta;
            let future_file: i8 = king.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                let maybe_other_piece = self.piece_at(future_rank as u8, future_file as u8);
                match maybe_other_piece {
                    Some(other_piece) => match king.color == other_piece.color {
                        true => {
                            // same color, invalid move
                            if observed_mode {
                                // this is not a legal move, we return it when `observed_mode` is true
                                // to allow us to check if a friendly piece is protected.
                                moves.push(Move {
                                    piece: *king,
                                    from: king.position,
                                    to: Position {
                                        rank: future_rank as u8,
                                        file: future_file as u8,
                                    },
                                    captured: Some(*other_piece),
                                    move_flag: MoveFlag::Regular,
                                });
                            } else {
                                continue;
                            }
                        }
                        false => moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position {
                                rank: future_rank as u8,
                                file: future_file as u8,
                            },
                            captured: Some(*other_piece),
                            move_flag: MoveFlag::Regular,
                        }),
                    },
                    None => moves.push(Move {
                        piece: *king,
                        from: king.position,
                        to: Position {
                            rank: future_rank as u8,
                            file: future_file as u8,
                        },
                        captured: None,
                        move_flag: MoveFlag::Regular,
                    }),
                }
            }
        }
        // castles
        // the relevant squares being free from opponent observed squares is checked in the legal moves function
        match king.color {
            Color::White => {
                if self.castle_kingside_white
                    // piece at rook position is our rook, not captured or anything
                    && self.piece_at(1, 8).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::White)
                    // the way is free
                    && self.piece_at(1, 7).is_none()
                    && self.piece_at(1, 6).is_none() {
                        moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position {
                                rank: 1,
                                file: 7,
                            },
                            captured: None,
                            move_flag: MoveFlag::CastleKingside,
                        })
                }
                if self.castle_queenside_white
                    // piece at rook position is our rook, not captured or anything
                    && self.piece_at(1, 1).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::White)
                    // the way is free
                    && self.piece_at(1, 2).is_none()
                    && self.piece_at(1, 3).is_none()
                    && self.piece_at(1, 4).is_none() {
                        moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position {
                                rank: 1,
                                file: 3,
                            },
                            captured: None,
                            move_flag: MoveFlag::CastleQueenside,
                        })
                };
            },
            Color::Black => {
                if self.castle_kingside_white
                    // piece at rook position is our rook, not captured or anything
                    && self.piece_at(8, 8).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::Black)
                    // the way is free
                    && self.piece_at(8, 7).is_none()
                    && self.piece_at(8, 6).is_none() {
                        moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position {
                                rank: 8,
                                file: 7,
                            },
                            captured: None,
                            move_flag: MoveFlag::CastleKingside,
                        })
                }
                if self.castle_queenside_white
                    // piece at rook position is our rook, not captured or anything
                    && self.piece_at(8, 1).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::Black)
                    // the way is free
                    && self.piece_at(8, 2).is_none()
                    && self.piece_at(8, 3).is_none()
                    && self.piece_at(8, 4).is_none() {
                        moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position {
                                rank: 8,
                                file: 3,
                            },
                            captured: None,
                            move_flag: MoveFlag::CastleQueenside,
                        })
                };
            },
        }

        moves
    }

    /// Compute `color`'s pinned pieces, as well as the list of squares they can move to
    /// Note that this list of possible squares does not mean the piece can legally move there,
    /// (eg, it might not be a possible move for that piece), but simply that those are the pieces
    /// where the piece would maintain protection of the king, by blocking the ray or capturing
    /// the pinning piece.
    fn get_pins(&self, color: &Color) -> Vec<PinnedPiece> {
        let opponent_sliding_pieces: Vec<&Piece> = self
            .pieces
            .iter()
            .filter(|p| p.color == color.other_color() && p.piece_type.is_sliding())
            .collect();

        let king = self.get_king(*color);
        let mut pins: Vec<PinnedPiece> = vec![];

        for piece in opponent_sliding_pieces {
            match piece.piece_type {
                PieceType::Rook => {
                    if let Some(pin) = self.get_rook_pins(&king, piece) {
                        pins.push(pin);
                    }
                }
                PieceType::Bishop => {
                    if let Some(pin) = self.get_bishop_pins(&king, piece) {
                        pins.push(pin);
                    }
                }
                PieceType::Queen => {
                    if let Some(pin) = self.get_rook_pins(&king, piece) {
                        pins.push(pin);
                        continue; // both axes can't be pinning
                    }
                    if let Some(pin) = self.get_bishop_pins(&king, piece) {
                        pins.push(pin);
                    }
                }
                _ => (),
            }
        }
        pins
    }

    fn get_pin_for_ray(&self, king: &Piece, pinning: &Piece) -> Option<PinnedPiece> {
        if king.color == pinning.color {
            panic!("Cannot pin a piece of the same color as our king")
        }
        let ray = pinning.position.ray_to(&king.position);
        if let Some(ray) = ray {
            // get any pieces in the ray
            let pieces_in_ray: Vec<&Piece> = ray
                .iter()
                .map(|pos| self.piece_at_position(pos))
                .filter_map(|p| p)
                .collect();

            if pieces_in_ray.iter().skip(1).any(|p| p.color == pinning.color) {
                // there is a piece of our color in the way, this is not a pin
                // we skip the first element because that one is always going to be out own piece
                return None
            }
            if pieces_in_ray.iter().skip(1).len() == 1 {
                // if there is a *single* piece of the other color in the ray, it's not a pin for `pinning_piece`
                // again we skip the first piece because that's the one that is pinning and will always be there
                return Some(PinnedPiece::new(*pieces_in_ray[1], ray));
            }
            // More than a single piece of opposing color in the ray, so it's not a pin
        }
        None
    }

    fn get_rook_pins(&self, king: &Piece, rook: &Piece) -> Option<PinnedPiece> {
        if rook.position.rank == king.position.rank || rook.position.file == king.position.file {
            return self.get_pin_for_ray(king, rook);
        }
        None
    }

    fn get_bishop_pins(&self, king: &Piece, bishop: &Piece) -> Option<PinnedPiece> {
        if (bishop.position.rank as i8 - king.position.rank as i8).abs()
            == (bishop.position.file as i8 - king.position.file as i8).abs()
            // TODO: maybe this condition needs to be in absolute value?
        {
           return self.get_pin_for_ray(king, bishop);
        }
        None
    }

    fn display_piece_moves(&self, piece: &Piece) {
        let moves = self.get_pseudo_moves(piece, false);
        let destinations: Vec<&Position> = moves.iter().map(|move_cap| &move_cap.to).collect();
        let moves = self.get_pseudo_moves(piece, false);
        let captures: Vec<Option<Piece>> = moves.iter().map(|move_cap| move_cap.captured).collect();

        for r in (1..=8).rev() {
            for f in 1..9 {
                if piece.position.rank == r && piece.position.file == f {
                    print!("{} ", piece.to_symbol())
                } else if destinations.contains(&&Position { rank: r, file: f }) {
                    let move_idx = destinations
                        .iter()
                        .position(|m| **m == Position { rank: r, file: f })
                        .unwrap();
                    if captures[move_idx].is_some() {
                        print!("X ");
                    } else {
                        print!("O ");
                    }
                } else {
                    print!(". ")
                }
            }
            println!("");
        }
    }

    pub fn draw_board(&self) -> String {
        let mut string = String::new();

        for r in (1..9).rev() {
            for f in 1..9 {
                let p = self.piece_at(r, f);
                string = format!(
                    "{} {}",
                    string,
                    match p {
                        Some(pp) => pp.to_symbol(),
                        None => ".",
                    }
                );
            }
            string = format!("{}\n", string);
        }
        string
    }

    pub fn draw_to_terminal(&self) {
        println!("{}", self.draw_board());
    }
}



#[cfg(test)]
mod tests {
    use super::{Color, Move, PieceType, Position, Piece, MoveFlag, Board, Status, PinnedPiece, PIECES_CAN_PROMOTE_TO, STARTING_POSITION_FEN};
    #[test]
    fn test_color_from_char() {
        assert_eq!(Color::from_char('w'), Color::White);
        assert_eq!(Color::from_char('b'), Color::Black);
    }

    #[test]
    #[should_panic]
    fn test_color_from_char_fail() {
        Color::from_char('g');
    }

    #[test]
    fn test_color_from_case() {
        assert_eq!(Color::from_case('K'), Color::White);
        assert_eq!(Color::from_case('k'), Color::Black);
    }

    #[test]
    #[should_panic]
    fn test_color_from_case_fail() {
        Color::from_case('1');
    }

    #[test]
    fn test_other_color() {
        assert_eq!(Color::White, Color::Black.other_color());
        assert_eq!(Color::Black, Color::White.other_color());
    }

    #[test]
    fn test_piece_type_from_char() {
        assert_eq!(PieceType::from_char('p'), PieceType::Pawn);
        assert_eq!(PieceType::from_char('R'), PieceType::Rook);
        assert_eq!(PieceType::from_char('n'), PieceType::Knight);
        assert_eq!(PieceType::from_char('B'), PieceType::Bishop);
        assert_eq!(PieceType::from_char('Q'), PieceType::Queen);
        assert_eq!(PieceType::from_char('k'), PieceType::King);
    }

    #[test]
    #[should_panic]
    fn test_piece_type_from_char_fail() {
        PieceType::from_char('x');
    }

    #[test]
    fn test_is_sliding() {
        assert!(!PieceType::Pawn.is_sliding());
        assert!(PieceType::Rook.is_sliding());
        assert!(PieceType::Bishop.is_sliding());
        assert!(!PieceType::Knight.is_sliding());
        assert!(PieceType::Queen.is_sliding());
        assert!(!PieceType::King.is_sliding());
    }

    #[test]
    fn test_position_from_algebraic() {
        assert_eq!(
            Position::from_algebraic("a1"),
            Position { rank: 1, file: 1 }
        );
        assert_eq!(
            Position::from_algebraic("h4"),
            Position { rank: 4, file: 8 }
        )
    }

    #[test]
    fn test_ray_to() {
        let pos = Position { rank: 1, file: 1 };

        assert_eq!(pos.ray_to(&pos), Some(Vec::new()));

        assert_eq!(
            pos.ray_to(&Position { rank: 1, file: 3 }),
            Some(vec![
                Position { rank: 1, file: 1 },
                Position { rank: 1, file: 2 },
            ])
        );
        assert_eq!(
            Position { rank: 1, file: 3 }.ray_to(&pos),
            Some(vec![
                Position { rank: 1, file: 3 },
                Position { rank: 1, file: 2 },
            ])
        );

        assert_eq!(
            pos.ray_to(&Position { rank: 5, file: 1 }),
            Some(vec![
                Position { rank: 1, file: 1 },
                Position { rank: 2, file: 1 },
                Position { rank: 3, file: 1 },
                Position { rank: 4, file: 1 },
            ])
        );
        assert_eq!(
            Position { rank: 5, file: 1 }.ray_to(&pos),
            Some(vec![
                Position { rank: 5, file: 1 },
                Position { rank: 4, file: 1 },
                Position { rank: 3, file: 1 },
                Position { rank: 2, file: 1 },
            ])
        );

        // diagonal y=x
        assert_eq!(
            pos.ray_to(&Position { rank: 5, file: 5 }),
            Some(vec![
                Position { rank: 1, file: 1 },
                Position { rank: 2, file: 2 },
                Position { rank: 3, file: 3 },
                Position { rank: 4, file: 4 },
            ])
        );

        assert_eq!(
            Position { rank: 5, file: 5 }.ray_to(&pos),
            Some(vec![
                Position { rank: 5, file: 5 },
                Position { rank: 4, file: 4 },
                Position { rank: 3, file: 3 },
                Position { rank: 2, file: 2 },
            ])
        );

        // diagonal y=-x+b
        let pos2 = Position { rank: 8, file: 2 };
        assert_eq!(
            pos2.ray_to(&Position { rank: 4, file: 6 }),
            Some(vec![
                Position { rank: 8, file: 2 },
                Position { rank: 7, file: 3 },
                Position { rank: 6, file: 4 },
                Position { rank: 5, file: 5 },
            ])
        );

        assert_eq!(
            Position { rank: 4, file: 6 }.ray_to(&pos2),
            Some(vec![
                Position { rank: 4, file: 6 },
                Position { rank: 5, file: 5 },
                Position { rank: 6, file: 4 },
                Position { rank: 7, file: 3 },
            ])
        );
        assert!(pos.ray_to(&Position { rank: 3, file: 2 }).is_none());
    }

    #[test]
    fn test_piece_from_alegraic() {
        assert_eq!(
            Piece::from_algebraic('R', "a1"),
            Piece {
                color: Color::White,
                piece_type: PieceType::Rook,
                position: Position { rank: 1, file: 1 },
            }
        )
    }

    #[test]
    fn test_piece_from_rank_file() {
        assert_eq!(
            Piece::from_rank_file('k', 2, 3),
            Piece {
                color: Color::Black,
                piece_type: PieceType::King,
                position: Position { rank: 2, file: 3 },
            }
        );
    }

    #[test]
    fn build_starting_board() {
        let b = Board::from_fen(STARTING_POSITION_FEN);
        // in the starting position all castles are enabled
        // (but not technically possible due to obstruction)
        assert!(b.castle_queenside_white);
        assert!(b.castle_kingside_white);
        assert!(b.castle_queenside_black);
        assert!(b.castle_kingside_black);

        assert_eq!(b.pieces.len(), 8 * 4);

        // count pawns
        assert_eq!(
            b.pieces
                .iter()
                .filter(|p| p.piece_type == PieceType::Pawn)
                .count(),
            8 * 2
        );

        // count pairwise pieces
        for pt in [PieceType::Rook, PieceType::Bishop, PieceType::Knight] {
            assert_eq!(b.pieces.iter().filter(|p| p.piece_type == pt).count(), 4);
        }

        for pt in [PieceType::King, PieceType::Queen] {
            assert_eq!(b.pieces.iter().filter(|p| p.piece_type == pt).count(), 2);
        }

        // check some individual pieces

        let kings: Vec<&Piece> = b
            .pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::King)
            .collect();

        assert_eq!(kings.len(), 2);
        assert_eq!(kings[0].position, Position { rank: 8, file: 5 });
        assert_eq!(kings[1].position, Position { rank: 1, file: 5 });

        assert_eq!(b.halfmove_clock, 0);
        assert_eq!(b.fullmove_clock, 1);
    }

    // Tests for the pin  code
    #[test]
    fn test_rook_pin() {
        // . . . . . . . ♚
        // . . . ♜ . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♖ . . . .
        // . . . . . . . .
        // . . . ♔ . . . .
        // . . . . . . . .
        let b = Board::from_fen("7k/3r4/8/8/3R4/8/3K4/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 4);
        for pos in vec!["d3", "d5", "d6", "d7"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_rook_pin_horizontal_other_piece() {
        // ♚ . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♔ . ♘ . ♜
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        let b = Board::from_fen("k7/8/8/3K1N1r/8/8/8/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Knight);
        assert_eq!(pins[0].piece.position, Position { rank: 5, file: 6 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 3);
        for pos in vec!["e5", "h5", "g5"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_double_rook_pin() {
        // . . . ♜ . . . .
        // . . . ♜ . . . .
        // . . . . . . . .
        // . . . . . . . ♚
        // . . . ♕ . . . .
        // . . . . . . . .
        // . . . ♔ . . . .
        // . . . . . . . .
        let b = Board::from_fen("3r4/3r4/8/7k/3Q4/8/3K4/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1); // other rook is not pinning
        assert_eq!(pins[0].piece.piece_type, PieceType::Queen);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 4);
        for pos in vec!["d3", "d5", "d6", "d7"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_double_rook_pin_2() {
        // . . . ♜ . . . .
        // . . . . . . . ♚
        // . . . ♜ . . . .
        // . . . . . . . .
        // . . . ♖ . . . .
        // . . . . . . . .
        // . . . ♔ . . . .
        // . . . . . . . .
        let b = Board::from_fen("3r4/7k/3r4/8/3R4/8/3K4/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1); // other rook is not pinning
        assert_eq!(pins[0].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 3);
        for pos in vec!["d3", "d5", "d6"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_not_rook_pin() {
        // . . . . . . . ♚
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♖ . . . .
        // . . . . . . ♜ .
        // . . . ♔ . . . .
        // . . . . . . . .
        let b = Board::from_fen("7k/8/8/8/3R4/6r1/3K4/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 0);
    }

    #[test]
    fn test_not_rook_pin_but_check() {
        // . . . . . . . ♚
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♖ . . . .
        // . . . . . . . .
        // . . . ♔ . . ♜ .
        // . . . . . . . .
        let b = Board::from_fen("7k/8/8/8/3R4/8/3K2r1/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 0);
    }

    #[test]
    fn test_not_rook_pin_but_check_in_same_direction() {
        // . . . . . . . ♚
        // . . . . . . . .
        // . . . ♖ . . . .
        // . . . . . . . .
        // . . . ♔ . . . .
        // . . . . . . . .
        // . . . ♜ . . . .
        // . . . . . . . .
        let b = Board::from_fen("7k/8/3R4/8/3K4/8/3r4/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 0);
    }

    #[test]
    fn test_two_rook_pins() {
        // . . . ♜ . . . .
        // . . . . . . . ♚
        // . . . ♖ . . . .
        // . . . ♔ . . . .
        // . . . ♖ . . . .
        // . . . . . . . .
        // . . . ♜ . . . .
        // . . . . . . . .
        let b = Board::from_fen("3r4/7k/3R4/3K4/3R4/8/3r4/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 2);

        assert_eq!(pins[0].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[0].piece.position, Position { rank: 6, file: 4 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["d7", "d8"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }

        assert_eq!(pins[1].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[1].piece.position, Position { rank: 4, file: 4 });
        pins[1].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[1].valid_responses.len(), 2);
        for pos in vec!["d2", "d3"] {
            assert!(pins[1].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_bishop_pin_1() {
        // ♚ . . . . . . .
        // . . . . . . . .
        // . . . . . . ♝ .
        // . . . . . . . .
        // . . . . ♙ . . .
        // . . . ♔ . . . .
        // . . . . . . . .
        let b = Board::from_fen("k7/8/6b1/8/4P3/3K4/8/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 5 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["f5", "g6"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_bishop_pin_2() {
        // . . . . . . . ♚
        // ♝ . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♙ . . . .
        // . . . . ♔ . . .
        // . . . . . . . .
        // . . . . . . . .
        let b = Board::from_fen("7k/b7/8/8/3P4/4K3/8/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 3);
        for pos in vec!["a7", "b6", "c5"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_bishop_pin_3() {
        // . . . . . . . ♚
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . ♔ . . .
        // . . . ♙ . . . .
        // . . ♝ . . . . .
        // . . . . . . . .
        let b = Board::from_fen("7k/8/8/8/4K3/3P4/2b5/8 w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 3, file: 4 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 1);
        for pos in vec!["c2"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_bishop_pin_4() {
        // . . . . . . . ♚
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . ♔ . . .
        // . . . . . ♕ . .
        // . . . . . . . .
        // . . . . . . . ♝

        let b = Board::from_fen("7k/8/8/8/4K3/5Q2/8/7b w - - 0 1");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Queen);
        assert_eq!(pins[0].piece.position, Position { rank: 3, file: 6 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g2", "h1"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_pin_fail_1() {
        let b = Board::from_fen("rnbqkbnr/p1pppppp/8/1B6/8/4P3/PPPP1PPP/RNBQK1NR b KQkq - 0 2");
        b.draw_to_terminal();
        let pins = b.get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].piece, Piece::from_algebraic('p', "d7"));

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(!legal_moves.iter().any(|m| m.piece == Piece::from_algebraic('p', "d7")));
    }

    #[test]
    fn test_pin_fail_1_from_start() {
        let b = Board::new();

        // white moves pawn from e2 to e3
        // black moves pawn from b7 to b5
        // white moves bishop from f1 to b5 capturing black pawn at b5
        // black moves pawn from d7 to d6
        // white moves bishop from b5 to e8 capturing black king at e8
        let m = &Move::new(
            Piece::from_algebraic('P', "e2"),
            Position::from_algebraic("e3"),
            None,
        );
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        assert!(legal_moves.iter().any(|mm| mm == m));
        let b = b.execute_move(m);

        b.draw_to_terminal();

        let m = Move {
            piece: Piece::from_algebraic('p', "b7"),
            from: Position::from_algebraic("b7"),
            to: Position::from_algebraic("b5"),
            captured: None,
            move_flag: MoveFlag::DoublePawnPush(Position::from_algebraic("b6")),
        };
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        println!("{}: {}, {:?}", m.to_algebraic(), m.to_human(), m.move_flag);
        legal_moves.iter().for_each(|m| println!("{} {} {:?}", m.to_algebraic(), m.to_human(), m.move_flag));
        assert!(legal_moves.iter().any(|mm| *mm == m));
        let b = b.execute_move(&m);

        b.draw_to_terminal();

        let m = &Move::new(
            Piece::from_algebraic('B', "f1"),
            Position { rank: 5, file: 2 },
            Some(Piece::from_algebraic('p', "b5")),
        );
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(legal_moves.iter().any(|mm| mm == m));
        let b = b.execute_move(m);
        b.draw_to_terminal();

        assert!(b.piece_at_position(&Position::from_algebraic("b5")).is_some());
        assert_eq!(b.piece_at_position(&Position::from_algebraic("b5")).unwrap(), &Piece::from_algebraic('B', "b5"));


        let pins = b.get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].piece, Piece::from_algebraic('p', "d7"));



        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(!legal_moves.iter().any(|m| m.piece == Piece::from_algebraic('p', "d7")));
    }

    #[test]
    fn test_execute_move() {
        let b = Board::new();
        let p = Piece::from_algebraic('p', "e2");
        let m = Move::new(
            p,
            Position { rank: 4, file: 5 },
            None,
        );
        let new_b = b.execute_move(&m);
        new_b.draw_to_terminal();
        assert_eq!(*new_b.piece_at(4, 5).unwrap(), Piece::from_algebraic('p', "e4"));
        assert!(new_b.piece_at_position(&p.position).is_none());
    }

    #[test]
    fn execute_capture() {
        let b = Board::from_fen("4k3/8/8/8/8/2b5/3P4/3K4 w - - 0 1");
        b.draw_to_terminal();
        let p = Piece::from_algebraic('P', "d2");
        let m = Move::from_algebraic(&b, "d2", "c3");
        let captured = Piece::from_algebraic('b', "c3");
        assert_eq!(m.captured, Some(captured));
        assert!(b.pieces.contains(&p));
        assert!(b.pieces.contains(&captured));

        let b_after = b.execute_move(&m);
        assert!(b_after.pieces.contains(&Piece::from_algebraic('P', "c3")));
        assert!(!b_after.pieces.contains(&p));
        assert!(!b_after.pieces.contains(&captured));
    }

    #[test]
    fn execute_move_with_promotion() {
        let b = Board::from_fen("4K3/3P4/8/8/8/8/8/7k w - - 0 1");
        b.draw_to_terminal();

        // . . . . ♚ . . .
        // . . . ♟︎ . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . ♔

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();

        PIECES_CAN_PROMOTE_TO.iter().for_each(|pt| {
            let m = Move{
                piece: Piece::from_algebraic('P', "d7"),
                from: Position::from_algebraic("d7"),
                to: Position::from_algebraic("d8"),
                captured: None,
                move_flag: MoveFlag::Promotion(*pt),
            };
            assert!(legal_moves.contains(&m));
            let new_b = b.execute_move(&m);
            assert!(new_b.pieces.contains(&Piece::from_algebraic(pt.to_char(), "d8")));
        });
    }

    #[test]
    fn execute_ep_capture() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/8/4p3/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        b.draw_to_terminal();

        // ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎ ♟︎
        // . . . . . . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . . . . . .
        // ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
        // ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

        assert!(b.en_passant_target.is_none());
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        assert_eq!(legal_moves.len(), 19);

        let m = Move{
            piece: Piece::from_algebraic('P', "d2"),
            from: Position::from_algebraic("d2"),
            to: Position::from_algebraic("d4"),
            captured: None,
            move_flag: MoveFlag::DoublePawnPush(Position::from_algebraic("d3")),
        };
        // assert the move m is legal
        assert!(legal_moves.contains(&m));

        let b = b.execute_move(&m);
        b.draw_to_terminal();
        // ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎ ♟︎
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♙ ♟︎ . . .
        // . . . . . . . .
        // ♙ ♙ ♙ . ♙ ♙ ♙ ♙
        // ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

        assert_eq!(b.active_color, Color::Black);
        assert_eq!(b.en_passant_target, Some(Position::from_algebraic("d3")));

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{} {} {:?}", m.to_algebraic(), m.to_human(), m.move_flag));

        let ep_capture = Move{
            piece: Piece::from_algebraic('p', "e4"),
            from: Position::from_algebraic("e4"),
            to: Position::from_algebraic("d3"),
            captured: Some(Piece::from_algebraic('P', "d4")),
            move_flag: MoveFlag::EnPassantCapture,
        };
        assert!(legal_moves.contains(&ep_capture));

        let b = b.execute_move(&ep_capture);
        b.draw_to_terminal();
        // ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎ ♟︎
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♟︎ . . . .
        // ♙ ♙ ♙ . ♙ ♙ ♙ ♙
        // ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

        assert_eq!(b.active_color, Color::White);
        assert_eq!(b.en_passant_target, None);
        assert!(b.piece_at_position(&Position::from_algebraic("d4")).is_none());
        assert_eq!(b.piece_at_position(&Position::from_algebraic("d3")).unwrap(), &Piece::from_algebraic('p', "d3"));
    }


    #[test]
    fn execute_move_with_castle() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1");
        // ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
        // ♖ . . . ♔ . . ♖

        b.draw_to_terminal();

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 2);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));

        legal_castles.iter().for_each(|m| {
            let new_b = b.execute_move(m);
            new_b.draw_to_terminal();
            assert_eq!(new_b.active_color, Color::Black);
            assert_eq!(new_b.castle_kingside_white, false);
            assert_eq!(new_b.castle_queenside_white, false);
            assert_eq!(new_b.castle_kingside_black, true);
            assert_eq!(new_b.castle_queenside_black, true);

            match m.move_flag {
                MoveFlag::CastleKingside => {
                    assert_eq!(new_b.piece_at_position(&Position::from_algebraic("f1")).unwrap(), &Piece::from_algebraic('R', "f1"));
                    assert_eq!(new_b.piece_at_position(&Position::from_algebraic("g1")).unwrap(), &Piece::from_algebraic('K', "g1"));
                },
                MoveFlag::CastleQueenside => {
                    assert_eq!(new_b.piece_at_position(&Position::from_algebraic("d1")).unwrap(), &Piece::from_algebraic('R', "d1"));
                    assert_eq!(new_b.piece_at_position(&Position::from_algebraic("c1")).unwrap(), &Piece::from_algebraic('K', "c1"));
                },
                _ => panic!("unexpected move flag"),
            }
        });
    }

    #[test]
    fn cannot_castle_through_check() {
        let b = Board::from_fen("3rk3/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        b.draw_to_terminal();

        // . . . ♜ ♚ . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♖ . . . ♔ . . ♖

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(!legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));

        // same side, still not possible due to check
        let b = Board::from_fen("2r1k3/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        b.draw_to_terminal();
        // . . ♜ . ♚ . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♖ . . . ♔ . . ♖

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(!legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));

        // same side, but now possible because the rook does not cover the king path
        let b = Board::from_fen("1r2k3/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        b.draw_to_terminal();
        // . ♜ . . ♚ . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♖ . . . ♔ . . ♖

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 2);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));


        // other side, but now possible because the rook does not cover the king path
        let b = Board::from_fen("4kr2/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        b.draw_to_terminal();
        // . . . . ♚ ♜ . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♖ . . . ♔ . . ♖

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(!legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));

        // other side, still not possible due to check
        let b = Board::from_fen("4k1r1/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        b.draw_to_terminal();
        // . . . . ♚ . ♜ .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♖ . . . ♔ . . ♖

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(!legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));

        // other side, possible again
        let b = Board::from_fen("4k2r/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        b.draw_to_terminal();
        // . . . . ♚ . . ♜
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♖ . . . ♔ . . ♖

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        let legal_castles = legal_moves.iter().filter(|m| m.move_flag.is_castle()).collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 2);
        legal_castles.iter().for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles.iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));
    }

    #[test]
    fn cannot_castle_through_piece() {
        for file in [2, 3, 4] {
            let pre = file - 2;
            let post = 4 - file;
            let b = Board::from_fen(format!("4k3/8/8/8/8/8/8/R{}N{}K2R w KQ - 0 1", pre, post).as_str());
            b.draw_to_terminal();
            assert!(b.piece_at(1, file).is_some_and(|p| p.piece_type == PieceType::Knight));
            // assert that white can only castle kingside, not queenside
            assert!(b.get_legal_moves(&b.active_color).unwrap().iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
            assert!(!b.get_legal_moves(&b.active_color).unwrap().iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));

        }

        for file in [6, 7] {
            let pre = file - 6;
            let post = 7 - file;
            let b = Board::from_fen(format!("4k3/8/8/8/8/8/8/R3K{}N{}R w KQ - 0 1", pre, post).as_str());
            b.draw_to_terminal();
            assert!(b.piece_at(1, file).is_some_and(|p| p.piece_type == PieceType::Knight));
            // assert that white can only castle queenside
            assert!(!b.get_legal_moves(&b.active_color).unwrap().iter().any(|m| m.move_flag == MoveFlag::CastleKingside));
            assert!(b.get_legal_moves(&b.active_color).unwrap().iter().any(|m| m.move_flag == MoveFlag::CastleQueenside));
        }
    }


    #[test]
    fn count_legal_moves_from_start_position() {
        let b = Board::from_fen(STARTING_POSITION_FEN);
        let legal_moves = b.get_legal_moves(&b.active_color);
        assert_eq!(legal_moves.unwrap().len(), 20); // 8*2 pawn moves, 2*2 knight moves
    }

    #[test]
    fn test_only_one_legal_move() {
        // Here the queen is defended, the king has a single move
        // . . . . . . . ♔
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . ♖ . . . . .
        // . . ♕ . . . . .
        // . ♚ . . . . . .

        let fen = "7K/8/8/8/8/2R5/2Q5/1k6 b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();

        assert_eq!(legal_moves.len(), 1);

        let only_move = &legal_moves[0];
        assert_eq!(only_move.piece.piece_type, PieceType::King);
        assert_eq!(only_move.from, Position { rank: 1, file: 2 });
        assert_eq!(only_move.to, Position { rank: 1, file: 1 });
        assert!(only_move.captured.is_none());
    }

    #[test]
    fn test_one_legal_move_with_king() {
        // Single possible move for the black king
        // . . . . . . . ♔
        // . . ♖ . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . ♔ . . . . . .
        // . . . . . . . .
        // . ♚ . . . . . .
        let fen = "7K/2R5/8/8/8/1K6/8/1k6 b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();

        assert_eq!(legal_moves.len(), 1);

        let only_move = &legal_moves[0];
        assert_eq!(only_move.piece.piece_type, PieceType::King);
        assert_eq!(only_move.from, Position { rank: 1, file: 2 });
        assert_eq!(only_move.to, Position { rank: 1, file: 1 });
        assert!(only_move.captured.is_none());
    }

    #[test]
    fn test_checkmate_1() {
        // No moves: checkmate!
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . ♔ . . . . .
        // . ♕ . . . . . .
        // . ♚ . . . . . .
        let fen = "8/8/8/8/8/2K5/1Q6/1k6 b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);
        let legal_moves = b.get_legal_moves(&b.active_color);
        assert_eq!(legal_moves.unwrap_err(), Status::Checkmate(Color::White));
    }

    #[test]
    fn test_checkmate_2() {
        // No moves: checkmate! Note the the king cannot retreat back
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . ♜
        // . . . . . . . .
        // . . . . . ♚ . ♔
        // . . . . . . . . <-- note that this sqaure is not a valid move for the King!!
        let fen = "8/8/8/8/7r/8/5k1K/8 w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color);
        assert_eq!(legal_moves.unwrap_err(), Status::Checkmate(Color::Black));
    }


    #[test]
    fn test_block_check() {
        // only move is to block check
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . ♜
        // . ♖ . . . . . . <-- block the check here is only move
        // . . . . . ♚ . ♔
        // . . . . . . . .
        let fen = "8/8/8/8/7r/1R6/5k1K/8 w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));
        assert_eq!(legal_moves.len(), 1);
        assert_eq!(legal_moves[0].to, Position::from_algebraic("h3"));
    }


    #[test]
    fn test_block_check_or_capture_checker() {
        // only move is to block check
        // . . . . . . ♜ .
        // . . . . . . . .
        // . . . . . . . ♔
        // . . . . . . . . <-- block here with the Knight
        // . . . . . ♘ . .
        // . . . . . . . ♜ <-- or capture checking rook here (better)
        // . . . . . ♚ . .
        // . . . . . . . .
        let fen = "6r1/8/7K/8/5N2/7r/5k2/8 w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));
        assert_eq!(legal_moves.len(), 2);
        // TODO: make order deterministic in tests
        assert_eq!(legal_moves[0].to, Position::from_algebraic("h3"));
        assert_eq!(legal_moves[1].to, Position::from_algebraic("h5"));
    }

    #[test]
    fn test_block_check_with_pawn() {
        // ♜ . ♝ ♛ ♚ ♝ ♞ ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
        // ♞ . . . . ♟︎ . .  <- g6 only move
        // . . . . . . . ♕
        // . . . . . . . .
        // . . . . ♙ . . .
        // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
        // ♖ ♘ ♗ . ♔ ♗ ♘ ♖
        let fen = "r1bqkbnr/ppppp1pp/n4p2/7Q/8/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));
        assert_eq!(legal_moves.len(), 1);
        // TODO: make order deterministic in tests
        assert_eq!(legal_moves[0].to, Position::from_algebraic("g6"));
    }

    #[test]
    fn test_panics_in_search() {
        // . ♜ ♝ ♛ ♚ ♝ . ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎
        // ♞ . . . . . . .
        // . . . . . . . ♕
        // . . . . ♞ . . .
        // ♙ ♙ ♙ . ♙ . . .
        // . . . ♙ . ♙ ♙ ♙
        // ♖ ♘ ♗ . ♔ ♗ ♘ ♖
        let fen = "1rbqkb1r/pppppppp/n7/7Q/4n3/PPP1P3/3P1PPP/RNB1KBNR b KQk - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);

        let pins = b.get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 7, file: 6 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g6", "h5"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));

        // no legal moves for pawn on f7
        assert!(!legal_moves.iter().any(|m| m.piece == Piece::from_algebraic('P', "f7")));
    }

    #[test]
    fn test_panics_in_search_with_bishop() {
        // . ♜ ♝ ♛ ♚ ♝ . ♜
        // ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎ ♟︎
        // ♞ . . . . . . .
        // . . . . . . . ♗
        // . . . . ♞ . . .
        // ♙ ♙ ♙ . ♙ . . .
        // . . . ♙ . ♙ ♙ ♙
        // ♖ ♘ ♗ . ♔ ♗ ♘ ♖
        let fen = "1rbqkb1r/pppppppp/n7/7B/4n3/PPP1P3/3P1PPP/RNB1KBNR b KQk - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);

        let pins = b.get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 7, file: 6 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g6", "h5"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));

        // no legal moves for pawn on f7
        assert!(!legal_moves.iter().any(|m| m.piece == Piece::from_algebraic('P', "f7")));
    }

    #[test]
    fn test_panics_in_search_with_bishop_smaller() {
        // . . . . ♚ . . .
        // . . . . . ♟︎ . .
        // . . . . . . . .
        // . . . . . . . ♗
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . ♔ . . .
        let fen = "4k3/5p2/8/7B/8/8/8/4K3 b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);

        let pins = b.get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 7, file: 6 });
        pins[0].valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g6", "h5"] {
            assert!(pins[0].valid_responses.contains(&Position::from_algebraic(pos)));
        }

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));
        let moves_to = legal_moves.iter().map(|m| m.to).collect::<Vec<_>>();
        // Miving the f pawn as black is illegal because it would put the king in check
        assert!(!moves_to.contains(&Position::from_algebraic("f6")));
        assert!(!moves_to.contains(&Position::from_algebraic("f5")));
    }


    #[test]
    fn test_stalemate_1() {
        // Black is in stalemate, no legal moves
        // . . . . . . . ♔
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . ♕ . . . . .
        // ♚ . . . . . . .
        let fen = "7K/8/8/8/8/8/2Q5/k7 b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);
        let legal_moves = b.get_legal_moves(&b.active_color);
        assert_eq!(legal_moves.unwrap_err(), Status::Stalemate);
    }

    #[test]
    fn test_stalemate_with_pawn() {
        // White is in stalemate, no legal moves
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . ♚ .
        // . . . . . . . ♟︎
        // . . . . . . . ♔
        let fen = "8/8/8/8/8/6k1/7p/7K w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color);
        assert_eq!(legal_moves.unwrap_err(), Status::Stalemate);
    }

    #[test]
    fn test_checkmate_with_pawn() {
        // White is in checkmate, no legal moves
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . ♚ .
        // . . . . . . ♟︎ ♟︎
        // . . . . . . . ♔
        let fen = "8/8/8/8/8/6k1/6pp/7K w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color);
        assert_eq!(legal_moves.unwrap_err(), Status::Checkmate(Color::Black));
    }

    #[test]
    fn test_two_legal_moves() {
        // Here the queen is not defended, there are two moves: capture the queen
        // ore retreat with the king
        // . . . . . . . ♔
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . ♕ . . . . .
        // . ♚ . . . . . .
        let fen = "7K/8/8/8/8/8/2Q5/1k6 b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::Black);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();

        assert_eq!(legal_moves.len(), 2);

        let retreat = &legal_moves[0];
        assert_eq!(retreat.piece.piece_type, PieceType::King);
        assert_eq!(retreat.from, Position { rank: 1, file: 2 });
        assert_eq!(retreat.to, Position { rank: 1, file: 1 });
        assert!(retreat.captured.is_none());

        let capture = &legal_moves[1];
        assert_eq!(capture.piece.piece_type, PieceType::King);
        assert_eq!(capture.from, Position { rank: 1, file: 2 });
        assert_eq!(capture.to, Position { rank: 2, file: 3 });
        assert!(capture.captured.is_some());
        assert!(capture.captured.unwrap().piece_type == PieceType::Queen);
    }

    #[test]
    fn test_double_check_1() {
        // . . . . . . . ♚
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // ♜ . . . . . . .
        // . . . . . . . .
        // . . ♗ . . . . . <- bishop can capture either rook, but not both
        // ♔ . . ♜ . . . .          so the king must move
        let fen = "7k/8/8/8/r7/8/2B5/K2r4 w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));
        assert_eq!(legal_moves.len(), 1);
        assert_eq!(legal_moves[0].piece.piece_type, PieceType::King);
        assert_eq!(legal_moves[0].to, Position::from_algebraic("b2"));
    }

    #[test]
    fn test_double_check_2() {
        // Same double check sittuation, but white has a ton more pieces, and it changes nothing
        // ♕ . . . . . . .
        // . . . . . . ♚ .
        // . . . . . . . .
        // . . ♕ . . . . .
        // ♜ . . . . . . .
        // . . . . . ♕ . .
        // . . ♗ . . . . . <- bishop can capture either rook, but not both
        // ♔ . . ♜ . . . ♕          so the king must move
        let fen = "Q/6k1/2Q5/8/r7/5Q2/2B5/K2r3Q w - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        assert_eq!(b.active_color, Color::White);
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves.iter().for_each(|m| println!("{}", m.to_human()));
        assert_eq!(legal_moves.len(), 1);
        assert_eq!(legal_moves[0].piece.piece_type, PieceType::King);
        assert_eq!(legal_moves[0].to, Position::from_algebraic("b2"));
    }
}
