use crate::bitboard::{position_to_bb, position_to_sq, ATTACK_TABLES, BitboardIter, bishop_attacks, rook_attacks, queen_attacks};
use crate::evaluate::{get_pst_value, Material, MAX_PHASE, PHASE_WEIGHTS};
use crate::movegen::{MoveGenerator, PinnedPiece};
use crate::types::*;
use crate::zobrist::ZOBRIST_KEYS;

pub const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

/// Convert PieceType to an index (0-5) for piece_bb array
#[inline(always)]
pub const fn piece_type_to_index(pt: PieceType) -> usize {
    match pt {
        PieceType::Pawn => 0,
        PieceType::Rook => 1,
        PieceType::Knight => 2,
        PieceType::Bishop => 3,
        PieceType::Queen => 4,
        PieceType::King => 5,
    }
}

#[derive(Clone)]
pub struct Board {
    // who's move it is
    active_color: Color,
    pub castle_kingside_white: bool,
    pub castle_queenside_white: bool,
    pub castle_kingside_black: bool,
    pub castle_queenside_black: bool,
    pub en_passant_target: Option<Position>,
    // number of half moves since last capture or pawn advance
    halfmove_clock: u32,
    // number of full moves. Starts at 1, and gets incremented after every black move
    fullmove_clock: u32,
    board_to_piece: [[Option<Piece>; 8]; 8],
    pub white_king_position: Position,
    pub black_king_position: Position,
    /// Zobrist hash of the position for transposition table lookups
    pub zobrist_hash: u64,

    // === Bitboard representation ===
    /// Piece bitboards: piece_bb[color as usize][piece_type as usize]
    /// Indexed as: [0=White, 1=Black][0=Pawn, 1=Rook, 2=Knight, 3=Bishop, 4=Queen, 5=King]
    piece_bb: [[u64; 6]; 2],
    /// All occupied squares
    occupied: u64,
    /// Attack maps: all squares attacked by each color (cached, recomputed on move)
    white_attack_map: u64,
    black_attack_map: u64,

    // === Incremental evaluation ===
    /// Material scores for [White, Black]
    material: [i32; 2],
    /// Middlegame PST scores for [White, Black]
    pst_mg: [i32; 2],
    /// Endgame PST scores for [White, Black]
    pst_eg: [i32; 2],
    /// Game phase (0 = endgame, MAX_PHASE = opening/middlegame)
    phase: i32,
}

impl Board {
    pub fn from_fen(fen_string: &str) -> Board {
        if fen_string.chars().filter(|c| *c == ' ').count() != 5 {
            panic!("Fen string must have 6 fields, space delimited")
        };
        let parts: Vec<&str> = fen_string.splitn(6, " ").collect();

        assert!(parts.len() == 6);
        let mut board_to_piece: [[Option<Piece>; 8]; 8] = [[None; 8]; 8];

        // Initialize piece bitboards and incremental evaluation
        let mut piece_bb = [[0u64; 6]; 2];
        let mut occupied = 0u64;
        let mut material = [0i32; 2];
        let mut pst_mg = [0i32; 2];
        let mut pst_eg = [0i32; 2];
        let mut phase = 0i32;
        let mut zobrist_hash: u64 = 0;
        let mut white_king_position = Position { rank: 1, file: 5 }; // Default, will be set
        let mut black_king_position = Position { rank: 8, file: 5 }; // Default, will be set

        let piece_data = parts[0];
        let mut rank = 8;
        let mut file = 1;
        for piece_char in piece_data.chars() {
            if piece_char.is_alphabetic() {
                let p = Piece::from_rank_file(piece_char, rank, file);
                board_to_piece[(rank - 1) as usize][(file - 1) as usize] = Some(p);

                // Update bitboards and evaluation
                let sq_bb = position_to_bb(&p.position);
                let color_idx = p.color as usize;
                let piece_idx = piece_type_to_index(p.piece_type);
                piece_bb[color_idx][piece_idx] |= sq_bb;
                occupied |= sq_bb;
                material[color_idx] += Material::piece_to_material(p.piece_type);
                pst_mg[color_idx] += get_pst_value(p.piece_type, p.color, p.position.rank, p.position.file, false);
                pst_eg[color_idx] += get_pst_value(p.piece_type, p.color, p.position.rank, p.position.file, true);
                phase += PHASE_WEIGHTS[piece_idx];
                zobrist_hash ^= ZOBRIST_KEYS.piece_key(p.color, p.piece_type, &p.position);

                // Track king positions
                if p.piece_type == PieceType::King {
                    match p.color {
                        Color::White => white_king_position = p.position,
                        Color::Black => black_king_position = p.position,
                    }
                }

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

        let active_color = Color::from_char(parts[1].chars().next().unwrap());

        let castling = parts[2];
        let castle_kingside_white = castling.contains("K");
        let castle_kingside_black = castling.contains("k");
        let castle_queenside_white = castling.contains("Q");
        let castle_queenside_black = castling.contains("q");

        let en_passant_target = if parts[3] == "-" {
            None
        } else {
            Some(Position::from_algebraic(parts[3].get(0..2).unwrap()))
        };

        let halfmove_clock: u32 = parts[4].parse().expect("Halfmove clock should be an u32");
        let fullmove_clock: u32 = parts[5].parse().expect("Fullmove clock should be a u32");

        // Hash side to move
        if active_color == Color::Black {
            zobrist_hash ^= ZOBRIST_KEYS.side_to_move;
        }

        // Hash castling rights
        if castle_kingside_white {
            zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_white;
        }
        if castle_queenside_white {
            zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_white;
        }
        if castle_kingside_black {
            zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_black;
        }
        if castle_queenside_black {
            zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_black;
        }

        // Hash en passant file
        if let Some(ep) = en_passant_target {
            zobrist_hash ^= ZOBRIST_KEYS.en_passant[(ep.file - 1) as usize];
        }

        // Compute initial attack maps
        let white_attack_map = Self::compute_attack_map_static(&piece_bb, occupied, Color::White, Some(black_king_position));
        let black_attack_map = Self::compute_attack_map_static(&piece_bb, occupied, Color::Black, Some(white_king_position));

        Board {
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
            zobrist_hash,
            piece_bb,
            occupied,
            white_attack_map,
            black_attack_map,
            material,
            pst_mg,
            pst_eg,
            phase,
        }
    }

    pub fn from_fen_no_moves(fen_string: &str) -> Board {
        Board::from_fen(format!("{} 0 1", fen_string).as_str())
    }

    pub fn new() -> Board {
        Board::from_fen(STARTING_POSITION_FEN)
    }

    /// Compute the attack map for a given color using piece bitboards
    /// This is a static method that doesn't require &self, used during construction
    fn compute_attack_map_static(
        piece_bb: &[[u64; 6]; 2],
        occupied: u64,
        color: Color,
        opponent_king_pos: Option<Position>,
    ) -> u64 {
        let color_idx = color as usize;
        let mut attacks = 0u64;

        // For xray attacks through opponent king, we remove the king from occupied
        let occupied_no_king = match opponent_king_pos {
            Some(pos) => occupied & !position_to_bb(&pos),
            None => occupied,
        };

        // Pawn attacks
        for sq in BitboardIter(piece_bb[color_idx][0]) {
            attacks |= ATTACK_TABLES.pawn[color_idx][sq as usize];
        }

        // Rook attacks (magic bitboards)
        for sq in BitboardIter(piece_bb[color_idx][1]) {
            attacks |= rook_attacks(sq, occupied_no_king);
        }

        // Knight attacks
        for sq in BitboardIter(piece_bb[color_idx][2]) {
            attacks |= ATTACK_TABLES.knight[sq as usize];
        }

        // Bishop attacks (magic bitboards)
        for sq in BitboardIter(piece_bb[color_idx][3]) {
            attacks |= bishop_attacks(sq, occupied_no_king);
        }

        // Queen attacks (magic bitboards)
        for sq in BitboardIter(piece_bb[color_idx][4]) {
            attacks |= queen_attacks(sq, occupied_no_king);
        }

        // King attacks
        for sq in BitboardIter(piece_bb[color_idx][5]) {
            attacks |= ATTACK_TABLES.king[sq as usize];
        }

        attacks
    }

    /// Recompute attack maps for both colors (call after position changes)
    fn recompute_attack_maps(&mut self) {
        self.white_attack_map = Self::compute_attack_map_static(
            &self.piece_bb,
            self.occupied,
            Color::White,
            Some(self.black_king_position),
        );
        self.black_attack_map = Self::compute_attack_map_static(
            &self.piece_bb,
            self.occupied,
            Color::Black,
            Some(self.white_king_position),
        );
    }

    /// Get the attack map for a given color
    #[inline]
    pub fn get_attack_map(&self, color: Color) -> u64 {
        match color {
            Color::White => self.white_attack_map,
            Color::Black => self.black_attack_map,
        }
    }

    /// Get the occupied squares bitboard
    #[inline]
    pub fn get_occupied(&self) -> u64 {
        self.occupied
    }

    /// Get piece bitboard for a specific color and piece type
    #[inline]
    pub fn get_piece_bb(&self, color: Color, piece_type: PieceType) -> u64 {
        self.piece_bb[color as usize][piece_type_to_index(piece_type)]
    }

    /// Get all pieces bitboard for a specific color
    #[inline]
    pub fn get_pieces_bb(&self, color: Color) -> u64 {
        let c = color as usize;
        self.piece_bb[c][0] | self.piece_bb[c][1] | self.piece_bb[c][2] |
        self.piece_bb[c][3] | self.piece_bb[c][4] | self.piece_bb[c][5]
    }

    // === Incremental evaluation getters ===

    /// Get material score for a color
    #[inline]
    pub fn get_material(&self, color: Color) -> i32 {
        self.material[color as usize]
    }

    /// Get middlegame PST score for a color
    #[inline]
    pub fn get_pst_mg(&self, color: Color) -> i32 {
        self.pst_mg[color as usize]
    }

    /// Get endgame PST score for a color
    #[inline]
    pub fn get_pst_eg(&self, color: Color) -> i32 {
        self.pst_eg[color as usize]
    }

    /// Get game phase (0 = endgame, MAX_PHASE = opening/middlegame)
    #[inline]
    pub fn get_phase(&self) -> i32 {
        self.phase
    }

    /// Get tapered evaluation score (positive = white advantage)
    #[inline]
    pub fn get_tapered_score(&self) -> i32 {
        let mg_score = self.material[0] + self.pst_mg[0] - self.material[1] - self.pst_mg[1];
        let eg_score = self.material[0] + self.pst_eg[0] - self.material[1] - self.pst_eg[1];
        let phase = self.phase.clamp(0, MAX_PHASE);
        (mg_score * phase + eg_score * (MAX_PHASE - phase)) / MAX_PHASE
    }

    /// Iterate over all pieces on the board using bitboards.
    /// This is more efficient than maintaining a separate Vec<Piece>.
    pub fn iter_pieces(&self) -> impl Iterator<Item = Piece> + '_ {
        const PIECE_TYPES: [PieceType; 6] = [
            PieceType::Pawn,
            PieceType::Rook,
            PieceType::Knight,
            PieceType::Bishop,
            PieceType::Queen,
            PieceType::King,
        ];
        const COLORS: [Color; 2] = [Color::White, Color::Black];

        COLORS.into_iter().flat_map(move |color| {
            PIECE_TYPES.into_iter().flat_map(move |piece_type| {
                let bb = self.piece_bb[color as usize][piece_type_to_index(piece_type)];
                BitboardIter(bb).map(move |sq| {
                    let rank = (sq / 8) as u8 + 1;
                    let file = (sq % 8) as u8 + 1;
                    Piece {
                        color,
                        piece_type,
                        position: Position { rank, file },
                    }
                })
            })
        })
    }

    /// Count total number of pieces on the board
    #[inline]
    pub fn piece_count(&self) -> u32 {
        self.occupied.count_ones()
    }

    /// Count pieces of a specific color
    #[inline]
    pub fn piece_count_for_color(&self, color: Color) -> u32 {
        self.get_pieces_bb(color).count_ones()
    }

    pub fn to_fen(&self) -> String {
        let mut fen = "".to_string();
        for rank in 0..8 {
            let board_rank = 7 - rank;
            let mut empty_squares = 0;
            for file in 0..8 {
                if let Some(piece) = self.board_to_piece[board_rank][file] {
                    if empty_squares > 0 {
                        fen.push_str(&empty_squares.to_string());
                        empty_squares = 0;
                    };
                    fen.push_str(&piece.to_algebraic_pgn());
                } else {
                    empty_squares += 1;
                }
            }
            if empty_squares > 0 {
                fen.push_str(&empty_squares.to_string());
            }
            if rank < 7 {
                fen.push('/');
            }
        }

        // active color
        fen.push_str(" ");
        let active_color = self.active_color.to_human().chars().next().unwrap();
        fen.push(active_color);

        // castling
        fen.push(' ');
        if self.castle_kingside_white {
            fen.push('K');
        }
        if self.castle_queenside_white {
            fen.push('Q');
        }
        if self.castle_kingside_black {
            fen.push('k');
        }
        if self.castle_queenside_black {
            fen.push('q');
        }
        if !self.castle_kingside_white
            && !self.castle_queenside_white
            && !self.castle_kingside_black
            && !self.castle_queenside_black
        {
            fen.push('-');
        }

        // en passant target
        fen.push(' ');
        if self.en_passant_target.is_none() {
            fen.push_str("-");
        } else {
            fen.push_str(&self.en_passant_target.unwrap().to_algebraic());
        }

        // halfmove clock
        fen.push(' ');
        fen.push_str(&self.halfmove_clock.to_string());

        // fullmove clock
        fen.push(' ');
        fen.push_str(&self.fullmove_clock.to_string());

        fen
    }

    pub fn to_fen_no_moves(&self) -> String {
        self.to_fen()
            .rsplitn(3, " ")
            .skip(2)
            .next()
            .unwrap()
            .to_string()
    }

    pub fn piece_at(&self, rank: u8, file: u8) -> Option<&Piece> {
        // TODO: speed this up by storing a board representation as well
        self.board_to_piece[(rank - 1) as usize][(file - 1) as usize].as_ref()
    }

    pub fn piece_at_position(&self, pos: &Position) -> Option<&Piece> {
        // self.piece_at(pos.rank, pos.file)
        self.board_to_piece[(pos.rank - 1) as usize][(pos.file - 1) as usize].as_ref()
    }

    pub fn piece_at_algebraic(&self, pos: &str) -> Option<&Piece> {
        let pos = Position::from_algebraic(pos);
        self.piece_at_position(&pos)
    }

    /// Get the color of the side to move
    pub fn get_active_color(&self) -> Color {
        self.active_color
    }

    /// Compute Polyglot-compatible zobrist hash for opening book lookups.
    /// Uses the standard Polyglot random numbers from polyglot_keys.rs.
    pub fn polyglot_hash(&self) -> u64 {
        use crate::polyglot_keys::POLYGLOT_RANDOM64;
        let mut hash = 0u64;

        // Polyglot kind_of_piece: BlackPawn=0, WhitePawn=1, BlackKnight=2, WhiteKnight=3,
        // BlackBishop=4, WhiteBishop=5, BlackRook=6, WhiteRook=7,
        // BlackQueen=8, WhiteQueen=9, BlackKing=10, WhiteKing=11
        // Index = kind_of_piece * 64 + row * 8 + file
        fn polyglot_kind(pt: PieceType, color: Color) -> usize {
            let base = match pt {
                PieceType::Pawn => 0,
                PieceType::Knight => 2,
                PieceType::Bishop => 4,
                PieceType::Rook => 6,
                PieceType::Queen => 8,
                PieceType::King => 10,
            };
            base + if color == Color::White { 1 } else { 0 }
        }

        for rank in 1..=8u8 {
            for file in 1..=8u8 {
                if let Some(piece) = self.piece_at(rank, file) {
                    let kind = polyglot_kind(piece.piece_type, piece.color);
                    let sq = ((rank - 1) * 8 + (file - 1)) as usize;
                    hash ^= POLYGLOT_RANDOM64[kind * 64 + sq];
                }
            }
        }

        if self.castle_kingside_white  { hash ^= POLYGLOT_RANDOM64[768]; }
        if self.castle_queenside_white { hash ^= POLYGLOT_RANDOM64[769]; }
        if self.castle_kingside_black  { hash ^= POLYGLOT_RANDOM64[770]; }
        if self.castle_queenside_black { hash ^= POLYGLOT_RANDOM64[771]; }

        // Polyglot only includes en passant if there's actually an enemy pawn
        // that could capture (unlike FEN which always sets ep after double push).
        if let Some(ep) = self.en_passant_target {
            let ep_file = ep.file;
            let (pawn_rank, capturing_color) = if self.active_color == Color::White {
                (ep.rank - 1, Color::White) // white pawn must be on rank below ep square
            } else {
                (ep.rank + 1, Color::Black)
            };
            let pawn_bb = self.get_piece_bb(capturing_color, PieceType::Pawn);
            let can_capture = (ep_file > 1 && pawn_bb & (1u64 << ((pawn_rank - 1) * 8 + (ep_file - 2))) != 0)
                || (ep_file < 8 && pawn_bb & (1u64 << ((pawn_rank - 1) * 8 + ep_file)) != 0);
            if can_capture {
                hash ^= POLYGLOT_RANDOM64[772 + (ep_file - 1) as usize];
            }
        }

        // Polyglot XORs turn key when white to move
        if self.active_color == Color::White {
            hash ^= POLYGLOT_RANDOM64[780];
        }

        hash
    }

    pub fn check_for_insufficient_material(&self) -> Option<Status> {
        // Use bitboard population count for efficiency
        let white_piece_count = self.get_pieces_bb(Color::White).count_ones();
        let black_piece_count = self.get_pieces_bb(Color::Black).count_ones();

        if white_piece_count >= 3 || black_piece_count >= 3 {
            return None;
        }

        let white_bishop_count = self.get_piece_bb(Color::White, PieceType::Bishop).count_ones();
        let black_bishop_count = self.get_piece_bb(Color::Black, PieceType::Bishop).count_ones();
        let white_knight_count = self.get_piece_bb(Color::White, PieceType::Knight).count_ones();
        let black_knight_count = self.get_piece_bb(Color::Black, PieceType::Knight).count_ones();

        let black_insufficient = black_piece_count == 1
            || (black_piece_count == 2 && (black_knight_count == 1 || black_bishop_count == 1));

        let white_insufficient = white_piece_count == 1
            || (white_piece_count == 2 && (white_knight_count == 1 || white_bishop_count == 1));

        if black_insufficient && white_insufficient {
            Some(Status::InsufficientMaterial)
        } else {
            None
        }
    }

    pub fn check_for_threefold_repetition(
        &self,
        previous_board_fens: &Vec<String>,
    ) -> Option<Status> {
        let mut count = 0;
        let current_fen = self.to_fen_no_moves();
        for fen in previous_board_fens {
            if fen == &current_fen {
                count += 1;
            }
        }
        if count >= 3 {
            Some(Status::ThreefoldRepetition)
        } else {
            None
        }
    }

    pub fn check_for_fifty_move_rule(&self) -> Option<Status> {
        if self.halfmove_clock >= 100 {
            Some(Status::FiftyMoveRule)
        } else {
            None
        }
    }

    pub fn execute_move(&self, selected_move: &Move) -> Board {
        if selected_move
            .captured
            .is_some_and(|p| p.piece_type == PieceType::King)
        {
            self.draw_to_terminal();
            panic!(
                "King cannot be captured, something is amiss. Move was {}",
                selected_move.to_human()
            );
        };

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

                // Update the board map
                board_to_piece[(selected_move.from.rank - 1) as usize]
                    [(selected_move.from.file - 1) as usize] = None;
                board_to_piece[(castle_rank - 1) as usize]
                    [(old_rook_file - 1) as usize] = None;
                if let Some(captured) = selected_move.captured {
                    // In the case of en passant, the captures square is not the target square of the move
                    board_to_piece[(captured.position.rank - 1) as usize]
                        [(captured.position.file - 1) as usize] = None;
                }
                board_to_piece[(selected_move.to.rank - 1) as usize]
                    [(selected_move.to.file - 1) as usize] = Some(new_king);
                board_to_piece[(new_rook.position.rank - 1) as usize]
                    [(new_rook.position.file - 1) as usize] = Some(new_rook);
            }
            _ => {
                // Not a castle - update the board map
                board_to_piece[(selected_move.from.rank - 1) as usize]
                    [(selected_move.from.file - 1) as usize] = None;
                if let Some(captured) = selected_move.captured {
                    // In the case of en passant, the captures square is not the target square of the move
                    board_to_piece[(captured.position.rank - 1) as usize]
                        [(captured.position.file - 1) as usize] = None;
                }
                board_to_piece[(selected_move.to.rank - 1) as usize]
                    [(selected_move.to.file - 1) as usize] = Some(new_piece);
            }
        };

        // update castling rights
        let mut castle_kingside_white = true;
        let mut castle_queenside_white = true;
        let mut castle_kingside_black = true;
        let mut castle_queenside_black = true;

        if selected_move.piece.piece_type == PieceType::King {
            match selected_move.piece.color {
                Color::White => {
                    castle_kingside_white = false;
                    castle_queenside_white = false;
                }
                Color::Black => {
                    castle_kingside_black = false;
                    castle_queenside_black = false;
                }
            }
        } else if selected_move.piece.piece_type == PieceType::Rook {
            match selected_move.piece.color {
                Color::White => {
                    if selected_move.from.rank == 1 && selected_move.from.file == 1 {
                        castle_queenside_white = false;
                    }
                    if selected_move.from.rank == 1 && selected_move.from.file == 8 {
                        castle_kingside_white = false;
                    }
                }
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
                Color::White => white_king_position = selected_move.to,
                Color::Black => black_king_position = selected_move.to,
            };
        };

        // self.check_for_insufficient_material();

        let move_is_irriversible =
            selected_move.captured.is_some() || selected_move.piece.piece_type == PieceType::Pawn;

        let new_active_color = self.active_color.other_color();
        let new_castle_kingside_white = self.castle_kingside_white && castle_kingside_white;
        let new_castle_queenside_white = self.castle_queenside_white && castle_queenside_white;
        let new_castle_kingside_black = self.castle_kingside_black && castle_kingside_black;
        let new_castle_queenside_black = self.castle_queenside_black && castle_queenside_black;
        let new_en_passant_target = if let MoveFlag::DoublePawnPush(ep_target) = selected_move.move_flag {
            Some(ep_target)
        } else {
            None
        };

        // Compute piece bitboards, incremental evaluation, and Zobrist hash from board_to_piece
        let mut piece_bb = [[0u64; 6]; 2];
        let mut occupied = 0u64;
        let mut material = [0i32; 2];
        let mut pst_mg = [0i32; 2];
        let mut pst_eg = [0i32; 2];
        let mut phase = 0i32;
        let mut zobrist_hash: u64 = 0;

        for rank in 0..8 {
            for file in 0..8 {
                if let Some(piece) = board_to_piece[rank][file] {
                    let sq_bb = position_to_bb(&piece.position);
                    let color_idx = piece.color as usize;
                    let piece_idx = piece_type_to_index(piece.piece_type);
                    piece_bb[color_idx][piece_idx] |= sq_bb;
                    occupied |= sq_bb;

                    // Update incremental evaluation
                    material[color_idx] += Material::piece_to_material(piece.piece_type);
                    pst_mg[color_idx] += get_pst_value(piece.piece_type, piece.color, piece.position.rank, piece.position.file, false);
                    pst_eg[color_idx] += get_pst_value(piece.piece_type, piece.color, piece.position.rank, piece.position.file, true);
                    phase += PHASE_WEIGHTS[piece_idx];

                    // Update Zobrist hash
                    zobrist_hash ^= ZOBRIST_KEYS.piece_key(piece.color, piece.piece_type, &piece.position);
                }
            }
        }

        // Hash additional state
        if new_active_color == Color::Black {
            zobrist_hash ^= ZOBRIST_KEYS.side_to_move;
        }
        if new_castle_kingside_white {
            zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_white;
        }
        if new_castle_queenside_white {
            zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_white;
        }
        if new_castle_kingside_black {
            zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_black;
        }
        if new_castle_queenside_black {
            zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_black;
        }
        if let Some(ep) = new_en_passant_target {
            zobrist_hash ^= ZOBRIST_KEYS.en_passant[(ep.file - 1) as usize];
        }

        // Compute attack maps
        let white_attack_map = Self::compute_attack_map_static(&piece_bb, occupied, Color::White, Some(black_king_position));
        let black_attack_map = Self::compute_attack_map_static(&piece_bb, occupied, Color::Black, Some(white_king_position));

        Board {
            active_color: new_active_color,
            halfmove_clock: if move_is_irriversible {
                0
            } else {
                self.halfmove_clock + 1
            },
            fullmove_clock: self.fullmove_clock
                + if self.active_color == Color::Black {
                    1
                } else {
                    0
                },
            castle_kingside_white: new_castle_kingside_white,
            castle_queenside_white: new_castle_queenside_white,
            castle_kingside_black: new_castle_kingside_black,
            castle_queenside_black: new_castle_queenside_black,
            en_passant_target: new_en_passant_target,
            board_to_piece,
            white_king_position,
            black_king_position,
            zobrist_hash,
            piece_bb,
            occupied,
            white_attack_map,
            black_attack_map,
            material,
            pst_mg,
            pst_eg,
            phase,
        }
    }

    /// Make a move in-place, returning undo information to restore the position.
    /// This is much faster than execute_move() which clones the entire board.
    pub fn make_move(&mut self, mv: &Move) -> UndoInfo {
        // Save state for undo
        let mut undo = UndoInfo {
            mv: *mv,
            castle_kingside_white: self.castle_kingside_white,
            castle_queenside_white: self.castle_queenside_white,
            castle_kingside_black: self.castle_kingside_black,
            castle_queenside_black: self.castle_queenside_black,
            en_passant_target: self.en_passant_target,
            halfmove_clock: self.halfmove_clock,
            original_piece_type: mv.piece.piece_type,
            rook_old_position: None,
            zobrist_hash: self.zobrist_hash,
            material: self.material,
            pst_mg: self.pst_mg,
            pst_eg: self.pst_eg,
            phase: self.phase,
        };

        let color_idx = mv.piece.color as usize;

        // Update Zobrist hash: remove piece from old position
        self.zobrist_hash ^= ZOBRIST_KEYS.piece_key(mv.piece.color, mv.piece.piece_type, &mv.from);

        // Incremental eval: remove piece from old position
        self.pst_mg[color_idx] -= get_pst_value(mv.piece.piece_type, mv.piece.color, mv.from.rank, mv.from.file, false);
        self.pst_eg[color_idx] -= get_pst_value(mv.piece.piece_type, mv.piece.color, mv.from.rank, mv.from.file, true);

        // Clear the from square on the board array
        self.board_to_piece[(mv.from.rank - 1) as usize][(mv.from.file - 1) as usize] = None;

        // Handle captures (remove captured piece)
        if let Some(captured) = mv.captured {
            let cap_color_idx = captured.color as usize;
            let cap_piece_idx = piece_type_to_index(captured.piece_type);

            // Update Zobrist hash: remove captured piece
            self.zobrist_hash ^= ZOBRIST_KEYS.piece_key(captured.color, captured.piece_type, &captured.position);

            // Incremental eval: remove captured piece
            self.material[cap_color_idx] -= Material::piece_to_material(captured.piece_type);
            self.pst_mg[cap_color_idx] -= get_pst_value(captured.piece_type, captured.color, captured.position.rank, captured.position.file, false);
            self.pst_eg[cap_color_idx] -= get_pst_value(captured.piece_type, captured.color, captured.position.rank, captured.position.file, true);
            self.phase -= PHASE_WEIGHTS[cap_piece_idx];

            // Clear the captured piece's square
            self.board_to_piece[(captured.position.rank - 1) as usize]
                [(captured.position.file - 1) as usize] = None;
        }

        // Handle castling
        match mv.move_flag {
            MoveFlag::CastleKingside | MoveFlag::CastleQueenside => {
                let castle_rank = match mv.piece.color {
                    Color::White => 1,
                    Color::Black => 8,
                };
                let old_rook_file = match mv.move_flag {
                    MoveFlag::CastleKingside => 8,
                    MoveFlag::CastleQueenside => 1,
                    _ => unreachable!(),
                };
                let new_rook_file = match mv.move_flag {
                    MoveFlag::CastleKingside => 6,
                    MoveFlag::CastleQueenside => 4,
                    _ => unreachable!(),
                };

                let old_rook_pos = Position {
                    rank: castle_rank,
                    file: old_rook_file,
                };
                let new_rook_pos = Position {
                    rank: castle_rank,
                    file: new_rook_file,
                };

                undo.rook_old_position = Some(old_rook_pos);

                // Update Zobrist hash: move rook
                self.zobrist_hash ^= ZOBRIST_KEYS.piece_key(mv.piece.color, PieceType::Rook, &old_rook_pos);
                self.zobrist_hash ^= ZOBRIST_KEYS.piece_key(mv.piece.color, PieceType::Rook, &new_rook_pos);

                // Incremental eval: move rook
                self.pst_mg[color_idx] -= get_pst_value(PieceType::Rook, mv.piece.color, old_rook_pos.rank, old_rook_pos.file, false);
                self.pst_eg[color_idx] -= get_pst_value(PieceType::Rook, mv.piece.color, old_rook_pos.rank, old_rook_pos.file, true);
                self.pst_mg[color_idx] += get_pst_value(PieceType::Rook, mv.piece.color, new_rook_pos.rank, new_rook_pos.file, false);
                self.pst_eg[color_idx] += get_pst_value(PieceType::Rook, mv.piece.color, new_rook_pos.rank, new_rook_pos.file, true);

                // Update rook position in board_to_piece
                let rook = Piece {
                    piece_type: PieceType::Rook,
                    color: mv.piece.color,
                    position: new_rook_pos,
                };
                self.board_to_piece[(old_rook_pos.rank - 1) as usize]
                    [(old_rook_pos.file - 1) as usize] = None;
                self.board_to_piece[(new_rook_pos.rank - 1) as usize]
                    [(new_rook_pos.file - 1) as usize] = Some(rook);
            }
            _ => {}
        }

        // Handle promotion
        let final_piece_type = if let MoveFlag::Promotion(promoted_to_type) = mv.move_flag {
            // Incremental eval: promotion changes material and phase
            let pawn_material = Material::piece_to_material(PieceType::Pawn);
            let promoted_material = Material::piece_to_material(promoted_to_type);
            self.material[color_idx] += promoted_material - pawn_material;
            self.phase += PHASE_WEIGHTS[piece_type_to_index(promoted_to_type)];
            // Note: pawn phase weight is 0, so no need to subtract

            promoted_to_type
        } else {
            mv.piece.piece_type
        };

        // Update Zobrist hash: add piece to new position (with correct type after promotion)
        self.zobrist_hash ^= ZOBRIST_KEYS.piece_key(mv.piece.color, final_piece_type, &mv.to);

        // Incremental eval: add piece to new position
        self.pst_mg[color_idx] += get_pst_value(final_piece_type, mv.piece.color, mv.to.rank, mv.to.file, false);
        self.pst_eg[color_idx] += get_pst_value(final_piece_type, mv.piece.color, mv.to.rank, mv.to.file, true);

        // Update the destination square on the board array
        let moved_piece = Piece {
            piece_type: final_piece_type,
            color: mv.piece.color,
            position: mv.to,
        };
        self.board_to_piece[(mv.to.rank - 1) as usize][(mv.to.file - 1) as usize] = Some(moved_piece);

        // Update castling rights and Zobrist hash
        if mv.piece.piece_type == PieceType::King {
            match mv.piece.color {
                Color::White => {
                    if self.castle_kingside_white {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_white;
                        self.castle_kingside_white = false;
                    }
                    if self.castle_queenside_white {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_white;
                        self.castle_queenside_white = false;
                    }
                }
                Color::Black => {
                    if self.castle_kingside_black {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_black;
                        self.castle_kingside_black = false;
                    }
                    if self.castle_queenside_black {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_black;
                        self.castle_queenside_black = false;
                    }
                }
            }
        } else if mv.piece.piece_type == PieceType::Rook {
            match mv.piece.color {
                Color::White => {
                    if mv.from.rank == 1 && mv.from.file == 1 && self.castle_queenside_white {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_white;
                        self.castle_queenside_white = false;
                    }
                    if mv.from.rank == 1 && mv.from.file == 8 && self.castle_kingside_white {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_white;
                        self.castle_kingside_white = false;
                    }
                }
                Color::Black => {
                    if mv.from.rank == 8 && mv.from.file == 1 && self.castle_queenside_black {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_queenside_black;
                        self.castle_queenside_black = false;
                    }
                    if mv.from.rank == 8 && mv.from.file == 8 && self.castle_kingside_black {
                        self.zobrist_hash ^= ZOBRIST_KEYS.castle_kingside_black;
                        self.castle_kingside_black = false;
                    }
                }
            }
        }

        // Update king positions
        if mv.piece.piece_type == PieceType::King {
            match mv.piece.color {
                Color::White => self.white_king_position = mv.to,
                Color::Black => self.black_king_position = mv.to,
            }
        }

        // Update en passant target and Zobrist hash
        // First, remove old en passant from hash
        if let Some(old_ep) = self.en_passant_target {
            self.zobrist_hash ^= ZOBRIST_KEYS.en_passant[(old_ep.file - 1) as usize];
        }
        // Then, set new en passant and add to hash
        self.en_passant_target = if let MoveFlag::DoublePawnPush(ep_target) = mv.move_flag {
            self.zobrist_hash ^= ZOBRIST_KEYS.en_passant[(ep_target.file - 1) as usize];
            Some(ep_target)
        } else {
            None
        };

        // Update halfmove clock
        let move_is_irreversible =
            mv.captured.is_some() || mv.piece.piece_type == PieceType::Pawn;
        self.halfmove_clock = if move_is_irreversible {
            0
        } else {
            self.halfmove_clock + 1
        };

        // Update fullmove clock (increments after Black's move)
        if self.active_color == Color::Black {
            self.fullmove_clock += 1;
        }

        // Switch active color and update Zobrist hash
        self.zobrist_hash ^= ZOBRIST_KEYS.side_to_move;
        self.active_color = self.active_color.other_color();

        // Update bitboards
        let color_idx = mv.piece.color as usize;
        let from_bb = position_to_bb(&mv.from);
        let to_bb = position_to_bb(&mv.to);

        // Remove piece from old square
        let original_piece_idx = piece_type_to_index(mv.piece.piece_type);
        self.piece_bb[color_idx][original_piece_idx] &= !from_bb;
        self.occupied &= !from_bb;

        // Handle captured piece
        if let Some(captured) = mv.captured {
            let cap_bb = position_to_bb(&captured.position);
            let cap_color_idx = captured.color as usize;
            let cap_piece_idx = piece_type_to_index(captured.piece_type);
            self.piece_bb[cap_color_idx][cap_piece_idx] &= !cap_bb;
            self.occupied &= !cap_bb;
        }

        // Handle castling rook
        if let Some(old_rook_pos) = undo.rook_old_position {
            let new_rook_pos = match mv.move_flag {
                MoveFlag::CastleKingside => Position { rank: old_rook_pos.rank, file: 6 },
                MoveFlag::CastleQueenside => Position { rank: old_rook_pos.rank, file: 4 },
                _ => unreachable!(),
            };
            let old_rook_bb = position_to_bb(&old_rook_pos);
            let new_rook_bb = position_to_bb(&new_rook_pos);
            let rook_idx = piece_type_to_index(PieceType::Rook);
            self.piece_bb[color_idx][rook_idx] &= !old_rook_bb;
            self.piece_bb[color_idx][rook_idx] |= new_rook_bb;
            self.occupied &= !old_rook_bb;
            self.occupied |= new_rook_bb;
        }

        // Add piece to new square (with correct piece type for promotion)
        let final_piece_idx = piece_type_to_index(final_piece_type);
        self.piece_bb[color_idx][final_piece_idx] |= to_bb;
        self.occupied |= to_bb;

        // Recompute attack maps
        self.recompute_attack_maps();

        undo
    }

    /// Unmake a move, restoring the position to before the move was made.
    pub fn unmake_move(&mut self, undo: &UndoInfo) {
        let mv = &undo.mv;

        // Restore Zobrist hash
        self.zobrist_hash = undo.zobrist_hash;

        // Restore incremental evaluation state
        self.material = undo.material;
        self.pst_mg = undo.pst_mg;
        self.pst_eg = undo.pst_eg;
        self.phase = undo.phase;

        // Switch active color back
        self.active_color = self.active_color.other_color();

        // Restore fullmove clock
        if self.active_color == Color::Black {
            self.fullmove_clock -= 1;
        }

        // Restore halfmove clock
        self.halfmove_clock = undo.halfmove_clock;

        // Restore en passant target
        self.en_passant_target = undo.en_passant_target;

        // Restore king position
        if mv.piece.piece_type == PieceType::King {
            match mv.piece.color {
                Color::White => self.white_king_position = mv.from,
                Color::Black => self.black_king_position = mv.from,
            }
        }

        // Restore castling rights
        self.castle_kingside_white = undo.castle_kingside_white;
        self.castle_queenside_white = undo.castle_queenside_white;
        self.castle_kingside_black = undo.castle_kingside_black;
        self.castle_queenside_black = undo.castle_queenside_black;

        // Clear the destination square
        self.board_to_piece[(mv.to.rank - 1) as usize][(mv.to.file - 1) as usize] = None;

        // Move piece back to original position with original piece type
        let original_piece = Piece {
            piece_type: undo.original_piece_type,
            color: mv.piece.color,
            position: mv.from,
        };
        self.board_to_piece[(mv.from.rank - 1) as usize][(mv.from.file - 1) as usize] =
            Some(original_piece);

        // Handle castling - restore rook position
        if let Some(old_rook_pos) = undo.rook_old_position {
            let new_rook_pos = match mv.move_flag {
                MoveFlag::CastleKingside => Position {
                    rank: old_rook_pos.rank,
                    file: 6,
                },
                MoveFlag::CastleQueenside => Position {
                    rank: old_rook_pos.rank,
                    file: 4,
                },
                _ => unreachable!(),
            };

            // Move rook back
            let rook = Piece {
                piece_type: PieceType::Rook,
                color: mv.piece.color,
                position: old_rook_pos,
            };
            self.board_to_piece[(new_rook_pos.rank - 1) as usize]
                [(new_rook_pos.file - 1) as usize] = None;
            self.board_to_piece[(old_rook_pos.rank - 1) as usize]
                [(old_rook_pos.file - 1) as usize] = Some(rook);
        }

        // Restore captured piece
        if let Some(captured) = mv.captured {
            self.board_to_piece[(captured.position.rank - 1) as usize]
                [(captured.position.file - 1) as usize] = Some(captured);
        }

        // Restore bitboards
        let color_idx = mv.piece.color as usize;
        let from_bb = position_to_bb(&mv.from);
        let to_bb = position_to_bb(&mv.to);
        let piece_idx_bb = piece_type_to_index(undo.original_piece_type);

        // Remove piece from destination square (with promotion type if applicable)
        let moved_piece_idx = if let MoveFlag::Promotion(promoted_to) = mv.move_flag {
            piece_type_to_index(promoted_to)
        } else {
            piece_idx_bb
        };
        self.piece_bb[color_idx][moved_piece_idx] &= !to_bb;
        self.occupied &= !to_bb;

        // Add piece back to original square
        self.piece_bb[color_idx][piece_idx_bb] |= from_bb;
        self.occupied |= from_bb;

        // Handle castling rook restoration
        if let Some(old_rook_pos) = undo.rook_old_position {
            let new_rook_pos = match mv.move_flag {
                MoveFlag::CastleKingside => Position { rank: old_rook_pos.rank, file: 6 },
                MoveFlag::CastleQueenside => Position { rank: old_rook_pos.rank, file: 4 },
                _ => unreachable!(),
            };
            let old_rook_bb = position_to_bb(&old_rook_pos);
            let new_rook_bb = position_to_bb(&new_rook_pos);
            let rook_idx = piece_type_to_index(PieceType::Rook);
            self.piece_bb[color_idx][rook_idx] &= !new_rook_bb;
            self.piece_bb[color_idx][rook_idx] |= old_rook_bb;
            self.occupied &= !new_rook_bb;
            self.occupied |= old_rook_bb;
        }

        // Restore captured piece bitboards
        if let Some(captured) = mv.captured {
            let cap_bb = position_to_bb(&captured.position);
            let cap_color_idx = captured.color as usize;
            let cap_piece_idx = piece_type_to_index(captured.piece_type);
            self.piece_bb[cap_color_idx][cap_piece_idx] |= cap_bb;
            self.occupied |= cap_bb;
        }

        // Recompute attack maps
        self.recompute_attack_maps();
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
        // Use cached attack map for fast check detection
        let opponent_attack_map = self.get_attack_map(color.other_color());

        // 1: get all valid opponent moves and compute pins
        // We still need the full move list for:
        // - Computing which pieces are giving check (for blocking logic)
        // - Computing pins
        let opponent_potential_moves = self.get_all_pseudo_moves(color.other_color(), true);

        // 2: Compute checks using the move list for blocking/capture logic
        let my_king = self.get_king(*color);
        let current_checks: Vec<&Move> = opponent_potential_moves
            .iter()
            .filter(|m| m.to == my_king.position)
            .collect();

        // Use bitboard for fast check detection
        let is_in_check = self.is_in_check(*color);

        // Generate all pseudo-legal moves using bitboard-based MoveGenerator
        let mut my_possible_moves: Vec<_> = MoveGenerator::new(self, *color)
            .collect()
            .into_iter()
            // castling out of check is not allowed
            .filter(|m| {
                !(is_in_check
                    && (m.move_flag == MoveFlag::CastleKingside
                        || m.move_flag == MoveFlag::CastleQueenside))
            })
            .collect();

        // Double check: only king can move
        if current_checks.len() >= 2 {
            my_possible_moves = my_possible_moves
                .into_iter()
                .filter(|m| m.piece.piece_type == PieceType::King)
                .collect();
        }

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
                        return checking_piece_ray
                            .as_ref()
                            .is_some_and(|ray| ray.contains(&m.to));
                    }
                    false
                })
                .collect();
        }

        let pin_movegen = MoveGenerator::new(self, *color);
        let pins = pin_movegen.get_pins(color);

        // Use bitboard attack map to check castle obstruction
        let castle_rank = if *color == Color::White { 1 } else { 8 };

        // Check if castling squares are attacked using bitboards
        let c3 = position_to_bb(&Position { rank: castle_rank, file: 3 });
        let d4 = position_to_bb(&Position { rank: castle_rank, file: 4 });
        let f6 = position_to_bb(&Position { rank: castle_rank, file: 6 });
        let g7 = position_to_bb(&Position { rank: castle_rank, file: 7 });

        let castle_queenside_obstructed = (opponent_attack_map & (c3 | d4)) != 0;
        let castle_kingside_obstructed = (opponent_attack_map & (f6 | g7)) != 0;

        // 4: further filter moves
        my_possible_moves = my_possible_moves
            .into_iter()
            // 4.A Filter out moves that put the king in check using bitboard attack map
            .filter(|m| match m.piece.piece_type {
                // King cannot move into check - use bitboard for O(1) check
                PieceType::King => (position_to_bb(&m.to) & opponent_attack_map) == 0,
                _ => true,
            })
            // 4.B. Filter out invalid moves of a pinned piece
            .filter(|m| {
                let mut can_move_to = true;
                // here handle the pins
                // we iterate over all the pinned pieces
                for PinnedPiece {
                    piece: pinned_piece,
                    valid_responses,
                } in pins.iter()
                {
                    // if the pinned piece is the one we're currently considering
                    if *pinned_piece == m.piece {
                        // check if the move is allowed by the pin
                        can_move_to = valid_responses.iter().find(|&&x| x == m.to).is_some();
                    }
                }
                can_move_to
            })
            // 4.C. En passant discovered check: removing both pawns may expose king on the rank
            .filter(|m| {
                if m.move_flag != MoveFlag::EnPassantCapture {
                    return true;
                }
                // The captured pawn is on the same rank as the capturing pawn (m.from)
                // After EP, both m.from pawn and the captured pawn are removed from this rank.
                // Check if a horizontal slider (rook/queen) would attack the king.
                let king_sq = position_to_sq(&my_king.position);
                let ep_from_sq = position_to_sq(&m.from);
                let captured_sq = position_to_sq(&m.captured.unwrap().position);

                // Only relevant if king is on the same rank
                if king_sq / 8 != ep_from_sq / 8 {
                    return true;
                }

                // Simulate removal of both pawns from occupied bitboard
                let occupied_after = self.get_occupied()
                    & !(1u64 << ep_from_sq)
                    & !(1u64 << captured_sq);

                // Check if any opponent rook/queen attacks the king along this rank
                let rook_attacks = rook_attacks(king_sq, occupied_after);
                let opponent = color.other_color();
                let enemy_rq = self.get_piece_bb(opponent, PieceType::Rook)
                    | self.get_piece_bb(opponent, PieceType::Queen);
                (rook_attacks & enemy_rq) == 0
            })
            // Filter out castles that go through observed squares
            .filter(|m| match m.move_flag {
                MoveFlag::CastleKingside => {
                    !castle_kingside_obstructed
                        && match color {
                            Color::White => self.castle_kingside_white,
                            Color::Black => self.castle_kingside_black,
                        }
                }
                MoveFlag::CastleQueenside => {
                    !castle_queenside_obstructed
                        && match color {
                            Color::White => self.castle_queenside_white,
                            Color::Black => self.castle_queenside_black,
                        }
                }
                _ => true,
            })
            .collect();

        // 5: check for checkmate or stalemate
        if my_possible_moves.len() == 0 {
            if current_checks.len() > 0 {
                return Err(Status::Checkmate(*color));
            } else {
                return Err(Status::Stalemate);
            }
        };
        Ok(my_possible_moves)
    }

    /// Get all legal capture moves for the given color.
    /// This is an optimized version for quiescence search that only generates captures.
    pub fn get_legal_captures(&self, color: &Color) -> Vec<Move> {
        // Get opponent attacks for king safety checks
        let opponent_potential_moves = self.get_all_pseudo_moves(color.other_color(), true);

        let my_king = self.get_king(*color);
        let current_checks: Vec<&Move> = opponent_potential_moves
            .iter()
            .filter(|m| m.to == my_king.position)
            .collect();

        // Generate only capture pseudo-moves
        let mut my_possible_captures: Vec<_> = MoveGenerator::new(self, *color)
            .collect_captures()
            .into_iter()
            .collect();

        // Double check: only king can move
        if current_checks.len() >= 2 {
            my_possible_captures = my_possible_captures
                .into_iter()
                .filter(|m| m.piece.piece_type == PieceType::King)
                .collect();
        }

        // Single check: filter to moves that capture the checking piece or king moves
        if current_checks.len() == 1 {
            let checking_piece = current_checks[0].piece;

            my_possible_captures = my_possible_captures
                .into_iter()
                .filter(|m| {
                    // Capture of the checking piece is valid
                    if m.captured.is_some_and(|p| p == checking_piece) {
                        return true;
                    }
                    // King captures are valid (will filter for safety below)
                    if m.piece.piece_type == PieceType::King {
                        return true;
                    }
                    false
                })
                .collect();
        }

        // Get pins
        let pin_movegen = MoveGenerator::new(self, *color);
        let pins = pin_movegen.get_pins(color);

        // Filter for legality
        my_possible_captures
            .into_iter()
            // King cannot capture into check
            .filter(|m| match m.piece.piece_type {
                PieceType::King => !opponent_potential_moves.iter().any(|om| om.to == m.to),
                _ => true,
            })
            // Pinned pieces can only capture along pin ray
            .filter(|m| {
                for PinnedPiece {
                    piece: pinned_piece,
                    valid_responses,
                } in pins.iter()
                {
                    if *pinned_piece == m.piece {
                        return valid_responses.iter().any(|&x| x == m.to);
                    }
                }
                true
            })
            // En passant discovered check: removing both pawns may expose king on the rank
            .filter(|m| {
                if m.move_flag != MoveFlag::EnPassantCapture {
                    return true;
                }
                let king_sq = position_to_sq(&my_king.position);
                let ep_from_sq = position_to_sq(&m.from);
                let captured_sq = position_to_sq(&m.captured.unwrap().position);
                if king_sq / 8 != ep_from_sq / 8 {
                    return true;
                }
                let occupied_after = self.get_occupied()
                    & !(1u64 << ep_from_sq)
                    & !(1u64 << captured_sq);
                let rook_atks = rook_attacks(king_sq, occupied_after);
                let opponent = color.other_color();
                let enemy_rq = self.get_piece_bb(opponent, PieceType::Rook)
                    | self.get_piece_bb(opponent, PieceType::Queen);
                (rook_atks & enemy_rq) == 0
            })
            .collect()
    }

    pub fn get_all_pseudo_moves(&self, color: Color, observed_mode: bool) -> Vec<Move> {
        let mut move_generator = MoveGenerator::new(self, color);
        if observed_mode {
            move_generator.collect_observed()
        } else {
            move_generator.collect()
        }
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
    pub fn get_king(&self, color: Color) -> Piece {
        let king_pos = self.get_king_position(color);
        self.piece_at(king_pos.rank, king_pos.file)
            .expect("King should be here!")
            .clone()
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

    /// Check if the given color's king is in check using cached attack maps
    /// This is O(1) instead of O(n_pieces) since we use precomputed bitboards
    #[inline]
    pub fn is_in_check(&self, color: Color) -> bool {
        let king_bb = position_to_bb(&self.get_king_position(color));
        let opponent_attacks = match color {
            Color::White => self.black_attack_map,
            Color::Black => self.white_attack_map,
        };
        (king_bb & opponent_attacks) != 0
    }

    /// Check if a specific square is attacked by the given color
    #[inline]
    pub fn is_square_attacked_by(&self, pos: &Position, color: Color) -> bool {
        let sq_bb = position_to_bb(pos);
        let attacks = self.get_attack_map(color);
        (sq_bb & attacks) != 0
    }

    /// Make a null move (pass turn to opponent without moving)
    /// Used for null move pruning in search
    /// Returns undo info to restore the position
    pub fn make_null_move(&mut self) -> NullMoveUndo {
        let undo = NullMoveUndo {
            en_passant_target: self.en_passant_target,
            zobrist_hash: self.zobrist_hash,
        };

        // Clear en passant (it's no longer valid after a null move)
        if let Some(ep) = self.en_passant_target {
            self.zobrist_hash ^= ZOBRIST_KEYS.en_passant[(ep.file - 1) as usize];
        }
        self.en_passant_target = None;

        // Switch side to move
        self.active_color = self.active_color.other_color();
        self.zobrist_hash ^= ZOBRIST_KEYS.side_to_move;

        undo
    }

    /// Unmake a null move, restoring the position
    pub fn unmake_null_move(&mut self, undo: &NullMoveUndo) {
        // Restore side to move
        self.active_color = self.active_color.other_color();

        // Restore en passant and hash
        self.en_passant_target = undo.en_passant_target;
        self.zobrist_hash = undo.zobrist_hash;
    }

    // =========================================================================
    // CompactMove support methods
    // =========================================================================

    /// Make a compact move, returning undo information.
    /// This converts to Move internally and calls make_move.
    /// For optimal performance, this could be optimized to work directly with CompactMove.
    #[inline]
    pub fn make_compact_move(&mut self, mv: &CompactMove) -> UndoInfo {
        let full_move = mv.to_move(self);
        self.make_move(&full_move)
    }

    /// Check if a compact move is legal.
    /// First checks pseudo-legality (piece exists, correct color, reachable destination),
    /// then makes the move and verifies the king is not left in check.
    #[inline]
    pub fn is_legal_compact(&mut self, mv: &CompactMove) -> bool {
        // Must be pseudo-legal first (catches unreachable moves like rook moving diagonally)
        if !self.is_pseudo_legal_compact(mv) {
            return false;
        }

        let color = self.active_color;

        // Make the move
        let undo = self.make_compact_move(mv);

        // Check if king is in check (use cached attack maps after recompute)
        let king_in_check = self.is_in_check(color);

        // Unmake the move
        self.unmake_move(&undo);

        // Move is legal if king is NOT in check after the move
        !king_in_check
    }

    /// Quick pseudo-legality check for a compact move.
    /// This verifies basic validity without making the move.
    /// Used by MovePicker to filter out obviously illegal moves before full legality test.
    #[inline]
    pub fn is_pseudo_legal_compact(&self, mv: &CompactMove) -> bool {
        if *mv == CompactMove::NONE {
            return false;
        }

        let from_sq = mv.from_sq();
        let to_sq = mv.to_sq();
        let from_pos = CompactMove::sq_to_pos(from_sq);

        // Check that there's a piece at the from square
        let piece = match self.piece_at_position(&from_pos) {
            Some(p) => p,
            None => return false,
        };

        // Check that the piece belongs to the side to move
        if piece.color != self.active_color {
            return false;
        }

        // Check that the piece type matches
        if piece.piece_type != mv.piece_type() {
            return false;
        }

        // Check destination square validity
        let to_pos = CompactMove::sq_to_pos(to_sq);
        let to_piece = self.piece_at_position(&to_pos);

        if mv.is_capture() && !mv.is_en_passant() {
            // For captures, check that there's an enemy piece at destination
            match to_piece {
                Some(captured) => {
                    if captured.color == self.active_color {
                        return false; // Can't capture own piece
                    }
                }
                None => return false, // No piece to capture
            }
        } else if !mv.is_capture() && !mv.is_castle() {
            // For quiet moves (non-captures, non-castles), destination must be empty
            if to_piece.is_some() {
                return false; // Can't move to occupied square
            }
        }

        // For en passant, check that the target matches the board's EP square
        if mv.is_en_passant() {
            match self.en_passant_target {
                Some(ep_target) => {
                    if CompactMove::pos_to_sq(&ep_target) != to_sq {
                        return false;
                    }
                }
                None => return false,
            }
        }

        // For castling, check that rights are available and squares are clear
        let move_type = mv.move_type();
        match move_type {
            MoveType::CastleKingside => {
                let (can_castle, rank) = match self.active_color {
                    Color::White => (self.castle_kingside_white, 1),
                    Color::Black => (self.castle_kingside_black, 8),
                };
                if !can_castle {
                    return false;
                }
                let f_pos = Position { rank, file: 6 };
                let g_pos = Position { rank, file: 7 };
                if self.piece_at_position(&f_pos).is_some()
                    || self.piece_at_position(&g_pos).is_some()
                {
                    return false;
                }
                return true; // Castling doesn't need reachability check below
            }
            MoveType::CastleQueenside => {
                let (can_castle, rank) = match self.active_color {
                    Color::White => (self.castle_queenside_white, 1),
                    Color::Black => (self.castle_queenside_black, 8),
                };
                if !can_castle {
                    return false;
                }
                let b_pos = Position { rank, file: 2 };
                let c_pos = Position { rank, file: 3 };
                let d_pos = Position { rank, file: 4 };
                if self.piece_at_position(&b_pos).is_some()
                    || self.piece_at_position(&c_pos).is_some()
                    || self.piece_at_position(&d_pos).is_some()
                {
                    return false;
                }
                return true; // Castling doesn't need reachability check below
            }
            _ => {}
        }

        // Verify the piece can actually reach the destination square.
        // This catches TT hash collisions that produce impossible moves
        // (e.g., a rook "moving" diagonally).
        let to_bb = 1u64 << to_sq;
        let occupied = self.get_occupied();

        match piece.piece_type {
            PieceType::Pawn => {
                let color_idx = self.active_color as usize;
                // Pawn captures (including en passant)
                if mv.is_capture() || mv.is_en_passant() {
                    if (ATTACK_TABLES.pawn[color_idx][from_sq as usize] & to_bb) == 0 {
                        return false;
                    }
                } else {
                    // Pawn pushes: single or double
                    let (single_step, start_rank): (i8, u8) = match self.active_color {
                        Color::White => (8, 1), // rank index 1 = rank 2
                        Color::Black => (-8, 6), // rank index 6 = rank 7
                    };
                    let single_target = (from_sq as i8 + single_step) as u8;
                    if to_sq == single_target {
                        // Single push - OK (already checked destination is empty)
                    } else {
                        // Must be double push
                        let from_rank = from_sq / 8;
                        if from_rank != start_rank {
                            return false;
                        }
                        let double_target = (from_sq as i8 + 2 * single_step) as u8;
                        if to_sq != double_target {
                            return false;
                        }
                        // Intermediate square must also be empty
                        if (occupied & (1u64 << single_target)) != 0 {
                            return false;
                        }
                    }
                }
            }
            PieceType::Knight => {
                if (ATTACK_TABLES.knight[from_sq as usize] & to_bb) == 0 {
                    return false;
                }
            }
            PieceType::Bishop => {
                if (bishop_attacks(from_sq, occupied) & to_bb) == 0 {
                    return false;
                }
            }
            PieceType::Rook => {
                if (rook_attacks(from_sq, occupied) & to_bb) == 0 {
                    return false;
                }
            }
            PieceType::Queen => {
                if (queen_attacks(from_sq, occupied) & to_bb) == 0 {
                    return false;
                }
            }
            PieceType::King => {
                if (ATTACK_TABLES.king[from_sq as usize] & to_bb) == 0 {
                    return false;
                }
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::{
        Board, Color, Move, MoveFlag, Piece, PieceType, Position, Status, PIECES_CAN_PROMOTE_TO,
        STARTING_POSITION_FEN,
    };
    use crate::types::{CompactMove, MoveType};
    use crate::movegen::MoveGenerator;
    use pretty_assertions::assert_eq;

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

        assert_eq!(b.piece_count(), 8 * 4);

        // count pawns using bitboards
        let white_pawns = b.get_piece_bb(Color::White, PieceType::Pawn).count_ones();
        let black_pawns = b.get_piece_bb(Color::Black, PieceType::Pawn).count_ones();
        assert_eq!(white_pawns + black_pawns, 8 * 2);

        // count pairwise pieces using bitboards
        for pt in [PieceType::Rook, PieceType::Bishop, PieceType::Knight] {
            let white_count = b.get_piece_bb(Color::White, pt).count_ones();
            let black_count = b.get_piece_bb(Color::Black, pt).count_ones();
            assert_eq!(white_count + black_count, 4);
        }

        for pt in [PieceType::King, PieceType::Queen] {
            let white_count = b.get_piece_bb(Color::White, pt).count_ones();
            let black_count = b.get_piece_bb(Color::Black, pt).count_ones();
            assert_eq!(white_count + black_count, 2);
        }

        // check king positions
        assert_eq!(b.black_king_position, Position { rank: 8, file: 5 });
        assert_eq!(b.white_king_position, Position { rank: 1, file: 5 });

        assert_eq!(b.halfmove_clock, 0);
        assert_eq!(b.fullmove_clock, 1);

        assert_eq!(b.to_fen(), STARTING_POSITION_FEN);
    }

    #[test]
    fn test_to_fen() {
        [
            "7k/3r4/8/8/3R4/8/3K4/8 w - - 0 1",
            "3r4/7k/3r4/8/3R4/8/3K4/8 w - - 0 1",
        ]
        .iter()
        .for_each(|fen| {
            let b = Board::from_fen(fen);
            assert_eq!(b.to_fen(), *fen);
        })
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 4);
        for pos in vec!["d3", "d5", "d6", "d7"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Knight);
        assert_eq!(pins[0].piece.position, Position { rank: 5, file: 6 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 3);
        for pos in vec!["e5", "h5", "g5"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1); // other rook is not pinning
        assert_eq!(pins[0].piece.piece_type, PieceType::Queen);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 4);
        for pos in vec!["d3", "d5", "d6", "d7"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1); // other rook is not pinning
        assert_eq!(pins[0].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 3);
        for pos in vec!["d3", "d5", "d6"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 2);

        // Find pin for rook on d6 (order-independent)
        let pin_d6 = pins.iter().find(|p| p.piece.position == Position { rank: 6, file: 4 }).unwrap();
        assert_eq!(pin_d6.piece.piece_type, PieceType::Rook);
        pin_d6.valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pin_d6.valid_responses.len(), 2);
        for pos in vec!["d7", "d8"] {
            assert!(pin_d6.valid_responses.contains(&Position::from_algebraic(pos)));
        }

        // Find pin for rook on d4 (order-independent)
        let pin_d4 = pins.iter().find(|p| p.piece.position == Position { rank: 4, file: 4 }).unwrap();
        assert_eq!(pin_d4.piece.piece_type, PieceType::Rook);
        pin_d4.valid_responses.iter().for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pin_d4.valid_responses.len(), 2);
        for pos in vec!["d2", "d3"] {
            assert!(pin_d4.valid_responses.contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 5 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["f5", "g6"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 4, file: 4 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 3);
        for pos in vec!["a7", "b6", "c5"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 3, file: 4 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 1);
        for pos in vec!["c2"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = MoveGenerator::new(&b, Color::White).get_pins(&Color::White);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Queen);
        assert_eq!(pins[0].piece.position, Position { rank: 3, file: 6 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g2", "h1"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
        }
    }

    #[test]
    fn test_pin_fail_1() {
        let b = Board::from_fen("rnbqkbnr/p1pppppp/8/1B6/8/4P3/PPPP1PPP/RNBQK1NR b KQkq - 0 2");
        b.draw_to_terminal();
        let pins = MoveGenerator::new(&b, Color::Black).get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].piece, Piece::from_algebraic('p', "d7"));

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves
            .iter()
            .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(!legal_moves
            .iter()
            .any(|m| m.piece == Piece::from_algebraic('p', "d7")));
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
        legal_moves
            .iter()
            .for_each(|m| println!("{} {} {:?}", m.to_algebraic(), m.to_human(), m.move_flag));
        assert!(legal_moves.iter().any(|mm| *mm == m));
        let b = b.execute_move(&m);

        b.draw_to_terminal();

        let m = &Move::new(
            Piece::from_algebraic('B', "f1"),
            Position { rank: 5, file: 2 },
            Some(Piece::from_algebraic('p', "b5")),
        );
        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves
            .iter()
            .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(legal_moves.iter().any(|mm| mm == m));
        let b = b.execute_move(m);
        b.draw_to_terminal();

        assert!(b
            .piece_at_position(&Position::from_algebraic("b5"))
            .is_some());
        assert_eq!(
            b.piece_at_position(&Position::from_algebraic("b5"))
                .unwrap(),
            &Piece::from_algebraic('B', "b5")
        );

        let pins = MoveGenerator::new(&b, Color::Black).get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].piece, Piece::from_algebraic('p', "d7"));

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves
            .iter()
            .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(!legal_moves
            .iter()
            .any(|m| m.piece == Piece::from_algebraic('p', "d7")));
    }

    #[test]
    fn test_execute_move() {
        let b = Board::new();
        let p = Piece::from_algebraic('p', "e2");
        let m = Move::new(p, Position { rank: 4, file: 5 }, None);
        let new_b = b.execute_move(&m);
        new_b.draw_to_terminal();
        assert_eq!(
            *new_b.piece_at(4, 5).unwrap(),
            Piece::from_algebraic('p', "e4")
        );
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
        assert!(b.piece_at_position(&p.position).is_some_and(|piece| *piece == p));
        assert!(b.piece_at_position(&captured.position).is_some_and(|piece| *piece == captured));

        let b_after = b.execute_move(&m);
        // After capture, the pawn should be at c3
        assert!(b_after.piece_at_position(&Position::from_algebraic("c3"))
            .is_some_and(|piece| piece.piece_type == PieceType::Pawn && piece.color == Color::White));
        // Original pawn position should be empty
        assert!(b_after.piece_at_position(&p.position).is_none());
        // Captured piece position now has the capturing piece
        assert!(b_after.piece_at_position(&captured.position)
            .is_some_and(|piece| piece.piece_type == PieceType::Pawn && piece.color == Color::White));
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
            let m = Move {
                piece: Piece::from_algebraic('P', "d7"),
                from: Position::from_algebraic("d7"),
                to: Position::from_algebraic("d8"),
                captured: None,
                move_flag: MoveFlag::Promotion(*pt),
            };
            assert!(legal_moves.contains(&m));
            let new_b = b.execute_move(&m);
            // Check that the promoted piece is at d8
            let promoted_piece = new_b.piece_at_position(&Position::from_algebraic("d8"));
            assert!(promoted_piece.is_some_and(|p| p.piece_type == *pt && p.color == Color::White));
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

        let m = Move {
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
        legal_moves
            .iter()
            .for_each(|m| println!("{} {} {:?}", m.to_algebraic(), m.to_human(), m.move_flag));

        let ep_capture = Move {
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
        assert!(b
            .piece_at_position(&Position::from_algebraic("d4"))
            .is_none());
        assert_eq!(
            b.piece_at_position(&Position::from_algebraic("d3"))
                .unwrap(),
            &Piece::from_algebraic('p', "d3")
        );
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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 2);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));

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
                    assert_eq!(
                        new_b
                            .piece_at_position(&Position::from_algebraic("f1"))
                            .unwrap(),
                        &Piece::from_algebraic('R', "f1")
                    );
                    assert_eq!(
                        new_b
                            .piece_at_position(&Position::from_algebraic("g1"))
                            .unwrap(),
                        &Piece::from_algebraic('K', "g1")
                    );
                }
                MoveFlag::CastleQueenside => {
                    assert_eq!(
                        new_b
                            .piece_at_position(&Position::from_algebraic("d1"))
                            .unwrap(),
                        &Piece::from_algebraic('R', "d1")
                    );
                    assert_eq!(
                        new_b
                            .piece_at_position(&Position::from_algebraic("c1"))
                            .unwrap(),
                        &Piece::from_algebraic('K', "c1")
                    );
                }
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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(!legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleQueenside));

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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(!legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleQueenside));

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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 2);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleQueenside));

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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(!legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleQueenside));

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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 1);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(!legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleQueenside));

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
        let legal_castles = legal_moves
            .iter()
            .filter(|m| m.move_flag.is_castle())
            .collect::<Vec<_>>();
        assert_eq!(legal_castles.len(), 2);
        legal_castles
            .iter()
            .for_each(|m| println!("{}: {}", m.to_algebraic(), m.to_human()));
        // assert that white can only castle kingside, not queenside
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleKingside));
        assert!(legal_castles
            .iter()
            .any(|m| m.move_flag == MoveFlag::CastleQueenside));
    }

    #[test]
    fn cannot_castle_through_piece() {
        for file in [2, 3, 4] {
            let pre = file - 2;
            let post = 4 - file;
            let b = Board::from_fen(
                format!("4k3/8/8/8/8/8/8/R{}N{}K2R w KQ - 0 1", pre, post).as_str(),
            );
            b.draw_to_terminal();
            assert!(b
                .piece_at(1, file)
                .is_some_and(|p| p.piece_type == PieceType::Knight));
            // assert that white can only castle kingside, not queenside
            assert!(b
                .get_legal_moves(&b.active_color)
                .unwrap()
                .iter()
                .any(|m| m.move_flag == MoveFlag::CastleKingside));
            assert!(!b
                .get_legal_moves(&b.active_color)
                .unwrap()
                .iter()
                .any(|m| m.move_flag == MoveFlag::CastleQueenside));
        }

        for file in [6, 7] {
            let pre = file - 6;
            let post = 7 - file;
            let b = Board::from_fen(
                format!("4k3/8/8/8/8/8/8/R3K{}N{}R w KQ - 0 1", pre, post).as_str(),
            );
            b.draw_to_terminal();
            assert!(b
                .piece_at(1, file)
                .is_some_and(|p| p.piece_type == PieceType::Knight));
            // assert that white can only castle queenside
            assert!(!b
                .get_legal_moves(&b.active_color)
                .unwrap()
                .iter()
                .any(|m| m.move_flag == MoveFlag::CastleKingside));
            assert!(b
                .get_legal_moves(&b.active_color)
                .unwrap()
                .iter()
                .any(|m| m.move_flag == MoveFlag::CastleQueenside));
        }
    }

    #[test]
    fn pawn_capture_test() {
        let b =
            Board::from_fen("r1b1kbnr/ppp2ppp/2nppq2/3N4/2PP4/5N2/PP2PPPP/R1BQKB1R b KQkq - 4 5");
        b.draw_to_terminal();

        assert_eq!(b.active_color, Color::Black);

        let black_pseudo_moves = b.get_all_pseudo_moves(b.get_active_color(), true);
        black_pseudo_moves
            .iter()
            .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(black_pseudo_moves.contains(&Move::from_algebraic(&b, "e6", "d5")));

        println!();

        let black_moves = b.get_legal_moves(&b.active_color).unwrap();
        black_moves
            .iter()
            .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(black_moves.contains(&Move::from_algebraic(&b, "e6", "d5")));
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
        assert_eq!(legal_moves.unwrap_err(), Status::Checkmate(Color::Black));
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
        assert_eq!(legal_moves.unwrap_err(), Status::Checkmate(Color::White));
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
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));
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
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));
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
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));
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

        let pins = MoveGenerator::new(&b, Color::Black).get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 7, file: 6 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g6", "h5"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
        }

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));

        // no legal moves for pawn on f7
        assert!(!legal_moves
            .iter()
            .any(|m| m.piece == Piece::from_algebraic('P', "f7")));
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

        let pins = MoveGenerator::new(&b, Color::Black).get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 7, file: 6 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g6", "h5"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
        }

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));

        // no legal moves for pawn on f7
        assert!(!legal_moves
            .iter()
            .any(|m| m.piece == Piece::from_algebraic('P', "f7")));
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

        let pins = MoveGenerator::new(&b, Color::Black).get_pins(&Color::Black);
        assert_eq!(pins.len(), 1);
        assert_eq!(pins[0].piece.piece_type, PieceType::Pawn);
        assert_eq!(pins[0].piece.position, Position { rank: 7, file: 6 });
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["g6", "h5"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
        }

        let legal_moves = b.get_legal_moves(&b.active_color).unwrap();
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));
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
        assert_eq!(legal_moves.unwrap_err(), Status::Checkmate(Color::White));
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
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));
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
        legal_moves
            .iter()
            .for_each(|m| println!("{}", m.to_human()));
        assert_eq!(legal_moves.len(), 1);
        assert_eq!(legal_moves[0].piece.piece_type, PieceType::King);
        assert_eq!(legal_moves[0].to, Position::from_algebraic("b2"));
    }

    #[test]
    fn test_make_unmake_simple_move() {
        let mut b = Board::from_fen(STARTING_POSITION_FEN);
        let original_fen = b.to_fen();

        // Make a pawn move e2-e4
        let moves = b.get_legal_moves(&Color::White).unwrap();
        let e2e4 = moves
            .iter()
            .find(|m| m.from == Position::from_algebraic("e2") && m.to == Position::from_algebraic("e4"))
            .unwrap()
            .clone();

        let undo = b.make_move(&e2e4);

        // Verify the move was made
        assert_eq!(b.active_color, Color::Black);
        assert!(b.piece_at_position(&Position::from_algebraic("e2")).is_none());
        assert!(b.piece_at_position(&Position::from_algebraic("e4")).is_some());
        assert_eq!(
            b.en_passant_target,
            Some(Position::from_algebraic("e3"))
        );

        // Unmake and verify state is restored
        b.unmake_move(&undo);
        assert_eq!(b.to_fen(), original_fen);
    }

    #[test]
    fn test_make_unmake_capture() {
        // Position with a capturable piece
        let mut b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2");
        let original_fen = b.to_fen();
        let original_piece_count = b.piece_count();

        // Capture e4xd5
        let moves = b.get_legal_moves(&Color::White).unwrap();
        let capture = moves
            .iter()
            .find(|m| m.from == Position::from_algebraic("e4") && m.to == Position::from_algebraic("d5"))
            .unwrap()
            .clone();

        assert!(capture.captured.is_some());

        let undo = b.make_move(&capture);

        // Verify capture happened
        assert_eq!(b.piece_count(), original_piece_count - 1);
        assert!(b.piece_at_position(&Position::from_algebraic("d5")).is_some());
        assert_eq!(
            b.piece_at_position(&Position::from_algebraic("d5")).unwrap().color,
            Color::White
        );

        // Unmake and verify
        b.unmake_move(&undo);
        assert_eq!(b.to_fen(), original_fen);
        assert_eq!(b.piece_count(), original_piece_count);
    }

    #[test]
    fn test_make_unmake_castling_kingside() {
        let mut b = Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1");
        let original_fen = b.to_fen();

        let moves = b.get_legal_moves(&Color::White).unwrap();
        let castle = moves
            .iter()
            .find(|m| m.move_flag == MoveFlag::CastleKingside)
            .unwrap()
            .clone();

        let undo = b.make_move(&castle);

        // Verify castling happened
        assert_eq!(b.white_king_position, Position::from_algebraic("g1"));
        assert!(b.piece_at_position(&Position::from_algebraic("f1")).is_some()); // Rook
        assert!(b.piece_at_position(&Position::from_algebraic("g1")).is_some()); // King
        assert!(b.piece_at_position(&Position::from_algebraic("h1")).is_none()); // Old rook square
        assert!(b.piece_at_position(&Position::from_algebraic("e1")).is_none()); // Old king square
        assert!(!b.castle_kingside_white);
        assert!(!b.castle_queenside_white);

        // Unmake and verify
        b.unmake_move(&undo);
        assert_eq!(b.to_fen(), original_fen);
    }

    #[test]
    fn test_make_unmake_castling_queenside() {
        let mut b = Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1");
        let original_fen = b.to_fen();

        let moves = b.get_legal_moves(&Color::White).unwrap();
        let castle = moves
            .iter()
            .find(|m| m.move_flag == MoveFlag::CastleQueenside)
            .unwrap()
            .clone();

        let undo = b.make_move(&castle);

        // Verify castling happened
        assert_eq!(b.white_king_position, Position::from_algebraic("c1"));
        assert!(b.piece_at_position(&Position::from_algebraic("d1")).is_some()); // Rook
        assert!(b.piece_at_position(&Position::from_algebraic("c1")).is_some()); // King
        assert!(b.piece_at_position(&Position::from_algebraic("a1")).is_none()); // Old rook square
        assert!(b.piece_at_position(&Position::from_algebraic("e1")).is_none()); // Old king square

        // Unmake and verify
        b.unmake_move(&undo);
        assert_eq!(b.to_fen(), original_fen);
    }

    #[test]
    fn test_make_unmake_promotion() {
        let mut b = Board::from_fen("8/P7/8/8/8/8/8/4K2k w - - 0 1");
        let original_fen = b.to_fen();

        let moves = b.get_legal_moves(&Color::White).unwrap();
        let promotion = moves
            .iter()
            .find(|m| matches!(m.move_flag, MoveFlag::Promotion(PieceType::Queen)))
            .unwrap()
            .clone();

        let undo = b.make_move(&promotion);

        // Verify promotion
        let promoted_piece = b.piece_at_position(&Position::from_algebraic("a8")).unwrap();
        assert_eq!(promoted_piece.piece_type, PieceType::Queen);

        // Unmake and verify
        b.unmake_move(&undo);
        assert_eq!(b.to_fen(), original_fen);
        let pawn = b.piece_at_position(&Position::from_algebraic("a7")).unwrap();
        assert_eq!(pawn.piece_type, PieceType::Pawn);
    }

    #[test]
    fn test_make_unmake_en_passant() {
        // Position where white can capture en passant
        let mut b = Board::from_fen("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3");
        let original_fen = b.to_fen();
        let original_piece_count = b.piece_count();

        let moves = b.get_legal_moves(&Color::White).unwrap();
        let ep_capture = moves
            .iter()
            .find(|m| m.move_flag == MoveFlag::EnPassantCapture)
            .unwrap()
            .clone();

        let undo = b.make_move(&ep_capture);

        // Verify en passant capture
        assert!(b.piece_at_position(&Position::from_algebraic("f5")).is_none()); // Captured pawn gone
        assert!(b.piece_at_position(&Position::from_algebraic("f6")).is_some()); // Capturing pawn arrived
        assert_eq!(b.piece_count(), original_piece_count - 1);

        // Unmake and verify
        b.unmake_move(&undo);
        assert_eq!(b.to_fen(), original_fen);
        assert_eq!(b.piece_count(), original_piece_count);
    }

    #[test]
    fn test_make_unmake_sequence() {
        // Test a sequence of moves and unmakes
        let mut b = Board::from_fen(STARTING_POSITION_FEN);
        let original_fen = b.to_fen();

        let mut undos = Vec::new();

        // Make several moves
        for _ in 0..4 {
            let moves = b.get_legal_moves(&b.get_active_color()).unwrap();
            if moves.is_empty() {
                break;
            }
            let mv = moves[0].clone();
            undos.push(b.make_move(&mv));
        }

        // Unmake all moves
        while let Some(undo) = undos.pop() {
            b.unmake_move(&undo);
        }

        assert_eq!(b.to_fen(), original_fen);
    }

    #[test]
    fn test_make_unmake_matches_execute_move() {
        // Verify make_move produces the same board state as execute_move
        let positions = [
            STARTING_POSITION_FEN,
            "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1",
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2",
        ];

        for fen in positions {
            let mut b1 = Board::from_fen(fen);
            let b2 = Board::from_fen(fen);

            let moves = b1.get_legal_moves(&b1.get_active_color()).unwrap();
            for mv in moves.iter().take(5) {
                // Test first 5 moves
                let expected = b2.execute_move(mv);
                let undo = b1.make_move(mv);

                assert_eq!(b1.to_fen(), expected.to_fen(), "FEN mismatch for move {} in position {}", mv.to_algebraic(), fen);
                assert_eq!(b1.zobrist_hash, expected.zobrist_hash, "Zobrist hash mismatch for move {} in position {}", mv.to_algebraic(), fen);

                b1.unmake_move(&undo);
            }
        }
    }

    #[test]
    fn test_zobrist_hash_restored_on_unmake() {
        let positions = [
            STARTING_POSITION_FEN,
            "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1",
            "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", // EP position
            "8/P7/8/8/8/8/8/4K2k w - - 0 1", // Promotion position
        ];

        for fen in positions {
            let mut b = Board::from_fen(fen);
            let original_hash = b.zobrist_hash;

            let moves = b.get_legal_moves(&b.get_active_color()).unwrap();
            for mv in &moves {
                let undo = b.make_move(mv);
                // Hash should have changed (unless it's an extremely rare collision)
                b.unmake_move(&undo);
                assert_eq!(b.zobrist_hash, original_hash, "Hash not restored after unmake for move {} in position {}", mv.to_algebraic(), fen);
            }
        }
    }

    #[test]
    fn test_zobrist_hash_consistency_across_move_sequences() {
        // Verify that reaching the same position through different move orders produces the same hash
        let mut b1 = Board::from_fen(STARTING_POSITION_FEN);
        let mut b2 = Board::from_fen(STARTING_POSITION_FEN);

        // Sequence 1: e4, e5
        let e4 = b1.get_legal_moves(&Color::White).unwrap().into_iter()
            .find(|m| m.from == Position::from_algebraic("e2") && m.to == Position::from_algebraic("e4"))
            .unwrap();
        b1.make_move(&e4);
        let e5 = b1.get_legal_moves(&Color::Black).unwrap().into_iter()
            .find(|m| m.from == Position::from_algebraic("e7") && m.to == Position::from_algebraic("e5"))
            .unwrap();
        b1.make_move(&e5);

        // Same sequence on b2
        let e4 = b2.get_legal_moves(&Color::White).unwrap().into_iter()
            .find(|m| m.from == Position::from_algebraic("e2") && m.to == Position::from_algebraic("e4"))
            .unwrap();
        b2.make_move(&e4);
        let e5 = b2.get_legal_moves(&Color::Black).unwrap().into_iter()
            .find(|m| m.from == Position::from_algebraic("e7") && m.to == Position::from_algebraic("e5"))
            .unwrap();
        b2.make_move(&e5);

        assert_eq!(b1.zobrist_hash, b2.zobrist_hash, "Same position should have same hash");
        assert_eq!(b1.to_fen(), b2.to_fen());
    }

    // =======================================================================
    // is_pseudo_legal_compact reachability tests
    // =======================================================================
    // These verify that TT hash collision moves (impossible piece movements)
    // are correctly rejected. This prevents the engine from playing illegal
    // moves like rooks moving diagonally.

    #[test]
    fn pseudo_legal_rejects_rook_diagonal() {
        // Position from game QzVbY7vl where h1e4 was produced
        let board = Board::from_fen("rnb1k1nr/pp1pbppp/2p5/4N3/3Pq3/N3P1P1/PPP2P1P/R1BQKB1R w KQkq - 1 7");
        // h1 has a rook, e4 has a queen — rook can't move diagonally
        let rook_diagonal = CompactMove::new_quiet(7, 28, PieceType::Rook); // h1 -> e4
        assert!(!board.is_pseudo_legal_compact(&rook_diagonal),
            "Rook should not be able to move diagonally from h1 to e4");
    }

    #[test]
    fn pseudo_legal_rejects_bishop_orthogonal() {
        // Bishop on c1 trying to move to c4 (orthogonal, not diagonal)
        let board = Board::from_fen("4k3/8/8/8/8/8/8/2B1K3 w - - 0 1");
        let bishop_ortho = CompactMove::new_quiet(2, 26, PieceType::Bishop); // c1 -> c4
        assert!(!board.is_pseudo_legal_compact(&bishop_ortho),
            "Bishop should not be able to move orthogonally");
    }

    #[test]
    fn pseudo_legal_rejects_knight_wrong_target() {
        let board = Board::from_fen("4k3/8/8/8/8/8/8/N3K3 w - - 0 1");
        // Knight on a1 trying to move to a3 (straight line, not L-shape)
        let knight_straight = CompactMove::new_quiet(0, 16, PieceType::Knight); // a1 -> a3
        assert!(!board.is_pseudo_legal_compact(&knight_straight),
            "Knight should not be able to move in a straight line");
    }

    #[test]
    fn pseudo_legal_accepts_valid_rook_move() {
        let board = Board::from_fen("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1");
        // Rook on a1 to a4 (vertical, valid)
        let rook_valid = CompactMove::new_quiet(0, 24, PieceType::Rook); // a1 -> a4
        assert!(board.is_pseudo_legal_compact(&rook_valid),
            "Rook should be able to move vertically");
    }

    #[test]
    fn pseudo_legal_accepts_valid_bishop_move() {
        let board = Board::from_fen("4k3/8/8/8/8/8/8/2B1K3 w - - 0 1");
        // Bishop on c1 to f4 (diagonal, valid)
        let bishop_valid = CompactMove::new_quiet(2, 29, PieceType::Bishop); // c1 -> f4
        assert!(board.is_pseudo_legal_compact(&bishop_valid),
            "Bishop should be able to move diagonally");
    }

    #[test]
    fn pseudo_legal_rejects_blocked_rook() {
        // Rook on a1 blocked by pawn on a2
        let board = Board::from_fen("4k3/8/8/8/8/8/P7/R3K3 w Q - 0 1");
        let rook_blocked = CompactMove::new_quiet(0, 24, PieceType::Rook); // a1 -> a4
        assert!(!board.is_pseudo_legal_compact(&rook_blocked),
            "Rook should not be able to jump over a blocking pawn");
    }

    #[test]
    fn pseudo_legal_rejects_pawn_triple_push() {
        let board = Board::new();
        // Pawn on e2 trying to move to e5 (three squares)
        let pawn_triple = CompactMove::new_quiet(12, 36, PieceType::Pawn); // e2 -> e5
        assert!(!board.is_pseudo_legal_compact(&pawn_triple),
            "Pawn should not be able to push three squares");
    }

    #[test]
    fn pseudo_legal_accepts_pawn_double_push() {
        let board = Board::new();
        // Pawn on e2 to e4 (valid double push from starting rank)
        let pawn_double = CompactMove::new(12, 28, PieceType::Pawn, MoveType::DoublePawnPush, None);
        assert!(board.is_pseudo_legal_compact(&pawn_double),
            "Pawn should be able to double push from starting rank");
    }

    #[test]
    fn pseudo_legal_rejects_wrong_piece_type() {
        let board = Board::new();
        // There's a rook on a1, but we claim it's a bishop
        let wrong_type = CompactMove::new_quiet(0, 16, PieceType::Bishop); // a1 -> a3 as "bishop"
        assert!(!board.is_pseudo_legal_compact(&wrong_type),
            "Should reject move when piece type doesn't match");
    }

    #[test]
    fn pseudo_legal_rejects_king_long_move() {
        let board = Board::from_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1");
        // King on e1 trying to move to e3 (two squares, not castling)
        let king_long = CompactMove::new_quiet(4, 20, PieceType::King); // e1 -> e3
        assert!(!board.is_pseudo_legal_compact(&king_long),
            "King should not be able to move two squares (non-castling)");
    }
}
