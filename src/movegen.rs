use crate::types::{Piece, Position};

use crate::bitboard::{
    pos_to_sq, position_to_bb, sq_to_bb, sq_to_position, ATTACK_TABLES, BitboardIter,
};
use crate::board::Board;
use crate::types::*;

const MAX_MOVES: usize = 218;

pub struct MoveGenerator<'a> {
    board: &'a Board,
    color: Color,
    moves: Vec<Move>,
}

impl<'a> MoveGenerator<'a> {
    pub fn new(board: &'a Board, color: Color) -> Self {
        Self {
            board,
            color,
            moves: Vec::with_capacity(MAX_MOVES), // 1 malloc here
        }
    }

    pub fn collect(&mut self) -> Vec<Move> {
        self.collect_with_mode(false, false)
    }

    pub fn collect_observed(&mut self) -> Vec<Move> {
        self.collect_with_mode(true, false)
    }

    /// Collect only capture moves (for quiescence search)
    pub fn collect_captures(&mut self) -> Vec<Move> {
        self.collect_with_mode(false, true)
    }

    fn collect_with_mode(&mut self, observed_mode: bool, captures_only: bool) -> Vec<Move> {
        // Use bitboard-based move generation
        self.collect_with_mode_bb(observed_mode, captures_only)
    }

    /// Bitboard-based move generation - much faster than piece iteration
    fn collect_with_mode_bb(&mut self, observed_mode: bool, captures_only: bool) -> Vec<Move> {
        let occupied = self.board.get_occupied();
        let friendly = self.board.get_pieces_bb(self.color);
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        // Get opponent king position for xray attacks in observed mode
        let opponent_king_bb = if observed_mode {
            self.board.get_piece_bb(self.color.other_color(), PieceType::King)
        } else {
            0
        };
        let occupied_no_king = occupied & !opponent_king_bb;

        // Generate knight moves
        self.generate_knight_moves_bb(occupied, friendly, enemy, observed_mode, captures_only);

        // Generate king moves
        self.generate_king_moves_bb(occupied, friendly, enemy, observed_mode, captures_only);

        // Generate rook moves
        self.generate_rook_moves_bb(occupied_no_king, friendly, enemy, observed_mode, captures_only);

        // Generate bishop moves
        self.generate_bishop_moves_bb(occupied_no_king, friendly, enemy, observed_mode, captures_only);

        // Generate queen moves (rook + bishop)
        self.generate_queen_moves_bb(occupied_no_king, friendly, enemy, observed_mode, captures_only);

        // Generate pawn moves
        self.generate_pawn_moves_bb(occupied, friendly, enemy, observed_mode, captures_only);

        std::mem::take(&mut self.moves)
    }

    /// Generate knight moves using bitboards
    fn generate_knight_moves_bb(
        &mut self,
        _occupied: u64,
        friendly: u64,
        enemy: u64,
        observed_mode: bool,
        captures_only: bool,
    ) {
        let knights = self.board.get_piece_bb(self.color, PieceType::Knight);

        for from_sq in BitboardIter(knights) {
            let attacks = ATTACK_TABLES.knight[from_sq as usize];
            let from_pos = sq_to_position(from_sq);
            let piece = Piece {
                color: self.color,
                piece_type: PieceType::Knight,
                position: from_pos,
            };

            // In observed mode, include squares with friendly pieces
            let valid_targets = if observed_mode {
                attacks
            } else {
                attacks & !friendly
            };

            // In captures_only mode, only target enemy pieces
            let targets = if captures_only {
                valid_targets & enemy
            } else {
                valid_targets
            };

            for to_sq in BitboardIter(targets) {
                let to_pos = sq_to_position(to_sq);
                let captured = self.board.piece_at_position(&to_pos).copied();
                self.moves.push(Move {
                    piece,
                    from: from_pos,
                    to: to_pos,
                    captured,
                    move_flag: MoveFlag::Regular,
                });
            }
        }
    }

    /// Generate king moves using bitboards (excluding castling)
    fn generate_king_moves_bb(
        &mut self,
        _occupied: u64,
        friendly: u64,
        enemy: u64,
        observed_mode: bool,
        captures_only: bool,
    ) {
        let king_bb = self.board.get_piece_bb(self.color, PieceType::King);
        if king_bb == 0 {
            return;
        }

        let from_sq = king_bb.trailing_zeros() as u8;
        let attacks = ATTACK_TABLES.king[from_sq as usize];
        let from_pos = sq_to_position(from_sq);
        let piece = Piece {
            color: self.color,
            piece_type: PieceType::King,
            position: from_pos,
        };

        let valid_targets = if observed_mode {
            attacks
        } else {
            attacks & !friendly
        };

        let targets = if captures_only {
            valid_targets & enemy
        } else {
            valid_targets
        };

        for to_sq in BitboardIter(targets) {
            let to_pos = sq_to_position(to_sq);
            let captured = self.board.piece_at_position(&to_pos).copied();
            self.moves.push(Move {
                piece,
                from: from_pos,
                to: to_pos,
                captured,
                move_flag: MoveFlag::Regular,
            });
        }

        // Generate castling moves (only if not captures_only)
        if !captures_only {
            self.generate_castling_moves(&piece);
        }
    }

    /// Generate castling moves
    fn generate_castling_moves(&mut self, king: &Piece) {
        let occupied = self.board.get_occupied();

        match self.color {
            Color::White => {
                // Kingside castling
                if self.board.castle_kingside_white {
                    let rook_sq = sq_to_bb(pos_to_sq(1, 8));
                    let f1 = sq_to_bb(pos_to_sq(1, 6));
                    let g1 = sq_to_bb(pos_to_sq(1, 7));
                    let rook_bb = self.board.get_piece_bb(Color::White, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0 && (occupied & (f1 | g1)) == 0 {
                        self.moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position { rank: 1, file: 7 },
                            captured: None,
                            move_flag: MoveFlag::CastleKingside,
                        });
                    }
                }
                // Queenside castling
                if self.board.castle_queenside_white {
                    let rook_sq = sq_to_bb(pos_to_sq(1, 1));
                    let b1 = sq_to_bb(pos_to_sq(1, 2));
                    let c1 = sq_to_bb(pos_to_sq(1, 3));
                    let d1 = sq_to_bb(pos_to_sq(1, 4));
                    let rook_bb = self.board.get_piece_bb(Color::White, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0 && (occupied & (b1 | c1 | d1)) == 0 {
                        self.moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position { rank: 1, file: 3 },
                            captured: None,
                            move_flag: MoveFlag::CastleQueenside,
                        });
                    }
                }
            }
            Color::Black => {
                // Kingside castling
                if self.board.castle_kingside_black {
                    let rook_sq = sq_to_bb(pos_to_sq(8, 8));
                    let f8 = sq_to_bb(pos_to_sq(8, 6));
                    let g8 = sq_to_bb(pos_to_sq(8, 7));
                    let rook_bb = self.board.get_piece_bb(Color::Black, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0 && (occupied & (f8 | g8)) == 0 {
                        self.moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position { rank: 8, file: 7 },
                            captured: None,
                            move_flag: MoveFlag::CastleKingside,
                        });
                    }
                }
                // Queenside castling
                if self.board.castle_queenside_black {
                    let rook_sq = sq_to_bb(pos_to_sq(8, 1));
                    let b8 = sq_to_bb(pos_to_sq(8, 2));
                    let c8 = sq_to_bb(pos_to_sq(8, 3));
                    let d8 = sq_to_bb(pos_to_sq(8, 4));
                    let rook_bb = self.board.get_piece_bb(Color::Black, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0 && (occupied & (b8 | c8 | d8)) == 0 {
                        self.moves.push(Move {
                            piece: *king,
                            from: king.position,
                            to: Position { rank: 8, file: 3 },
                            captured: None,
                            move_flag: MoveFlag::CastleQueenside,
                        });
                    }
                }
            }
        }
    }

    /// Generate rook moves using bitboards
    fn generate_rook_moves_bb(
        &mut self,
        occupied: u64,
        friendly: u64,
        enemy: u64,
        observed_mode: bool,
        captures_only: bool,
    ) {
        let rooks = self.board.get_piece_bb(self.color, PieceType::Rook);

        for from_sq in BitboardIter(rooks) {
            let attacks = ATTACK_TABLES.rook_attacks(from_sq, occupied);
            let from_pos = sq_to_position(from_sq);
            let piece = Piece {
                color: self.color,
                piece_type: PieceType::Rook,
                position: from_pos,
            };

            let valid_targets = if observed_mode {
                attacks
            } else {
                attacks & !friendly
            };

            let targets = if captures_only {
                valid_targets & enemy
            } else {
                valid_targets
            };

            for to_sq in BitboardIter(targets) {
                let to_pos = sq_to_position(to_sq);
                let captured = self.board.piece_at_position(&to_pos).copied();
                self.moves.push(Move {
                    piece,
                    from: from_pos,
                    to: to_pos,
                    captured,
                    move_flag: MoveFlag::Regular,
                });
            }
        }
    }

    /// Generate bishop moves using bitboards
    fn generate_bishop_moves_bb(
        &mut self,
        occupied: u64,
        friendly: u64,
        enemy: u64,
        observed_mode: bool,
        captures_only: bool,
    ) {
        let bishops = self.board.get_piece_bb(self.color, PieceType::Bishop);

        for from_sq in BitboardIter(bishops) {
            let attacks = ATTACK_TABLES.bishop_attacks(from_sq, occupied);
            let from_pos = sq_to_position(from_sq);
            let piece = Piece {
                color: self.color,
                piece_type: PieceType::Bishop,
                position: from_pos,
            };

            let valid_targets = if observed_mode {
                attacks
            } else {
                attacks & !friendly
            };

            let targets = if captures_only {
                valid_targets & enemy
            } else {
                valid_targets
            };

            for to_sq in BitboardIter(targets) {
                let to_pos = sq_to_position(to_sq);
                let captured = self.board.piece_at_position(&to_pos).copied();
                self.moves.push(Move {
                    piece,
                    from: from_pos,
                    to: to_pos,
                    captured,
                    move_flag: MoveFlag::Regular,
                });
            }
        }
    }

    /// Generate queen moves using bitboards
    fn generate_queen_moves_bb(
        &mut self,
        occupied: u64,
        friendly: u64,
        enemy: u64,
        observed_mode: bool,
        captures_only: bool,
    ) {
        let queens = self.board.get_piece_bb(self.color, PieceType::Queen);

        for from_sq in BitboardIter(queens) {
            let attacks = ATTACK_TABLES.queen_attacks(from_sq, occupied);
            let from_pos = sq_to_position(from_sq);
            let piece = Piece {
                color: self.color,
                piece_type: PieceType::Queen,
                position: from_pos,
            };

            let valid_targets = if observed_mode {
                attacks
            } else {
                attacks & !friendly
            };

            let targets = if captures_only {
                valid_targets & enemy
            } else {
                valid_targets
            };

            for to_sq in BitboardIter(targets) {
                let to_pos = sq_to_position(to_sq);
                let captured = self.board.piece_at_position(&to_pos).copied();
                self.moves.push(Move {
                    piece,
                    from: from_pos,
                    to: to_pos,
                    captured,
                    move_flag: MoveFlag::Regular,
                });
            }
        }
    }

    /// Generate pawn moves using bitboards
    fn generate_pawn_moves_bb(
        &mut self,
        occupied: u64,
        _friendly: u64,
        enemy: u64,
        observed_mode: bool,
        captures_only: bool,
    ) {
        let pawns = self.board.get_piece_bb(self.color, PieceType::Pawn);
        let color_idx = self.color as usize;

        // Pawn attack generation for observed mode
        if observed_mode {
            for from_sq in BitboardIter(pawns) {
                let attacks = ATTACK_TABLES.pawn[color_idx][from_sq as usize];
                let from_pos = sq_to_position(from_sq);
                let piece = Piece {
                    color: self.color,
                    piece_type: PieceType::Pawn,
                    position: from_pos,
                };

                for to_sq in BitboardIter(attacks) {
                    let to_pos = sq_to_position(to_sq);
                    let captured = self.board.piece_at_position(&to_pos).copied();
                    self.moves.push(Move {
                        piece,
                        from: from_pos,
                        to: to_pos,
                        captured,
                        move_flag: MoveFlag::Regular,
                    });
                }
            }
            return;
        }

        // Regular pawn move generation
        for from_sq in BitboardIter(pawns) {
            let from_pos = sq_to_position(from_sq);
            let piece = Piece {
                color: self.color,
                piece_type: PieceType::Pawn,
                position: from_pos,
            };

            // Pawn captures
            let attacks = ATTACK_TABLES.pawn[color_idx][from_sq as usize];
            let capture_targets = attacks & enemy;

            for to_sq in BitboardIter(capture_targets) {
                let to_pos = sq_to_position(to_sq);
                let captured = self.board.piece_at_position(&to_pos).copied();

                // Check for promotion
                let promotion_rank = if self.color == Color::White { 8 } else { 1 };
                if to_pos.rank == promotion_rank {
                    for promo_type in PIECES_CAN_PROMOTE_TO {
                        self.moves.push(Move {
                            piece,
                            from: from_pos,
                            to: to_pos,
                            captured,
                            move_flag: MoveFlag::Promotion(promo_type),
                        });
                    }
                } else {
                    self.moves.push(Move {
                        piece,
                        from: from_pos,
                        to: to_pos,
                        captured,
                        move_flag: MoveFlag::Regular,
                    });
                }
            }

            // En passant captures
            if let Some(ep_target) = self.board.en_passant_target {
                let ep_bb = position_to_bb(&ep_target);
                if (attacks & ep_bb) != 0 {
                    // Find the captured pawn (it's on our rank, at the ep file)
                    let captured_pos = Position {
                        rank: from_pos.rank,
                        file: ep_target.file,
                    };
                    let captured = self.board.piece_at_position(&captured_pos).copied();
                    self.moves.push(Move {
                        piece,
                        from: from_pos,
                        to: ep_target,
                        captured,
                        move_flag: MoveFlag::EnPassantCapture,
                    });
                }
            }

            // Pawn pushes (skip if captures_only)
            if captures_only {
                continue;
            }

            let one_step = pawn_step_forward(from_pos.rank, self.color);
            let one_step_pos = Position {
                rank: one_step,
                file: from_pos.file,
            };
            let one_step_bb = position_to_bb(&one_step_pos);

            // One step forward (must be empty)
            if (occupied & one_step_bb) == 0 {
                let promotion_rank = if self.color == Color::White { 8 } else { 1 };
                if one_step == promotion_rank {
                    for promo_type in PIECES_CAN_PROMOTE_TO {
                        self.moves.push(Move {
                            piece,
                            from: from_pos,
                            to: one_step_pos,
                            captured: None,
                            move_flag: MoveFlag::Promotion(promo_type),
                        });
                    }
                } else {
                    self.moves.push(Move {
                        piece,
                        from: from_pos,
                        to: one_step_pos,
                        captured: None,
                        move_flag: MoveFlag::Regular,
                    });

                    // Two step forward from starting position
                    let start_rank = if self.color == Color::White { 2 } else { 7 };
                    if from_pos.rank == start_rank {
                        let two_step = pawn_step_forward(one_step, self.color);
                        let two_step_pos = Position {
                            rank: two_step,
                            file: from_pos.file,
                        };
                        let two_step_bb = position_to_bb(&two_step_pos);

                        if (occupied & two_step_bb) == 0 {
                            let ep_square = Position {
                                rank: one_step,
                                file: from_pos.file,
                            };
                            self.moves.push(Move {
                                piece,
                                from: from_pos,
                                to: two_step_pos,
                                captured: None,
                                move_flag: MoveFlag::DoublePawnPush(ep_square),
                            });
                        }
                    }
                }
            }
        }
    }

    /// Compute `color`'s pinned pieces, as well as the list of squares they can move to
    /// Note that this list of possible squares does not mean the piece can legally move there,
    /// (eg, it might not be a possible move for that piece), but simply that those are the pieces
    /// where the piece would maintain protection of the king, by blocking the ray or capturing
    /// the pinning piece.
    pub fn get_pins(&self, color: &Color) -> Vec<PinnedPiece> {
        let opponent_sliding_pieces: Vec<&Piece> = self
            .board
            .pieces
            .iter()
            .filter(|p| p.color == color.other_color() && p.piece_type.is_sliding())
            .collect();

        let king = self.board.get_king(*color);
        // TODO OPTIM: make this a vec with size init
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
                .map(|pos| self.board.piece_at_position(pos))
                .filter_map(|p| p)
                .collect();

            if pieces_in_ray
                .iter()
                .skip(1)
                .any(|p| p.color == pinning.color)
            {
                // there is a piece of our color in the way, this is not a pin
                // we skip the first element because that one is always going to be out own piece
                return None;
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
}

/// Direction of pawn movement for given color
pub fn pawn_step_forward(rank: u8, color: Color) -> u8 {
    match color {
        Color::White => rank + 1,
        Color::Black => rank - 1,
    }
}

/// This represents a pinned piece (to the king), as well as the valid moves it can have
/// whilst staying between the king and the attacking piece.
pub struct PinnedPiece {
    /// The pinned piece
    pub piece: Piece,
    /// The positions in the ray between the attacking piece (inclusive) and the king (exclusive)
    pub valid_responses: Vec<Position>,
}

impl PinnedPiece {
    pub fn new(piece: Piece, valid_responses: Vec<Position>) -> Self {
        Self {
            piece,
            valid_responses: valid_responses
                .into_iter()
                .filter(|p| p != &piece.position)
                .collect(),
        }
    }
}
