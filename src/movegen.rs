use crate::types::{Piece, Position};

use crate::bitboard::{
    pos_to_sq, position_to_bb, sq_to_bb, sq_to_position, ATTACK_TABLES, BitboardIter,
    bishop_attacks, rook_attacks, queen_attacks,
};
use crate::board::Board;
use crate::movelist::MoveList;
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
        let opponent_attacks = self.board.get_attack_map(self.color.other_color());
        let king_bb = position_to_bb(&king.position);

        // Can't castle while in check
        if (king_bb & opponent_attacks) != 0 {
            return;
        }

        match self.color {
            Color::White => {
                // Kingside castling
                if self.board.castle_kingside_white {
                    let rook_sq = sq_to_bb(pos_to_sq(1, 8));
                    let f1 = sq_to_bb(pos_to_sq(1, 6));
                    let g1 = sq_to_bb(pos_to_sq(1, 7));
                    let rook_bb = self.board.get_piece_bb(Color::White, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0
                        && (occupied & (f1 | g1)) == 0
                        && (opponent_attacks & (f1 | g1)) == 0
                    {
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

                    // b1 only needs to be empty, c1 and d1 must be unattacked
                    if (rook_bb & rook_sq) != 0
                        && (occupied & (b1 | c1 | d1)) == 0
                        && (opponent_attacks & (c1 | d1)) == 0
                    {
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

                    if (rook_bb & rook_sq) != 0
                        && (occupied & (f8 | g8)) == 0
                        && (opponent_attacks & (f8 | g8)) == 0
                    {
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

                    if (rook_bb & rook_sq) != 0
                        && (occupied & (b8 | c8 | d8)) == 0
                        && (opponent_attacks & (c8 | d8)) == 0
                    {
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
            let attacks = rook_attacks(from_sq, occupied);
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
            let attacks = bishop_attacks(from_sq, occupied);
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
            let attacks = queen_attacks(from_sq, occupied);
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

            // Pawn pushes
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
                    // Always include promotions (even in captures_only mode for quiescence)
                    for promo_type in PIECES_CAN_PROMOTE_TO {
                        self.moves.push(Move {
                            piece,
                            from: from_pos,
                            to: one_step_pos,
                            captured: None,
                            move_flag: MoveFlag::Promotion(promo_type),
                        });
                    }
                } else if !captures_only {
                    // Skip non-promotion pawn pushes in captures_only mode
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
        // Use iter_pieces() to iterate over opponent's sliding pieces
        let opponent_sliding_pieces: Vec<Piece> = self
            .board
            .iter_pieces()
            .filter(|p| p.color == color.other_color() && p.piece_type.is_sliding())
            .collect();

        let king = self.board.get_king(*color);
        let mut pins: Vec<PinnedPiece> = vec![];

        for piece in &opponent_sliding_pieces {
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

// =============================================================================
// CompactMoveGenerator - High-performance move generation into MoveList
// =============================================================================

/// High-performance move generator that writes CompactMove directly into a MoveList.
/// This avoids all heap allocations by using stack-allocated storage.
pub struct CompactMoveGenerator<'a> {
    board: &'a Board,
    color: Color,
}

impl<'a> CompactMoveGenerator<'a> {
    #[inline(always)]
    pub fn new(board: &'a Board, color: Color) -> Self {
        Self { board, color }
    }

    /// Generate all pseudo-legal moves into the provided MoveList.
    #[inline]
    pub fn generate_all(&self, list: &mut MoveList) {
        let occupied = self.board.get_occupied();
        let friendly = self.board.get_pieces_bb(self.color);
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        self.generate_knight_moves(list, friendly, enemy);
        self.generate_king_moves(list, occupied, friendly, enemy);
        self.generate_rook_moves(list, occupied, friendly, enemy);
        self.generate_bishop_moves(list, occupied, friendly, enemy);
        self.generate_queen_moves(list, occupied, friendly, enemy);
        self.generate_pawn_moves(list, occupied, enemy);
    }

    /// Generate only capture moves (for quiescence search).
    #[inline]
    pub fn generate_captures(&self, list: &mut MoveList) {
        let occupied = self.board.get_occupied();
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        self.generate_knight_captures(list, enemy);
        self.generate_king_captures(list, enemy);
        self.generate_rook_captures(list, occupied, enemy);
        self.generate_bishop_captures(list, occupied, enemy);
        self.generate_queen_captures(list, occupied, enemy);
        self.generate_pawn_captures(list, enemy);
    }

    /// Generate only quiet (non-capture) moves.
    #[inline]
    pub fn generate_quiets(&self, list: &mut MoveList) {
        let occupied = self.board.get_occupied();
        let friendly = self.board.get_pieces_bb(self.color);

        self.generate_knight_quiets(list, friendly);
        self.generate_king_quiets(list, occupied, friendly);
        self.generate_rook_quiets(list, occupied, friendly);
        self.generate_bishop_quiets(list, occupied, friendly);
        self.generate_queen_quiets(list, occupied, friendly);
        self.generate_pawn_quiets(list, occupied);
    }

    // =========================================================================
    // Knight moves
    // =========================================================================

    #[inline]
    fn generate_knight_moves(&self, list: &mut MoveList, friendly: u64, enemy: u64) {
        let knights = self.board.get_piece_bb(self.color, PieceType::Knight);

        for from_sq in BitboardIter(knights) {
            let attacks = ATTACK_TABLES.knight[from_sq as usize];
            let targets = attacks & !friendly;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                let move_type = if captured.is_some() {
                    MoveType::Capture
                } else {
                    MoveType::Quiet
                };
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Knight,
                    move_type,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_knight_captures(&self, list: &mut MoveList, enemy: u64) {
        let knights = self.board.get_piece_bb(self.color, PieceType::Knight);

        for from_sq in BitboardIter(knights) {
            let attacks = ATTACK_TABLES.knight[from_sq as usize];
            let targets = attacks & enemy;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Knight,
                    MoveType::Capture,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_knight_quiets(&self, list: &mut MoveList, friendly: u64) {
        let knights = self.board.get_piece_bb(self.color, PieceType::Knight);
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        for from_sq in BitboardIter(knights) {
            let attacks = ATTACK_TABLES.knight[from_sq as usize];
            let targets = attacks & !friendly & !enemy;

            for to_sq in BitboardIter(targets) {
                list.push(CompactMove::new_quiet(from_sq, to_sq, PieceType::Knight));
            }
        }
    }

    // =========================================================================
    // King moves
    // =========================================================================

    #[inline]
    fn generate_king_moves(&self, list: &mut MoveList, occupied: u64, friendly: u64, enemy: u64) {
        let king_bb = self.board.get_piece_bb(self.color, PieceType::King);
        if king_bb == 0 {
            return;
        }

        let from_sq = king_bb.trailing_zeros() as u8;
        let attacks = ATTACK_TABLES.king[from_sq as usize];
        let targets = attacks & !friendly;

        for to_sq in BitboardIter(targets) {
            let captured = self.captured_piece_type(to_sq, enemy);
            let move_type = if captured.is_some() {
                MoveType::Capture
            } else {
                MoveType::Quiet
            };
            list.push(CompactMove::new(
                from_sq,
                to_sq,
                PieceType::King,
                move_type,
                captured,
            ));
        }

        // Generate castling moves
        self.generate_castling(list, occupied, from_sq);
    }

    #[inline]
    fn generate_king_captures(&self, list: &mut MoveList, enemy: u64) {
        let king_bb = self.board.get_piece_bb(self.color, PieceType::King);
        if king_bb == 0 {
            return;
        }

        let from_sq = king_bb.trailing_zeros() as u8;
        let attacks = ATTACK_TABLES.king[from_sq as usize];
        let targets = attacks & enemy;

        for to_sq in BitboardIter(targets) {
            let captured = self.captured_piece_type(to_sq, enemy);
            list.push(CompactMove::new(
                from_sq,
                to_sq,
                PieceType::King,
                MoveType::Capture,
                captured,
            ));
        }
    }

    #[inline]
    fn generate_king_quiets(&self, list: &mut MoveList, occupied: u64, friendly: u64) {
        let king_bb = self.board.get_piece_bb(self.color, PieceType::King);
        if king_bb == 0 {
            return;
        }

        let from_sq = king_bb.trailing_zeros() as u8;
        let attacks = ATTACK_TABLES.king[from_sq as usize];
        let enemy = self.board.get_pieces_bb(self.color.other_color());
        let targets = attacks & !friendly & !enemy;

        for to_sq in BitboardIter(targets) {
            list.push(CompactMove::new_quiet(from_sq, to_sq, PieceType::King));
        }

        // Castling is a quiet move
        self.generate_castling(list, occupied, from_sq);
    }

    #[inline]
    fn generate_castling(&self, list: &mut MoveList, occupied: u64, king_sq: u8) {
        // Get opponent's attack map to check castling legality:
        // King must not be in check, and must not pass through or land on attacked squares
        let opponent_attacks = self.board.get_attack_map(self.color.other_color());
        let king_bb = sq_to_bb(king_sq);

        // Can't castle while in check
        if (king_bb & opponent_attacks) != 0 {
            return;
        }

        match self.color {
            Color::White => {
                // Kingside castling (e1 -> g1)
                if self.board.castle_kingside_white {
                    let f1 = sq_to_bb(pos_to_sq(1, 6));
                    let g1 = sq_to_bb(pos_to_sq(1, 7));
                    let rook_sq = sq_to_bb(pos_to_sq(1, 8));
                    let rook_bb = self.board.get_piece_bb(Color::White, PieceType::Rook);

                    // Squares must be empty AND king must not pass through/land on attacked squares
                    if (rook_bb & rook_sq) != 0
                        && (occupied & (f1 | g1)) == 0
                        && (opponent_attacks & (f1 | g1)) == 0
                    {
                        let to_sq = pos_to_sq(1, 7);
                        list.push(CompactMove::new(
                            king_sq,
                            to_sq,
                            PieceType::King,
                            MoveType::CastleKingside,
                            None,
                        ));
                    }
                }
                // Queenside castling (e1 -> c1)
                if self.board.castle_queenside_white {
                    let b1 = sq_to_bb(pos_to_sq(1, 2));
                    let c1 = sq_to_bb(pos_to_sq(1, 3));
                    let d1 = sq_to_bb(pos_to_sq(1, 4));
                    let rook_sq = sq_to_bb(pos_to_sq(1, 1));
                    let rook_bb = self.board.get_piece_bb(Color::White, PieceType::Rook);

                    // b1 only needs to be empty (rook passes through), not unattacked
                    // c1 and d1 must be unattacked (king passes through d1 and lands on c1)
                    if (rook_bb & rook_sq) != 0
                        && (occupied & (b1 | c1 | d1)) == 0
                        && (opponent_attacks & (c1 | d1)) == 0
                    {
                        let to_sq = pos_to_sq(1, 3);
                        list.push(CompactMove::new(
                            king_sq,
                            to_sq,
                            PieceType::King,
                            MoveType::CastleQueenside,
                            None,
                        ));
                    }
                }
            }
            Color::Black => {
                // Kingside castling (e8 -> g8)
                if self.board.castle_kingside_black {
                    let f8 = sq_to_bb(pos_to_sq(8, 6));
                    let g8 = sq_to_bb(pos_to_sq(8, 7));
                    let rook_sq = sq_to_bb(pos_to_sq(8, 8));
                    let rook_bb = self.board.get_piece_bb(Color::Black, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0
                        && (occupied & (f8 | g8)) == 0
                        && (opponent_attacks & (f8 | g8)) == 0
                    {
                        let to_sq = pos_to_sq(8, 7);
                        list.push(CompactMove::new(
                            king_sq,
                            to_sq,
                            PieceType::King,
                            MoveType::CastleKingside,
                            None,
                        ));
                    }
                }
                // Queenside castling (e8 -> c8)
                if self.board.castle_queenside_black {
                    let b8 = sq_to_bb(pos_to_sq(8, 2));
                    let c8 = sq_to_bb(pos_to_sq(8, 3));
                    let d8 = sq_to_bb(pos_to_sq(8, 4));
                    let rook_sq = sq_to_bb(pos_to_sq(8, 1));
                    let rook_bb = self.board.get_piece_bb(Color::Black, PieceType::Rook);

                    if (rook_bb & rook_sq) != 0
                        && (occupied & (b8 | c8 | d8)) == 0
                        && (opponent_attacks & (c8 | d8)) == 0
                    {
                        let to_sq = pos_to_sq(8, 3);
                        list.push(CompactMove::new(
                            king_sq,
                            to_sq,
                            PieceType::King,
                            MoveType::CastleQueenside,
                            None,
                        ));
                    }
                }
            }
        }
    }

    // =========================================================================
    // Rook moves
    // =========================================================================

    #[inline]
    fn generate_rook_moves(&self, list: &mut MoveList, occupied: u64, friendly: u64, enemy: u64) {
        let rooks = self.board.get_piece_bb(self.color, PieceType::Rook);

        for from_sq in BitboardIter(rooks) {
            let attacks = rook_attacks(from_sq, occupied);
            let targets = attacks & !friendly;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                let move_type = if captured.is_some() {
                    MoveType::Capture
                } else {
                    MoveType::Quiet
                };
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Rook,
                    move_type,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_rook_captures(&self, list: &mut MoveList, occupied: u64, enemy: u64) {
        let rooks = self.board.get_piece_bb(self.color, PieceType::Rook);

        for from_sq in BitboardIter(rooks) {
            let attacks = rook_attacks(from_sq, occupied);
            let targets = attacks & enemy;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Rook,
                    MoveType::Capture,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_rook_quiets(&self, list: &mut MoveList, occupied: u64, friendly: u64) {
        let rooks = self.board.get_piece_bb(self.color, PieceType::Rook);
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        for from_sq in BitboardIter(rooks) {
            let attacks = rook_attacks(from_sq, occupied);
            let targets = attacks & !friendly & !enemy;

            for to_sq in BitboardIter(targets) {
                list.push(CompactMove::new_quiet(from_sq, to_sq, PieceType::Rook));
            }
        }
    }

    // =========================================================================
    // Bishop moves
    // =========================================================================

    #[inline]
    fn generate_bishop_moves(&self, list: &mut MoveList, occupied: u64, friendly: u64, enemy: u64) {
        let bishops = self.board.get_piece_bb(self.color, PieceType::Bishop);

        for from_sq in BitboardIter(bishops) {
            let attacks = bishop_attacks(from_sq, occupied);
            let targets = attacks & !friendly;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                let move_type = if captured.is_some() {
                    MoveType::Capture
                } else {
                    MoveType::Quiet
                };
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Bishop,
                    move_type,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_bishop_captures(&self, list: &mut MoveList, occupied: u64, enemy: u64) {
        let bishops = self.board.get_piece_bb(self.color, PieceType::Bishop);

        for from_sq in BitboardIter(bishops) {
            let attacks = bishop_attacks(from_sq, occupied);
            let targets = attacks & enemy;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Bishop,
                    MoveType::Capture,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_bishop_quiets(&self, list: &mut MoveList, occupied: u64, friendly: u64) {
        let bishops = self.board.get_piece_bb(self.color, PieceType::Bishop);
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        for from_sq in BitboardIter(bishops) {
            let attacks = bishop_attacks(from_sq, occupied);
            let targets = attacks & !friendly & !enemy;

            for to_sq in BitboardIter(targets) {
                list.push(CompactMove::new_quiet(from_sq, to_sq, PieceType::Bishop));
            }
        }
    }

    // =========================================================================
    // Queen moves
    // =========================================================================

    #[inline]
    fn generate_queen_moves(&self, list: &mut MoveList, occupied: u64, friendly: u64, enemy: u64) {
        let queens = self.board.get_piece_bb(self.color, PieceType::Queen);

        for from_sq in BitboardIter(queens) {
            let attacks = queen_attacks(from_sq, occupied);
            let targets = attacks & !friendly;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                let move_type = if captured.is_some() {
                    MoveType::Capture
                } else {
                    MoveType::Quiet
                };
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Queen,
                    move_type,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_queen_captures(&self, list: &mut MoveList, occupied: u64, enemy: u64) {
        let queens = self.board.get_piece_bb(self.color, PieceType::Queen);

        for from_sq in BitboardIter(queens) {
            let attacks = queen_attacks(from_sq, occupied);
            let targets = attacks & enemy;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                list.push(CompactMove::new(
                    from_sq,
                    to_sq,
                    PieceType::Queen,
                    MoveType::Capture,
                    captured,
                ));
            }
        }
    }

    #[inline]
    fn generate_queen_quiets(&self, list: &mut MoveList, occupied: u64, friendly: u64) {
        let queens = self.board.get_piece_bb(self.color, PieceType::Queen);
        let enemy = self.board.get_pieces_bb(self.color.other_color());

        for from_sq in BitboardIter(queens) {
            let attacks = queen_attacks(from_sq, occupied);
            let targets = attacks & !friendly & !enemy;

            for to_sq in BitboardIter(targets) {
                list.push(CompactMove::new_quiet(from_sq, to_sq, PieceType::Queen));
            }
        }
    }

    // =========================================================================
    // Pawn moves
    // =========================================================================

    #[inline]
    fn generate_pawn_moves(&self, list: &mut MoveList, occupied: u64, enemy: u64) {
        self.generate_pawn_captures(list, enemy);
        self.generate_pawn_quiets(list, occupied);
    }

    #[inline]
    fn generate_pawn_captures(&self, list: &mut MoveList, enemy: u64) {
        let pawns = self.board.get_piece_bb(self.color, PieceType::Pawn);
        let color_idx = self.color as usize;
        let promo_rank = if self.color == Color::White { 7 } else { 0 }; // 0-indexed

        for from_sq in BitboardIter(pawns) {
            let attacks = ATTACK_TABLES.pawn[color_idx][from_sq as usize];
            let targets = attacks & enemy;

            for to_sq in BitboardIter(targets) {
                let captured = self.captured_piece_type(to_sq, enemy);
                let to_rank = to_sq / 8;

                if to_rank == promo_rank {
                    // Promotion captures
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoCaptureQueen,
                        captured,
                    ));
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoCaptureRook,
                        captured,
                    ));
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoCaptureBishop,
                        captured,
                    ));
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoCaptureKnight,
                        captured,
                    ));
                } else {
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::Capture,
                        captured,
                    ));
                }
            }

            // En passant
            if let Some(ep_target) = self.board.en_passant_target {
                let ep_sq = CompactMove::pos_to_sq(&ep_target);
                let ep_bb = sq_to_bb(ep_sq);
                if (attacks & ep_bb) != 0 {
                    // Captured pawn is on our rank at the ep file
                    list.push(CompactMove::new(
                        from_sq,
                        ep_sq,
                        PieceType::Pawn,
                        MoveType::EnPassantCapture,
                        Some(PieceType::Pawn),
                    ));
                }
            }
        }
    }

    #[inline]
    fn generate_pawn_quiets(&self, list: &mut MoveList, occupied: u64) {
        let pawns = self.board.get_piece_bb(self.color, PieceType::Pawn);
        let empty = !occupied;
        let (start_rank, promo_rank, push_shift): (u8, u8, i8) = match self.color {
            Color::White => (1, 7, 8),  // rank 2 (idx 1), rank 8 (idx 7), +8 squares
            Color::Black => (6, 0, -8), // rank 7 (idx 6), rank 1 (idx 0), -8 squares
        };

        for from_sq in BitboardIter(pawns) {
            let from_rank = from_sq / 8;

            // Single push
            let to_sq = (from_sq as i8 + push_shift) as u8;
            let to_bb = sq_to_bb(to_sq);

            if (empty & to_bb) != 0 {
                let to_rank = to_sq / 8;
                if to_rank == promo_rank {
                    // Quiet promotions
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoQueen,
                        None,
                    ));
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoRook,
                        None,
                    ));
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoBishop,
                        None,
                    ));
                    list.push(CompactMove::new(
                        from_sq,
                        to_sq,
                        PieceType::Pawn,
                        MoveType::PromoKnight,
                        None,
                    ));
                } else {
                    list.push(CompactMove::new_quiet(from_sq, to_sq, PieceType::Pawn));

                    // Double push from starting rank
                    if from_rank == start_rank {
                        let to_sq2 = (to_sq as i8 + push_shift) as u8;
                        let to_bb2 = sq_to_bb(to_sq2);

                        if (empty & to_bb2) != 0 {
                            list.push(CompactMove::new(
                                from_sq,
                                to_sq2,
                                PieceType::Pawn,
                                MoveType::DoublePawnPush,
                                None,
                            ));
                        }
                    }
                }
            }
        }
    }

    // =========================================================================
    // Helper methods
    // =========================================================================

    /// Get the piece type at a square if it's an enemy piece.
    #[inline(always)]
    fn captured_piece_type(&self, sq: u8, enemy: u64) -> Option<PieceType> {
        let sq_bb = sq_to_bb(sq);
        if (enemy & sq_bb) == 0 {
            return None;
        }

        let opp = self.color.other_color();
        if (self.board.get_piece_bb(opp, PieceType::Pawn) & sq_bb) != 0 {
            return Some(PieceType::Pawn);
        }
        if (self.board.get_piece_bb(opp, PieceType::Knight) & sq_bb) != 0 {
            return Some(PieceType::Knight);
        }
        if (self.board.get_piece_bb(opp, PieceType::Bishop) & sq_bb) != 0 {
            return Some(PieceType::Bishop);
        }
        if (self.board.get_piece_bb(opp, PieceType::Rook) & sq_bb) != 0 {
            return Some(PieceType::Rook);
        }
        if (self.board.get_piece_bb(opp, PieceType::Queen) & sq_bb) != 0 {
            return Some(PieceType::Queen);
        }
        if (self.board.get_piece_bb(opp, PieceType::King) & sq_bb) != 0 {
            return Some(PieceType::King);
        }
        None
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;

    /// Helper: check if a specific UCI move string is among moves generated by the compact generator
    fn compact_has_move(board: &Board, color: Color, from_sq: u8, to_sq: u8, move_type: MoveType) -> bool {
        let gen = CompactMoveGenerator::new(board, color);
        let mut list = MoveList::new();
        gen.generate_all(&mut list);
        for i in 0..list.len() {
            let mv = list.get(i);
            if mv.from_sq() == from_sq && mv.to_sq() == to_sq && mv.move_type() == move_type {
                return true;
            }
        }
        false
    }

    /// Helper: check if a specific castling move is among moves generated by the old-style generator
    fn legacy_has_castle(board: &Board, color: Color, flag: MoveFlag) -> bool {
        let mut gen = MoveGenerator::new(board, color);
        let moves = gen.collect();
        moves.iter().any(|m| m.move_flag == flag)
    }

    // =========================================================================
    // Castling: can't castle THROUGH check (king passes through attacked square)
    // =========================================================================

    #[test]
    fn white_cannot_castle_queenside_through_check_d1_attacked() {
        // Black rook on d8 attacks d1 — king can't pass through d1 for O-O-O
        // FEN from actual Lichess game Omivc0UO where the bug caused a resignation
        let board = Board::from_fen("2kr3r/1p2qp1p/p3bp2/2p1n2Q/4P3/1N2P3/PPP1B1PP/R3K2R w KQ - 5 17");
        let e1 = pos_to_sq(1, 5);
        let c1 = pos_to_sq(1, 3);
        let g1 = pos_to_sq(1, 7);

        // Queenside castling should be blocked (d1 attacked by Rd8)
        assert!(!compact_has_move(&board, Color::White, e1, c1, MoveType::CastleQueenside),
            "White should NOT be able to castle queenside when d1 is attacked");
        assert!(!legacy_has_castle(&board, Color::White, MoveFlag::CastleQueenside),
            "Legacy gen: White should NOT be able to castle queenside when d1 is attacked");

        // Kingside castling should still be allowed (f1, g1 not attacked)
        assert!(compact_has_move(&board, Color::White, e1, g1, MoveType::CastleKingside),
            "White SHOULD be able to castle kingside (f1, g1 clear and unattacked)");
        assert!(legacy_has_castle(&board, Color::White, MoveFlag::CastleKingside),
            "Legacy gen: White SHOULD be able to castle kingside");
    }

    #[test]
    fn white_cannot_castle_kingside_through_check_f1_attacked() {
        // Black bishop on c4 attacks f1 (e2 pawn removed to clear diagonal)
        let board = Board::from_fen("r3k2r/pppppppp/8/8/2b5/8/PPPP1PPP/R3K2R w KQkq - 0 1");
        let e1 = pos_to_sq(1, 5);
        let g1 = pos_to_sq(1, 7);
        let c1 = pos_to_sq(1, 3);

        assert!(!compact_has_move(&board, Color::White, e1, g1, MoveType::CastleKingside),
            "White should NOT castle kingside when f1 is attacked by Bc4");
        assert!(!legacy_has_castle(&board, Color::White, MoveFlag::CastleKingside),
            "Legacy: White should NOT castle kingside when f1 is attacked by Bc4");

        // Queenside should be fine (c1, d1 not attacked by Bc4)
        assert!(compact_has_move(&board, Color::White, e1, c1, MoveType::CastleQueenside),
            "White SHOULD be able to castle queenside");
        assert!(legacy_has_castle(&board, Color::White, MoveFlag::CastleQueenside),
            "Legacy: White SHOULD be able to castle queenside");
    }

    #[test]
    fn black_cannot_castle_queenside_through_check_d8_attacked() {
        // White rook on d1, d2 and d7 pawns removed so d-file is open — d8 attacked
        let board = Board::from_fen("r3k2r/ppp1pppp/8/8/8/8/PPP1PPPP/3RK2R b Kkq - 0 1");
        let e8 = pos_to_sq(8, 5);
        let c8 = pos_to_sq(8, 3);
        let g8 = pos_to_sq(8, 7);

        assert!(!compact_has_move(&board, Color::Black, e8, c8, MoveType::CastleQueenside),
            "Black should NOT castle queenside when d8 is attacked by Rd1");
        assert!(!legacy_has_castle(&board, Color::Black, MoveFlag::CastleQueenside),
            "Legacy: Black should NOT castle queenside when d8 is attacked");

        // Kingside should be fine
        assert!(compact_has_move(&board, Color::Black, e8, g8, MoveType::CastleKingside),
            "Black SHOULD be able to castle kingside");
        assert!(legacy_has_castle(&board, Color::Black, MoveFlag::CastleKingside),
            "Legacy: Black SHOULD be able to castle kingside");
    }

    #[test]
    fn black_cannot_castle_kingside_through_check_f8_attacked() {
        // White bishop on c5, e7+f7 pawns removed so diagonal c5-f8 is clear
        let board = Board::from_fen("r3k2r/pppp2pp/8/2B5/8/8/PPPPPPPP/R3K2R b KQkq - 0 1");
        let e8 = pos_to_sq(8, 5);
        let g8 = pos_to_sq(8, 7);

        assert!(!compact_has_move(&board, Color::Black, e8, g8, MoveType::CastleKingside),
            "Black should NOT castle kingside when f8 is attacked by Bc5");
        assert!(!legacy_has_castle(&board, Color::Black, MoveFlag::CastleKingside),
            "Legacy: Black should NOT castle kingside when f8 is attacked");
    }

    // =========================================================================
    // Castling: can't castle WHILE in check
    // =========================================================================

    #[test]
    fn white_cannot_castle_while_in_check() {
        // Black rook on e8 gives check to white king on e1
        let board = Board::from_fen("4r3/8/8/8/8/8/8/R3K2R w KQ - 0 1");
        let e1 = pos_to_sq(1, 5);
        let g1 = pos_to_sq(1, 7);
        let c1 = pos_to_sq(1, 3);

        assert!(!compact_has_move(&board, Color::White, e1, g1, MoveType::CastleKingside),
            "Can't castle kingside while in check");
        assert!(!compact_has_move(&board, Color::White, e1, c1, MoveType::CastleQueenside),
            "Can't castle queenside while in check");
        assert!(!legacy_has_castle(&board, Color::White, MoveFlag::CastleKingside));
        assert!(!legacy_has_castle(&board, Color::White, MoveFlag::CastleQueenside));
    }

    #[test]
    fn black_cannot_castle_while_in_check() {
        // White rook on e1 gives check to black king on e8
        let board = Board::from_fen("r3k2r/8/8/8/8/8/8/4R3 b kq - 0 1");
        let e8 = pos_to_sq(8, 5);
        let g8 = pos_to_sq(8, 7);
        let c8 = pos_to_sq(8, 3);

        assert!(!compact_has_move(&board, Color::Black, e8, g8, MoveType::CastleKingside),
            "Can't castle kingside while in check");
        assert!(!compact_has_move(&board, Color::Black, e8, c8, MoveType::CastleQueenside),
            "Can't castle queenside while in check");
        assert!(!legacy_has_castle(&board, Color::Black, MoveFlag::CastleKingside));
        assert!(!legacy_has_castle(&board, Color::Black, MoveFlag::CastleQueenside));
    }

    // =========================================================================
    // Castling: can't castle INTO check (destination square attacked)
    // =========================================================================

    #[test]
    fn white_cannot_castle_kingside_into_check() {
        // Black rook on g8, g2 pawn removed so g-file is open — g1 attacked
        let board = Board::from_fen("6r1/8/8/8/8/8/PPPPPP1P/R3K2R w KQ - 0 1");
        let e1 = pos_to_sq(1, 5);
        let g1 = pos_to_sq(1, 7);

        assert!(!compact_has_move(&board, Color::White, e1, g1, MoveType::CastleKingside),
            "Can't castle kingside into check (g1 attacked)");
        assert!(!legacy_has_castle(&board, Color::White, MoveFlag::CastleKingside));
    }

    #[test]
    fn white_cannot_castle_queenside_into_check() {
        // Black rook on c8, c2 pawn removed so c-file is open — c1 attacked
        let board = Board::from_fen("2r5/8/8/8/8/8/PP1PPPPP/R3K2R w KQ - 0 1");
        let e1 = pos_to_sq(1, 5);
        let c1 = pos_to_sq(1, 3);

        assert!(!compact_has_move(&board, Color::White, e1, c1, MoveType::CastleQueenside),
            "Can't castle queenside into check (c1 attacked)");
        assert!(!legacy_has_castle(&board, Color::White, MoveFlag::CastleQueenside));
    }

    // =========================================================================
    // Castling: b-file attack should NOT block queenside castling
    // =========================================================================

    #[test]
    fn white_can_castle_queenside_when_b1_attacked() {
        // b1 attacked but that's fine — only c1 and d1 matter for the king's path
        let board = Board::from_fen("1r6/8/8/8/8/8/PPPPPPPP/R3K2R w KQ - 0 1");
        let e1 = pos_to_sq(1, 5);
        let c1 = pos_to_sq(1, 3);

        assert!(compact_has_move(&board, Color::White, e1, c1, MoveType::CastleQueenside),
            "White SHOULD castle queenside even when b1 is attacked (rook passes through, not king)");
        assert!(legacy_has_castle(&board, Color::White, MoveFlag::CastleQueenside));
    }

    #[test]
    fn black_can_castle_queenside_when_b8_attacked() {
        let board = Board::from_fen("r3k2r/pppppppp/8/8/8/8/8/1R6 b kq - 0 1");
        let e8 = pos_to_sq(8, 5);
        let c8 = pos_to_sq(8, 3);

        assert!(compact_has_move(&board, Color::Black, e8, c8, MoveType::CastleQueenside),
            "Black SHOULD castle queenside even when b8 is attacked");
        assert!(legacy_has_castle(&board, Color::Black, MoveFlag::CastleQueenside));
    }

    // =========================================================================
    // Castling: valid castling should work
    // =========================================================================

    #[test]
    fn white_can_castle_both_sides_when_clear() {
        let board = Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1");
        let e1 = pos_to_sq(1, 5);

        assert!(compact_has_move(&board, Color::White, e1, pos_to_sq(1, 7), MoveType::CastleKingside));
        assert!(compact_has_move(&board, Color::White, e1, pos_to_sq(1, 3), MoveType::CastleQueenside));
        assert!(legacy_has_castle(&board, Color::White, MoveFlag::CastleKingside));
        assert!(legacy_has_castle(&board, Color::White, MoveFlag::CastleQueenside));
    }

    #[test]
    fn black_can_castle_both_sides_when_clear() {
        let board = Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1");
        let e8 = pos_to_sq(8, 5);

        assert!(compact_has_move(&board, Color::Black, e8, pos_to_sq(8, 7), MoveType::CastleKingside));
        assert!(compact_has_move(&board, Color::Black, e8, pos_to_sq(8, 3), MoveType::CastleQueenside));
        assert!(legacy_has_castle(&board, Color::Black, MoveFlag::CastleKingside));
        assert!(legacy_has_castle(&board, Color::Black, MoveFlag::CastleQueenside));
    }
}
