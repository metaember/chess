use crate::types::{Piece, Position};

use crate::board::Board;
use crate::types::*;

const MAX_MOVES: usize = 218;

/// The squares observed by the pawn.
/// One of these should not be
struct PawnObserved {
    left: Option<Vec<Move>>,
    right: Option<Vec<Move>>,
}

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
        self.collect_with_mode(false)
    }

    pub fn collect_observed(&mut self) -> Vec<Move> {
        self.collect_with_mode(true)
    }

    fn collect_with_mode(&mut self, observed_mode: bool) -> Vec<Move> {
        let dominated_pieces: Vec<Piece> = self
            .board
            .pieces
            .iter()
            .filter(|p| p.color == self.color)
            .copied()
            .collect();
        for piece in dominated_pieces.iter() {
            self.get_pseudo_moves(piece, observed_mode);
        }
        std::mem::take(&mut self.moves)
    }

    /// Get a vector of all pseudo valid moves for the side `color`.
    ///
    /// We define a pseudo valid move to be a move that obeys the piece move directions,
    /// stays on the board, does not skip over pieces, etc, but does not check for
    /// checks, pins etc.
    ///
    /// If `bserved_mode` is True, then we check all squares that are attacked by `color`,
    /// including squares that we have pieces of our own color on, and including squares that
    /// are "past" the opponent king for sliding pieces.

    /// Get a vector of pseudo valid moves for the piece `piece`.
    fn get_pseudo_moves(&mut self, piece: &Piece, observed_mode: bool) {
        match piece.piece_type {
            PieceType::Pawn => {
                if observed_mode {
                    // In observed mode, generate all diagonal squares the pawn could attack
                    let pos = &piece.position;
                    let next_rank = pawn_step_forward(pos.rank, piece.color);

                    // Left diagonal
                    if pos.file > 1 {
                        let left_pos = Position { rank: next_rank, file: pos.file - 1 };
                        let captured = self.board.piece_at_position(&left_pos).copied();
                        self.moves.push(Move {
                            piece: *piece,
                            from: piece.position,
                            to: left_pos,
                            captured,
                            move_flag: MoveFlag::Regular,
                        });
                    }
                    // Right diagonal
                    if pos.file < 8 {
                        let right_pos = Position { rank: next_rank, file: pos.file + 1 };
                        let captured = self.board.piece_at_position(&right_pos).copied();
                        self.moves.push(Move {
                            piece: *piece,
                            from: piece.position,
                            to: right_pos,
                            captured,
                            move_flag: MoveFlag::Regular,
                        });
                    }
                } else {
                    self.get_pseudo_pawn_pushes(piece);
                    self.get_pseudo_pawn_captures(piece);
                }
            }
            PieceType::Rook => self.get_pseudo_rook_moves(piece, observed_mode),
            PieceType::Knight => self.get_pseudo_knight_moves(piece, observed_mode),
            PieceType::Bishop => self.get_pseudo_bishop_moves(piece, observed_mode),
            PieceType::Queen => {
                self.get_pseudo_bishop_moves(piece, observed_mode);
                self.get_pseudo_rook_moves(piece, observed_mode);
            }
            PieceType::King => self.get_pseudo_king_moves(piece, observed_mode),
        };
    }

    /// Squares the pawn *could* capture if there is a piece there
    fn get_pawn_observed_squares(&mut self, piece: &Piece) -> PawnObserved {
        let pos = &piece.position;

        if (piece.color == Color::White && piece.position.rank >= 8)
            || (piece.color == Color::Black && piece.position.rank <= 1)
        {
            panic!("Pawn is on a promotion square, should not move from here.")
        }

        let next_rank = pawn_step_forward(pos.rank, piece.color);
        let promotion_rank = if piece.color == Color::White { 8 } else { 1 };
        let ep_rank = if piece.color == Color::White { 6 } else { 3 };

        let left_pos = Position {
            rank: next_rank,
            file: pos.file - 1,
        };
        let left_move = build_pawn_move(self.board, *piece, left_pos);

        let right_pos = Position {
            rank: next_rank,
            file: pos.file + 1,
        };
        let right_move = build_pawn_move(self.board, *piece, right_pos);

        return PawnObserved {
            left: left_move,
            right: right_move,
        };
    }

    /// Get the observed squares for the pawn, and filter out the ones with
    /// a friendly piece there
    fn get_pseudo_pawn_captures(&mut self, pawn: &Piece) {
        let observed = self.get_pawn_observed_squares(pawn);

        if let Some(left) = observed.left {
            for m in left {
                if m.captured.is_some_and(|p| p.color != pawn.color) {
                    self.moves.push(m)
                }
            }
        }

        if let Some(right) = observed.right {
            for m in right {
                if m.captured.is_some_and(|p| p.color != pawn.color) {
                    self.moves.push(m)
                }
            }
        }
    }

    /// get the valid pushes for a pawn
    fn get_pseudo_pawn_pushes(&mut self, piece: &Piece) {
        let pos = &piece.position;

        if (piece.color == Color::White && piece.position.rank >= 8)
            || (piece.color == Color::Black && piece.position.rank <= 1)
        {
            panic!("Pawn is on a promotion square, should not move from here.")
        }

        // move one square forward, requires no piece there
        let one_step = pawn_step_forward(pos.rank, piece.color);
        let one_step_pos = Position {
            rank: one_step,
            file: piece.position.file,
        };

        if let Some(moves) = build_pawn_move(self.board, *piece, one_step_pos) {
            self.moves.extend(moves);
        } else {
            // If one step is blocked, can't do two step either
            return;
        }

        // potential double push at start pos
        let two_step = pawn_step_forward(one_step, piece.color);
        let two_step_pos = Position {
            rank: two_step,
            file: piece.position.file,
        };
        if let Some(moves) = build_pawn_move(self.board, *piece, two_step_pos) {
            self.moves.extend(moves);
        }
    }

    fn get_pseudo_rook_moves(&mut self, piece: &Piece, observed_mode: bool) {
        let pos = &piece.position;

        // up
        for rank in (pos.rank + 1)..=8 {
            let candidate = Position {
                rank,
                file: pos.file,
            };
            let potential_move = self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    self.moves.push(Move {
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
            let potential_move = self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    self.moves.push(Move {
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
            let potential_move = self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    self.moves.push(Move {
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
            let potential_move = self.check_move_target(piece, &candidate, observed_mode);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    self.moves.push(Move {
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
    }

    fn get_pseudo_bishop_moves(&mut self, piece: &Piece, observed_mode: bool) {
        let pos = &piece.position;

        let directions = [(1, 1), (1, -1), (-1, 1), (-1, -1)];

        for direction in directions {
            let mut cur_rank = pos.rank as i8 + direction.0;
            let mut cur_file = pos.file as i8 + direction.1;

            while 1 <= cur_rank && cur_rank <= 8 && 1 <= cur_file && cur_file <= 8 {
                let candidate = Position {
                    rank: cur_rank as u8,
                    file: cur_file as u8,
                };
                let potential_move = self.check_move_target(piece, &candidate, observed_mode);
                match potential_move {
                    PotentialMove::Invalid => break,
                    PotentialMove::Valid(maybe_other) => {
                        self.moves.push(Move {
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
        match self.board.piece_at_position(candidate_pos) {
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
    fn get_pseudo_knight_moves(&mut self, piece: &Piece, observed_mode: bool) {
        for (rank_delta, file_delta) in
            std::iter::zip([-2, -2, -1, -1, 1, 1, 2, 2], [-1, 1, -2, 2, -2, 2, -1, 1])
        {
            let future_rank: i8 = piece.position.rank as i8 + rank_delta;
            let future_file: i8 = piece.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                let future_rank = future_rank as u8;
                let future_file = future_file as u8;
                let maybe_other_piece = self.board.piece_at(future_rank, future_file);
                if maybe_other_piece.is_some_and(|p| p.color == piece.color && !observed_mode) {
                    continue;
                } else {
                    self.moves.push(Move {
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
    }

    fn get_pseudo_king_moves(&mut self, king: &Piece, observed_mode: bool) {
        for (rank_delta, file_delta) in std::iter::zip(
            [-1, -1, -1, 0, 0, 1, 1, 1],
            [-1, 0, 1, -1, 1, -1, 0, 1],
        ) {
            let future_rank: i8 = king.position.rank as i8 + rank_delta;
            let future_file: i8 = king.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                let maybe_other_piece = self.board.piece_at(future_rank as u8, future_file as u8);
                match maybe_other_piece {
                    Some(other_piece) => match king.color == other_piece.color {
                        true => {
                            // same color, invalid move
                            if observed_mode {
                                // this is not a legal move, we return it when `observed_mode` is true
                                // to allow us to check if a friendly piece is protected.
                                self.moves.push(Move {
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
                        false => self.moves.push(Move {
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
                    None => self.moves.push(Move {
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
                if self.board.castle_kingside_white
                    // piece at rook position is our rook, not captured or anything
                    && self.board.piece_at(1, 8).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::White)
                    // the way is free
                    && self.board.piece_at(1, 7).is_none()
                    && self.board.piece_at(1, 6).is_none()
                {
                    self.moves.push(Move {
                        piece: *king,
                        from: king.position,
                        to: Position { rank: 1, file: 7 },
                        captured: None,
                        move_flag: MoveFlag::CastleKingside,
                    })
                }
                if self.board.castle_queenside_white
                    // piece at rook position is our rook, not captured or anything
                    && self.board.piece_at(1, 1).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::White)
                    // the way is free
                    && self.board.piece_at(1, 2).is_none()
                    && self.board.piece_at(1, 3).is_none()
                    && self.board.piece_at(1, 4).is_none()
                {
                    self.moves.push(Move {
                        piece: *king,
                        from: king.position,
                        to: Position { rank: 1, file: 3 },
                        captured: None,
                        move_flag: MoveFlag::CastleQueenside,
                    })
                };
            }
            Color::Black => {
                if self.board.castle_kingside_black
                    // piece at rook position is our rook, not captured or anything
                    && self.board.piece_at(8, 8).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::Black)
                    // the way is free
                    && self.board.piece_at(8, 7).is_none()
                    && self.board.piece_at(8, 6).is_none()
                {
                    self.moves.push(Move {
                        piece: *king,
                        from: king.position,
                        to: Position { rank: 8, file: 7 },
                        captured: None,
                        move_flag: MoveFlag::CastleKingside,
                    })
                }
                if self.board.castle_queenside_black
                    // piece at rook position is our rook, not captured or anything
                    && self.board.piece_at(8, 1).is_some_and(|p| p.piece_type == PieceType::Rook && p.color == Color::Black)
                    // the way is free
                    && self.board.piece_at(8, 2).is_none()
                    && self.board.piece_at(8, 3).is_none()
                    && self.board.piece_at(8, 4).is_none()
                {
                    self.moves.push(Move {
                        piece: *king,
                        from: king.position,
                        to: Position { rank: 8, file: 3 },
                        captured: None,
                        move_flag: MoveFlag::CastleQueenside,
                    })
                };
            }
        };
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

fn build_pawn_move(board: &Board, piece: Piece, to: Position) -> Option<Vec<Move>> {
    let promotion_rank = if piece.color == Color::White { 8 } else { 1 };
    let ep_rank = if piece.color == Color::White { 6 } else { 3 };

    if to.file == 0 || to.file == 9 {
        // no moving off the edge of the board
        return None;
    }

    // pushes
    if to.file == piece.position.file {
        if to.rank == pawn_step_forward(piece.position.rank, piece.color) {
            // regular push
            if board.piece_at(to.rank, to.file).is_some() {
                // can't push if there is a piece there
                return None;
            }
            // promotion push
            if to.rank == promotion_rank {
                return Some(
                    PIECES_CAN_PROMOTE_TO
                        .iter()
                        .map(|piece_type| Move {
                            piece,
                            from: piece.position,
                            to,
                            captured: None,
                            move_flag: MoveFlag::Promotion(*piece_type),
                        })
                        .collect(),
                );
            }
            // regular one-step push (non-promotion)
            return Some(vec![Move {
                piece,
                from: piece.position,
                to,
                captured: None,
                move_flag: MoveFlag::Regular,
            }]);
        }
        // two step push from start pos only
        let start_rank = if piece.color == Color::White { 2 } else { 7 };
        let one_step = pawn_step_forward(piece.position.rank, piece.color);
        let two_step = pawn_step_forward(one_step, piece.color);

        if piece.position.rank == start_rank
            && board.piece_at(one_step, piece.position.file).is_none()
            && board.piece_at(two_step, piece.position.file).is_none()
        {
            return Some(vec![Move {
                piece: piece,
                from: piece.position,
                to,
                captured: None,
                move_flag: MoveFlag::DoublePawnPush(Position {
                    rank: one_step,
                    file: piece.position.file,
                }),
            }]);
        }
        return None;
    }

    // captures
    if let Some(other_piece) = board.piece_at(to.rank, to.file) {
        if to.rank == promotion_rank {
            return Some(
                PIECES_CAN_PROMOTE_TO
                    .iter()
                    .map(|piece_type| Move {
                        piece,
                        from: piece.position,
                        to,
                        captured: Some(*other_piece),
                        move_flag: MoveFlag::Promotion(*piece_type),
                    })
                    .collect(),
            );
        } else {
            return Some(vec![Move {
                piece,
                from: piece.position,
                to,
                captured: Some(*other_piece),
                move_flag: MoveFlag::Regular,
            }]);
        }
    }

    // ep captures
    if to.rank == ep_rank
        && board.en_passant_target
            == Some(Position {
                rank: to.rank,
                file: to.file,
            })
    {
        // En passant capture
        let captured_piece = board
            .piece_at(piece.position.rank, to.file)
            .expect("Should have piece at the en passant target");
        if captured_piece.piece_type != PieceType::Pawn {
            panic!("En passant capture should be a pawn")
        }

        return Some(vec![Move {
            piece: piece,
            from: piece.position,
            to,
            captured: Some(*captured_piece),
            move_flag: MoveFlag::EnPassantCapture,
        }]);
    };
    None
}

/// Direction of pawn movement for given color
pub fn pawn_step_forward(rank: u8, color: Color) -> u8 {
    match color {
        Color::White => rank + 1,
        Color::Black => rank - 1,
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
