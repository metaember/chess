use crate::movegen::{MoveGenerator, PinnedPiece};
use crate::types::*;

pub const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

pub struct Board {
    pub pieces: Vec<Piece>,
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
            Some(Position::from_algebraic(parts[3].get(0..2).unwrap()))
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

    pub fn from_fen_no_moves(fen_string: &str) -> Board {
        Board::from_fen(format!("{} 0 1", fen_string).as_str())
    }

    pub fn new() -> Board {
        Board::from_fen(STARTING_POSITION_FEN)
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

    pub fn check_for_insufficient_material(&self) -> Option<Status> {
        let white_pieces: Vec<_> = self
            .pieces
            .iter()
            .filter(|p| p.color == Color::White)
            .collect();
        let black_pieces: Vec<_> = self
            .pieces
            .iter()
            .filter(|p| p.color == Color::Black)
            .collect();

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

        let black_insufficient = black_pieces.len() == 1
            || black_pieces.len() == 2 && { black_knight_count == 1 || black_bishop_count == 1 };

        let white_insufficient = white_pieces.len() == 1
            || white_pieces.len() == 2 && { white_knight_count == 1 || white_bishop_count == 1 };

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
                    .filter(|p| {
                        p != &old_rook
                            && p != &selected_move.piece
                            && match selected_move.captured {
                                Some(cap_piece) => p != &cap_piece, // .. maybe remove the captured piece ..
                                None => true,
                            }
                    })
                    .collect();

                // .. add the new pieces
                pieces.push(new_king);
                pieces.push(new_rook);

                // update the board map
                board_to_piece[(selected_move.from.rank - 1) as usize]
                    [(selected_move.from.file - 1) as usize] = None;
                board_to_piece[(old_rook.position.rank - 1) as usize]
                    [(old_rook.position.file - 1) as usize] = None;
                if let Some(captured) = selected_move.captured {
                    // In the case of en passant, the captures sqaure is not the target square of the move
                    board_to_piece[(captured.position.rank - 1) as usize]
                        [(captured.position.file - 1) as usize] = None;
                }
                board_to_piece[(selected_move.to.rank - 1) as usize]
                    [(selected_move.to.file - 1) as usize] = Some(new_king);
                board_to_piece[(new_rook.position.rank - 1) as usize]
                    [(new_rook.position.file - 1) as usize] = Some(new_rook);
            }
            _ => {
                // not a castle
                pieces = pieces
                    .into_iter()
                    .filter(|p| {
                        // remove the piece that moved ...
                        p != &selected_move.piece
                            && match selected_move.captured {
                                Some(cap_piece) => p != &cap_piece, // .. maybe remove the captured piece ..
                                None => true,
                            }
                    })
                    .collect();

                pieces.push(new_piece);

                // update the board map
                board_to_piece[(selected_move.from.rank - 1) as usize]
                    [(selected_move.from.file - 1) as usize] = None;
                if let Some(captured) = selected_move.captured {
                    // In the case of en passant, the captures sqaure is not the target square of the move
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
        // TODO: update the other states too
        Board {
            pieces,
            active_color: self.active_color.other_color(),
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
            castle_kingside_white: self.castle_kingside_white && castle_kingside_white,
            castle_queenside_white: self.castle_queenside_white && castle_queenside_white,
            castle_kingside_black: self.castle_kingside_black && castle_kingside_black,
            castle_queenside_black: self.castle_queenside_black && castle_queenside_black,
            en_passant_target: if let MoveFlag::DoublePawnPush(ep_target) = selected_move.move_flag
            {
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
        let opponent_potential_moves = self.get_all_pseudo_moves(color.other_color(), true);

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

        // TODO: reimplement the piece subsettting if we're in double check

        // let mut my_possible_moves: Vec<_> = pieces_to_compute_moves_for
        //     .into_iter()
        //     .map(|p| self.get_pseudo_moves(&p, false))
        //     .flatten()
        //     // castling out of check is not allowed
        //     .filter(|m| !(is_in_check && (m.move_flag == MoveFlag::CastleKingside || m.move_flag == MoveFlag::CastleQueenside)))
        //     .collect();

        // TODO OPTIM: make sure that this return of the vec is not a copy
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

        let pins = self.moveget_pins(color);

        let mut castle_kingside_obstructed = false;
        let mut castle_queenside_obstructed = false;

        let castle_rank = if *color == Color::White { 1 } else { 8 };

        for Move {
            piece: _,
            from: _,
            to,
            captured: _,
            move_flag: _,
        } in opponent_potential_moves.iter()
        {
            if !castle_queenside_obstructed
                && to.rank == castle_rank
                && (to.file == 3 || to.file == 4)
            {
                castle_queenside_obstructed = true;
            }
            if !castle_kingside_obstructed
                && to.rank == castle_rank
                && (to.file == 7 || to.file == 6)
            {
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
                    if pinned_piece == m.piece {
                        // check if the move is allowed by the pin
                        // TODO OPTIMISE: this is a nested loop, can we rewrite another way?
                        can_move_to = valid_responses.iter().find(|&&x| x == m.to).is_some();
                    }
                }
                can_move_to
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
                return Err(Status::Checkmate(color.other_color()));
            } else {
                return Err(Status::Stalemate);
            }
        };
        Ok(my_possible_moves)
    }

    pub fn get_all_pseudo_moves(&self, color: Color, observed_mode: bool) -> Vec<Move> {
        let move_generator = MoveGenerator::new(self, color);
        move_generator.collect()
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
}

#[cfg(test)]
mod tests {
    use super::{
        Board, Color, Move, MoveFlag, Piece, PieceType, Position, Status, PIECES_CAN_PROMOTE_TO,
        STARTING_POSITION_FEN,
    };
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
        let pins = b.get_pins(&Color::White);
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
        let pins = b.get_pins(&Color::White);
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
        let pins = b.get_pins(&Color::White);
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
        let pins = b.get_pins(&Color::White);
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
        pins[0]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[0].valid_responses.len(), 2);
        for pos in vec!["d7", "d8"] {
            assert!(pins[0]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
        }

        assert_eq!(pins[1].piece.piece_type, PieceType::Rook);
        assert_eq!(pins[1].piece.position, Position { rank: 4, file: 4 });
        pins[1]
            .valid_responses
            .iter()
            .for_each(|p| println!("{}", p.to_algebraic()));
        assert_eq!(pins[1].valid_responses.len(), 2);
        for pos in vec!["d2", "d3"] {
            assert!(pins[1]
                .valid_responses
                .contains(&Position::from_algebraic(pos)));
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
        let pins = b.get_pins(&Color::White);
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
        let pins = b.get_pins(&Color::White);
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
        let pins = b.get_pins(&Color::White);
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
        let pins = b.get_pins(&Color::Black);
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

        let pins = b.get_pins(&Color::Black);
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
            let m = Move {
                piece: Piece::from_algebraic('P', "d7"),
                from: Position::from_algebraic("d7"),
                to: Position::from_algebraic("d8"),
                captured: None,
                move_flag: MoveFlag::Promotion(*pt),
            };
            assert!(legal_moves.contains(&m));
            let new_b = b.execute_move(&m);
            assert!(new_b
                .pieces
                .contains(&Piece::from_algebraic(pt.to_char(), "d8")));
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

        let black_pseudo_moves = b.get_all_pseudo_moves(&b.get_active_color(), true);
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

        let pins = b.get_pins(&Color::Black);
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

        let pins = b.get_pins(&Color::Black);
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

        let pins = b.get_pins(&Color::Black);
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
}
