use rand::prelude::*;

const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Debug, PartialEq, Eq)]
enum Color {
    White,
    Black
}

impl Color {
    fn from_char(c: char) -> Color {
        if c == 'w' {
            Color::White    
        } else if c == 'b' {
            Color::Black
        } else {
            panic!("Color string must be either `b` or `w`.")
        }
    }
    fn from_case(c: char) -> Color {
        if c.is_uppercase(){
            Color::White
        } else if c.is_lowercase() {
            Color::Black
        } else {
            panic!("Color char must be either upper or lowercase.")
        }
    }

    fn other_color(&self) -> Color {
        if *self == Color::White {Color::Black} else {Color::White}
    }
}


#[derive(Debug, PartialEq, Eq)]
enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl PieceType {
    fn from_char(c: char) -> PieceType{
        match c.to_lowercase().next().unwrap() {
            'p' => PieceType::Pawn,
            'r' => PieceType::Rook,
            'n' => PieceType::Knight,
            'b' => PieceType::Bishop,
            'q' => PieceType::Queen,
            'k' => PieceType::King,
            other => panic!("Unrecognized piece type {other}.")
        }
    }

    /// Is the piece a sliding piece (one which can move multiple square in a given direction)
    fn is_sliding(&self) -> bool {
        match *self {
            PieceType::Rook | PieceType::Bishop | PieceType::Queen => true,
            _ => false,
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
struct Position {
    rank: u8, // row, 1 is start
    file: u8, // col, 1 is start
}

impl Position {
    fn from_algebraic(s: &str) -> Position {
        if s.len() != 2{
            panic!("Algebraic notation must be of length 2.")
        }

        let mut char_iter = s.chars();
        let rank_char = char_iter.next().unwrap();
        let file_char = char_iter.next().unwrap();

        let rank = 1 + rank_char as u8 - 'a' as u8;
        let file = file_char as u8 - '0' as u8;

        Position {rank, file}
    }
}


#[derive(Debug, PartialEq, Eq)]
struct Piece {
    color: Color,
    piece_type: PieceType,
    position: Position,
}

impl Piece {
    fn from_algebraic(piece: char, position: &str) -> Piece {
        Piece {
            color: Color::from_case(piece),
            piece_type: PieceType::from_char(piece),
            position: Position::from_algebraic(position)
        }
    }
    fn from_rank_file(piece: char, rank: u8, file: u8) -> Piece {
        Piece {
            color: Color::from_case(piece),
            piece_type: PieceType::from_char(piece),
            position: Position { rank, file }
        }
    }

    /// lists the possible moves, accounting for board edges but 
    /// not for piece occlusion or pins or checks.
    fn candidate_moves(&self) -> Vec<Position> {
        // These are the possible moves, not accounting for the board edgegs
        let candidates = match self.piece_type {
            PieceType::Pawn => self.pawn_moves(),
            PieceType::Rook => self.rook_moves(),
            PieceType::Knight => self.knight_moves(),
            PieceType::Bishop => self.bishop_moves(),
            PieceType::King => self.king_moves(),
            PieceType::Queen => {
                let mut moves = self.rook_moves();
                moves.append(&mut self.bishop_moves());
                moves
            }
        };

        candidates
    }

    fn pawn_moves(&self) -> Vec<Position> {
        let mut moves = vec![Position{
            rank: if self.color == Color::White {self.position.rank + 1} else {self.position.rank - 1},
            file: self.position.file
        }];
        if self.position.rank == 2 && self.color == Color::White {
            // we are at the start position so we can also move 2 squares
            moves.push(Position{rank: 4, file: self.position.file}); 
        } else if self.position.rank == 7 && self.color == Color::Black {
            // we are at the start position so we can also move 2 squares
            moves.push(Position{rank: 5, file: self.position.file}); 
        }
        moves 
    }

    fn pawn_captures(&self) -> Vec<Position> {
        vec![]
    }


    fn rook_moves(&self) -> Vec<Position> {
        let mut moves: Vec<Position> = vec![];
        let mut vertical: Vec<Position> = (1u8..9)
            .filter(|rank| *rank != self.position.rank)
            .map(move |rank| Position{rank, file:self.position.file})
            .collect();


        let mut horizontal: Vec<Position> = (1u8..9)
            .filter(|file| *file != self.position.file)
            .map(move |file| Position{rank: self.position.rank, file})
            .collect();

        moves.append(&mut vertical);
        moves.append(&mut horizontal);
        moves
    }
    
    fn knight_moves(&self) -> Vec<Position> {
        let mut moves: Vec<Position> = vec![];

        for (rank_delta, file_delta) in std::iter::zip(
            [-2, -2, -1, -1, 1, 1, 2, 2],
            [-1, 1, -2, 2, -2, 2, -1, 1]
        ) {
            let future_rank: i8 = self.position.rank as i8 + rank_delta;
            let future_file: i8 = self.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                moves.push(Position { rank: future_rank as u8, file: future_file  as u8});
            }
        }
        moves
    }
    
    fn bishop_moves(&self) -> Vec<Position> {
        let mut moves: Vec<Position> = vec![];

        for i in 1u8..8 {
            if self.position.rank + i <= 8 && self.position.file + i <= 8 {
                moves.push(Position{rank: self.position.rank + i, file: self.position.file + i});
                continue;
            }
            break;
        }
        for i in 1u8..8 {
            if self.position.rank - i >= 1 && self.position.file - i >= 1 {
                moves.push(Position{rank: self.position.rank - i, file: self.position.file - i});
                continue;
            }
            break;
        }

        for i in 1u8..8 {
            if self.position.rank + i <= 8 && self.position.file - i >= 1 {
                moves.push(Position{rank: self.position.rank + i, file: self.position.file - i});
                continue;
            }
            break;
        }
        for i in 1u8..8 {
            if self.position.rank - i >= 1 && self.position.file + i <= 8 {
                moves.push(Position{rank: self.position.rank - i, file: self.position.file + i});
                continue;
            }
            break;
        }
        moves
    }

    fn king_moves(&self) -> Vec<Position> {
        let mut moves: Vec<Position> = vec![];

        for (rank_delta, file_delta) in std::iter::zip(
            [-1, -1, -1, 0, 0, 0, 1, 1, 1],
            [-1, 0, 1, -1, 0, 1, -1, 0, 1]
        ) {
            let future_rank: i8 = self.position.rank as i8 + rank_delta;
            let future_file: i8 = self.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                moves.push(Position { rank: future_rank as u8, file: future_file  as u8});
            }
        }
        moves
    }

    fn king_castles(&self)  -> Vec<Position> {
        vec![]
    }


    fn filter_moves_inside_board(squares: Vec<&Position>) -> Vec<&Position> {
        squares.into_iter()
            .filter(|p| p.rank >= 1 && p.rank <= 8 && p.file >= 1 && p.file <= 8)
            .collect()
    }
    
    fn display_piece_moves(&self) {
        let moves = self.candidate_moves();
        for r in (1..=8).rev() {
            for f in 1..9 {
                if self.position.rank ==r && self.position.file == f {
                    print!("{} ", piece_to_str(self))
                } else if moves.contains(&Position{rank:r, file:f}) {
                    print!("X ")
                } else {
                    print!(". ")
                }
            }
            println!("");
        }
    }
    
}

/// Potential move, either valid, in which case has a bool inicating
/// if it's a capture
enum PotentialMove<'a> {
    Valid(Option<&'a Piece>),
    Invalid
}

impl<'a> PotentialMove<'a> {
    fn continue_search_in_direction(&self) -> bool{
        match self {
            PotentialMove::Invalid => false,
            PotentialMove::Valid(Some(_)) => false,
            PotentialMove::Valid(None) => true,
        }
    }
}

struct Move<'a> {
    piece: &'a Piece,
    from: &'a Position,
    to: Position,
    captured: Option<&'a Piece>
}


struct Board {
    pieces: Vec<Piece>,
    // who's move it is
    active_color: Color,
    castle_kingside_white: bool,
    castle_queenside_white: bool,
    castle_kingside_black: bool,
    castle_queenside_black: bool,
    en_passant_target: Option<Position>,
    // number of half moves since last capture or parwn advance
    halfmove_clock: u32, 
    // number of full moves. Starts at 1, and gets incremented after every black move
    fullmove_clock: u32,
}

impl Board {
    fn from_fen(fen_string: &str) -> Board {
        if fen_string.chars().filter(|c| *c == ' ').count() != 5 {
            panic!("Fen string must have 6 fields, space delimited")
        };
        let parts: Vec<&str> = fen_string.splitn(6, " ").collect();
        
        assert!(parts.len() == 6);

        let piece_data = parts[0];
        let mut pieces: Vec<Piece>  = vec![];
        let mut rank = 8;
        let mut file = 1;
        for piece_char in piece_data.chars() {
            if piece_char.is_alphabetic() {
                pieces.push(Piece::from_rank_file(piece_char, rank, file));
                file += 1;
            }
            else if piece_char.is_numeric() {
                file += piece_char as u8 - '0' as u8;
            }
            else if piece_char == '/' {
                rank -= 1;
                file = 1;
            }
            else {
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
            Some(Position::from_algebraic(parts[3].get(0..3).unwrap()))
        };

        let halfmove_clock: u32 = parts
            [4]
            .parse()
            .expect("Halfmove clock should be an u32");
        
        let fullmove_clock: u32 = parts
            [5]
            .parse()
            .expect("Fullmove clock should be a u32");

        Board {
            pieces, active_color, castle_kingside_white, castle_queenside_white, castle_kingside_black, castle_queenside_black, en_passant_target, halfmove_clock, fullmove_clock
        }
    }

    fn piece_at(&self, rank: u8, file: u8) -> Option<&Piece>{
        // TODO: speed this up by storing a board representation as well
        self.pieces
            .iter()
            .filter(|p| p.position.rank == rank && p.position.file == file)
            .next()
    }

    fn piece_at_position(&self, pos: &Position) -> Option<&Piece> {
        self.piece_at(pos.rank, pos.file)
    }

    fn piece_at_algebraic(&self, pos: &str) -> Option<&Piece> {
        let pos = Position::from_algebraic(pos);
        self.piece_at_position(&pos)
    }

    // fn get_all_valid_moves(&self, color: &Color) -> Vec<Move> {
    //
    // }
    /// Get the valid moves for side `color`
    fn get_all_valid_moves(&self, color: &Color) -> Vec<Move> {
        let in_check = self.is_in_check(color);

        let mut moves: Vec<Move> = vec![];
        for piece in &self.pieces{
            if piece.color == *color {
                moves.append(&mut self.get_valid_moves(&piece))
            }
        }
        moves
    }

    fn get_king_position(&self, color: &Color) -> Position {
        // TODO: implement a data structure to make this lookup const time
        for piece in self.pieces {
            if piece.color == *color && piece.piece_type == PieceType::King {
                return piece.position;
            }
        }
        // king should always be on the board
        panic!("King not found");
    }

    fn get_valid_moves<'a>(&'a self, piece: &'a Piece) -> Vec<Move> {
        match piece.piece_type {
            PieceType::Pawn => self.get_valid_pawn_moves(piece),
            PieceType::Rook => self.get_valid_rook_moves(piece),
            PieceType::Knight => self.get_valid_knight_moves(piece),
            PieceType::Bishop => self.get_valid_bishop_moves(piece),
            PieceType::Queen => {
                let mut bishop = self.get_valid_bishop_moves(piece);
                let mut rook = self.get_valid_rook_moves(piece);
                bishop.append(&mut rook);
                bishop
            }
            PieceType::King => self.get_valid_king_moves(piece),
        }
    }

    fn get_valid_pawn_moves<'a>(&'a self, piece: &'a Piece) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;
        
        if (piece.color == Color::White && piece.position.rank >= 8) 
            || (piece.color == Color::Black && piece.position.rank <= 1) {
            panic!("Pawn is on a promotion square, should not move from here.")
        }

        // get the rank one step forwards, up the baord for white and down for black
        let step_forward = |rank: u8| -> u8 {
            match piece.color {
                Color::White => rank + 1,
                Color::Black => rank - 1
            }
        };
        
        /// Starting rank
        fn pawn_starting_rank(color: &Color) -> u8 {
            match color {
                Color::White => 2,
                Color::Black => 7
            }
        }

        // move one square forward, requires no piece there
        let one_step = step_forward(pos.rank);
        if self.piece_at(one_step, pos.file).is_none() {
            moves.push(Move{
                piece,
                from: &piece.position,
                to: Position{rank: one_step, file: pos.file},
                captured: None});

            // if starting position for white and both squares in front free
            let start_rank = pawn_starting_rank(&piece.color);
            let two_step = step_forward(step_forward(start_rank));
            if pos.rank == start_rank && self.piece_at(two_step, pos.file).is_none(){
                moves.push(Move {
                    piece, 
                    from: &piece.position, 
                    to: Position{rank: two_step, file: pos.file}, 
                    captured: None
                });
            }
        }

        // check diagonal captures
        if pos.file >= 2 {
            let maybe_other_piece = self.piece_at(one_step, pos.file - 1);
            match maybe_other_piece {
                Some(other_piece) => if other_piece.color != piece.color {
                    moves.push(Move{
                        piece,
                        from: &piece.position,
                        to: Position{rank: one_step, file: pos.file - 1},
                        captured: Some(other_piece)
                    });
                },
                None => (),
            };
        }
        if pos.file <=7 {
            let maybe_other_piece = self.piece_at(one_step, pos.file + 1);
            match maybe_other_piece {
                Some(other_piece) => if other_piece.color != piece.color {
                    moves.push(Move{
                        piece,
                        from: &piece.position,
                        to: Position{rank: one_step, file: pos.file + 1},
                        captured: Some(other_piece)
                    });
                },
                None => (),
            };
        }


        // check en passant: should be a case of checking if one of the two captures squares
        // is the en passant square saved in self
         
        // signal promotion ?
        //
        moves
    }

    
    fn get_valid_rook_moves<'a>(&'a self, piece: &'a Piece) -> Vec<Move> {
        // vector of possible move, is_capture
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;

        // up
        for rank in (pos.rank + 1)..=8 {
            let candidate = Position{rank, file: pos.file};
            let potential_move = self.check_move_target(piece, &candidate);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move{
                        piece,
                        from: &piece.position,
                        to: candidate, 
                        captured: maybe_other 
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        } 
        // down
        for rank in (1..pos.rank).rev() {
            let candidate = Position{rank, file: pos.file};
            let potential_move = self.check_move_target(piece, &candidate);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move{
                        piece,
                        from: &piece.position,
                        to: candidate, 
                        captured: maybe_other 
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        } 

        // right
        for file in (pos.file + 1)..=8 {
            let candidate = Position{rank: pos.rank, file};
            let potential_move = self.check_move_target(piece, &candidate);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move{
                        piece,
                        from: &piece.position,
                        to: candidate, 
                        captured: maybe_other 
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        } 
        // left
        for file in (1..pos.file).rev() {
            let candidate = Position{rank: pos.rank, file};
            let potential_move = self.check_move_target(piece, &candidate);
            match potential_move {
                PotentialMove::Invalid => break,
                PotentialMove::Valid(maybe_other) => {
                    moves.push(Move{
                        piece,
                        from: &piece.position,
                        to: candidate, 
                        captured: maybe_other 
                    });
                    if !potential_move.continue_search_in_direction() {
                        break;
                    }
                }
            }
        } 
        moves
    }


    fn get_valid_bishop_moves<'a>(&'a self, piece: &'a Piece) -> Vec<Move> {
        // vector of possible move, is_capture
        let mut moves: Vec<Move> = vec![];
        let pos = &piece.position;

        // nice, clean code
        let zips: [std::iter::Zip<_, _>; 4] = [
            std::iter::zip(
                ((pos.rank + 1)..9).collect::<Vec<_>>().into_iter(), 
                ((pos.file + 1)..9).collect::<Vec<_>>().into_iter()
            ),
            std::iter::zip(
                ((pos.rank + 1)..9).collect::<Vec<_>>().into_iter(),
                (1..pos.file).rev().collect::<Vec<_>>().into_iter()
            ),
            std::iter::zip(
                (1..pos.rank).rev().collect::<Vec<_>>().into_iter(), 
                (pos.file + 1 ..9).collect::<Vec<_>>().into_iter()
            ),
            std::iter::zip(
                (1..pos.rank).rev().collect::<Vec<_>>().into_iter(), 
                (1..pos.file).rev().collect::<Vec<_>>().into_iter()
            ),
        ];

        for direction in zips {
            for (rank, file) in direction {
                let candidate = Position{rank, file};
                let potential_move = self.check_move_target(piece, &candidate);
                match potential_move {
                    PotentialMove::Invalid => break,
                    PotentialMove::Valid(maybe_other) => {
                        moves.push(Move{
                            piece,
                            from: &piece.position,
                            to: candidate,
                            captured: maybe_other,
                        });
                        if maybe_other.is_some() {
                            break;
                        }
                    },
                }
                // For sliding pieces like bishop and rook and queen, to check for pins we can keep
                // searching in the direction past the point were we would break, and count the
                // number of pieces betweeen the target square and the opponenet king.
                // if it's 2 or more, we can stop. If it's 1, that piece is pinned, we should
                // return that info back up.
                //
                // A pinned piece can still move, but it's limited to the squares observed in this
                // direction, up to an including capturing the pinning piece.
            } 
        }
        moves
    }


    // This funciton will check if the `piece` can move to `candidate_pos`. If so, it returns
    // a bool indicating if it's valid, if it's a capture, and a bool indicating if further search along
    // this axis is required.
    fn check_move_target(&self, piece: &Piece, candidate_pos: &Position) -> PotentialMove {
        match self.piece_at_position(candidate_pos) {
            Some(other_piece) => {
                if piece.color == other_piece.color {
                    // this is not a valid move and no need to search further in this direction
                    return PotentialMove::Invalid;
                } else {
                    // this is a valid move, it's a capture! no need to look any further
                    // though.
                    return PotentialMove::Valid(Some(other_piece));
                }
            },
            // no piece here, so it's a valid move (not a capture)
            // and we can keep searching in this direction
            None => return PotentialMove::Valid(None)
        };
    }

    // TODO: add type annotations to make this only take knight pieces
    // TODO: same for the other functions above
    fn get_valid_knight_moves<'a>(&'a self, piece: &'a Piece) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];

        for (rank_delta, file_delta) in std::iter::zip(
            [-2, -2, -1, -1, 1, 1, 2, 2],
            [-1, 1, -2, 2, -2, 2, -1, 1]
        ) {
            let future_rank: i8 = piece.position.rank as i8 + rank_delta;
            let future_file: i8 = piece.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                let future_rank = future_rank as u8;
                let future_file = future_file as u8;
                let maybe_other_piece = self.piece_at(future_rank, future_file);
                match maybe_other_piece {
                    Some(other_piece) => match piece.color == other_piece.color {
                        true => continue, // same color, invalid move
                        false => moves.push(Move {
                            piece,
                            from: &piece.position,
                            to: Position { rank: future_rank, file: future_file},
                            captured: Some(other_piece)
                        }),
                    },
                    None => moves.push(Move {
                        piece,
                        from: &piece.position,
                        to: Position { rank: future_rank, file: future_file},
                        captured: None
                    }),
                }
            }
        }
        moves
    }

    fn get_valid_king_moves<'a> (&'a self, piece: &'a Piece) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];

        for (rank_delta, file_delta) in std::iter::zip(
            [-1, -1, -1, 0, 0, 0, 1, 1, 1],
            [-1, 0, 1, -1, 0, 1, -1, 0, 1]
        ) {
            let future_rank: i8 = piece.position.rank as i8 + rank_delta;
            let future_file: i8 = piece.position.file as i8 + file_delta;
            if future_rank >= 1 && future_rank <= 8 && future_file >= 1 && future_file <= 8 {
                let maybe_other_piece = self.piece_at(future_rank as u8, future_file as u8);
                match maybe_other_piece {
                    Some(other_piece) => match piece.color == other_piece.color {
                        true => continue, // same color, invalid move
                        false => moves.push(Move {
                            piece,
                            from: &piece.position,
                            to: Position { rank: future_rank as u8, file: future_file as u8},
                            captured: Some(other_piece)
                        }),
                    },
                    None => moves.push(Move {
                        piece,
                        from: &piece.position,
                        to: Position { rank: future_rank as u8, file: future_file as u8},
                        captured: None
                    }),
                }

            }
        }
        moves
    }




    fn display_piece_moves(&self, piece: &Piece) {
        // let moves: Vec<Position> = self.get_valid_moves(piece)
        //     .into_iter()
        //     .map(|move_cap| move_cap.0)
        //     .collect();

        let moves = self.get_valid_moves(piece);
        let destinations: Vec<&Position> = moves.iter().map(|move_cap| &move_cap.to).collect();
        let moves = self.get_valid_moves(piece);
        let captures: Vec<Option<&Piece>> = moves.iter().map(|move_cap| move_cap.captured).collect();
        
        for r in (1..=8).rev() {
            for f in 1..9 {
                if piece.position.rank ==r && piece.position.file == f {
                    print!("{} ", piece_to_str(piece))
                } else if destinations.contains(&&Position{rank:r, file:f}) {
                    let move_idx = destinations.iter().position(|m| **m == Position{rank:r, file:f}).unwrap();
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

    fn draw_to_terminal(&self) {
        for r in 1..9 {
            for f in 1..9 {
                let p = self.piece_at(r, f);
                print!("{} ", match p {
                    Some(pp) => piece_to_str(pp),
                None => "."
                });
            }
            println!();
        }
    }

    fn clear(&mut self) {
        self.pieces.clear();
    }


}

fn piece_to_str(piece: &Piece) -> &str{
    let is_white = piece.color == Color::White;
    match piece.piece_type {
        PieceType::Pawn => if is_white {"♙"} else {"♟︎"},
        PieceType::Rook => if is_white {"♖"} else {"♜"},
        PieceType::Knight => if is_white {"♘"} else {"♞"},
        PieceType::Bishop => if is_white {"♗"} else {"♝"},
        PieceType::Queen => if is_white {"♕"} else {"♛"},
        PieceType::King => if is_white {"♔"} else {"♚"},
    }
}


/// Show an empty board with each piece at position `pos`, 
/// showing which squares that piece can move to
fn visualize_moves_from_position(pos: &Position) {
    for piece_type in [PieceType::Pawn, PieceType::Rook, PieceType::Bishop, PieceType:: Knight, PieceType:: King, PieceType::Queen] {
        let piece = Piece{color:Color::White, piece_type, position: Position{rank: pos.rank, file: pos.file}};
        println!("{:?}", &piece);
        piece.display_piece_moves();
        println!();
    }
}


fn main() {
    let mut b = Board::from_fen(STARTING_POSITION_FEN);
    b.draw_to_terminal();

    println!();

    for piece_type in [PieceType::Pawn, PieceType::Rook, PieceType::Bishop, 
                       PieceType:: Knight, PieceType:: King, PieceType::Queen] {
        println!("{:?}", &piece_type);
        b.pieces.push(Piece{
                color: Color::White, 
                piece_type, 
                position: Position { rank: 6, file: 6 },
        });
        b.display_piece_moves(&b.pieces.last().unwrap());
        println!("");
    }
    // for p in b.pieces {
    //     println!("{:?}", p);
    //     p.display_piece_moves();
    //     println!();
    // }

    // visualize_moves_from_position(&Position{file: 4, rank: 4})

    // dbg!(b.pieces);
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_position_from_algebraic() {
        assert_eq!(Position::from_algebraic("a1"), Position{rank: 1, file: 1});
        assert_eq!(Position::from_algebraic("h4"), Position{rank: 8, file: 4})
    }

    #[test]
    fn test_piece_from_alegraic() {
        assert_eq!(Piece::from_algebraic('R', "a1"), Piece{
            color: Color::White,
            piece_type: PieceType::Rook,
            position: Position { rank: 1, file: 1 },
        })
    }
    
    #[test]
    fn test_piece_from_rank_file() {
        assert_eq!(Piece::from_rank_file('k', 2, 3), Piece{
            color: Color::Black,
            piece_type: PieceType::King,
            position: Position { rank: 2, file: 3 },
        });
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
        assert_eq!(b.pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::Pawn)
            .count(),
            8 * 2
        );

        // count pairwise pieces
        for pt in [PieceType::Rook, PieceType::Bishop, PieceType::Knight] {
            assert_eq!(b.pieces
                .iter()
                .filter(|p| p.piece_type == pt)
                .count(),
                4
            );
        }

        for pt in [PieceType::King, PieceType::Queen] {
            assert_eq!(b.pieces
                .iter()
                .filter(|p| p.piece_type == pt)
                .count(),
                2
            );
        }

        // check some individual pieces

        let kings: Vec<&Piece> = b.pieces
            .iter()
            .filter(|p| p.piece_type == PieceType::King)
            .collect();

        assert_eq!(kings.len(), 2);
        assert_eq!(kings[0].position, Position{rank: 8, file: 5});
        assert_eq!(kings[1].position, Position{rank: 1, file: 5});


        assert_eq!(b.halfmove_clock, 0);
        assert_eq!(b.fullmove_clock, 1);


    }

}
