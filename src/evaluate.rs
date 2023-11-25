use crate::board::*;


pub struct Material {
    pub white_material: i32,
    pub white_piece_location_adjustment: i32,
    pub white_pawn_material: i32,
    pub white_piece_material: i32,

    pub black_material: i32,
    pub black_piece_location_adjustment: i32,
    pub black_pawn_material: i32,
    pub black_piece_material: i32,
}

impl Material {
    pub fn get_material_difference(&self) -> i32 {
        self.white_material - self.black_material
    }

    pub fn get_location_adjusted_material_difference(&self) -> i32 {
        self.white_material + self.white_piece_location_adjustment
         - self.black_material - self.black_piece_location_adjustment
    }

    fn piece_to_material(piece: &Piece) -> i32 {
        match piece.piece_type {
            PieceType::Pawn => 100,
            PieceType::Rook => 500,
            PieceType::Knight => 300,
            PieceType::Bishop => 320,  // Bishop is worth slightly more than a knight
            PieceType::Queen => 900,
            PieceType::King => 0,
        }
    }

    pub fn compute_material(board: &Board) -> Material {
        let mut white_material = 0;
        let mut black_material = 0;
        let mut white_piece_location_adjustment = 0;
        let mut black_piece_location_adjustment = 0;
        let mut white_pawn_material = 0;
        let mut black_pawn_material = 0;
        let mut white_piece_material = 0;
        let mut black_piece_material = 0;

        for piece in board.white.all_pieces().into_iter() {
            let current_piece_material = Material::piece_to_material(&piece);

            white_material += current_piece_material;
            white_piece_location_adjustment += get_piece_adjustment_value(&piece, false);
            if piece.piece_type == PieceType::Pawn {
                white_pawn_material += current_piece_material;
            } else {
                white_piece_material += current_piece_material
            };
        };

        for piece in board.black.all_pieces().into_iter() {
            let current_piece_material = Material::piece_to_material(&piece);

            black_material += current_piece_material;
            black_piece_location_adjustment += get_piece_adjustment_value(&piece, false);
            if piece.piece_type == PieceType::Pawn {
                black_pawn_material += current_piece_material;
            } else {
                black_piece_material += current_piece_material;
            };
        };
        Material {
            white_material,
            white_piece_location_adjustment,
            white_pawn_material,
            white_piece_material,
            black_material,
            black_piece_location_adjustment,
            black_pawn_material,
            black_piece_material,
        }
    }
}

pub fn evaluate_board(board: &Board) -> i32 {
    let material = Material::compute_material(board);
    let unsigned_score = material.get_location_adjusted_material_difference();
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
        score += material_difference_multiplier * (
            Material::piece_to_material(&captured_piece) - Material::piece_to_material(&mv.piece));
    };

    // TODO: add promotion bonus equal to the value of the promoted piece
    // score

    // TODO: penalize moves that move a piece into a position where it can be captured by a pawn
    // thse shold be cached when we evaluate the board before the move
    score
}




fn get_piece_adjustment_value(piece: &Piece, is_endgame: bool) -> i32 {
    if piece.piece_type == PieceType::King {
        return 0;
    }
    let raw_table = get_raw_adjustemnt_table(piece, is_endgame);
    let index = match piece.color {
        Color::White => (piece.position.rank - 1) * 8 + piece.position.file - 1,
        Color::Black => (piece.position.rank - 1) + piece.position.file - 1,
    };
    raw_table[index as usize]
}

fn get_raw_adjustemnt_table(piece: &Piece, is_endgame: bool) -> &[i32; 8*8] {
    match piece.piece_type {
        PieceType::Pawn => {
            if is_endgame {
                &PAWNS_END
            } else {
                &PAWNS
            }
        },
        PieceType::Rook => &ROOKS,
        PieceType::Knight => &KNIGHTS,
        PieceType::Bishop => &BISHOPS,
        PieceType::Queen => &QUEENS,
        PieceType::King => todo!("King has no adjustment table"),
    }
}

// This part of the code aattributes different weights to pieces in different positions
// stolen from : https://github.com/SebLague/Chess-Coding-Adventure/blob/Chess-V2-UCI/Chess-Coding-Adventure/src/Core/Evaluation/PieceSquareTable.cs


const PAWNS: [i32; 8*8] = [
    0,   0,   0,   0,   0,   0,   0,   0,
   50,  50,  50,  50,  50,  50,  50,  50,
   10,  10,  20,  30,  30,  20,  10,  10,
    5,   5,  10,  25,  25,  10,   5,   5,
    0,   0,   0,  20,  20,   0,   0,   0,
    5,  -5, -10,   0,   0, -10,  -5,   5,
    5,  10,  10, -20, -20,  10,  10,   5,
    0,   0,   0,   0,   0,   0,   0,   0
];

const PAWNS_END: [i32; 8*8] = [
    0,   0,   0,   0,   0,   0,   0,   0,
   80,  80,  80,  80,  80,  80,  80,  80,
   50,  50,  50,  50,  50,  50,  50,  50,
   30,  30,  30,  30,  30,  30,  30,  30,
   20,  20,  20,  20,  20,  20,  20,  20,
   10,  10,  10,  10,  10,  10,  10,  10,
   10,  10,  10,  10,  10,  10,  10,  10,
    0,   0,   0,   0,   0,   0,   0,   0
];

const ROOKS: [i32; 8*8] = [
   0,  0,  0,  0,  0,  0,  0,  0,
   5, 10, 10, 10, 10, 10, 10,  5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   0,  0,  0,  5,  5,  0,  0,  0
];

const KNIGHTS: [i32; 8*8] = [
   -50,-40,-30,-30,-30,-30,-40,-50,
   -40,-20,  0,  0,  0,  0,-20,-40,
   -30,  0, 10, 15, 15, 10,  0,-30,
   -30,  5, 15, 20, 20, 15,  5,-30,
   -30,  0, 15, 20, 20, 15,  0,-30,
   -30,  5, 10, 15, 15, 10,  5,-30,
   -40,-20,  0,  5,  5,  0,-20,-40,
   -50,-40,-30,-30,-30,-30,-40,-50,
];

const BISHOPS: [i32; 8*8] = [
   -20,-10,-10,-10,-10,-10,-10,-20,
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5, 10, 10,  5,  0,-10,
   -10,  5,  5, 10, 10,  5,  5,-10,
   -10,  0, 10, 10, 10, 10,  0,-10,
   -10, 10, 10, 10, 10, 10, 10,-10,
   -10,  5,  0,  0,  0,  0,  5,-10,
   -20,-10,-10,-10,-10,-10,-10,-20,
];

const QUEENS: [i32; 8*8] = [
   -20,-10,-10, -5, -5,-10,-10,-20,
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5,  5,  5,  5,  0,-10,
   -5,  0,  5,  5,  5,  5,  0, -5,
   0,  0,  5,  5,  5,  5,  0, -5,
   -10,  5,  5,  5,  5,  5,  0,-10,
   -10,  0,  5,  0,  0,  0,  0,-10,
   -20,-10,-10, -5, -5,-10,-10,-20
];