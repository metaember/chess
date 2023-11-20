use crate::board::*;


pub struct Material {
    pub white: i32,
    pub black: i32,
}

pub fn evaluate_board(board: &Board) -> i32 {
    let material = sum_material(board);
    let mut score = material.white - material.black;

    if board.get_active_color() == Color::Black {
        score *= -1;
    };
    score
}

fn to_material(piece: &Piece) -> i32 {
    match piece.piece_type {
        PieceType::Pawn => 100,
        PieceType::Rook => 500,
        PieceType::Knight => 300,
        PieceType::Bishop => 320,  // Bishop is worth slightly more than a knight
        PieceType::Queen => 900,
        PieceType::King => 0,
    }
}

fn sum_material(b: &Board) -> Material {
    let mut white_material = 0;
    let mut black_material = 0;

    // TODO: Implement endgame detection
    let is_endgame = false;

    for p in b.pieces.iter() {
        match p.color {
            Color::White => white_material += to_material(p) + get_piece_adjustment_value(p, is_endgame),
            Color::Black => black_material += to_material(p) + get_piece_adjustment_value(p, is_endgame)
        }
    }
    Material {
        white: white_material,
        black: black_material,
    }
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