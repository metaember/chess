use crate::board::*;


pub struct Material {
    pub white: f32,
    pub black: f32,
}

pub fn evaluate_board(board: &Board) -> f32 {
    let material = sum_material(board);
    let mut score = material.white - material.black;

    if board.get_active_color() == Color::Black {
        score *= -1.0;
    };
    score
}


fn sum_material(b: &Board) -> Material {
    let mut white_material = 0.0;
    let mut black_material = 0.0;

    for p in b.pieces.iter() {
        match p.color {
            Color::White => white_material += p.piece_type.to_material() as f32,
            Color::Black => black_material += p.piece_type.to_material() as f32,
        }
    }
    Material {
        white: white_material,
        black: black_material,
    }
}

