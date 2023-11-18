use rand::prelude::*;
mod board;
use crate::board::*; 

fn main() {
    let mut rng = rand::thread_rng();

    let mut b = Board::from_fen(STARTING_POSITION_FEN);
    b.draw_to_terminal();
    println!();

    const MAX_MOVES: i32 = 5;

    for i in 1..=MAX_MOVES {
        let legal_moves = b.get_legal_moves(&b.get_active_color()).expect(&"No legal moves");
        
        let random_move = legal_moves.choose(&mut rng).expect("No moves!");

        println!("move {}: {}", (i + 1) / 2, random_move.to_human());

        b = b.execute_move(random_move);

        b.draw_to_terminal();
        println!();
    }
}
