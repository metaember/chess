use rand::prelude::*;
use crate::board::*;
use crate::evaluate::*;

const MAX_SCORE: f32 = 99999.0;
const MIN_SCORE: f32 = -99999.0;

const DEBUG: bool = false;

pub fn search(max_depth: u8, board: &Board) -> Move {
    minimax(max_depth, board).0.expect("No move found")
}


pub fn minimax(max_depth: u8, board: &Board) -> (Option<Move>, f32) {
    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(color)) => return if color == Color::White { (None, MAX_SCORE) } else { (None, MIN_SCORE) },
        Err(Status::Stalemate) => return (None, 0.0),
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    let mut best_move = &legal_moves[0];
    let mut best_score = -99999.0;

    for m in &legal_moves {
        let b = board.execute_move(&m);
        let score = minimax_helper(max_depth - 1, &b) as f32 * -1.;

        if score > best_score {
            best_score = score;
            best_move = m;
        }
    };
    (Some(*best_move), best_score)
}

pub fn minimax_helper(max_depth: u8, board: &Board) -> f32 {
    if max_depth == 0 {
        // If we've reached the maximum depth, evaluate the board naively
        return evaluate_board(board);
    }

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(color)) => return if color == Color::White { MAX_SCORE} else { MIN_SCORE },
        Err(Status::Stalemate) => return 0.0,
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    if DEBUG {
        let _board_str = board.draw_board();
    }
     // for debugging

    let mut best_score = -99999.0;

    for m in &legal_moves {
        let b = board.execute_move(&m);
        let score = minimax_helper(max_depth - 1, &b) as f32 * -1.;

        if score > best_score {
            best_score = score;
        }
    };
    best_score
}

// Returns a random legal move
// pub fn random_move(board: &Board) -> Move {
//     let mut rng = rand::thread_rng();

//     let legal_moves = board.get_legal_moves(&board.get_active_color()).expect(&"No legal moves");

//     *legal_moves.choose(&mut rng).expect("No moves!")
// }