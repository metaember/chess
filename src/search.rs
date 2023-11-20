use rand::prelude::*;
use crate::board::*;
use crate::evaluate::*;

const MAX_SCORE: f32 = 99999.0;
const MIN_SCORE: f32 = -99999.0;

const DEBUG: bool = false;


pub struct SearchResult {
    pub best_move: Option<Move>,
    pub best_score: f32,
    pub nodes_searched: i32,
}

pub fn search(max_depth: u8, board: &Board) -> Move {
    let result = minimax(max_depth, board);
    println!("Nodes searched: {}, evaluation is {}", result.nodes_searched, result.best_score);
    result.best_move.expect("No move found")
}


pub fn minimax(max_depth: u8, board: &Board) -> SearchResult {
    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(color)) => return if color == Color::White {
            SearchResult{best_move: None, best_score: MAX_SCORE, nodes_searched: 0}
        } else {
            SearchResult{best_move: None, best_score: MIN_SCORE, nodes_searched: 0}
        },
        Err(Status::Stalemate) => return SearchResult{best_move: None, best_score: 0., nodes_searched: 0},
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    let mut best_move = &legal_moves[0];
    let mut best_score = -99999.0;
    let mut total_nodes_searched = 0;

    for m in &legal_moves {
        let b = board.execute_move(&m);
        let res = minimax_helper(max_depth - 1, &b);
        let score = res.0 as f32 * -1.;
        total_nodes_searched += res.1;

        if score > best_score {
            best_score = score;
            best_move = m;
        }
    };
    SearchResult {best_move: Some(*best_move), best_score: best_score, nodes_searched: total_nodes_searched }
}

pub fn minimax_helper(max_depth: u8, board: &Board) -> (f32, i32) {
    if max_depth == 0 {
        // If we've reached the maximum depth, evaluate the board naively
        return (evaluate_board(board), 1);
    }

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(color)) => return if color == Color::White { (MAX_SCORE, 0) } else { (MIN_SCORE, 0) },
        Err(Status::Stalemate) => return (0.0, 0),
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    if DEBUG {
        let _board_str = board.draw_board();
    }
     // for debugging

    let mut best_score = -99999.0;
    let mut total_nodes_searched = 0;

    for m in &legal_moves {
        let b = board.execute_move(&m);
        let res = minimax_helper(max_depth - 1, &b);
        let score = res.0 as f32 * -1.;
        total_nodes_searched += res.1;

        if score > best_score {
            best_score = score;
        }
    };
    (best_score, total_nodes_searched)
}

// Returns a random legal move
// pub fn random_move(board: &Board) -> Move {
//     let mut rng = rand::thread_rng();

//     let legal_moves = board.get_legal_moves(&board.get_active_color()).expect(&"No legal moves");

//     *legal_moves.choose(&mut rng).expect("No moves!")
// }