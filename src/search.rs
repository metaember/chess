use crate::board;
use crate::board::*;
use crate::evaluate::*;

pub const MIN_SCORE: i32 = -1_000_000_000;
pub const MAX_SCORE: i32 = 1_000_000_000;

#[derive(Debug)]
pub struct SearchResult {
    pub best_move: Option<Move>,
    pub best_score: i32,
    pub nodes_searched: i32,
    pub quiescent_nodes_searched: i32,
    // TODO remove these two fields in prod for speed reasons
    pub moves: Vec<Move>, // path of moves down the tree
}

impl SearchResult {
    pub fn print(&self) {
        println!("Search result: [{}, nodes: {} tot, {} quies] {}: {}", self.best_score, self.nodes_searched,
            self.quiescent_nodes_searched, self.best_move.unwrap().to_algebraic(), self.best_move.unwrap().to_human());
        println!("Move path:");
        self.moves.iter().rev().for_each(|m| println!("  • {}: {}", m.to_algebraic(), m.to_human()));
    }
}

pub fn search(max_depth: u8, board: &Board) -> Move {
    let result = minimax(max_depth, board);
    if result.is_err() {
        let moves = result.err().unwrap();
        panic!(
            "Error {}",
            moves
                .into_iter()
                .map(|m| m.to_human() + "\n")
                .collect::<String>()
        );
    }
    let result = result.unwrap();
    result.best_move.expect("No move found")
}


pub fn minimax(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, true, true, true)
}

pub fn minimax_no_quiescence(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, true, true, false)
}


pub fn minimax_no_ordering(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, false, false, false)
}

pub fn minimax_no_pruning(max_depth: u8, board: &Board) -> Result<SearchResult, Vec<Move>> {
    let (alpha, beta) = (MIN_SCORE, MAX_SCORE);
    negamax(max_depth, board, alpha, beta, false, false, false)
}

/// Main minimax function
/// TODO: maybe discount further moves by a tiny bit to favor short mating sequences
pub fn negamax(max_depth: u8, board: &Board, alpha: i32, beta: i32,
               use_ab_pruning: bool, use_move_ordering: bool, quiescence: bool) -> Result<SearchResult, Vec<Move>> {

    if max_depth == 0 {
        // If we've reached the maximum depth
        if quiescence {
            return negamax_captures_only(board, alpha, beta);
        } else {
            let score = evaluate_board(board);
            return Ok(SearchResult {
                best_move: None,
                best_score: score,
                nodes_searched: 1,
                quiescent_nodes_searched: 0,
                moves: vec![],
            });

        };
    };

    // for alpha-beta pruning
    let mut alpha = alpha;


    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: MIN_SCORE,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
            })
        }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    // if using move ordersing, we sort the moves by their value heuristic here
    let legal_moves = if use_move_ordering {
        let mut moves_and_scores = legal_moves
            .iter()
            .map(|m| (m, guess_move_value(board, m)))
            .collect::<Vec<(&Move, i32)>>();

        moves_and_scores.sort_by(|a, b| b.1.cmp(&a.1));
        moves_and_scores
            .iter()
            .map(|(m, _)| **m)
            .collect::<Vec<Move>>()
    } else {
        legal_moves
    };

    let mut current_best_move = &legal_moves[0];
    let mut current_best_score = MIN_SCORE;
    let mut total_nodes_searched = 0;
    let mut total_quiescent_nodes_searched = 0;
    let mut current_moves = vec![];
    let mut current_scores = vec![];

    for m in &legal_moves {
        if let Some(captured) = m.captured {
            if captured.piece_type == PieceType::King {
                return Err(vec![*m]);
            }
        }
        let b = board.execute_move(&m);
        let res = negamax(max_depth - 1, &b, -beta, -alpha,
            use_ab_pruning, use_move_ordering, quiescence);
        match res {
            Ok(SearchResult {
                best_move: _,  // best_move from prev iteration
                best_score: evaluation,
                nodes_searched,
                quiescent_nodes_searched,
                moves,
            }) => {
                total_nodes_searched += nodes_searched;
                total_quiescent_nodes_searched += quiescent_nodes_searched;
                let evaluation = -evaluation;

                if evaluation > current_best_score {
                    current_best_score = evaluation;
                    current_best_move = m;
                    current_moves = moves;
                };

                if use_ab_pruning {
                    // The last move was too good, meaning the opponent would not have allowed
                    // us to get to this position by playing a different move earlier on,
                    // so we can stop searching the remaining moves
                    if evaluation >= beta {
                        current_best_score = beta;
                        break;
                    }
                    alpha = alpha.max(evaluation);
                }
            }
            Err(moves) => {
                b.draw_to_terminal();
                let mut cur_move_vec = vec![*m];
                cur_move_vec.append(&mut moves.clone());
                return Err(cur_move_vec);
            }
        }
    }

    current_moves.push(current_best_move.clone());
    current_scores.push(current_best_score);

    Ok(SearchResult {
        best_move: Some(*current_best_move),
        best_score: current_best_score,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        moves: current_moves,
    })
}

/// Minimax run with no depth limit for the quiescence search.
/// We only consider captures. This version returns the score and some
/// metadata on the bast path found
///
/// TODO: maybe add checks and promotions here as well?
pub fn negamax_captures_only(board: &Board, alpha: i32, beta: i32) -> Result<SearchResult, Vec<Move>> {
    let evaluation = evaluate_board(&board);
    if evaluation > beta {
        return Ok(SearchResult {
            best_move: None,
            best_score: beta,
            nodes_searched: 1,
            quiescent_nodes_searched: 1,
            moves: vec![],
        });
    }

    let mut alpha = alpha.max(evaluation);

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: MIN_SCORE,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
            })
        }
        Err(Status::Stalemate) => {
            return Ok(SearchResult {
                best_move: None,
                best_score: 0,
                nodes_searched: 0,
                quiescent_nodes_searched: 0,
                moves: vec![],
            })
        }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    // TODO: put this flag in the move generation to save on some copies
    let captures = legal_moves
        .into_iter()
        .filter(|m| m.captured.is_some())
        .collect::<Vec<Move>>();


    let mut current_best_move = None;
    let mut total_nodes_searched = 0;
    let mut total_quiescent_nodes_searched = 0;
    let mut current_moves = vec![];
    let mut current_scores = vec![];

    // TODO: use move ordering in the quiescence search??
    for m in captures {
        // board.draw_to_terminal();
        let b = board.execute_move(&m);
        // b.draw_to_terminal();
        let search_res = negamax_captures_only(&b, -beta, -alpha).unwrap();

        let evaluation = -search_res.best_score;
        total_nodes_searched += search_res.nodes_searched;
        total_quiescent_nodes_searched += search_res.quiescent_nodes_searched;

        if evaluation >= beta {
            current_best_move = Some(m);
            current_moves = search_res.moves;
            current_moves.push(m.clone());
            current_scores.push(beta);

            return Ok(SearchResult {
                best_move: current_best_move,
                best_score: beta,
                nodes_searched: total_nodes_searched,
                quiescent_nodes_searched: total_quiescent_nodes_searched,
                moves: current_moves,
            })
        }
        alpha = alpha.max(evaluation);
    }
    return Ok(SearchResult {
        best_move: current_best_move,
        best_score: alpha,
        nodes_searched: total_nodes_searched,
        quiescent_nodes_searched: total_quiescent_nodes_searched,
        moves: current_moves,
    })
}

pub fn negamax_fast(deapth: i32, board: &Board, alpha: i32, beta: i32, quiescence: bool) -> i32 {
    if deapth == 0 {
        if quiescence {
            return negamax_captures_only_fast(board, alpha, beta);
        }
        return evaluate_board(&board);
    };

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => { return MIN_SCORE; }
        Err(Status::Stalemate) => { return 0; }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    let mut alpha = alpha;

    for m in legal_moves {
        let b = board.execute_move(&m);
        let evaluation = -negamax_fast(deapth - 1, &b, -beta, -alpha, quiescence);
        if evaluation >= beta {
            return beta;
        }
        alpha = alpha.max(evaluation);
    }
    return alpha;
}

/// Minimax run with no depth limit for the quiescence search.
/// We only consider captures. This version returns the score of the position only
///
/// TODO: maybe add checks and promotions here as well?
pub fn negamax_captures_only_fast(board: &Board, alpha: i32, beta: i32) -> i32 {
    let evaluation = evaluate_board(&board);
    if evaluation > beta {
        return beta;
    }
    let mut alpha = alpha.max(evaluation);

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => { return MIN_SCORE; }
        Err(Status::Stalemate) => { return 0; }
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    let captures = legal_moves
        .into_iter()
        .filter(|m| m.captured.is_some())
        .collect::<Vec<Move>>();

    // TODO: use move ordering in the quiescence search??

    for m in captures {
        let b = board.execute_move(&m);
        let evaluation = -negamax_captures_only_fast(&b, -beta, -alpha);
        if evaluation >= beta {
            return beta;
        }
        alpha = alpha.max(evaluation);
    }
    return alpha;
}


// Returns a random legal move
// pub fn random_move(board: &Board) -> Move {
//     let mut rng = rand::thread_rng();

//     let legal_moves = board.get_legal_moves(&board.get_active_color()).expect(&"No legal moves");

//     *legal_moves.choose(&mut rng).expect("No moves!")
// }


#[cfg(test)]
mod test {
    use std::time::Instant;
    use super::*;

    #[test]
    fn test_quiescence_search_1(){
        // . . . . . . ♖ .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . . . ♖ . .
        // . . . . . ♔ . ♚
        let b = Board::from_fen("6R1/8/8/8/8/4p3/5R2/5K1k b - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_eq!(search_result.nodes_searched, 2);
        assert_eq!(m, Move::from_algebraic(&b, "e3", "f2"));
        println!("");

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // unfortunately 1 move deep is too shallow to see the draw opportunity
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_eq!(m, Move::from_algebraic(&b, "e3", "f2"));
    }

    #[test]
    fn test_quiescence_search_2(){
        // This is an example of a position where we need to look past the first
        // capture to reject the capture

        // . . . . . . . .
        // . . . . . ♚ . .
        // . . . . . ♟︎ . .
        // . . . . ♟︎ . . .
        // . . . . ♖ . . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . ♔ . . . .
        let b = Board::from_fen("8/5k2/5p2/4p3/4R3/8/8/3K4 w - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the pawn with the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        let eval = search_result.best_score;
        assert_eq!(m, Move::from_algebraic(&b, "e4", "e5"));
        assert_eq!(eval, 350);
        println!("");

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // we find that the next move is black recapturing the rook, so we
        // don't play the move
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_ne!(m, Move::from_algebraic(&b, "e4", "e5"));
    }

    #[test]
    fn test_quiescence_search_3(){
        // This is an example of a position where looking past the first capture
        // allows us to recognize the sac is actually good

        // . . . . ♚ . . .
        // . . . . . . . .
        // . . . . ♜ . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . ♟︎ . . . .
        // . . . . . . . .
        // . . . ♖ ♕ ♔ . .
        let b = Board::from_fen("4k3/8/4r3/8/4p3/3p4/8/3RQK2 w - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the pawn with the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        let eval = search_result.best_score;
        assert!(m == Move::from_algebraic(&b, "d1", "d3") || m == Move::from_algebraic(&b, "e1", "e4"));
        println!("{} {}", eval, search_result.nodes_searched);

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // we find that the next move is black recapturing the rook, so we
        // but the move after that we recapture their rook with the queen so we
        // still play it
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_ne!(m, Move::from_algebraic(&b, "e4", "e5"));
        println!("{} {}", eval, search_result.nodes_searched);
    }


    #[test]
    fn test_quiescence_search_4(){
        // This is an example of a position where looking past the first capture
        // allows us to discard it

        // . . . . ♚ . . .
        // . . . . . . . .
        // . . . . ♜ . . .
        // . . . . . . . .
        // . . . . ♟︎ . . .
        // . . . ♟︎ . . . .
        // . . . . . . . .
        // . . . ♖ . ♔ . .
        let b = Board::from_fen("4k3/8/4r3/8/4p3/3p4/8/3R1K2 w - - 0 1");
        b.draw_to_terminal();

        // First a regular search 1 deep without quiescence
        // we expect to greedily capture the pawn with the rook
        let search_result = minimax_no_quiescence(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        let eval = search_result.best_score;
        assert!(m == Move::from_algebraic(&b, "d1", "d3"));
        println!("{} {}", eval, search_result.nodes_searched);

        // now we enable quiescence search, still one move deep
        // this time during the search, we keep looking after the capture.
        // we find that the next move is black recapturing the rook, so we
        // don't play the move
        let search_result = minimax(1, &b).unwrap();
        search_result.print();
        let m = search_result.best_move.unwrap();
        assert_ne!(m, Move::from_algebraic(&b, "d1", "d3"));
        println!("{} {}", eval, search_result.nodes_searched);
    }

    #[test]
    fn test_blunders_knight_1() {
        // . ♞ . ♛ ♚ ♝ . ♜
        // ♜ ♝ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
        // ♟︎ ♟︎ . . . . . .
        // . . . . . ♟︎ . .
        // . . . . ♘ . . .
        // . . . ♕ ♙ . . .`
        // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
        // ♖ . ♗ . ♔ . ♘ ♖
        let fen = "1n1qkb1r/rbppp1pp/pp6/5p2/4N3/3QP3/PPPP1PPP/R1B1K1NR w KQk - 0 8";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('N', "e4"));
    }

    #[test]
    fn test_blunders_knight_2() {
        // . ♞ . ♛ ♚ ♝ . ♜
        // ♜ ♝ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
        // ♟︎ ♟︎ . . . . . .
        // . . . . . ♟︎ . .
        // . . . . ♘ . . .
        // . . . ♕ ♙ . . .
        // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
        // ♖ . ♗ . ♔ . ♘ ♖

        // 1n1qkb1r/rbppp1pp/pp6/5p2/4N3/3QP3/PPPP1PPP/R1B1K1NR w KQk - 0 8

        // 1. Nb1c3 Ng8f6 2. e2e3 a7a6 3. Bf1d3 Ra8a7 4. Qd1e2 b7b6 5. Bd3e4 Nf6xe4 6. Nc3xe4 Bc8b7
        // 7. Qe2d3 f7f5 8. f2f3?? f5xe4
        // this series of moves should get us to the position from the previous test. When running the test above it
        // passes but when playing the game it still blunders f3
        let b = Board::new();
        b.draw_to_terminal();

        // 1
        let b = b.execute_move(&Move::from_algebraic(&b, "b1", "c3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "g8", "f6"));
        // 2
        let b = b.execute_move(&Move::from_algebraic(&b, "e2", "e3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "a7", "a6"));
        // 3
        let b = b.execute_move(&Move::from_algebraic(&b, "f1", "d3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "a8", "a7"));
        // 4
        let b = b.execute_move(&Move::from_algebraic(&b, "d1", "e2"));
        let b = b.execute_move(&Move::from_algebraic(&b, "b7", "b6"));
        // 5
        let b = b.execute_move(&Move::from_algebraic(&b, "d3", "e4"));
        let b = b.execute_move(&Move::from_algebraic(&b, "f6", "e4"));
        b.draw_to_terminal();

        // 6
        let b = b.execute_move(&Move::from_algebraic(&b, "c3", "e4"));
        let b = b.execute_move(&Move::from_algebraic(&b, "c8", "b7"));
        b.draw_to_terminal();

        // 7
        let b = b.execute_move(&Move::from_algebraic(&b, "e2", "d3"));
        let b = b.execute_move(&Move::from_algebraic(&b, "f7", "f5"));
        b.draw_to_terminal();

        // make sure the Knight has legal moves
        let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
        let knight_moves = legal_moves
            .iter()
            .filter(|m| m.piece.piece_type == PieceType::Knight && m.piece.position.rank == 4)
            .collect::<Vec<&Move>>();
        assert!(knight_moves.len() > 5);
        // knight_moves
        //     .iter()
        //     .for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(knight_moves.contains(&&Move::from_algebraic(&b, "e4", "c3")));
        // ok the knight has good squares to move to, that's not the issue
        // also it's not pinned or it would not show up in the legal moves


        let blunder_move = Move::from_algebraic(&b, "f2", "f3");
        let b_after_blunder = b.execute_move(&blunder_move);
        println!("After blunder:");
        b_after_blunder.draw_to_terminal();

        let black_pseudo_moves = b_after_blunder.get_all_pseudo_moves(&b_after_blunder.get_active_color(), true);
        let capture_knight_move = Move::from_algebraic(&b_after_blunder, "f5", "e4");
        assert!(black_pseudo_moves.contains(&&capture_knight_move));
        assert!(capture_knight_move.captured.is_some_and(|p| p == Piece::from_algebraic('N', "e4")));
        // ok so white should know black can capture the knight

        // check the eval before and after the blunder
        let eval_before_blunder = evaluate_board(&b);
        let eval_after_blunder = evaluate_board(&b_after_blunder);
        println!("Eval before blunder: {}", eval_before_blunder);
        println!("Eval after blunder: {}\n", eval_after_blunder);
        assert!(eval_before_blunder - 150 < eval_before_blunder);

        let after_recapture = b_after_blunder.execute_move(&capture_knight_move);
        println!("After recapture:");
        after_recapture.draw_to_terminal();
        let eval_after_recapture = evaluate_board(&after_recapture);
        println!("Eval after recapture: {}\n", eval_after_recapture);


        // ok so going back to before the blunder
        println!("Search before blunder: 4 moves deep");
        assert!(b.get_active_color() == Color::White);
        let search_result = minimax(4, &b).unwrap();
        search_result.print();

        // search 2 moves deep
        println!("Search before blunder: 2 moves deep");
        assert!(b.get_active_color() == Color::White);
        let search_result = minimax(2, &b).unwrap();
        search_result.print();
        assert!(search_result.best_move.unwrap().piece == Piece::from_algebraic('N', "e4"));

        // search 5 moves deep
        println!("Search before blunder: 5 moves deep");
        assert!(b.get_active_color() == Color::White);
        let search_result = minimax(5, &b).unwrap();
        println!("search result: {} {}", search_result.best_move.unwrap().to_human(), search_result.best_score);
        search_result.moves.iter().for_each(|m| println!("{} {}", m.to_algebraic(), m.to_human()));
        assert!(search_result.best_move.unwrap().piece == Piece::from_algebraic('N', "e4"));


        let m = search(4, &b);
        print!("best move: {} {}", m.to_human(), m.to_algebraic());
        // Move the knight, it's under attacj by a pawn!
        assert!(m.piece == Piece::from_algebraic('N', "e4"));

        todo!("Implement");
    }

    #[test]
    fn sebastian_lague_test_position() {
        let b = Board::from_fen("r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1");

        let now_minimax = Instant::now();
        let pure_minimax_4 = minimax_no_pruning(4, &b).unwrap();
        // Sebastian has 1.16 s, 3_553_501 positions
        println!("pure minimax: {:.6}s evaluated {} positions", now_minimax.elapsed().as_secs_f32(),
            pure_minimax_4.nodes_searched);

        let now_pruning = Instant::now();
        let minimax_a_b_pruning = minimax_no_ordering(4, &b).unwrap();
        // Sebastian has 0.18 s, 464_795 positions
        println!("minimax a b pruning: {:.6}s evaluated {} positions", now_pruning.elapsed().as_secs_f32(),
            minimax_a_b_pruning.nodes_searched);

        let now_pruning_sorting = Instant::now();
        let minimax_a_b_pruning_sorting = minimax_no_quiescence(4, &b).unwrap();
        // Sebastian has 0.025 s, 4916 positions
        println!("minimax a b pruning with ordering: {:.6}s evaluated {} positions",
            now_pruning_sorting.elapsed().as_secs_f32(), minimax_a_b_pruning_sorting.nodes_searched);

        // assert things on the resulting positions being the same
        assert_eq!(pure_minimax_4.best_move, minimax_a_b_pruning.best_move);
        assert_eq!(pure_minimax_4.best_move, minimax_a_b_pruning_sorting.best_move);

        // let now_negamax_fast_no_q = Instant::now();
        // let res_neg_fast_no_q = negamax_fast(4, &b, MIN_SCORE, MAX_SCORE, false);
        // // Sebastian has 0.025 s, 4916 positions
        // println!("negamax fast: {:.6}s", now_negamax_fast_no_q.elapsed().as_secs_f32());

        // let now_negamax_fast = Instant::now();
        // let res_neg_fast = negamax_fast(4, &b, MIN_SCORE, MAX_SCORE, true);
        // // Sebastian has 0.025 s, 4916 positions
        // println!("negamax fast: {:.6}s", now_negamax_fast.elapsed().as_secs_f32());
    }

    #[test]
    fn mate_in_two_1() {
        // https://wtharvey.com/m8n2.txt
        // Magnus Carlsen vs Helgi Gretarsson, Rethymnon, 2003
        // r5q1/pp1b1kr1/2p2p2/2Q5/2PpB3/1P4NP/P4P2/4RK2 w - - 1 0
        // 1. Bg6+ Kxg6 2. Qh5#
        let fen = "r5q1/pp1b1kr1/2p2p2/2Q5/2PpB3/1P4NP/P4P2/4RK2 w - - 1 0";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('B', "e4"));
        assert_eq!(m.to, Position::from_algebraic("g6"));
    }

    #[test]
    fn mate_in_two_2() {
        // https://wtharvey.com/m8n2.txt
        // Jon Hammer vs Magnus Carlsen, Halkidiki, 2003
        // 5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1
        // 1... Qh5+ 2. gxh5 Rh4#

        todo!("Re-enable this test when it's faster to search.");

        let fen = "5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('q', "b5"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m.captured.is_none());

        let b = b.execute_move(&m);

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('P', "g4"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m
            .captured
            .is_some_and(|p| p == Piece::from_algebraic('q', "h5")));

        let b = b.execute_move(&m);

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('r', "e4"));
        assert_eq!(m.to, Position::from_algebraic("h4"));
    }

    #[test]
    fn mate_in_two_2_m2() {
        // checking evaluation and search after first move
        // https://wtharvey.com/m8n2.txt
        // Jon Hammer vs Magnus Carlsen, Halkidiki, 2003
        // 5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1
        // 1... Qh5+ 2. gxh5 Rh4#
        let fen = "5rk1/ppp2pbp/3p2p1/7q/4r1P1/1NP1B3/PP2nPP1/R2QR2K w - - 1 2";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
        assert_eq!(legal_moves.len(), 1);

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('P', "g4"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m
            .captured
            .is_some_and(|p| p == Piece::from_algebraic('q', "h5")));
        let b = b.execute_move(&m);

        b.draw_to_terminal();

        let m = search(2, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('r', "e4"));
        assert_eq!(m.to, Position::from_algebraic("h4"));
    }

    #[test]
    fn mate_in_three_1() {
        // https://wtharvey.com/m8n3.txt
        // Madame de Remusat vs Napoleon I, Paris, 1802
        // r1b1kb1r/pppp1ppp/5q2/4n3/3KP3/2N3PN/PPP4P/R1BQ1B1R b kq - 0 1
        // 1... Bc5+ 2. Kxc5 Qb6+ 3. Kd5 Qd6#

        todo!("Re-enable this test when it's faster to search.");

        let fen = "r1b1kb1r/pppp1ppp/5q2/4n3/3KP3/2N3PN/PPP4P/R1BQ1B1R b kq - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();

        let m = search(5, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('b', "f8"));
        assert!(m.to == Position::from_algebraic("c5"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('K', "d4"));
        assert!(m.to == Position::from_algebraic("c5"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('q', "f6"));
        assert!(m.to == Position::from_algebraic("b6"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('K', "c5"));
        assert!(m.to == Position::from_algebraic("d5"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('q', "b6"));
        assert!(m.to == Position::from_algebraic("d6"));
    }

    #[test]
    fn mate_in_three_2() {
        // https://wtharvey.com/m8n3.txt
        // Jean Netzer vs Maxime Vachier-Lagrave, Hyeres, 2002
        // 5r2/6k1/p2p4/6n1/P3p3/8/5P2/2q2QKR b - - 0 1
        // 1... Nf3+ 2. Kg2 Qg5+ 3. Kh3 Rh8#

        todo!("Re-enable this test when it's faster to search.");

        let fen = "5r2/6k1/p2p4/6n1/P3p3/8/5P2/2q2QKR b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();

        let m = search(5, &b); // this needs to be depth 5 to find the mate in 3
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('n', "g5"));
        assert_eq!(m.to, Position::from_algebraic("f3"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('K', "g1"));
        assert_eq!(m.to, Position::from_algebraic("g2"));

        let b = b.execute_move(&m);
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('q', "c1"));
        assert_eq!(m.to, Position::from_algebraic("g5"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('K', "g2"));
        assert_eq!(m.to, Position::from_algebraic("h3"));

        let b = b.execute_move(&m);
        let m = search(2, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('r', "f8"));
        assert_eq!(m.to, Position::from_algebraic("h8"));
    }

    /// Perft from starting position
    ///
    /// https://www.chessprogramming.org/Perft_Results
    ///
    /// | Depth | Nodes                         | Captures        | E.p.        | Castles       | Promotions | Checks         | Discovery Checks | Double Checks | Checkmates  |
    /// | ----- | ----------------------------- | --------------- | ----------- | ------------- | ---------- | -------------- | ---------------- | ------------- | ----------- |
    /// | 0     | 1                             | 0               | 0           | 0             | 0          | 0              | 0                | 0             | 0           |
    /// | 1     | 20                            | 0               | 0           | 0             | 0          | 0              | 0                | 0             | 0           |
    /// | 2     | 400                           | 0               | 0           | 0             | 0          | 0              | 0                | 0             | 0           |
    /// | 3     | 8,902                         | 34              | 0           | 0             | 0          | 12             | 0                | 0             | 0           |
    /// | 4     | 197,281                       | 1576            | 0           | 0             | 0          | 469            | 0                | 0             | 8           |
    /// | 5     | 4,865,609                     | 82,719          | 258         | 0             | 0          | 27,351         | 6                | 0             | 347         |
    /// | 6     | 119,060,324                   | 2,812,008       | 5248        | 0             | 0          | 809,099        | 329              | 46            | 10,828      |
    /// | 7     | 3,195,901,860                 | 108,329,926     | 319,617     | 883,453       | 0          | 33,103,848     | 18,026           | 1628          | 435,767     |
    /// | 8     | 84,998,978,956                | 3,523,740,106   | 7,187,977   | 23,605,205    | 0          | 968,981,593    | 847,039          | 147,215       | 9,852,036   |
    /// | 9     | 2,439,530,234,167             | 125,208,536,153 | 319,496,827 | 1,784,356,000 | 17,334,376 | 36,095,901,903 | 37,101,713       | 5,547,231     | 400,191,963 |
    /// | 10    | 69,352,859,712,417            |                 |             |               |            |                |                  |               |             |
    /// | 11    | 2,097,651,003,696,806         |                 |             |               |            |                |                  |               |             |
    /// | 12    | 62,854,969,236,701,747        |                 |             |               |            |                |                  |               |             |
    /// | 13    | 1,981,066,775,000,396,239     |                 |             |               |            |                |                  |               |             |
    /// | 14    | 61,885,021,521,585,529,237    |                 |             |               |            |                |                  |               |             |
    /// | 15    | 2,015,099,950,053,364,471,960 |                 |             |               |            |                |                  |               |             |
    #[test]
    fn perft_1() {
        let b = Board::new();
        let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
        assert_eq!(legal_moves.len(), 20);

        let mut total_nodes = 0;
        for m in legal_moves {
            let b = b.execute_move(&m);
            let legal_moves = b.get_legal_moves(&b.get_active_color()).unwrap();
            total_nodes += legal_moves.len();
        }
        assert_eq!(total_nodes, 400);
    }

    #[test]
    fn perft_2() {
        let b = Board::new();
        assert_eq!(minimax_no_pruning(1, &b).unwrap().nodes_searched, 20);
        assert_eq!(minimax_no_pruning(2, &b).unwrap().nodes_searched, 400);
        assert_eq!(minimax_no_pruning(3, &b).unwrap().nodes_searched, 8902);
        assert_eq!(minimax_no_pruning(4, &b).unwrap().nodes_searched, 197281);
    }
}
