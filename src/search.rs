use rand::prelude::*;
use crate::board::*;
use crate::evaluate::*;

const MAX_SCORE: i32 = 99999;
const MIN_SCORE: i32 = -99999;

const DEBUG: bool = false;


pub struct SearchResult {
    pub best_move: Option<Move>,
    pub best_score: i32,
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
        Err(Status::Checkmate(_)) => return SearchResult{best_move: None, best_score: MIN_SCORE, nodes_searched: 0},
        Err(Status::Stalemate) => return SearchResult{best_move: None, best_score: 0, nodes_searched: 0},
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    let mut best_move = &legal_moves[0];
    let mut best_score = MIN_SCORE;
    let mut total_nodes_searched = 0;

    for m in &legal_moves {
        let b = board.execute_move(&m);
        let res = minimax_helper(max_depth - 1, &b);
        let score = res.0 as i32 * -1;
        total_nodes_searched += res.1;

        if score > best_score {
            best_score = score;
            best_move = m;
        }
    };
    SearchResult {best_move: Some(*best_move), best_score, nodes_searched: total_nodes_searched }
}

pub fn minimax_helper(max_depth: u8, board: &Board) -> (i32, i32) {
    if max_depth == 0 {
        // If we've reached the maximum depth, evaluate the board naively
        return (evaluate_board(board), 1);
    }

    let legal_moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(Status::Checkmate(_)) => return (MIN_SCORE, 0),
        Err(Status::Stalemate) => return (0, 0),
        _ => panic!("No legal moves, not a stalemate or a checkmate"),
    };

    if DEBUG {
        // for debugging
        let _board_str = board.draw_board();
    }

    let mut best_score = MIN_SCORE;
    let mut total_nodes_searched = 0;

    for m in &legal_moves {
        let b = board.execute_move(&m);
        let res = minimax_helper(max_depth - 1, &b);
        let score = res.0 * -1;
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



mod test {
    use super::*;

    #[test]
    fn test_blunders_knight() {
        // . ♞ . ♛ ♚ ♝ . ♜
        // ♜ ♝ ♟︎ ♟︎ ♟︎ . ♟︎ ♟︎
        // ♟︎ ♟︎ . . . . . .
        // . . . . . ♟︎ . .
        // . . . . ♘ . . .
        // . . . ♕ ♙ . . .
        // ♙ ♙ ♙ ♙ . ♙ ♙ ♙
        // ♖ . ♗ . ♔ . ♘ ♖
        let fen = "1n1qkb1r/rbppp1pp/pp6/5p2/4N3/3QP3/PPPP1PPP/R1B1K1NR w KQk - 0 8";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();
        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('N', "e4"));
        assert!(false);
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
        let fen = "5rk1/ppp2pbp/3p2p1/1q6/4r1P1/1NP1B3/PP2nPP1/R2QR2K b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('q', "b5"));
        assert_eq!(m.to, Position::from_algebraic("h5"));

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert_eq!(m.piece, Piece::from_algebraic('P', "g4"));
        assert_eq!(m.to, Position::from_algebraic("h5"));
        assert!(m.captured.is_some_and(|p| p == Piece::from_algebraic('q', "h5")));

        let m = search(4, &b);
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
        let fen = "r1b1kb1r/pppp1ppp/5q2/4n3/3KP3/2N3PN/PPP4P/R1BQ1B1R b kq - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();

        let m = search(5, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('b', "f8"));
        assert!(m.to == Position::from_algebraic("c5"));

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('K', "d4"));
        assert!(m.to == Position::from_algebraic("c5"));

        let m = search(4, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('q', "f6"));
        assert!(m.to == Position::from_algebraic("b6"));

        let m = search(2, &b);
        print!("{} {}", m.to_human(), m.to_algebraic());
        assert!(m.piece == Piece::from_algebraic('K', "c5"));
        assert!(m.to == Position::from_algebraic("d5"));

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
        let fen = "5r2/6k1/p2p4/6n1/P3p3/8/5P2/2q2QKR b - - 0 1";
        let b = Board::from_fen(fen);
        b.draw_to_terminal();

        let m = search(5, &b);  // this needs to be depth 5 to find the mate in 3
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

}