use crate::board::Board;

/// Optimized perft using make_move/unmake_move (in-place) and bulk counting
pub fn run_perft_fast(board: &mut Board, depth: u8) -> u64 {
    if depth == 0 {
        return 1;
    }

    let moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(_) => return 0, // checkmate or stalemate
    };

    // Bulk counting optimization: at depth 1, just return the move count
    // instead of making/unmaking each move
    if depth == 1 {
        return moves.len() as u64;
    }

    let mut nodes = 0u64;
    for m in moves {
        let undo = board.make_move(&m);
        nodes += run_perft_fast(board, depth - 1);
        board.unmake_move(&undo);
    }
    nodes
}

/// Perft divide: shows node count per root move (for debugging)
pub fn run_perft_divide(board: &mut Board, depth: u8) -> Vec<(String, u64)> {
    let moves = match board.get_legal_moves(&board.get_active_color()) {
        Ok(moves) => moves,
        Err(_) => return vec![],
    };

    let mut results = Vec::new();
    for m in moves {
        let uci = format!("{}{}", m.from.to_algebraic(), m.to.to_algebraic());
        let undo = board.make_move(&m);
        let nodes = if depth <= 1 { 1 } else { run_perft_fast(board, depth - 1) };
        board.unmake_move(&undo);
        results.push((uci, nodes));
    }
    results.sort_by(|a, b| a.0.cmp(&b.0));
    results
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
fn get_perft_expected_node_count(depth: u8) -> u64 {
    match depth {
        0 => 1,
        1 => 20,
        2 => 400,
        3 => 8902,
        4 => 197_281,
        5 => 4_865_609,
        6 => 119_060_324,
        7 => 3_195_901_860,
        8 => 84_998_978_956,
        9 => 2_439_530_234_167,
        10 => 69_352_859_712_417,
        11 => 2_097_651_003_696_806,
        12 => 62_854_969_236_701_747,
        13 => 1_981_066_775_000_396_239,
        _ => panic!("No expected node count for depth {}", depth),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;
    use crate::types::{Color, MoveFlag};
    use std::time::Instant;

    const MAX_DEPTH: u8 = 6;

    #[test]
    fn perft_start_fast() {
        let mut board = Board::new();
        for depth in 0..=MAX_DEPTH {
            let expected_node_count = get_perft_expected_node_count(depth);

            let start = Instant::now();
            let node_count = run_perft_fast(&mut board, depth);
            let elapsed = start.elapsed();

            let nps = if elapsed.as_secs_f64() > 0.0 {
                node_count as f64 / elapsed.as_secs_f64() / 1_000_000.0
            } else {
                0.0
            };

            println!("Depth {}: {} nodes in {:?} ({:.2} M nodes/sec)",
                     depth, node_count, elapsed, nps);
            assert_eq!(expected_node_count, node_count);
        }
    }

    #[test]
    fn perft_pos_5_fast() {
        let mut board = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

        let expected: Vec<u64> = vec![0, 44, 1_486, 62_379, 2_103_487, 89_941_194];

        for depth in 1..expected.len() {
            let start = Instant::now();
            let node_count = run_perft_fast(&mut board, depth as u8);
            let elapsed = start.elapsed();

            let nps = if elapsed.as_secs_f64() > 0.0 {
                node_count as f64 / elapsed.as_secs_f64() / 1_000_000.0
            } else {
                0.0
            };

            println!("Depth {}: {} nodes in {:?} ({:.2} M nodes/sec)",
                     depth, node_count, elapsed, nps);
            assert_eq!(expected[depth], node_count);
        }
    }

    /// Helper to run perft_fast on a FEN with expected values at each depth
    fn run_perft_suite(fen: &str, name: &str, expected: &[u64]) {
        let mut board = Board::from_fen(fen);
        for (depth, &expected_nodes) in expected.iter().enumerate() {
            let depth = depth as u8 + 1; // expected[0] is depth 1
            let start = Instant::now();
            let node_count = run_perft_fast(&mut board, depth);
            let elapsed = start.elapsed();
            let nps = if elapsed.as_secs_f64() > 0.0 {
                node_count as f64 / elapsed.as_secs_f64() / 1_000_000.0
            } else {
                0.0
            };
            println!("{} depth {}: {} nodes in {:?} ({:.2} Mnps)",
                     name, depth, node_count, elapsed, nps);
            assert_eq!(expected_nodes, node_count,
                "{} depth {}: expected {} got {}", name, depth, expected_nodes, node_count);
        }
    }

    #[test]
    fn perft_kiwipete() {
        run_perft_suite(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            "Kiwipete",
            &[48, 2_039, 97_862, 4_085_603, 193_690_690],
        );
    }

    #[test]
    fn perft_position3() {
        run_perft_suite(
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
            "Position 3",
            &[14, 191, 2_812, 43_238, 674_624],
        );
    }

    #[test]
    fn perft_position4() {
        run_perft_suite(
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
            "Position 4",
            &[6, 264, 9_467, 422_333, 15_833_292],
        );
    }

    #[test]
    fn perft_position6() {
        run_perft_suite(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 1",
            "Position 6",
            &[46, 2_079, 89_890, 3_894_594, 164_075_551],
        );
    }

    #[test]
    fn en_passant_discovered_check_position3() {
        // Position 3: en passant capture f4xe3 or f4xg3 would expose the king on h4
        // to the rook on b4 along the 4th rank — these EP captures must be illegal.
        let mut board = Board::from_fen("8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - e3 0 1");
        let moves = board.get_legal_moves(&Color::Black).unwrap();
        let ep_move = moves.iter().find(|m| m.move_flag == MoveFlag::EnPassantCapture);
        assert!(ep_move.is_none(),
            "f4xe3 en passant should be illegal — exposes king on h4 to Rb4");

        let mut board2 = Board::from_fen("8/2p5/3p4/KP5r/1R3pPk/8/4P3/8 b - g3 0 1");
        let moves2 = board2.get_legal_moves(&Color::Black).unwrap();
        let ep_move2 = moves2.iter().find(|m| m.move_flag == MoveFlag::EnPassantCapture);
        assert!(ep_move2.is_none(),
            "f4xg3 en passant should be illegal — exposes king on h4 to Rb4");
    }
}
