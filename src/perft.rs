use crate::board::Board;


pub fn run_perft_test(board: &Board, depth: u8) -> i64 {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;
    let moves = board.get_legal_moves(&board.get_active_color()).unwrap();
    for m in moves {
        let b = board.execute_move(&m);
        nodes += run_perft_test(&b, depth - 1);
    }
    nodes
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
fn get_perft_expected_node_count(depth: u8) -> i64 {
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

    const MAX_DEPTH: u8 = 6;

    #[test]
    fn perft_start() {
        let board = Board::new();
        for depth in 0..=MAX_DEPTH {
            println!("Depth {}", depth);
            let expected_node_count = get_perft_expected_node_count(depth);
            let node_count = run_perft_test(&board, depth);
            assert_eq!(expected_node_count, node_count);
        }
    }

    /// Run the perft test on the "position 5" from
    /// https://www.chessprogramming.org/Perft_Results
    ///
    /// Depth | Nodes
    /// ----- | -----
    /// 1     | 44
    /// 2     | 1,486
    /// 3     | 62,379
    /// 4     | 2,103,487
    /// 5     | 89,941,194
    #[test]
    fn perft_pos_5() {
        let board = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");


        let expected = vec![0, 44, 1_486, 62_379, 2_103_487, 89_941_194];

        for depth in 1..=expected.len() {
            println!("Depth {}", depth);
            let node_count = run_perft_test(&board, depth as u8);
            assert_eq!(expected[depth as usize], node_count);
        }
    }

}