use rust_chess::board::Board;
use rust_chess::search::iterative_deepening_movepicker;
use rust_chess::tt::TranspositionTable;
use rust_chess::types::Status;
use std::time::Instant;

fn main() {
    println!("Chess Engine Timing Benchmark");
    println!("==============================\n");

    for depth in 1..=6 {
        benchmark_depth(depth);
    }
}

fn benchmark_depth(depth: u8) {
    println!("Depth {}", depth);
    println!("{}", "-".repeat(50));

    let mut timings: Vec<u128> = Vec::new();
    let games_to_play = 3;
    let max_moves_per_game = 20;

    for game_num in 1..=games_to_play {
        print!("  Game {}/{}: ", game_num, games_to_play);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();

        let mut board = Board::new();
        let mut moves_played = 0;

        loop {
            let color = board.get_active_color();
            match board.get_legal_moves(&color) {
                Ok(moves) if moves.is_empty() => break,
                Err(Status::Checkmate(_)) | Err(Status::Stalemate) => break,
                Err(_) => break,
                Ok(_) => {}
            }

            if board.check_for_insufficient_material().is_some() { break; }
            if board.check_for_fifty_move_rule().is_some() { break; }
            if moves_played >= max_moves_per_game * 2 { break; }

            let start = Instant::now();
            let mut tt = TranspositionTable::new(16);
            let result = iterative_deepening_movepicker(&mut board, depth, &mut tt);
            let elapsed = start.elapsed().as_millis();

            if let Some(m) = result.best_move {
                board.make_move(&m);
                timings.push(elapsed);
                moves_played += 1;
            } else {
                break;
            }
        }
        println!("{} moves", moves_played);
    }

    if timings.is_empty() {
        println!("  No data collected\n");
        return;
    }

    timings.sort();
    let count = timings.len();
    let sum: u128 = timings.iter().sum();
    let mean = sum as f64 / count as f64;
    let median = timings[count / 2] as f64;
    println!("  Mean: {:.0}ms  Median: {:.0}ms  Min: {}ms  Max: {}ms\n",
        mean, median, timings[0], timings[count - 1]);
}
