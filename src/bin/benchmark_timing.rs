use rust_chess::board::Board;
use rust_chess::search::minimax;
use rust_chess::types::Status;
use std::time::Instant;

fn main() {
    println!("Chess Engine Timing Benchmark");
    println!("==============================\n");

    // Test depths 1-8
    for depth in 1..=8 {
        benchmark_depth(depth);
    }
}

fn benchmark_depth(depth: u8) {
    println!("Depth {}", depth);
    println!("{}", "-".repeat(50));

    let mut timings: Vec<u128> = Vec::new();
    let games_to_play = 3;
    let max_moves_per_game = 40; // 40 moves = 80 ply

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

            // Check draw conditions
            if board.check_for_insufficient_material().is_some() {
                break;
            }
            if board.check_for_fifty_move_rule().is_some() {
                break;
            }

            if moves_played >= max_moves_per_game * 2 {
                break;
            }

            // Time the search
            let start = Instant::now();
            let result = minimax(depth, &board);
            let elapsed = start.elapsed().as_millis();

            match result {
                Ok(r) => {
                    if let Some(m) = r.best_move {
                        board.make_move(&m);
                        timings.push(elapsed);
                        moves_played += 1;
                    } else {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        println!("{} moves", moves_played);
    }

    if timings.is_empty() {
        println!("  No data collected\n");
        return;
    }

    // Calculate statistics
    timings.sort();
    let count = timings.len();
    let sum: u128 = timings.iter().sum();
    let mean = sum as f64 / count as f64;
    let median = if count % 2 == 0 {
        (timings[count / 2 - 1] + timings[count / 2]) as f64 / 2.0
    } else {
        timings[count / 2] as f64
    };
    let min = timings[0];
    let max = timings[count - 1];
    let p90_idx = ((count as f64 * 0.9) as usize).min(count - 1);
    let p95_idx = ((count as f64 * 0.95) as usize).min(count - 1);
    let p90 = timings[p90_idx];
    let p95 = timings[p95_idx];

    println!("\n  Statistics ({} moves total):", count);
    println!("    Mean:   {:.0}ms", mean);
    println!("    Median: {:.0}ms", median);
    println!("    Min:    {}ms", min);
    println!("    Max:    {}ms", max);
    println!("    P90:    {}ms", p90);
    println!("    P95:    {}ms", p95);

    // Suggested description
    let description = if mean < 10.0 {
        "Instant".to_string()
    } else if mean < 100.0 {
        "Fast".to_string()
    } else if mean < 500.0 {
        "~0.5 seconds".to_string()
    } else if mean < 1500.0 {
        "~1 second".to_string()
    } else if mean < 5000.0 {
        format!("~{} seconds", (mean / 1000.0).round() as u32)
    } else if mean < 30000.0 {
        format!("~{} seconds", (mean / 1000.0).round() as u32)
    } else if mean < 120000.0 {
        format!("~{} minute", (mean / 60000.0).round() as u32)
    } else {
        "Very slow".to_string()
    };
    println!("    Suggested: \"{}\"", description);
    println!();
}
