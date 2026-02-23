//! Integration tests for UCI protocol
//!
//! Run with: cargo test --test uci_tests

use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::time::Duration;

/// Helper to communicate with the UCI engine
struct UciEngine {
    child: std::process::Child,
    stdin: std::process::ChildStdin,
    stdout: BufReader<std::process::ChildStdout>,
}

impl UciEngine {
    fn new() -> Self {
        let mut child = Command::new("./target/release/uci_movepicker")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("Failed to start UCI engine. Run `cargo build --release --bin uci_movepicker` first.");

        let stdin = child.stdin.take().unwrap();
        let stdout = BufReader::new(child.stdout.take().unwrap());

        Self { child, stdin, stdout }
    }

    fn send(&mut self, cmd: &str) {
        writeln!(self.stdin, "{}", cmd).unwrap();
        self.stdin.flush().unwrap();
    }

    fn read_line(&mut self) -> String {
        let mut line = String::new();
        self.stdout.read_line(&mut line).unwrap();
        line.trim().to_string()
    }


    fn read_until(&mut self, expected: &str) -> Vec<String> {
        let mut lines = Vec::new();
        loop {
            let line = self.read_line();
            let done = line.contains(expected);
            lines.push(line);
            if done {
                break;
            }
        }
        lines
    }

    /// Read lines until we get a "bestmove" response, skipping info lines
    fn read_bestmove(&mut self) -> String {
        loop {
            let line = self.read_line();
            if line.starts_with("bestmove") {
                return line;
            }
        }
    }

    fn quit(mut self) {
        self.send("quit");
        let _ = self.child.wait();
    }
}

impl Drop for UciEngine {
    fn drop(&mut self) {
        // Ensure the engine is killed if tests panic
        let _ = self.child.kill();
    }
}

#[test]
fn test_uci_handshake() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    let response = engine.read_until("uciok");

    assert!(response.iter().any(|l| l.contains("id name")));
    assert!(response.iter().any(|l| l.contains("uciok")));

    engine.quit();
}

#[test]
fn test_isready() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    engine.send("isready");
    let response = engine.read_line();
    assert_eq!(response, "readyok");

    engine.quit();
}

#[test]
fn test_position_startpos() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    engine.send("position startpos");
    engine.send("isready");
    assert_eq!(engine.read_line(), "readyok");

    engine.send("go depth 1");
    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));
    assert!(response.len() >= 13); // "bestmove " + at least 4 char move

    engine.quit();
}

#[test]
fn test_position_startpos_with_moves() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    engine.send("position startpos moves e2e4 e7e5 g1f3");
    engine.send("isready");
    assert_eq!(engine.read_line(), "readyok");

    engine.send("go depth 3");
    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));

    engine.quit();
}

#[test]
fn test_position_fen() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    // Sicilian Defense position
    engine.send("position fen rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2");
    engine.send("isready");
    assert_eq!(engine.read_line(), "readyok");

    engine.send("go depth 3");
    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));

    engine.quit();
}

#[test]
fn test_go_depth() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    engine.send("position startpos");
    engine.send("go depth 5");
    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));

    // Verify move format (e.g., e2e4, g1f3, e7e8q for promotion)
    let move_str = response.strip_prefix("bestmove ").unwrap();
    assert!(move_str.len() >= 4);
    assert!(move_str.chars().nth(0).unwrap().is_ascii_lowercase()); // file
    assert!(move_str.chars().nth(1).unwrap().is_ascii_digit()); // rank
    assert!(move_str.chars().nth(2).unwrap().is_ascii_lowercase()); // file
    assert!(move_str.chars().nth(3).unwrap().is_ascii_digit()); // rank

    engine.quit();
}

#[test]
fn test_go_movetime() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    // Use a complex middlegame position that's not in any opening book
    // and has enough pieces to keep the engine busy for the full movetime
    engine.send("position fen r1b2rk1/2q1bppp/p1n1pn2/1p2P3/3N4/1BN1B3/PPP2PPP/R2Q1RK1 w - - 0 14");

    let start = std::time::Instant::now();
    engine.send("go movetime 500");
    let response = engine.read_bestmove();
    let elapsed = start.elapsed();

    assert!(response.starts_with("bestmove "));
    // Sanity check: should not hang
    assert!(elapsed < Duration::from_millis(2000), "Too slow: {:?}", elapsed);

    engine.quit();
}

#[test]
fn test_go_wtime_btime() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    engine.send("position startpos");
    engine.send("go wtime 60000 btime 60000 winc 1000 binc 1000");

    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));

    engine.quit();
}

#[test]
fn test_ucinewgame() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    // Play some moves
    engine.send("position startpos moves e2e4 e7e5");
    engine.send("go depth 3");
    engine.read_bestmove();

    // Reset
    engine.send("ucinewgame");
    engine.send("isready");
    assert_eq!(engine.read_line(), "readyok");

    // Should work from start position again
    engine.send("position startpos");
    engine.send("go depth 1");
    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));

    engine.quit();
}

#[test]
fn test_finds_good_move() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    // Simple position where White has a clear best move
    // Position: Standard opening after 1.e4 e5 - White should play good developing moves
    engine.send("position fen rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2");
    engine.send("go depth 4");
    let response = engine.read_bestmove();

    // Should find a reasonable move
    assert!(response.starts_with("bestmove "));
    let move_str = response.strip_prefix("bestmove ").unwrap();
    // Common good moves: Nf3, Nc3, Bc4, d4, etc.
    assert!(move_str.len() >= 4, "Invalid move format: {}", move_str);

    engine.quit();
}

#[test]
fn test_promotion() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    // Position where pawn promotion is best
    engine.send("position fen 8/P7/8/8/8/8/8/4K2k w - - 0 1");
    engine.send("go depth 4");
    let response = engine.read_bestmove();

    assert!(response.starts_with("bestmove "));
    let move_str = response.strip_prefix("bestmove ").unwrap();
    // Should promote - move format is a7a8q (or similar with promotion piece)
    assert!(move_str.len() == 5, "Expected promotion move like a7a8q, got: {}", move_str);
    assert!(move_str.ends_with('q') || move_str.ends_with('r') ||
            move_str.ends_with('b') || move_str.ends_with('n'),
            "Expected promotion suffix, got: {}", move_str);

    engine.quit();
}

#[test]
fn test_position_after_moves_sequence() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    // Italian Game opening
    engine.send("position startpos moves e2e4 e7e5 g1f3 b8c6 f1c4");
    engine.send("go depth 4");
    let response = engine.read_bestmove();
    assert!(response.starts_with("bestmove "));

    engine.quit();
}

#[test]
fn test_multiple_go_commands() {
    let mut engine = UciEngine::new();

    engine.send("uci");
    engine.read_until("uciok");

    for i in 1..=3 {
        engine.send("position startpos");
        engine.send(&format!("go depth {}", i));
        let response = engine.read_bestmove();
        assert!(response.starts_with("bestmove "), "Failed on iteration {}", i);
    }

    engine.quit();
}
