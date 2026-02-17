//! Generates reference move data from Stockfish for testing.
//!
//! Run with: cargo run --bin generate_stockfish_reference
//!
//! This reads test positions from test_data/test_positions.epd,
//! queries Stockfish for legal moves at each position,
//! and saves the results to test_data/stockfish_reference.json

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

#[derive(Debug, Serialize, Deserialize)]
struct PositionData {
    id: String,
    fen: String,
    legal_moves: Vec<String>,
    move_count: usize,
}

#[derive(Debug, Serialize, Deserialize)]
struct ReferenceData {
    generated_with: String,
    positions: Vec<PositionData>,
}

fn parse_epd_line(line: &str) -> Option<(String, String)> {
    // Skip comments and empty lines
    let line = line.trim();
    if line.is_empty() || line.starts_with('#') {
        return None;
    }

    // Parse: FEN ; id "name"
    let parts: Vec<&str> = line.split(';').collect();
    if parts.len() < 2 {
        return None;
    }

    let fen = parts[0].trim().to_string();

    // Extract id from ; id "name"
    let id_part = parts[1].trim();
    let id = if id_part.starts_with("id ") {
        id_part[3..].trim().trim_matches('"').to_string()
    } else {
        "unknown".to_string()
    };

    Some((id, fen))
}

fn get_stockfish_moves(fen: &str) -> Result<Vec<String>, String> {
    // Start Stockfish process
    let mut child = Command::new("stockfish")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| format!("Failed to start Stockfish: {}", e))?;

    let stdin = child.stdin.as_mut().ok_or("Failed to get stdin")?;

    // Send UCI commands
    writeln!(stdin, "uci").map_err(|e| e.to_string())?;
    writeln!(stdin, "isready").map_err(|e| e.to_string())?;
    writeln!(stdin, "position fen {}", fen).map_err(|e| e.to_string())?;
    writeln!(stdin, "go perft 1").map_err(|e| e.to_string())?;
    writeln!(stdin, "quit").map_err(|e| e.to_string())?;

    // Read output
    let stdout = child.stdout.take().ok_or("Failed to get stdout")?;
    let reader = BufReader::new(stdout);

    let mut moves: BTreeSet<String> = BTreeSet::new();

    for line in reader.lines() {
        let line = line.map_err(|e| e.to_string())?;
        // Perft output format: "a2a3: 1"
        if line.contains(':') && !line.starts_with("Nodes") && !line.starts_with("info") {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() >= 2 {
                let mv = parts[0].trim();
                // Validate it looks like a move (4-5 chars)
                if mv.len() >= 4 && mv.len() <= 5 {
                    moves.insert(mv.to_string());
                }
            }
        }
    }

    child.wait().map_err(|e| e.to_string())?;

    Ok(moves.into_iter().collect())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let epd_path = "test_data/test_positions.epd";
    let output_path = "test_data/stockfish_reference.json";

    println!("Reading positions from: {}", epd_path);

    let epd_content = fs::read_to_string(epd_path)?;
    let mut positions: Vec<PositionData> = Vec::new();

    for line in epd_content.lines() {
        if let Some((id, fen)) = parse_epd_line(line) {
            print!("Processing '{}' ... ", id);
            std::io::stdout().flush()?;

            match get_stockfish_moves(&fen) {
                Ok(moves) => {
                    let move_count = moves.len();
                    println!("{} moves", move_count);
                    positions.push(PositionData {
                        id,
                        fen,
                        legal_moves: moves,
                        move_count,
                    });
                }
                Err(e) => {
                    println!("ERROR: {}", e);
                }
            }
        }
    }

    // Get Stockfish version
    let version_output = Command::new("stockfish")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;

    let reader = BufReader::new(version_output.stdout.unwrap());
    let version = reader
        .lines()
        .next()
        .unwrap_or(Ok("Unknown".to_string()))?;

    let reference_data = ReferenceData {
        generated_with: version,
        positions,
    };

    let json = serde_json::to_string_pretty(&reference_data)?;
    fs::write(output_path, &json)?;

    println!("\nWrote {} positions to {}", reference_data.positions.len(), output_path);

    Ok(())
}
