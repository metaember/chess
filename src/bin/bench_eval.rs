//! Micro-benchmark for evaluate_board: reports ns/call for each term group.

use rust_chess::bitboard::{bishop_attacks, rook_attacks, FILE_MASKS, ATTACK_TABLES};
use rust_chess::board::Board;
use rust_chess::evaluate::{
    evaluate_board, evaluate_mobility, evaluate_outposts, evaluate_pawn_structure, evaluate_rooks,
};
use rust_chess::types::{Color, PieceType};
use std::hint::black_box;
use std::time::Instant;

const ITERS: u64 = 2_000_000;

fn time_ns<F: FnMut()>(mut f: F, calls: u64) -> f64 {
    // warmup
    for _ in 0..1000 { f(); }
    let t = Instant::now();
    for _ in 0..calls { f(); }
    t.elapsed().as_nanos() as f64 / calls as f64
}

fn main() {
    let positions: Vec<(&str, Board)> = vec![
        ("Starting pos",   Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")),
        ("Italian",        Board::from_fen("r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4")),
        ("Middlegame",     Board::from_fen("r2qkb1r/ppp2ppp/2n1bn2/3pp3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R w KQkq - 0 6")),
        ("Complex",        Board::from_fen("r1bq1rk1/ppp2ppp/2n1pn2/3p4/1bPP4/2NBPN2/PP3PPP/R1BQK2R w KQ - 0 7")),
        ("Endgame",        Board::from_fen("8/pp3pk1/2r5/2P5/4R3/5PK1/P7/8 w - - 0 1")),
        ("Pawn endgame",   Board::from_fen("8/5k2/3p4/1p1Pp2p/pP2Pp1P/P4P2/8/1K6 w - - 0 1")),
    ];

    println!("evaluate_board term breakdown ({ITERS}M total calls)");
    println!("{}", "=".repeat(70));
    println!("{:<18} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
        "Position", "full eval", "mat+PST", "pawn str", "mobility", "outposts", "rooks");
    println!("{}", "-".repeat(70));

    let mut total_full = 0.0_f64;
    let mut total_mat  = 0.0_f64;
    let mut total_pawn = 0.0_f64;
    let mut total_mob  = 0.0_f64;
    let mut total_out  = 0.0_f64;
    let mut total_rook = 0.0_f64;
    let n = positions.len() as u64;

    for (name, board) in &positions {
        let wp = board.get_piece_bb(Color::White, PieceType::Pawn);
        let bp = board.get_piece_bb(Color::Black, PieceType::Pawn);

        let full = time_ns(|| { black_box(evaluate_board(board)); }, ITERS);

        let mat = time_ns(|| {
            let phase = board.get_phase();
            let mg = board.get_material(Color::White) + board.get_pst_mg(Color::White)
                   - board.get_material(Color::Black) - board.get_pst_mg(Color::Black);
            let eg = board.get_material(Color::White) + board.get_pst_eg(Color::White)
                   - board.get_material(Color::Black) - board.get_pst_eg(Color::Black);
            black_box((mg * phase + eg * (24 - phase)) / 24);
        }, ITERS);

        let pawn = time_ns(|| {
            black_box(evaluate_pawn_structure(wp, bp));
        }, ITERS);

        let mob = time_ns(|| {
            black_box(evaluate_mobility(board, wp, bp));
        }, ITERS);

        let out = time_ns(|| {
            black_box(evaluate_outposts(board, wp, bp));
        }, ITERS);

        let rook = time_ns(|| {
            black_box(evaluate_rooks(board, wp, bp));
        }, ITERS);

        println!("{:<18} {:>9.1}ns {:>9.1}ns {:>9.1}ns {:>9.1}ns {:>9.1}ns {:>9.1}ns",
            name, full, mat, pawn, mob, out, rook);

        total_full += full; total_mat += mat; total_pawn += pawn;
        total_mob  += mob;  total_out += out;  total_rook += rook;
    }

    println!("{}", "-".repeat(70));
    println!("{:<18} {:>9.1}ns {:>9.1}ns {:>9.1}ns {:>9.1}ns {:>9.1}ns {:>9.1}ns",
        "AVERAGE",
        total_full/n as f64, total_mat/n as f64, total_pawn/n as f64,
        total_mob/n as f64, total_out/n as f64, total_rook/n as f64);

    println!();

    // Break down mobility: knight vs bishop+rook magic
    println!("Mobility breakdown (middlegame position):");
    println!("{}", "-".repeat(50));
    let board = &positions[2].1; // middlegame
    let wp = board.get_piece_bb(Color::White, PieceType::Pawn);
    let bp = board.get_piece_bb(Color::Black, PieceType::Pawn);
    let occupied = board.get_occupied();

    let knight_mob = time_ns(|| {
        let mut s = 0i32;
        for sq in rust_chess::bitboard::BitboardIter(board.get_piece_bb(Color::White, PieceType::Knight)) {
            s += (ATTACK_TABLES.knight[sq as usize]).count_ones() as i32;
        }
        for sq in rust_chess::bitboard::BitboardIter(board.get_piece_bb(Color::Black, PieceType::Knight)) {
            s += (ATTACK_TABLES.knight[sq as usize]).count_ones() as i32;
        }
        black_box(s);
    }, ITERS);

    let bishop_mob = time_ns(|| {
        let mut s = 0i32;
        for sq in rust_chess::bitboard::BitboardIter(board.get_piece_bb(Color::White, PieceType::Bishop)) {
            s += bishop_attacks(sq, occupied).count_ones() as i32;
        }
        for sq in rust_chess::bitboard::BitboardIter(board.get_piece_bb(Color::Black, PieceType::Bishop)) {
            s += bishop_attacks(sq, occupied).count_ones() as i32;
        }
        black_box(s);
    }, ITERS);

    let rook_mob = time_ns(|| {
        let mut s = 0i32;
        for sq in rust_chess::bitboard::BitboardIter(board.get_piece_bb(Color::White, PieceType::Rook)) {
            s += rook_attacks(sq, occupied).count_ones() as i32;
        }
        for sq in rust_chess::bitboard::BitboardIter(board.get_piece_bb(Color::Black, PieceType::Rook)) {
            s += rook_attacks(sq, occupied).count_ones() as i32;
        }
        black_box(s);
    }, ITERS);

    println!("  knight mobility only:  {:>7.1} ns", knight_mob);
    println!("  bishop mobility only:  {:>7.1} ns  (magic bitboard, ~256KB table)", bishop_mob);
    println!("  rook mobility only:    {:>7.1} ns  (magic bitboard, ~2MB table)", rook_mob);
    println!();
    println!("  bishop+rook total:     {:>7.1} ns  (would save this by removing them from eval)", bishop_mob + rook_mob);
}
