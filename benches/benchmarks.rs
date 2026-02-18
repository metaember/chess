use criterion::{black_box, criterion_group, criterion_main, Criterion, SamplingMode};
use rust_chess::board::Board;
use rust_chess::types::Color;
use rust_chess::evaluate::evaluate_board;
use rust_chess::game::Game;
use rust_chess::search::{minimax, negamax, negamax_fast, search, MAX_SCORE, MIN_SCORE};

const SEB_FEN: &str = "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1";


pub fn bench_legal_moves_from_start(c: &mut Criterion) {
    let bo = Board::new();
    c.bench_function("get legal moves from start", |b| {
        b.iter(|| bo.get_legal_moves(black_box(&Color::White)))
    });
}

pub fn bench_legal_moves_from_seb(c: &mut Criterion) {
    let bo = Board::from_fen(SEB_FEN);
    c.bench_function("get legal moves from seb", |b| {
        b.iter(|| bo.get_legal_moves(black_box(&Color::White)))
    });
}

pub fn bench_search_4(c: &mut Criterion) {
    let bo = Board::new();
    c.bench_function("search from start 4 ply", |b| {
        b.iter(|| search(black_box(4), black_box(&bo)))
    });
}

pub fn bench_search_5(c: &mut Criterion) {
    let bo = Board::new();
    c.bench_function("search from start 5 ply", |b| {
        b.iter(|| search(black_box(5), black_box(&bo)))
    });
}

pub fn bench_search_2_seb(c: &mut Criterion) {
    let bo = Board::from_fen(SEB_FEN);
    c.bench_function("search from seb 2 ply", |b| {
        b.iter(|| search(black_box(2), black_box(&bo)))
    });
}

pub fn bench_search_4_seb(c: &mut Criterion) {
    let mut group = c.benchmark_group("flat-sampling");
    // group.sampling_mode(SamplingMode::Flat).sample_size(10);
    group.sample_size(10);

    let bo = Board::from_fen(SEB_FEN);
    group.bench_function("search from seb 4 ply", |b| {
        b.iter(|| search(black_box(4), black_box(&bo)))
    });
    group.finish();
}

pub fn bench_search_5_seb(c: &mut Criterion) {
    let bo = Board::from_fen(SEB_FEN);
    c.bench_function("search from seb 5 ply", |b| {
        b.iter(|| search(black_box(5), black_box(&bo)))
    });
}

pub fn bench_minimax_4_sebastian(c: &mut Criterion) {
    let bo = Board::from_fen(SEB_FEN);
    c.bench_function("minimax from sebastian 4 ply + quies", |b| {
        b.iter(|| {
            negamax(
                black_box(4),
                black_box(&bo),
                MIN_SCORE,
                MAX_SCORE,
                true,
                true,
                false,
            )
        })
    });
}

pub fn bench_minimax_4_sebastian_fast(c: &mut Criterion) {
    let bo = Board::from_fen(SEB_FEN);
    c.bench_function("minimax from sebastian 4 ply + quies fast", |b| {
        b.iter(|| negamax_fast(black_box(4), black_box(&bo), MIN_SCORE, MAX_SCORE, true))
    });
}

pub fn bench_evaluate_start(c: &mut Criterion) {
    let bo = Board::new();
    c.bench_function("evaluate start", |b| {
        b.iter(|| evaluate_board(black_box(&bo)))
    });
}

pub fn bench_evaluate_sebastian(c: &mut Criterion) {
    let bo = Board::from_fen(SEB_FEN);
    c.bench_function("evaluate sebastian", |b| {
        b.iter(|| evaluate_board(black_box(&bo)))
    });
}

pub fn bench_moves_from_start_4_deep_1_move(c: &mut Criterion) {
    c.bench_function("move from start 4 deep 1 move", |b| {
        b.iter(|| Game::new_silent(black_box(4)).play(black_box(1), false, false))
    });
}

pub fn bench_moves_from_start_5_deep_1_move(c: &mut Criterion) {
    c.bench_function("move from start 5 deep 1 move", |b| {
        b.iter(|| Game::new_silent(black_box(5)).play(black_box(1), false, false))
    });
}

pub fn bench_moves_from_start_2_deep_1_sebastian(c: &mut Criterion) {
    let mut group = c.benchmark_group("flat-sampling");
    group.sample_size(10);
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("move from start 2 deep 1 move sebastian", |b| {
        b.iter(|| {
            Game::new_from_fen_silent(black_box(2), black_box(SEB_FEN.to_owned()))
                .play(black_box(1), false, false)
        })
    });
    group.finish();
}


pub fn bench_moves_from_start_4_deep_1_sebastian(c: &mut Criterion) {
    c.bench_function("move from start 4 deep 1 move sebastian", |b| {
        b.iter(|| {
            Game::new_from_fen_silent(black_box(4), black_box(SEB_FEN.to_owned()))
                .play(black_box(1), false, false)
        })
    });
}

pub fn bench_moves_from_start_5_deep_1_sebastian(c: &mut Criterion) {
    c.bench_function("move from start 5 deep 1 move sebastian", |b| {
        b.iter(|| {
            Game::new_from_fen_silent(black_box(5), black_box(SEB_FEN.to_owned()))
                .play(black_box(1), false, false)
        })
    });
}
criterion_group!(
    benches,
    bench_legal_moves_from_start,
    bench_legal_moves_from_seb,
    // bench_search_4,
    // bench_search_5,
    // bench_search_2_seb,
    // bench_search_4_seb,
    // bench_search_5_seb,
    // bench_minimax_4_sebastian,
    // bench_minimax_4_sebastian_fast,
    bench_evaluate_start,
    bench_evaluate_sebastian,
    // bench_moves_from_start_4_deep_1_move,
    // bench_moves_from_start_5_deep_1_move,
    bench_moves_from_start_2_deep_1_sebastian,
    // bench_moves_from_start_4_deep_1_sebastian,
    // bench_moves_from_start_5_deep_1_sebastian
);
criterion_main!(benches);
