use criterion::{black_box, criterion_group, criterion_main, Criterion};
use arrayvec::ArrayVec;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Move {
    from: u8,
    to: u8,
    piece: u8,
    captured: Option<u8>,
    flag: u8,
}

impl Move {
    fn new(from: u8, to: u8) -> Self {
        Move {
            from,
            to,
            piece: 0,
            captured: None,
            flag: 0,
        }
    }
}

fn generate_moves_vec(count: usize) -> Vec<Move> {
    (0..count).map(|i| Move::new(i as u8, (i + 1) as u8)).collect()
}

fn generate_moves_arrayvec(count: usize) -> ArrayVec<Move, 256> {
    (0..count).map(|i| Move::new(i as u8, (i + 1) as u8)).collect()
}

fn filter_vec(moves: Vec<Move>) -> Vec<Move> {
    moves.into_iter().filter(|m| m.from % 2 == 0).collect()
}

fn filter_arrayvec(moves: ArrayVec<Move, 256>) -> ArrayVec<Move, 256> {
    moves.into_iter().filter(|m| m.from % 2 == 0).collect()
}

fn vec_to_arrayvec(moves: Vec<Move>) -> ArrayVec<Move, 256> {
    moves.into_iter().collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("vec_generate_30", |b| {
        b.iter(|| black_box(generate_moves_vec(30)))
    });

    c.bench_function("arrayvec_generate_30", |b| {
        b.iter(|| black_box(generate_moves_arrayvec(30)))
    });

    c.bench_function("vec_filter_30", |b| {
        let moves = generate_moves_vec(30);
        b.iter(|| black_box(filter_vec(moves.clone())))
    });

    c.bench_function("arrayvec_filter_30", |b| {
        let moves = generate_moves_arrayvec(30);
        b.iter(|| black_box(filter_arrayvec(moves.clone())))
    });

    c.bench_function("vec_to_arrayvec_conversion_30", |b| {
        let moves = generate_moves_vec(30);
        b.iter(|| black_box(vec_to_arrayvec(moves.clone())))
    });

    // Simulate the full get_legal_moves flow
    c.bench_function("vec_full_chain", |b| {
        b.iter(|| {
            let moves = generate_moves_vec(40);
            let moves = filter_vec(moves);
            let moves = filter_vec(moves);
            black_box(moves)
        })
    });

    c.bench_function("arrayvec_full_chain", |b| {
        b.iter(|| {
            let moves = generate_moves_arrayvec(40);
            let moves = filter_arrayvec(moves);
            let moves = filter_arrayvec(moves);
            black_box(moves)
        })
    });

    c.bench_function("hybrid_vec_to_arrayvec", |b| {
        b.iter(|| {
            let moves = generate_moves_vec(40);
            let moves = filter_vec(moves);
            let moves = filter_vec(moves);
            let moves = vec_to_arrayvec(moves);
            black_box(moves)
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
