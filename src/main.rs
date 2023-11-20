mod board;
mod evaluate;
mod search;
mod game;

use crate::game::*;


fn run() {
    let mut game = Game::new(4);
    game.play(20);
    println!("{}", game.to_pgn());
}

fn main() {
    run();
}