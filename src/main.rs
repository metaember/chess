use clap::Parser;

mod board;
mod evaluate;
mod search;
mod game;


use crate::game::*;


/// Run a chess game of the bot against itself
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// number of moves deep to search
    #[arg(short, long, default_value_t = 4)]
    depth: u8,

    /// Number of moves to play
    #[arg(short, long, default_value_t = 20)]
    count: u32,
}


fn main() {
    let args = Args::parse();
    let mut game = Game::new(args.depth);
    game.play(args.count as i32);
    println!("{}", game.to_pgn());
}