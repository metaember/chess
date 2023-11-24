use clap::Parser;
use color_eyre::eyre::Result;

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

    /// Print the search results at each move
    #[arg(short, long, default_value_t = false)]
    print_search: bool,
}



fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Args::parse();

    println!("Starting game with depth: {}, count: {}, print_search: {}", args.depth, args.count, args.print_search);
    let mut game = Game::new(args.depth);
    game.play(args.count as i32, args.print_search);
    println!("{}", game.to_pgn());

    Ok(())
}