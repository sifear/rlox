use dispatch::{run_file, run_prompt};
use std::{env::args, process::exit};

mod dispatch;
mod parser;
mod scanner;
mod token;
mod environment;

fn main() {
    let args = args().collect::<Vec<String>>();
    println!("{:?}", args);

    if args.len() > 2 {
        println!("Usage: jlox: [script]");
        exit(64);
    }
    if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt();
    }
}
