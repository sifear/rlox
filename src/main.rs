use dispatch::{run_file, run_prompt};
use parser::{test2};

mod token;
mod scanner;
mod parser;
mod dispatch;
mod interpreter;

fn main() {
    test2();
    // let args = args().collect::<Vec<String>>();
    // println!("{:?}", args);
    // let args = args().collect::<Vec<String>>();
    // println!("{:?}", args);

    // if args.len() > 2 {
    //     println!("Usage: jlox: [script]");
    //     exit(64);
    // } else if args.len() == 2 {
    //     run_file(&args[1]);
    // } else {
    //     run_prompt();
    // }
}
