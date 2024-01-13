pub mod dispatch;
pub mod scanner;
pub mod token;
pub mod parser;

use dispatch::{run_file, run_prompt};
use parser::test;
use std::{env::args, process::exit};

use crate::scanner::Scanner;

fn main() {
    test();
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
