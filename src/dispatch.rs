use std::fs::File;
use std::io::{self, Read};

use crate::interpreter::Interpreter;
use crate::parser::Parser;

pub fn run_file(path: &String) {
    let mut buf = vec![];
    let mut file = File::open(path).unwrap();
    let _res = file.read_to_end(&mut buf).unwrap();

    let source = String::from_utf8(buf).unwrap();

    let mut scanner = crate::scanner::Scanner::new(&source);
    scanner.run();

    let mut parser = Parser::new(&scanner.tokens);
    let statements = parser.parse();

    // println!("{:?}", statements);

    let mut interpreter = Interpreter::new(statements);
    interpreter.interpret();
}

pub fn run_prompt() {
    let standard_input = io::stdin();

    loop {
        print! {"> "};
        let mut buf = String::from("");
        standard_input.read_line(&mut buf).unwrap();

        // println!("line to run: {}", buf);

        let mut scanner = crate::scanner::Scanner::new(&buf);
        scanner.run();

        let mut parser = Parser::new(&scanner.tokens);
        let statements = parser.parse();
        let mut interpreter = Interpreter::new(statements);
        interpreter.interpret();

    }
}
