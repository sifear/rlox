use std::fs::File;
use std::io::{self, Read};

pub fn run_file(path: &String) {
    let mut buf = vec![];
    let mut file = File::open(path).unwrap();
    let _res = file.read_to_end(&mut buf).unwrap();

    let source = String::from_utf8(buf).unwrap();

    let mut scanner = crate::scanner::Scanner::new(&source);
    scanner.run();
}

pub fn run_prompt() {
    let standard_input = io::stdin();

    loop {
        print! {"> "};
        let mut buf = String::from("");
        standard_input.read_line(&mut buf).unwrap();

        println!("line to run: {}", buf);

        let mut scanner = crate::scanner::Scanner::new(&buf);
        scanner.run();
    }
}
