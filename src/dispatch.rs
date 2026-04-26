use std::fs::{read_dir, File};
use std::io::{self, Read};

use crate::interpreter::Interpreter;
use crate::parser::Parser;

pub fn run_file(path: &String) -> String {
    let mut buf = vec![];
    let mut file = File::open(path).unwrap();
    let _res = file.read_to_end(&mut buf).unwrap();

    let source = String::from_utf8(buf).unwrap();

    let mut scanner = crate::scanner::Scanner::new(&source);
    scanner.run();

    let mut parser = Parser::new(&scanner.tokens);
    let statements = parser.parse();


    let mut interpreter = Interpreter::new(statements);
    interpreter.print_ast();
    interpreter.interpret();

    print!("{}", interpreter.result_buffer);

    String::from(interpreter.result_buffer.as_str())
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

mod tests {
    use std::fs::{self, ReadDir};

    use super::run_file;

    // #[test]
    // fn run() {
    //     let dir = fs::read_dir("tests");
    //     match dir {
    //         Ok(d) => {
    //             d.for_each(|e| match e {
    //                 Ok(file) => {
    //                     if let Ok(cucc) = file.file_type() {
    //                         if cucc.is_file() {
    //                             let result = run_file(&("main.lox".to_string()));

    //                             assert_eq!(
    //                                 result,
    //                                 String::from(
    //                                     "Calling the method start\ndani\n1234\nCalling the method end\n"
    //                                 )
    //                             )
    //                         }
    //                     }
    //                 }
    //                 Err(err) => {
    //                     println!("{}", err)
    //                 }
    //             });
    //         }
    //         Err(err) => {
    //             println!("{}", err)
    //         }
    //     }
    // }

    #[test]
    fn test1() {
        let result = run_file(&("tests/test1.lox".to_string()));

        assert_eq!(result, String::from("10\nDani\n"))
    }

    #[test]
    fn test2() {
        let result = run_file(&("tests/test2.lox".to_string()));

        assert_eq!(result, String::from("10\n9\n11\nDani\n10\n"))
    }

    #[test]
    fn test3() {
        let result = run_file(&("tests/test3.lox".to_string()));

        assert_eq!(result, String::from("10\n4\n6\n4\n4\n104\n104\n"))
    }

    #[test]
    fn test4() {
        let result = run_file(&("tests/test4.lox".to_string()));

        assert_eq!(result, String::from("Dani\nDani\n10\n"))
    }

    #[test]
    fn test5() {
        let result = run_file(&("tests/test5.lox".to_string()));

        assert_eq!(result, String::from("10\n11\n900\n11\n11\n"))
    }
    #[test]
    fn test6() {
        let result = run_file(&("tests/test6.lox".to_string()));

        assert_eq!(result, String::from("8\n999\n8\n998\n997\n"))
    }
        #[test]
    fn test7() {
        let result = run_file(&("tests/test7.lox".to_string()));

        assert_eq!(result, String::from("8\n999\n8\n998\n997\n"))
    }
}
