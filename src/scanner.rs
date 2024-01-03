use core::iter::Peekable;
use std::process::exit;
use std::str::Chars;

use crate::token::Token;
use crate::token::TokenType;

pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    had_errors: bool,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &String) -> Scanner {
        let input_chars: Peekable<Chars<'_>> = source.chars().peekable();

        Scanner {
            source: input_chars,
            had_errors: false,
            tokens: vec![],
            line: 1,
        }
    }

    pub fn run(&mut self) {
        let tokens = match self.scan_tokens(0) {
            Ok(tokens) => tokens,
            Err(err) => {
                report(1, String::from(""), &err);
                exit(65);
            }
        };

        for token in tokens {
            println!("{}", token.to_string());
        }
    }

    fn scan_tokens(&mut self, start: i32) -> Result<Vec<Token>, String> {
        let mut tokens = vec![];
        let mut current = start;

        loop {
            let next_char = match self.source.next() {
                Some(ch) => {
                    current += 1;

                    ch
                }
                None => break,
            };

            match self.scan_token(next_char) {
                Ok(maybe_token) => match maybe_token {
                    Some(token) => tokens.push(token),
                    None => {}
                },
                Err(err) => {
                    report(1, String::from(""), err);
                }
            };
        }

        tokens.push(Token::new(TokenType::Eof, String::from(""), None, 1));

        return Ok(tokens);
    }

    fn scan_token(&mut self, next_char: char) -> Result<Option<Token>, &'static str> {
        match next_char {
            '(' => Result::Ok(Some(Token::new(
                TokenType::LeftParen,
                String::from(""),
                None,
                1,
            ))),
            ')' => Result::Ok(Some(Token::new(
                TokenType::RightParen,
                String::from(""),
                None,
                1,
            ))),
            '{' => Result::Ok(Some(Token::new(
                TokenType::LeftBrace,
                String::from(""),
                None,
                1,
            ))),
            '}' => Result::Ok(Some(Token::new(
                TokenType::RightBrace,
                String::from(""),
                None,
                1,
            ))),
            ',' => Result::Ok(Some(Token::new(
                TokenType::Comma,
                String::from(""),
                None,
                1,
            ))),
            '.' => Result::Ok(Some(Token::new(TokenType::Dot, String::from(""), None, 1))),
            '-' => Result::Ok(Some(Token::new(
                TokenType::Minus,
                String::from(""),
                None,
                1,
            ))),
            '+' => Result::Ok(Some(Token::new(TokenType::Plus, String::from(""), None, 1))),
            ';' => Result::Ok(Some(Token::new(
                TokenType::Semicolon,
                String::from(""),
                None,
                1,
            ))),
            '*' => Result::Ok(Some(Token::new(TokenType::Star, String::from(""), None, 1))),
            '/' => self.peek_for_comment_or_slash(),
            '!' => self.bang_or_ne(),
            '=' => self.eq_or_eqeq(),
            '<' => self.le_or_leeq(),
            '>' => self.gr_or_greq(),
            ' ' => Ok(None),
            '\r' => Ok(None),
            '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            }
            _ => Result::Err("Invalid character in source"),
        }
    }

    fn bang_or_ne(&mut self) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == '=' => {
                self.source.next();

                Result::Ok(Some(Token::new(
                    TokenType::BangEqual,
                    String::from(""),
                    None,
                    self.line,
                )))
            }
            _ => Result::Ok(Some(Token::new(
                TokenType::Bang,
                String::from(""),
                None,
                self.line,
            ))),
        }
    }

    fn eq_or_eqeq(&mut self) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == '=' => {
                self.source.next();

                Result::Ok(Some(Token::new(
                    TokenType::EqualEqual,
                    String::from(""),
                    None,
                    self.line,
                )))
            }
            _ => Result::Ok(Some(Token::new(
                TokenType::Equal,
                String::from(""),
                None,
                self.line,
            ))),
        }
    }

    fn le_or_leeq(&mut self) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == '=' => {
                self.source.next();

                Result::Ok(Some(Token::new(
                    TokenType::LessEqual,
                    String::from(""),
                    None,
                    self.line,
                )))
            }
            _ => Result::Ok(Some(Token::new(
                TokenType::Less,
                String::from(""),
                None,
                self.line,
            ))),
        }
    }

    fn gr_or_greq(&mut self) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == '=' => {
                self.source.next();
                
                Result::Ok(Some(Token::new(
                    TokenType::GreaterEqual,
                    String::from(""),
                    None,
                    self.line,
                )))
            }
            _ => Result::Ok(Some(Token::new(
                TokenType::Greater,
                String::from(""),
                None,
                self.line,
            ))),
        }
    }

    fn peek_for_comment_or_slash(&mut self) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == '/' => loop {
                let consumed = self.source.next();
                match consumed {
                    Some(ch) if ch == '\n' => return Ok(None),
                    None => return Ok(None),
                    _ => {}
                }
            },
            _ => Result::Ok(Some(Token::new(
                TokenType::Slash,
                String::from(""),
                None,
                self.line,
            ))),
        }
    }
}

fn report(line_number: u32, location: String, message: &str) {
    println!("[line {}] Error {}: {}", line_number, location, message);
}
