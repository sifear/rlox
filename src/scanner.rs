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
            println!("{} in line {}", token.to_string(), token.line);
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

        tokens.push(Token::new(
            TokenType::Eof,
            String::from(""),
            None,
            self.line,
        ));

        return Ok(tokens);
    }

    fn scan_token(&mut self, next_char: char) -> Result<Option<Token>, &'static str> {
        match next_char {
            '(' => Result::Ok(Some(Token::new(
                TokenType::LeftParen,
                String::from(""),
                None,
                self.line,
            ))),
            ')' => Result::Ok(Some(Token::new(
                TokenType::RightParen,
                String::from(""),
                None,
                self.line,
            ))),
            '{' => Result::Ok(Some(Token::new(
                TokenType::LeftBrace,
                String::from(""),
                None,
                self.line,
            ))),
            '}' => Result::Ok(Some(Token::new(
                TokenType::RightBrace,
                String::from(""),
                None,
                self.line,
            ))),
            ',' => Result::Ok(Some(Token::new(
                TokenType::Comma,
                String::from(""),
                None,
                self.line,
            ))),
            '.' => Result::Ok(Some(Token::new(
                TokenType::Dot,
                String::from(""),
                None,
                self.line,
            ))),
            '-' => Result::Ok(Some(Token::new(
                TokenType::Minus,
                String::from(""),
                None,
                self.line,
            ))),
            '+' => Result::Ok(Some(Token::new(
                TokenType::Plus,
                String::from(""),
                None,
                self.line,
            ))),
            ';' => Result::Ok(Some(Token::new(
                TokenType::Semicolon,
                String::from(""),
                None,
                self.line,
            ))),
            '*' => Result::Ok(Some(Token::new(
                TokenType::Star,
                String::from(""),
                None,
                self.line,
            ))),
            '"' => self.string(),
            '0'..='9' => self.number(next_char),
            '/' => self.peek_for_comment_or_slash(),
            '!' => self.decide_on('=', TokenType::Bang, TokenType::BangEqual),
            '=' => self.decide_on('=', TokenType::Equal, TokenType::EqualEqual),
            '<' => self.decide_on('=', TokenType::Less, TokenType::LessEqual),
            '>' => self.decide_on('=', TokenType::Greater, TokenType::GreaterEqual),
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

    fn string(&mut self) -> Result<Option<Token>, &'static str> {
        let mut literal = String::from("");

        loop {
            let next: Option<char> = self.source.next();

            match next {
                Some(ch) => {
                    if ch == '"' {
                        break;
                    }
                    if ch == '\n' {
                        self.line += 1;
                    }

                    literal.push(ch);
                }
                None => return Result::Err("Unterminalted string iteral."),
            };
        }

        Ok(Some(Token::new(
            TokenType::String(literal.clone()),
            String::from(""),
            Some(literal),
            self.line,
        )))
    }

    fn number(&mut self, next_char: char) -> Result<Option<Token>, &'static str> {
        let mut literal = String::from(next_char);
        let mut had_decimal_point = false;

        let mut decimal_iterator = self.source.clone();

        loop {
            let next = self.source.peek();
            decimal_iterator.peek();

            match next {
                Some(ch) if ('0'..='9').contains(ch) => {
                    literal.push(*ch);

                    self.source.next();
                    decimal_iterator.next();
                }
                Some(ch) if *ch == '.' && !had_decimal_point => {
                    decimal_iterator.next(); // Consume decimal point

                    let after_decimal: Option<char> = decimal_iterator.next();
                    match after_decimal {
                        Some(ach) if ('0'..='9').contains(&ach) => {
                            had_decimal_point = true;
                            literal.push(*ch);
                            self.source.next(); // Consume decimal point on outer iterator
                        }
                        _ => {
                            break;
                        }
                    }
                }
                _ => {
                    break;
                }
            };
        }

        Ok(Some(Token::new(
            TokenType::Number(literal.parse::<f64>().unwrap()),
            String::from(""),
            Some(literal),
            self.line,
        )))
    }

    fn decide_on(
        &mut self,
        factor: char,
        token_type_a: TokenType,
        token_type_b: TokenType,
    ) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == factor => {
                self.source.next();

                Result::Ok(Some(Token::new(
                    token_type_b,
                    String::from(""),
                    None,
                    self.line,
                )))
            }
            _ => Result::Ok(Some(Token::new(
                token_type_a,
                String::from(""),
                None,
                self.line,
            ))),
        }
    }

    fn peek_for_comment_or_slash(&mut self) -> Result<Option<Token>, &'static str> {
        match self.source.peek() {
            Some(ch) if *ch == '/' => {
                self.source.next(); // Consume second slash

                loop {
                    let next_char = self.source.peek();

                    match next_char {
                        Some(ch) if *ch == '\n' => return Ok(None),
                        None => return Ok(None),
                        _ => {
                            self.source.next();
                        }
                    }
                }
            }
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
