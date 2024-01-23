use core::panic;
use std::io::{Error, ErrorKind};

use crate::token::{Token, TokenType};
use expression::{Binary, Expr, Unary};

use self::expression::{Grouping, Literal};
use self::parser_error::{ParserError, ParserErrorType};

pub mod expression;
pub mod parser_error;

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

pub fn test2() {
    let mut tokens = vec![];
    tokens.push(Token::new(TokenType::LeftParen, None, None, 1));
    tokens.push(Token::new(TokenType::Minus, None, None, 1));
    tokens.push(Token::new(TokenType::Number(1.0), None, None, 1));
    tokens.push(Token::new(TokenType::RightParen, None, None, 1));

    let mut a = Parser::new(&tokens);
    a.parse();
}

pub struct Parser<'a> {
    current: u32,
    tokens: &'a Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser { current: 0, tokens }
    }

    pub fn parse(&mut self) -> () {
        self.expression();

        ()
    }

    fn expression(&mut self) -> Box<dyn Expr> {
        println!("Expression");
        let eql = self.equality();

        eql
    }

    fn equality(&mut self) -> Box<dyn Expr> {
        println!("equality");

        let mut expr = self.comparison();

        loop {
            match self._match_(&[TokenType::BangEqual, TokenType::EqualEqual]) {
                Some(token) => {
                    let right = self.comparison();
                    let b = token.clone();
                    expr = Box::new(Binary::new(expr, right, b));
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn comparison(&mut self) -> Box<dyn Expr> {
        println!("comparison");
        let mut expr = self.term();

        loop {
            match self._match_(&[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ]) {
                Some(token) => {
                    let right = self.term();
                    expr = Box::new(Binary::new(expr, right, token.clone()));
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn term(&mut self) -> Box<dyn Expr> {
        println!("term");
        let mut expr = self.factor();

        loop {
            match self._match_(&[TokenType::Minus, TokenType::Plus]) {
                Some(token) => {
                    println!("minus pklus found {}", token.token_type);
                    let right = self.term();
                    expr = Box::new(Binary::new(expr, right, token.clone()));
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn factor(&mut self) -> Box<dyn Expr> {
        println!("factor");
        let mut expr = self.unary();

        loop {
            match self._match_(&[TokenType::Star, TokenType::Slash]) {
                Some(token) => {
                    let right = self.term();
                    expr = Box::new(Binary::new(expr, right, token.clone()));
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn unary(&mut self) -> Box<dyn Expr> {
        println!("unary");
        let a = self._match_(&[TokenType::Bang, TokenType::Minus]);

        let b = match a {
            Some(token) => {
                println!("minus bang found {}", token.token_type);
                let expr = self.unary();
                Box::new(Unary::new(expr, token.clone()))
            }
            None => self.primary(),
        };

        return b;
    }

    fn primary(&mut self) -> Box<dyn Expr> {
        println!("primary");
        match self._match_(&[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::String(String::from("")),
            TokenType::Number(1.0),
        ]) {
            Some(a) => {
                return Box::new(Literal {
                    literal_type: match a.token_type {
                        TokenType::True => TokenType::True,
                        TokenType::Nil => TokenType::Nil,
                        TokenType::String(val) => TokenType::String(val.clone()),
                        TokenType::Number(val) => TokenType::Number(val),
                        _ => {
                            panic!("Nemjo")
                        }
                    },
                })
            }
            _ => {
                // panic!("Nemjo")
            }
        };

        match self._match_(&[TokenType::LeftParen]) {
            Some(a) => {
                println!("left parent found {}", self.current);
                let expr = self.expression();
                let res = self.consume(&TokenType::RightParen);
                match res {
                    Ok(()) => return Box::new(Grouping { expr }),
                    Err(error) => panic!("{}", error),
                }
            }
            _ => {
                panic!("Nemjo")
            }
        };
    }

    fn consume(&mut self, expected: &TokenType) -> Result<(), ParserError> {
        if ((self.current) as usize) >= self.tokens.len() {
            return Err(ParserError::new(
                ParserErrorType::UnclosedGroup,
                self.tokens[self.current as usize - 1].line,
            ));
        }

        if self.tokens[self.current as usize].token_type == *expected {
            self.current += 1;
        } else {
            return Err(ParserError::new(
                ParserErrorType::UnclosedGroup,
                self.tokens[self.current as usize].line,
            ));
        }

        Ok(())
    }

    fn _match_(&mut self, token_types: &[TokenType]) -> Option<Token> {
        for tt in token_types {
            if self.check(&tt) {
                self.current += 1;
                return Some(self.tokens[(self.current - 1) as usize].clone());
            }
        }

        return None;
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if (self.current) as usize >= self.tokens.len() {
            return false;
        }

        self.tokens[(self.current) as usize].token_type == *token_type
    }
}

fn report(line_number: u32, location: String, message: &str) {
    println!("[line {}] Error {}: {}", line_number, location, message);
}
