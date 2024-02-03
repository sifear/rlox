use core::panic;
use std::io::{Error, ErrorKind};

use crate::token::{Token, TokenType};
use expression::{Binary, Expr, Unary};

use self::expression::{Grouping, Literal};
use self::parser_error::{ParserError, ParserErrorType};

pub mod expression;
pub mod parser_error;

// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ( "(" expression ")")* ")" ;

pub fn test2() {
    let mut tokens = vec![];
    tokens.push(Token::new(TokenType::LeftParen, None, None, 1));

    tokens.push(Token::new(TokenType::Number, None, None, 1));
    tokens.push(Token::new(TokenType::Plus, None, None, 1));
    tokens.push(Token::new(TokenType::LeftParen, None, None, 1));
    tokens.push(Token::new(TokenType::Minus, None, None, 1));
    tokens.push(Token::new(TokenType::Number, None, None, 1));
    tokens.push(Token::new(TokenType::RightParen, None, None, 1));

    tokens.push(Token::new(TokenType::Comma, None, None, 1));

    tokens.push(Token::new(TokenType::String, None, None, 1));

    tokens.push(Token::new(TokenType::RightParen, None, None, 1));

    let mut a = Parser::new(&tokens);
    let ast = a.parse();

    match ast {
        Ok(ast) => println!("{}", ast),
        Err(err) => println!("{}", err),
    }
}

pub struct Parser<'a> {
    current: u32,
    tokens: &'a Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser { current: 0, tokens }
    }

    pub fn parse(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut expr = self.comparison();
        if expr.is_err() {
            return expr;
        }

        loop {
            match self._match_(&[TokenType::BangEqual, TokenType::EqualEqual]) {
                Some(token) => {
                    let right = self.comparison();
                    if right.is_err() {
                        return right;
                    }
                    let b = token.clone();
                    expr = Result::Ok(Box::new(Binary::new(expr.unwrap(), right.unwrap(), b)));
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn comparison(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut expr = self.term();
        if expr.is_err() {
            return expr;
        }

        loop {
            match self._match_(&[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ]) {
                Some(token) => {
                    let right = self.term();
                    if right.is_err() {
                        return right;
                    } else {
                        expr = Result::Ok(Box::new(Binary::new(
                            expr.unwrap(),
                            right.unwrap(),
                            token.clone(),
                        )));
                    }
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn term(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut expr = self.factor();
        if expr.is_err() {
            return expr;
        }

        loop {
            match self._match_(&[TokenType::Minus, TokenType::Plus]) {
                Some(token) => {
                    println!("minus pklus found {}", token.token_type);
                    let right = self.term();
                    if right.is_err() {
                        return right;
                    } else {
                        expr = Result::Ok(Box::new(Binary::new(
                            expr.unwrap(),
                            right.unwrap(),
                            token.clone(),
                        )));
                    }
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn factor(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut expr = self.unary();
        if expr.is_err() {
            return expr;
        }

        loop {
            match self._match_(&[TokenType::Star, TokenType::Slash]) {
                Some(token) => {
                    let right = self.term();
                    if right.is_err() {
                        return right;
                    } else {
                        expr = Result::Ok(Box::new(Binary::new(
                            expr.unwrap(),
                            right.unwrap(),
                            token.clone(),
                        )));
                    }
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn unary(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let a = self._match_(&[TokenType::Bang, TokenType::Minus]);

        let b = match a {
            Some(token) => {
                let expr = self.unary();
                if expr.is_err() {
                    return expr;
                } else {
                    return Result::Ok(Box::new(Unary::new(expr.unwrap(), token.clone())));
                }
            }
            None => self.primary(),
        };

        return b;
    }

    fn primary(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        match self._match_(&[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::String,
            TokenType::Number,
        ]) {
            Some(a) => {
                return Result::Ok(Box::new(Literal {
                    literal_type: a.token_type,
                }))
            }
            _ => {
                // panic!("Nemjo")
            }
        };

        // Grouping from here

        let a = self.grouping();
        if a.is_err() {
            let err = a.unwrap_err();
            return Err(err);
        } else {
            let b = a.unwrap();
            return Result::Ok(b);
        }
    }

    fn grouping(&mut self) -> Result<Box<Grouping>, ParserError> {
        match self._match_(&[TokenType::LeftParen]) {
            Some(a) => {
                println!("left parent found {}", self.current);
                let mut grouping = Grouping { exprs: vec![] };
                let expr = self.expression();
                if expr.is_err() {
                    let err = expr.unwrap_err();
                    return Err(err);
                } else {
                    grouping.exprs.push(expr.unwrap());

                    match self._match_(&[TokenType::Comma]) {
                        Some(token) => {
                            let right = self.expression();
                            if right.is_err() {
                                let err = right.unwrap_err();
                                return Err(err);
                            } else {
                                grouping.exprs.push(right.unwrap());
                                let res = self.consume(&TokenType::RightParen);
                                match res {
                                    Ok(()) => {
                                        return Result::Ok(Box::new(grouping));
                                    }
                                    Err(_) => {
                                        return Err(ParserError::new(
                                            ParserErrorType::ExpressionListExpected,
                                            0,
                                        ))
                                    }
                                }
                            }
                        }
                        None => {
                            let res = self.consume(&TokenType::RightParen);
                            match res {
                                Ok(()) => return Result::Ok(Box::new(grouping)),
                                Err(error) => return Err(error),
                            }
                        }
                    }
                }
            }
            _ => {
                return Err(ParserError::new(ParserErrorType::ExpressionExpected, 0));
            }
        }
    }

    // Same as _match_ but parse error happen if expectation is not matched
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

    // Checks current token and advance if match
    fn _match_(&mut self, token_types: &[TokenType]) -> Option<Token> {
        for tt in token_types {
            if self.check(&tt) {
                self.current += 1;
                return Some(self.tokens[(self.current - 1) as usize].clone());
            }
        }

        return None;
    }

    // Check current, doesnt advance
    fn check(&self, token_type: &TokenType) -> bool {
        if (self.current) as usize >= self.tokens.len() {
            return false;
        }

        self.tokens[(self.current) as usize].token_type == *token_type
    }

    fn syncronize(&mut self) {
        self.current += 1;

        while self.current < self.tokens.len() as u32 {
            if self.tokens[self.current as usize].token_type == TokenType::Semicolon {
                return ();
            }

            match self.tokens[self.current as usize - 1].token_type {
                TokenType::Class => {
                    break;
                }
                TokenType::Fun => {
                    break;
                }
                TokenType::Var => {
                    break;
                }
                TokenType::For => {
                    break;
                }
                TokenType::If => {
                    break;
                }
                TokenType::While => {
                    break;
                }
                TokenType::Print => {
                    break;
                }
                TokenType::Return => {
                    break;
                }
                _ => {}
            }

            self.current += 1;
        }
    }
}

fn report(line_number: u32, location: String, message: &str) {
    println!("[line {}] Error {}: {}", line_number, location, message);
}
