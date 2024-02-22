use std::collections::HashMap;

use crate::environment::Environment;
use crate::parser::runtime_error::RuntimeErrorType;
use crate::scanner::token::{Token, TokenType};
use expression::{Binary, Expr, Unary};

use self::expression::{Empty, Grouping, Literal, Ternery, Variable};
use self::parser_error::{ParserError, ParserErrorType};
use self::runtime_error::RuntimeError;
use self::statement::{ExprStmt, PrintStmt, Statement, VarStmt};

pub mod evaluate;
pub mod expression;
pub mod parser_error;
pub mod runtime_error;
pub mod statement;

// program        → statement* EOF
//                | declaration* EOF;

// declaration    → varDecl
//                | statement ;

// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

// statement      → exprStmt
//                | printStmt ;

// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;

// expression     → equality ( "?" exression ":" expression )*
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ( "(" expression ")")* ")" ;
//                | IDENTIFIER ;

pub struct Parser<'a> {
    current: u32,
    tokens: &'a Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser { current: 0, tokens }
    }

    pub fn parse(&mut self) -> Vec<Box<dyn Statement>> {
        let mut statements = vec![];

        while self.current < ((*self.tokens).len() - 1) as u32 {
            match self.statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    println!("{}", err);
                    self.syncronize();
                }
            }
        }

        statements
    }

    pub fn statement(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        match self._match_(&[TokenType::Var]) {
            Some(token) => return self.var_declaration(),
            None => {}
        }

        match self._match_(&[TokenType::Print]) {
            Some(token) => {
                let print_stmt = self.print_stmt();
                match print_stmt {
                    Ok(expr) => return Ok(expr),
                    Err(err) => return Err(err),
                }
            }
            None => {}
        };

        let expr = self.expression();
        match expr {
            Ok(expr) => {
                let res = self.consume(&TokenType::Semicolon);
                match res {
                    Ok(_) => return Ok(Box::new(ExprStmt { expr })),
                    Err(a) => {
                        return Err(RuntimeError::new(
                            RuntimeErrorType::StatementMissingSemicolon,
                            0,
                        ))
                    }
                }
            }
            Err(err) => {
                println!("{}", err);

                Err(RuntimeError::new(RuntimeErrorType::StatementExpected, 0))
            }
        }
    }

    pub fn var_declaration(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        let res = self.consume(&TokenType::Identifier);
        if res.is_err() {
            println!("{}", res.unwrap_err());
            return Err(RuntimeError::new(RuntimeErrorType::IdentifierExpedcted, 0));
        }

        let mut initializer: Option<Box<dyn Expr>> = None;
        match self._match_(&[TokenType::Equal]) {
            Some(token) => {
                let init_expr = self.expression();
                match init_expr {
                    Ok(expr) => {
                        initializer = Some(expr);
                    }
                    Err(err) => {
                        println!("{}", err);

                        return Err(RuntimeError::new(
                            RuntimeErrorType::VarInitializerExpected,
                            0,
                        ));
                    }
                };
            }
            None => {}
        };

        self.consume(&TokenType::Semicolon);

        Ok(Box::new(VarStmt {
            initializer,
            name: res.unwrap(),
        }))
    }

    pub fn print_stmt(&mut self) -> Result<Box<PrintStmt>, RuntimeError> {
        let res = self.expression();
        match res {
            Ok(expr) => {
                let res = self.consume(&TokenType::Semicolon);
                match res {
                    Ok(_) => return Ok(Box::new(PrintStmt { expr })),
                    Err(a) => {
                        return Err(RuntimeError::new(
                            RuntimeErrorType::StatementMissingSemicolon,
                            0,
                        ))
                    }
                }
            }
            Err(err) => {
                println!("{}", err);

                Err(RuntimeError::new(RuntimeErrorType::StatementExpected, 0))
            }
        }
    }

    fn expression(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut errorous_binary_pos = None;
        match self._match_(&[TokenType::Slash, TokenType::Star]) {
            Some(token) => {
                errorous_binary_pos = Some(self.current);
                println!("Binary operand is missng left hand side at line {}", 0)
                // return Err(ParserError::new(ParserErrorType::BinaryMissingLHS, 0))
            }
            None => {}
        }

        let expr = self.equality();

        match errorous_binary_pos {
            Some(pos) => return Result::Ok(Box::new(Empty {})),
            None => {}
        };

        match self._match_(&[TokenType::QuestionMark]) {
            Some(_) => {
                let true_arm = self.expression();
                if true_arm.is_err() {
                    return Err(ParserError::new(ParserErrorType::PredicateMissingTrue, 0));
                }
                let res = self.consume(&TokenType::Colon);
                if res.is_err() {
                    return Err(ParserError::new(ParserErrorType::PredicateMissingFalse, 0));
                }
                let false_arm = self.expression();
                if false_arm.is_err() {
                    return Err(ParserError::new(ParserErrorType::ExpressionExpected, 0));
                }

                let ternery = Result::<Box<dyn Expr>, ParserError>::Ok(Box::new(Ternery {
                    predicate: expr.unwrap(),
                    true_arm: true_arm.unwrap(),
                    false_arm: false_arm.unwrap(),
                }));

                return ternery;
            }
            None => {}
        };

        expr
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
        let str_val = self._match_string_();
        if str_val.is_some() {
            return Result::Ok(Box::new(Literal::String(str_val.unwrap())));
        }

        let num_val = self._match_number_();
        if num_val.is_some() {
            return Result::Ok(Box::new(Literal::Number(num_val.unwrap())));
        }

        match self._match_(&[TokenType::False, TokenType::True, TokenType::Nil]) {
            Some(a) => match a.token_type {
                TokenType::False => return Result::Ok(Box::new(Literal::Boolean(false))),
                TokenType::True => return Result::Ok(Box::new(Literal::Boolean(true))),
                TokenType::Nil => return Result::Ok(Box::new(Literal::Null {})),
                _ => {}
            },
            _ => {
                // panic!("Nemjo")
            }
        };

        match self._match_(&[TokenType::Identifier]) {
            Some(token) => return Result::Ok(Box::new(Variable { name: token })),
            None => {}
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
                                    Ok(_) => {
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
                                Ok(_) => return Result::Ok(Box::new(grouping)),
                                Err(error) => return Err(error),
                            }
                        }
                    }
                }
            }
            _ => {
                println!("in grouping");
                return Err(ParserError::new(ParserErrorType::ExpressionExpected, 0));
            }
        }
    }

    // Same as _match_ but parse error happen if expectation is not matched
    fn consume(&mut self, expected: &TokenType) -> Result<Token, ParserError> {
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

        println!("consumed {}", self.tokens[self.current as usize]);

        Ok(self.tokens[(self.current - 1) as usize].clone())
    }

    // Checks current token and advance if match
    fn _match_(&mut self, token_types: &[TokenType]) -> Option<Token> {
        for tt in token_types {
            if self.check(&tt) {
                self.current += 1;
                return Some(self.tokens[(self.current - 1) as usize].clone());
            }
        }

        None
    }

    fn _match_string_(&mut self) -> Option<String> {
        if (self.current) as usize >= self.tokens.len() {
            return None;
        }

        match &self.tokens[(self.current) as usize].token_type {
            TokenType::String(str_val) => {
                self.current += 1;
                return Some(str_val.clone());
            }
            _ => None,
        }
    }

    fn _match_number_(&mut self) -> Option<f64> {
        if (self.current) as usize >= self.tokens.len() {
            return None;
        }

        match self.tokens[(self.current) as usize].token_type {
            TokenType::Number(n) => {
                self.current += 1;
                return Some(n);
            }
            _ => None,
        }
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
