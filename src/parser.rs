use std::any::{Any, TypeId};

use crate::interpreter::is_variable::as_variable;
use crate::interpreter::runtime_error::{RuntimeError, RuntimeErrorType};
use crate::scanner::token::{Token, TokenType};
use expression::{Binary, Expr, Unary};

use self::expression::{Assign, Empty, Grouping, Literal, Logical, Ternery, Variable};
use self::parser_error::{ParserError, ParserErrorType};
use self::statement::{
    BlockStmt, BreakStmt, ExprStmt, IfStmt, PrintStmt, Statement, VarStmt, WhileStmt,
};

pub mod evaluate;
pub mod expression;
pub mod parser_error;
pub mod statement;

// program        → statement* EOF
//                | declaration* EOF;

// declaration    → varDecl

// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

// statement      → exprStmt
//                | forStmt
//                | whileStmt
//                | printStmt
//                | block
//                | ifStmt
//                | breakStmt

// breakStmt      → "break" ";"

// forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
//                  expression? ";"
//                  expression? ")" statement ;

// whileStmt      → "while" "(" expression ")" statement ;

// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;

// block          → "{" declaration* "}" ;

// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;

// expression     → logic_or ( "?" exression ":" expression )* ;
//                | assignment ;
// assignment     → IDENTIFIER "=" assignment
//                | logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
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

        match self._match_(&[TokenType::If]) {
            Some(token) => return self.if_statement(),
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

        match self._match_(&[TokenType::While]) {
            Some(token) => {
                let while_stmt = self.while_stmt();
                match while_stmt {
                    Ok(expr) => return Ok(expr),
                    Err(err) => return Err(err),
                }
            }
            None => {}
        }

        match self._match_(&[TokenType::For]) {
            Some(token) => {
                let for_stmt = self.for_stmt();
                match for_stmt {
                    Ok(expr) => return Ok(expr),
                    Err(err) => return Err(err),
                }
            }
            None => {}
        }

        match self._match_(&[TokenType::LeftBrace]) {
            Some(token) => {
                let statements = self.block_stmt();
                match statements {
                    Ok(stmts) => return Ok(Box::new(BlockStmt { stmts })),
                    Err(err) => return Err(err),
                }
            }
            None => {}
        };

        match self._match_(&[TokenType::Break]) {
            Some( token) => {
                let semi = self.consume(&TokenType::Semicolon);
                if semi.is_err() {
                    return Err(RuntimeError::new(
                        RuntimeErrorType::StatementMissingSemicolon,
                        semi.unwrap_err().line,
                    ));
                }

                return Ok(Box::new(BreakStmt {}));
            }
            None => {}
        };

        self.expr_stmt()
    }

    pub fn expr_stmt(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        let expr = self.expression();
        match expr {
            Ok(expr) => {
                let res = self.consume(&TokenType::Semicolon);
                match res {
                    Ok(_) => return Ok(Box::new(ExprStmt { expr })),
                    Err(a) => {
                        return Err(RuntimeError::new(
                            RuntimeErrorType::StatementMissingSemicolon,
                            a.line,
                        ))
                    }
                }
            }
            Err(err) => {
                println!("{}", err);

                Err(RuntimeError::new(
                    RuntimeErrorType::StatementExpected,
                    err.line,
                ))
            }
        }
    }

    pub fn for_stmt(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        let res = self.consume(&TokenType::LeftParen);
        match res {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(
                    RuntimeErrorType::MissingForCondStartParenthesis,
                    err.line,
                ));
            }
            Ok(_) => {}
        }

        let initializer = match self._match_(&[TokenType::Semicolon]) {
            Some(token) => None,
            None => match self._match_(&[TokenType::Var]) {
                Some(token) => {
                    let var_declaration = self.var_declaration();
                    if var_declaration.is_err() {
                        return var_declaration;
                    }

                    Some(var_declaration.unwrap())
                }
                None => {
                    let expr_stmt = self.expr_stmt();
                    if expr_stmt.is_err() {
                        return expr_stmt;
                    }

                    Some(expr_stmt.unwrap())
                }
            },
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            let expr = self.expression();

            if expr.is_err() {
                return Err(RuntimeError::new(
                    RuntimeErrorType::ExpressionExpected,
                    expr.unwrap_err().line,
                ));
            }

            Some(expr.unwrap())
        } else {
            None
        };

        self.consume(&TokenType::Semicolon);

        let increment = if !self.check(&TokenType::RightParen) {
            let expr = self.expression();

            if expr.is_err() {
                return Err(RuntimeError::new(
                    RuntimeErrorType::ExpressionExpected,
                    expr.unwrap_err().line,
                ));
            }

            Some(expr.unwrap())
        } else {
            None
        };

        let res = self.consume(&TokenType::RightParen);
        if res.is_err() {
            return Err(RuntimeError::new(
                RuntimeErrorType::MissingForCondEndParenthesis,
                res.unwrap_err().line,
            ));
        }

        let body = self.statement();
        match body {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(
                    RuntimeErrorType::StatementExpected,
                    err.line,
                ));
            }
            Ok(_) => {}
        }

        let new_body = match increment {
            Some(expr) => {
                let mut wrapper_body = BlockStmt { stmts: vec![] };
                wrapper_body.stmts.push(body.unwrap());
                wrapper_body.stmts.push(Box::new(ExprStmt { expr: expr }));

                Box::new(wrapper_body)
            }
            None => body.unwrap(),
        };

        let for_condition = match condition {
            Some(cond) => cond,
            None => Box::new(Literal::Boolean(true)),
        };

        let while_stmt = WhileStmt {
            cond: for_condition,
            body: new_body,
        };

        match initializer {
            Some(initer) => {
                let mut block_stmt = BlockStmt { stmts: vec![] };
                block_stmt.stmts.push(initer);
                block_stmt.stmts.push(Box::new(while_stmt));

                Ok(Box::new(block_stmt))
            }
            None => Ok(Box::new(while_stmt)),
        }
    }

    pub fn while_stmt(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        let res = self.consume(&TokenType::LeftParen);
        match res {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(
                    RuntimeErrorType::MissingWhileCondStartParenthesis,
                    err.line,
                ));
            }
            Ok(_) => {}
        }

        let cond = self.expression();

        match cond {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
            Ok(_) => {}
        }

        let res = self.consume(&TokenType::RightParen);
        match res {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(
                    RuntimeErrorType::MissingWhileCondEndParenthesis,
                    err.line,
                ));
            }
            Ok(_) => {}
        }

        let body = self.statement();
        match body {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
            Ok(_) => {}
        }

        Ok(Box::new(WhileStmt {
            cond: cond.unwrap(),
            body: body.unwrap(),
        }))
    }

    pub fn if_statement(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        let res = self.consume(&TokenType::LeftParen);
        match res {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
            Ok(_) => {}
        }

        let cond = self.expression();
        match cond {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
            Ok(_) => {}
        }

        let res2 = self.consume(&TokenType::RightParen);
        match res2 {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
            Ok(_) => {}
        }

        let then = self.statement();
        if then.is_err() {
            return then;
        }

        let mut els = None;
        let _els = self.check(&TokenType::Else);
        if _els {
            self.consume(&TokenType::Else);
            let stmt = self.statement();
            match stmt {
                Err(err) => {
                    println!("{}", err);
                    return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
                }
                Ok(_) => {}
            }

            els = Some(stmt.unwrap());
        }

        Ok(Box::new(IfStmt {
            cond: cond.unwrap(),
            then: then.unwrap(),
            els,
        }))
    }

    pub fn block_stmt(&mut self) -> Result<Vec<Box<dyn Statement>>, RuntimeError> {
        let mut statements: Vec<Box<dyn Statement>> = vec![];

        loop {
            if self.check(&TokenType::RightBrace) {
                break;
            }

            let res = self.statement();
            match res {
                Ok(stmt) => {
                    statements.push(stmt);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        let res = self.consume(&TokenType::RightBrace);
        match res {
            Ok(_) => {}
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
        }

        Ok(statements)
    }

    pub fn var_declaration(&mut self) -> Result<Box<dyn Statement>, RuntimeError> {
        let res = self.consume(&TokenType::Identifier);
        match res {
            Err(err) => {
                println!("{}", err);
                return Err(RuntimeError::new(RuntimeErrorType::Unknown, err.line));
            }
            Ok(_) => {}
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
                            err.line,
                        ));
                    }
                };
            }
            None => {}
        };

        let semi = self.consume(&TokenType::Semicolon);

        match semi {
            Ok(_) => Ok(Box::new(VarStmt {
                initializer,
                name: res.unwrap(),
            })),
            Err(err) => Err(RuntimeError::new(
                RuntimeErrorType::StatementExpected,
                err.line,
            )),
        }
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
                            a.line,
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
                println!(
                    "Binary operand is missing left hand side at line {}",
                    token.line
                )
                // return Err(ParserError::new(ParserErrorType::BinaryMissingLHS, 0))
            }
            None => {}
        }

        let expr = self.assignment();

        match errorous_binary_pos {
            Some(pos) => return Result::Ok(Box::new(Empty {})),
            None => {}
        };

        match self._match_(&[TokenType::QuestionMark]) {
            Some(a) => {
                let true_arm = self.equality();
                if true_arm.is_err() {
                    return Err(ParserError::new(
                        ParserErrorType::PredicateMissingTrue,
                        a.line,
                    ));
                }
                let res = self.consume(&TokenType::Colon);
                if res.is_err() {
                    return Err(ParserError::new(
                        ParserErrorType::PredicateMissingFalse,
                        a.line,
                    ));
                }
                let false_arm = self.equality();
                if false_arm.is_err() {
                    return Err(ParserError::new(
                        ParserErrorType::ExpressionExpected,
                        a.line,
                    ));
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

    fn assignment(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let res = self.or();
        if res.is_err() {
            return res;
        }

        match self._match_(&[TokenType::Equal]) {
            Some(a) => {
                let value = self.assignment();
                if value.is_err() {
                    return value;
                }

                let _res = res.unwrap();

                match as_variable(_res.as_any()) {
                    Some(va) => {
                        return Ok(Box::new(Assign {
                            l_value: va.name,
                            value: value.unwrap(),
                        }));
                    }
                    None => {
                        report(a.line, "Invalid assignment target. parser");

                        return Err(ParserError::new(
                            ParserErrorType::PredicateMissingTrue,
                            a.line,
                        ));
                    }
                }
            }
            None => {}
        }

        res
    }

    fn or(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut expr: Result<Box<dyn Expr>, ParserError> = self.and();
        if expr.is_err() {
            return expr;
        }

        loop {
            let match_or = self._match_(&[TokenType::Or]);
            match match_or {
                Some(or_token) => {
                    let right = self.and();
                    if right.is_err() {
                        return right;
                    }

                    expr = Ok(Box::new(Logical {
                        op: or_token,
                        left: expr.unwrap(),
                        right: right.unwrap(),
                    }))
                }
                None => {
                    break;
                }
            }
        }

        expr
    }

    fn and(&mut self) -> Result<Box<dyn Expr>, ParserError> {
        let mut expr: Result<Box<dyn Expr>, ParserError> = self.equality();
        if expr.is_err() {
            return expr;
        }

        loop {
            let match_and = self._match_(&[TokenType::And]);
            match match_and {
                Some(and_token) => {
                    let right = self.equality();
                    if right.is_err() {
                        return right;
                    }

                    expr = Ok(Box::new(Logical {
                        op: and_token,
                        left: expr.unwrap(),
                        right: right.unwrap(),
                    }))
                }
                None => {
                    break;
                }
            }
        }

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
                    println! {"matched star or slash"};
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
            Some(token) => {
                let variable = Variable { name: token };
                return Result::Ok(Box::new(variable));
            }
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
                                    Ok(_) => Result::Ok(Box::new(grouping)),
                                    Err(err) => Err(ParserError::new(
                                        ParserErrorType::ExpressionListExpected,
                                        err.line,
                                    )),
                                }
                            }
                        }
                        None => {
                            let res = self.consume(&TokenType::RightParen);
                            match res {
                                Ok(_) => Result::Ok(Box::new(grouping)),
                                Err(error) => Err(error),
                            }
                        }
                    }
                }
            }
            None => {
                println!("in grouping");
                Err(ParserError::new(ParserErrorType::ExpressionExpected, 0))
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

        println!("consumed {}", self.tokens[(self.current - 1) as usize]);

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

fn report(line_number: u32, message: &str) {
    println!("[line {}] Error: {}", line_number, message);
}
