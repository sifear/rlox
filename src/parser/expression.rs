use crate::{
    environment::Environment,
    interpreter::{
        is_variable::as_variable,
        runtime_error::{RuntimeError, RuntimeErrorType},
    },
    scanner::token::{Token, TokenType},
};
use core::fmt;
use core::fmt::Debug;
use std::{any::{Any, TypeId}, borrow::BorrowMut};

use super::evaluate::{arithmetic, comparison, eq_comparison, plus};

pub trait Expr {
    fn to_string(&self) -> String;
    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError>;
    fn my_type(&self) -> i32;
    fn as_any(&self) -> &dyn Any;
}

impl Debug for dyn Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Expression {{{}}}", self.to_string())
    }
}

pub struct Empty {}

pub struct Ternery {
    pub predicate: Box<dyn Expr>,
    pub true_arm: Box<dyn Expr>,
    pub false_arm: Box<dyn Expr>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

pub struct Unary {
    op: Token,
    right: Box<dyn Expr>,
}
pub struct Binary {
    pub left: Box<dyn Expr>,
    pub op: Token,
    pub right: Box<dyn Expr>,
}

pub struct Variable {
    pub name: Token,
}

pub struct Assign {
    pub l_value: Token,
    pub value: Box<dyn Expr>,
}

impl Unary {
    pub fn new(right: Box<dyn Expr>, op: Token) -> Unary {
        Unary { right, op }
    }
}

impl Binary {
    pub fn new(left: Box<dyn Expr>, right: Box<dyn Expr>, op: Token) -> Binary {
        Binary { left, right, op }
    }
}

#[derive(Debug)]
pub struct Grouping {
    pub exprs: Vec<Box<dyn Expr>>,
}

impl fmt::Display for dyn Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr for Empty {
    fn to_string(&self) -> String {
        format!("<Discarded expression>")
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        return Ok(Literal::Null);
    }

    fn my_type(&self) -> i32 {
        0
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Ternery {
    fn to_string(&self) -> String {
        let mut cucc = String::from("(");
        cucc.push_str(&self.predicate.to_string());
        cucc.push_str(" ? ");
        cucc.push_str(&self.true_arm.to_string());
        cucc.push_str(" : ");
        cucc.push_str(&self.false_arm.to_string());
        cucc.push(')');
        return cucc;
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        return Ok(Literal::Null);
    }

    fn my_type(&self) -> i32 {
        1
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Null => String::from("(Null literal)"),
            Literal::Boolean(true) => String::from("(True literal)"),
            Literal::Boolean(false) => String::from("(False literal)"),
            Literal::Number(n) => format!("(Number literal: {})", n),
            Literal::String(str_val) => format!("(String literal: {})", str_val),
        }
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        return Ok(self.clone());
    }

    fn my_type(&self) -> i32 {
        2
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Unary {
    fn to_string(&self) -> String {
        format!("({} {})", self.op, self.right)
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        match self.op.token_type {
            TokenType::Minus => {
                let ampl = self.right.evaluate(env);
                match ampl {
                    Ok(res) => match res {
                        Literal::Number(n) => Ok(Literal::Number(-1.0 * n)),
                        _ => Err(RuntimeError::new(RuntimeErrorType::Unknown, 0)),
                    },
                    Err(err) => Err(err),
                }
            }
            TokenType::Bang => {
                let a = self.right.evaluate(env);
                match a {
                    Ok(res) => {
                        let ampl = is_truthy(res);

                        Ok(Literal::Boolean(!ampl))
                    }
                    Err(err) => Err(err),
                }
            }
            _ => {
                panic!("Unexpected operator token type while evaluating unary.")
            }
        }
    }

    fn my_type(&self) -> i32 {
        3
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Binary {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }
    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        match self.op.token_type {
            TokenType::Minus | TokenType::Star | TokenType::Slash => {
                let res = arithmetic(self, env);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            TokenType::Plus => {
                let res = plus(&self, env);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => {
                let res = comparison(self, env);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            TokenType::BangEqual | TokenType::EqualEqual => {
                let res = eq_comparison(self, env);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            _ => Ok(Literal::Null),
        }
    }

    fn my_type(&self) -> i32 {
        4
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Grouping {
    fn to_string(&self) -> String {
        let mut cucc = String::from("(");
        for (i, expr) in self.exprs.iter().enumerate() {
            cucc.push_str(&expr.to_string());

            if i < self.exprs.len() - 1 {
                cucc.push(',');
            }
        }
        cucc.push(')');

        return cucc;
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        self.exprs.iter().map(|a| a.evaluate(env)).last().unwrap()
    }

    fn my_type(&self) -> i32 {
        5
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

fn is_truthy(literal: Literal) -> bool {
    match literal {
        Literal::Boolean(false) => false,
        Literal::Null => false,
        _ => true,
    }
}

impl Expr for Variable {
    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        match &self.name.lexeme {
            Some(name) => {
                let a = env.get(name);
                match a {
                    Some(b) => Ok(b.clone()),
                    None => Err(RuntimeError::new(
                        RuntimeErrorType::IdentifierNotDefined,
                        self.name.line,
                    )),
                }
            }
            None => Err(RuntimeError::new(
                RuntimeErrorType::IdentifierTokenNotSaved,
                0,
            )),
        }
    }

    fn to_string(&self) -> String {
        format!("(VAR {})", self.name.lexeme.clone().take().unwrap())
    }

    fn my_type(&self) -> i32 {
        6
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Assign {
    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        let val = self.value.evaluate(env);
        let _val = match val {
            Ok(v) => v,
            Err(err) => {
                println!("Error while evaluating right hand side of assignment.");
                Literal::Null
            }
        };

        match &self.l_value.lexeme {
            Some(a) => {
                let success = env.assign(&a, &_val);
                if success {
                    Ok(_val)
                } else {
                    Err(RuntimeError::new(
                        RuntimeErrorType::IdentifierNotDefined,
                        self.l_value.line,
                    ))
                }
            }
            None => Err(RuntimeError::new(
                RuntimeErrorType::IdentifierNotDefined,
                self.l_value.line,
            )),
        }
    }

    fn to_string(&self) -> String {
        "".to_string()
    }

    fn my_type(&self) -> i32 {
        7
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
