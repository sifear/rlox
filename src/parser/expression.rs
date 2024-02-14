use crate::token::{self, Token, TokenType};
use core::fmt;
use core::fmt::Debug;

use super::evaluate::{arithmetic, plus};

pub trait Expr {
    fn to_string(&self) -> String;
    fn evaluate(&self) -> Literal;
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

    fn evaluate(&self) -> Literal {
        return Literal::Null;
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

    fn evaluate(&self) -> Literal {
        return Literal::Null;
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

    fn evaluate(&self) -> Literal {
        return self.clone();
    }
}

impl Expr for Unary {
    fn to_string(&self) -> String {
        format!("({} {})", self.op, self.right)
    }

    fn evaluate(&self) -> Literal {
        match self.op.token_type {
            TokenType::Minus => {
                let ampl = self.right.evaluate();
                match ampl {
                    Literal::Number(n) => Literal::Number(-1.0 * n),
                    _ => {
                        panic!("Error while evaluating minus unary.")
                    }
                }
            }
            TokenType::Bang => {
                let ampl = is_truthy(self.right.evaluate());
                return Literal::Boolean(!ampl);
            }
            _ => {
                panic!("Unexpected operator token type while evaluating unary.")
            }
        }
    }
}

impl Expr for Binary {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }
    fn evaluate(&self) -> Literal {
        match self.op.token_type {
            TokenType::Minus | TokenType::Star | TokenType::Slash => {
                let res = arithmetic(self);
                match res {
                    Ok(value) => value,
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Literal::Null
                    }
                }
            }
            TokenType::Plus => {
                let res = plus(&self);
                match res {
                    Ok(value) => value,
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Literal::Null
                    }
                }
            }
            _ => return Literal::Null,
        }
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

    fn evaluate(&self) -> Literal {
        self.exprs.iter().map(|a| a.evaluate()).last().unwrap()
    }
}

fn is_truthy(literal: Literal) -> bool {
    match literal {
        Literal::Boolean(false) => false,
        Literal::Null => false,
        _ => true,
    }
}
