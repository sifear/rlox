use crate::{
    environment::Environment, interpreter::runtime_error::{RuntimeError, RuntimeErrorType}, scanner::token::{Token, TokenType}
};
use core::fmt;
use core::fmt::Debug;
use std::any::Any;

use super::evaluate::{arithmetic, comparison, eq_comparison, plus};
use crate::is_truthy::is_truthy;

pub trait Expr {
    fn to_string(&self) -> String;
    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError>;
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

pub struct Logical {
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

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        return Ok(Literal::Null);
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

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        let res_of_predicate = self.predicate.evaluate(env);
        if res_of_predicate.is_err() {
            return res_of_predicate;
        }

        if is_truthy(&res_of_predicate.unwrap()) {
            self.true_arm.evaluate(env)
        } else {
            self.false_arm.evaluate(env)
        }
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

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        return Ok(self.clone());
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Unary {
    fn to_string(&self) -> String {
        format!("({} {})", self.op, self.right)
    }

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
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
                        let ampl = is_truthy(&res);

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

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Binary {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }
    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
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

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Logical {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        let mut left = self.left.evaluate(env);
        if left.is_err() {
            return left;
        }
        let _left = left.unwrap();

        if self.op.token_type == TokenType::Or {

            if is_truthy(&_left) {
                return Ok(_left)
            }
        } else {
            if !is_truthy(&_left) {
                return Ok(_left)
            }
        }

        self.right.evaluate(env)
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

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        self.exprs.iter().map(|a| a.evaluate(env)).last().unwrap()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Variable {
    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        match &self.name.lexeme {
            Some(name) => {
                let a = env.get(name);
                match a {
                    Some(b) => {
                        if !b.1 {
                            return Err(RuntimeError::new(
                                RuntimeErrorType::AccessToUninitiaizedVariable,
                                self.name.line,
                            ));
                        }

                        Ok(b.0.clone())
                    }
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

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Assign {
    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
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

    fn as_any(&self) -> &dyn Any {
        self
    }
}
