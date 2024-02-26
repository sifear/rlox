use crate::environment::Environment;
use crate::scanner::token::Token;

use super::expression::{Expr, Literal};
use crate::interpreter::runtime_error::RuntimeError;
use core::fmt::Debug;
use std::{borrow::BorrowMut, collections::HashMap};

pub trait Statement {
    fn evaluate<'a>(&self, env: &'a mut Environment<'a>) -> Result<Literal, RuntimeError>;
    fn to_string(&self) -> String;
}

impl Debug for dyn Statement {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Statement [{}]", self.to_string())
    }
}

pub struct ExprStmt {
    pub expr: Box<dyn Expr>,
}
pub struct PrintStmt {
    pub expr: Box<dyn Expr>,
}

pub struct BlockStmt {
    pub stmts: Vec<Box<dyn Statement>>,
}

pub struct VarStmt {
    pub initializer: Option<Box<dyn Expr>>,
    pub name: Token,
}

impl Statement for BlockStmt {
    fn to_string(&self) -> String {
        format!("<Block stmt>")
    }

    fn evaluate<'a>(&self, env: &'a mut Environment<'a>) -> Result<Literal, RuntimeError> {
        let mut last_value = Literal::Null;
        let mut local_env: Environment<'_> = Environment {
            values: HashMap::new(),
            enclosing: Some(env),
        };
        let b = &mut local_env;

        for statement in self.stmts.iter() {
            let res = statement.evaluate(b);
            match res {
                Ok(val) => {
                    last_value = val;
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        Ok(last_value)
    }
}

impl Statement for ExprStmt {
    fn to_string(&self) -> String {
        format!("<ExprStmt stmt>")
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        self.expr.evaluate(env)
    }
}
impl Statement for PrintStmt {
    fn to_string(&self) -> String {
        format!("<Print stmt>")
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        let res = self.expr.evaluate(env);
        if res.is_err() {
            return res;
        }

        println!("{}", res.unwrap().to_string());

        Ok(Literal::Null)
    }
}

impl Statement for VarStmt {
    fn to_string(&self) -> String {
        format!("<Var stmt>")
    }

    fn evaluate(&self, env: &mut Environment) -> Result<Literal, RuntimeError> {
        let initial_value = match &self.initializer {
            Some(initer) => initer.evaluate(env),
            None => Ok(Literal::Null),
        };

        if initial_value.is_err() {
            return initial_value;
        }

        match &self.name.lexeme {
            Some(name) => env.define(name.clone(), initial_value.unwrap()),
            None => {
                // Runtime exception?
            }
        }

        Ok(Literal::Null)
    }
}
