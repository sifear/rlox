use crate::scanner::token::Token;
use crate::{environment::Environment, is_truthy::is_truthy};

use super::expression::{Expr, Literal};
use crate::interpreter::runtime_error::RuntimeError;
use core::fmt::Debug;
use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap};

pub trait Statement {
    fn evaluate<'a>(&self, env: &'a Environment<'a>) -> Result<Literal, RuntimeError>;
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

pub struct IfStmt {
    pub cond: Box<dyn Expr>,
    pub then: Box<dyn Statement>,
    pub els: Option<Box<dyn Statement>>,
}

pub struct WhileStmt {
    pub cond: Box<dyn Expr>,
    pub body: Box<dyn Statement>,
}

impl Statement for IfStmt {
    fn evaluate<'a>(&self, env: &'a Environment<'a>) -> Result<Literal, RuntimeError> {
        let cond_eval = self.cond.evaluate(env);
        if cond_eval.is_err() {
            println!("{}", cond_eval.unwrap_err());
            return Ok(Literal::Null);
        }

        if is_truthy(&cond_eval.unwrap()) {
            self.then.evaluate(env)
        } else {
            match &self.els {
                Some(stmts) => stmts.evaluate(env),
                None => Ok(Literal::Null),
            }
        }
    }

    fn to_string(&self) -> String {
        format!("<If stmt>")
    }
}

impl Statement for BlockStmt {
    fn to_string(&self) -> String {
        format!("<Block stmt>")
    }

    fn evaluate<'a>(&self, env: &'a Environment<'a>) -> Result<Literal, RuntimeError> {
        let mut last_value = Literal::Null;
        let local_env: Environment<'_> = Environment {
            values: RefCell::new(HashMap::new()),
            enclosing: Some(env),
        };

        for statement in self.stmts.iter() {
            let res = statement.evaluate(&local_env);
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

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        self.expr.evaluate(env)
    }
}
impl Statement for PrintStmt {
    fn to_string(&self) -> String {
        format!("<Print stmt>")
    }

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
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
        format!("<Var stmt {:?}>", self.name.lexeme.clone().unwrap())
    }

    fn evaluate(&self, env: &Environment) -> Result<Literal, RuntimeError> {
        let initial_value = match &self.initializer {
            Some(initer) => {
                let res = initer.evaluate(env);
                if res.is_err() {
                    return res;
                }

                Some(res.unwrap())
            }
            None => None,
        };

        match &self.name.lexeme {
            Some(name) => env.define(name.clone(), initial_value),
            None => {
                // Runtime exception?
            }
        }

        Ok(Literal::Null)
    }
}

impl Statement for WhileStmt {
    fn evaluate<'a>(&self, env: &'a Environment<'a>) -> Result<Literal, RuntimeError> {
        println!("{:?}", self.body);
        println!("{:?}", self.cond);
        loop {
            let cond = self.cond.evaluate(env);
            if cond.is_err() {
                return cond;
            }

            if !is_truthy(&cond.unwrap()) {
                return  Ok(Literal::Null);
            }

            let block_eval = self.body.evaluate(env);
            if block_eval.is_err() {
                return block_eval;
            }
        }
    }

    fn to_string(&self) -> String {
        format!("<While stmt>")
    }
}
