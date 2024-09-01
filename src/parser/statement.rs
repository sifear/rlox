use crate::scanner::token::Token;
use crate::{environment::Environment, is_truthy::is_truthy};

use super::expression::{Expr, Literal};
use crate::interpreter::runtime_error::RuntimeError;
use core::fmt::Debug;
use std::any::{Any, TypeId};
use std::rc::Rc;
use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap};

pub trait Statement: Any {
    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError>;
    fn to_string(&self) -> String;
}

impl dyn Statement {
    // Helper method to check the type
    fn is<T: Any>(&self) -> bool {
        self.type_id() == std::any::TypeId::of::<T>()
    }
}

impl Debug for dyn Statement {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // write!(f, "Statement [{}]", self.to_string())
        write!(f, "")
    }
}

pub struct ExprStmt {
    pub expr: Rc<dyn Expr>,
}
pub struct PrintStmt {
    pub expr: Rc<dyn Expr>,
}

pub struct BlockStmt {
    pub stmts: Vec<Rc<dyn Statement>>,
}

pub struct VarStmt {
    pub initializer: Option<Rc<dyn Expr>>,
    pub name: Token,
}

pub struct FunStmt {
    pub name: String,
    pub arguments: Vec<Token>,
    pub body: Rc<BlockStmt>,
}

pub struct IfStmt {
    pub cond: Rc<dyn Expr>,
    pub then: Rc<dyn Statement>,
    pub els: Option<Rc<dyn Statement>>,
}

pub struct WhileStmt {
    pub cond: Rc<dyn Expr>,
    pub body: Rc<dyn Statement>,
}

pub struct BreakStmt {}

impl Statement for IfStmt {
    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        let cond_eval = self.cond.evaluate(env.clone());
        if cond_eval.is_err() {
            // println!("{}", cond_eval.unwrap_err());
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

    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        let mut last_value = Literal::Null;
        let mut local_env: Environment = Environment::new();
        local_env.values = RefCell::new(HashMap::new());
        local_env.enclosing = Some(env);
        let local_env_rc = Rc::new(local_env);

        for statement in self.stmts.iter() {
            let res = statement.evaluate(local_env_rc.clone());
            match res {
                Ok(val) => match val {
                    Literal::Break => {
                        return Ok(Literal::Break);
                    }
                    _ => {
                        last_value = val;
                    }
                },
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

    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        self.expr.evaluate(env)
    }
}

impl Statement for PrintStmt {
    fn to_string(&self) -> String {
        format!("<Print stmt>")
    }

    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
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

    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        let initial_value = match &self.initializer {
            Some(initer) => {
                let res = initer.evaluate(env.clone());
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

impl Statement for FunStmt {
    fn to_string(&self) -> String {
        format!("<Fun stmt {:?}>", self.name)
    }

    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        env.define_method(
            self.name.clone(),
            Rc::new(FunStmt {
                arguments: self.arguments.clone(),
                body: self.body.clone(),
                name: self.name.clone(),
            }),
        );

        Ok(Literal::Null)
    }
}

impl Statement for WhileStmt {
    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        // println!("{:?}", self.body);
        // println!("{:?}", self.cond);
        loop {
            let cond = self.cond.evaluate(env.clone());
            if cond.is_err() {
                return cond;
            }

            if !is_truthy(&cond.unwrap()) {
                return Ok(Literal::Null);
            }

            let block_eval = self.body.evaluate(env.clone());
            if block_eval.is_err() {
                return block_eval;
            }

            match block_eval {
                Ok(literal) => match literal {
                    Literal::Break => {
                        return Ok(Literal::Null);
                    }
                    _ => {}
                },
                Err(err) => {}
            }
        }
    }

    fn to_string(&self) -> String {
        format!("<While stmt>")
    }
}

impl Statement for BreakStmt {
    fn evaluate(&self, env: Rc<Environment>) -> Result<Literal, RuntimeError> {
        Ok(Literal::Break)
    }
    fn to_string(&self) -> String {
        format!("<Break stmt>")
    }
}
