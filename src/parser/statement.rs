use crate::parser::expression::Call;
use crate::parser::method::Callable;
use crate::scanner::token::Token;
use crate::{environment::Environment, is_truthy::is_truthy};

use super::expression::{Expr, Literal};
use crate::interpreter::runtime_error::RuntimeError;
use core::fmt::{self, Debug};
use std::any::{Any};
use std::rc::Rc;
use std::{ cell::RefCell, collections::HashMap};

pub trait Statement: Any + fmt::Display {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError>;
    fn as_any(&self) -> &dyn Any;
}

impl dyn Statement {
    // Helper method to check the type
    fn is<T: Any>(&self) -> bool {
        self.type_id() == std::any::TypeId::of::<T>()
    }
    fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref::<T>()
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

impl fmt::Debug for BlockStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BlockStmt").finish()
    }
}

pub struct VarStmt {
    pub initializer: Option<Rc<dyn Expr>>,
    pub name: Token,
}

#[derive(Debug)]
pub struct FunStmt {
    pub name: String,
    pub arguments: Vec<Token>,
    pub body: Rc<BlockStmt>,
    pub closure: Rc<RefCell<Environment>>,
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

pub struct ReturnStmt {
    pub value: Rc<dyn Expr>,
}

impl Statement for IfStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let cond_eval = self.cond.evaluate(env.clone(), result_buffer);
        if cond_eval.is_err() {
            // println!("{}", cond_eval.unwrap_err());
            return Ok(Literal::Null);
        }

        if is_truthy(&cond_eval.unwrap()) {
            self.then.evaluate(env, result_buffer)
        } else {
            match &self.els {
                Some(stmts) => stmts.evaluate(env, result_buffer),
                None => Ok(Literal::Null),
            }
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<If statement>")
    }
}

impl Statement for BlockStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let mut last_value = Literal::Null;
        let local_env = Rc::new(RefCell::new(Environment::new(
            RefCell::new(HashMap::new()),
            Some(env.clone()),
        )));

        for statement in self.stmts.iter() {
            let res = statement.evaluate(local_env.clone(), result_buffer);
            match res {
                Ok(val) => match val {
                    Literal::Break => {
                        return Ok(Literal::Break);
                    }
                    Literal::Return => {
                        let a = statement.as_ref().downcast_ref::<ReturnStmt>().unwrap();

                        match a.value.evaluate(local_env.clone(), result_buffer) {
                            Ok(a) => {
                                return Ok(a);
                            }
                            Err(err) => {
                                println!("aaa: {}", err)
                            }
                        }
                        // let b = a.value.evaluate(env)

                        // return Ok(b);
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

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for BlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut a = write!(f, "<Block stmt>");
        for stmt in self.stmts.iter() {
            a = write!(f, "\n\t{}", stmt.to_string());
        }

        a
    }
}

impl Statement for ExprStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        self.expr.evaluate(env, result_buffer)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for ExprStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<ExprStmt stmt>")
    }
}

impl Statement for PrintStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let res = self.expr.evaluate(env, result_buffer);
        if res.is_err() {
            return res;
        }

        let content = res.unwrap().to_string();
        result_buffer.push_str(&content);
        result_buffer.push_str("\n");

        Ok(Literal::Null)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for PrintStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Print stmt>")
    }
}

impl Statement for VarStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let initial_value = match &self.initializer {
            Some(initer) => {
                let res = initer.evaluate(env.clone(), result_buffer);
                if res.is_err() {
                    return res;
                }

                Some(res.unwrap())
            }
            None => None,
        };

        match &self.name.lexeme {
            Some(name) => env.borrow().define(name.clone(), initial_value),
            None => {
                // Runtime exception?
            }
        }

        Ok(Literal::Null)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for VarStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut res = write!(f, "<Var stmt {:?}>", self.name.lexeme.clone().unwrap());

        if let Some(initializer) = self.initializer.clone() {
            if let Some(call) = initializer.downcast_ref::<Call>() {
                res = write!(f, "\n\t{initializer}");
            }
        }

        if let Some(initializer) = self.initializer.clone() {
            if let Some(Callable) = initializer.downcast_ref::<Callable>() {
                res = write!(f, "\n\t{initializer}");
            }
        }

        if let Some(initializer) = self.initializer.clone() {
            if let Some(literal) = initializer.downcast_ref::<Literal>() {
                match &literal {
                    Literal::FnObject(name, b) => {
                        res = write!(f, "\n\tLambda: {}", b.borrow());
                    }
                    _ => {}
                }
            }
        }

        res
    }
}

impl Statement for FunStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        println!("Evaluating funstmt");
        let closure = Rc::new(RefCell::new(Environment::new(
            RefCell::new(HashMap::new()),
            Some(env.clone()),
        )));

        self.arguments.iter().for_each(|a| {
            closure.borrow().define(a.lexeme.clone().unwrap(), None);
        });

        env.borrow().define_method(
            self.name.clone(),
            Literal::FnObject(
                self.name.clone(),
                Rc::new(RefCell::new(FunStmt {
                    arguments: self.arguments.clone(),
                    body: self.body.clone(),
                    name: self.name.clone(),
                    closure: Rc::new(RefCell::new(Environment::new(
                        RefCell::new(HashMap::new()),
                        Some(env.clone()),
                    ))),
                })),
            ),
        );

        Ok(Literal::Null)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for FunStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Fun stmt {:?}>", self.name)
    }
}

impl Statement for WhileStmt {
    fn evaluate<'a>(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        // println!("{:?}", self.body);
        // println!("{:?}", self.cond);
        loop {
            let cond = self.cond.evaluate(env.clone(), result_buffer);
            if cond.is_err() {
                return cond;
            }

            if !is_truthy(&cond.unwrap()) {
                return Ok(Literal::Null);
            }

            let block_eval = self.body.evaluate(env.clone(), result_buffer);
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

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for WhileStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<While stmt>")
    }
}

impl Statement for BreakStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        Ok(Literal::Break)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for BreakStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Break stmt>")
    }
}

impl Statement for ReturnStmt {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        Ok(Literal::Return)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for ReturnStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Return stmt>")
    }
}
