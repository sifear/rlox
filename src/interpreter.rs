use std::{cell::RefCell, collections::HashMap};

use crate::{environment::Environment, parser::statement::Statement};

pub mod is_variable;
pub mod runtime_error;

pub struct Interpreter<'a> {
    statements: Vec<Box<dyn Statement>>,
    env: Environment<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new(statements: Vec<Box<dyn Statement>>) -> Interpreter<'a> {
        Interpreter {
            statements,
            env: Environment {
                values: RefCell::new(HashMap::new()),
                enclosing: None,
            },
        }
    }

    pub fn interpret(&'a mut self) {
        println!("{:?}", self.statements);

        for stmt in &self.statements {
            match stmt.evaluate(&mut self.env) {
                Ok(value) => {},
                Err(err) => {
                    println!("{err}")
                }
            }
        }
    }
}
