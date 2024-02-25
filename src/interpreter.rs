use std::collections::HashMap;

use crate::{environment::Environment, parser::statement::Statement};

pub mod runtime_error; 
pub mod is_variable;

pub struct Interpreter {
    statements: Vec<Box<dyn Statement>>,
    env: Environment,
}

impl Interpreter {
    pub fn new(statements: Vec<Box<dyn Statement>>) -> Interpreter {
        Interpreter {
            statements,
            env: Environment {
                values: HashMap::new(),
            },
        }
    }

    pub fn interpret(&mut self) {
        println!("{:?}", self.statements);

        for stmt in &self.statements {
            stmt.evaluate(&mut self.env);
        }
    }
}