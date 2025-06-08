use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{environment::Environment, parser::statement::Statement};

pub mod is_variable;
pub mod runtime_error;

pub struct Interpreter {
    statements: Vec<Rc<dyn Statement>>,
}

impl Interpreter {
    pub fn new(statements: Vec<Rc<dyn Statement>>) -> Interpreter {
        Interpreter { statements }
    }

    pub fn interpret(&mut self) {
        let global_env = Rc::new(RefCell::new(Environment::new(
            RefCell::new(HashMap::new()),
            None,
        )));

        for stmt in &self.statements {
            match stmt.evaluate(global_env.clone()) {
                Ok(value) => {}
                Err(err) => {
                    println!("{err}")
                }
            }
        }
    }
}
