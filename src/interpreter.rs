use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{environment::Environment, parser::statement::Statement};

pub mod is_variable;
pub mod runtime_error;

pub struct Interpreter {
    statements: Vec<Rc<dyn Statement>>,
    pub result_buffer: String,
}

impl Interpreter {
    pub fn new(statements: Vec<Rc<dyn Statement>>) -> Interpreter {
        Interpreter {
            statements,
            result_buffer: String::from(""),
        }
    }

    pub fn interpret(&mut self) {
        let global_env = Rc::new(RefCell::new(Environment::new(
            RefCell::new(HashMap::new()),
            RefCell::new(None),
        )));

        for stmt in &self.statements {
            match stmt.evaluate(global_env.clone(), &mut self.result_buffer) {
                Ok(value) => {}
                Err(err) => {
                    println!("{err}")
                }
            }
        }
    }

    pub fn print_ast(&self) {
        for statement in self.statements.iter() {
            println!("{}", statement)
        }
    }
}
