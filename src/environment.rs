use std::collections::HashMap;

use crate::parser::expression::Literal;

pub struct Environment {
    pub values: HashMap<String, Literal>,
}

impl Environment {
    pub fn define(&mut self, identifier: String, value: Literal) {
        self.values.insert(identifier, value);
    }

    pub fn get(&self, identifier: &String) -> Option<&Literal> {
        self.values.get(identifier)
    }
}
