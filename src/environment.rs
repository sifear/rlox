use std::collections::HashMap;

use crate::parser::expression::Literal;

pub struct Environment<'a> {
    pub values: HashMap<String, Literal>,
    pub enclosing: Option<&'a mut Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn define(&mut self, identifier: String, value: Literal) {
        self.values.insert(identifier, value);
    }

    pub fn assign(&mut self, identifier: &String, value: &Literal) -> bool {
        match self.values.get(identifier) {
            Some(val) => {
                self.values.insert(identifier.clone(), value.clone());

                true
            }
            None => match &mut self.enclosing {
                Some(enclosing) => {
                    enclosing.values.insert(identifier.clone(), value.clone());

                    return true;
                }
                None => return false,
            },
        }
    }

    pub fn get(&self, identifier: &String) -> Option<&Literal> {
        let res = self.values.get(identifier);
        if res.is_some() {
            return res;
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get(identifier);
        }

        None
    }
}
