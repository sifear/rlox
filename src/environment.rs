use std::{cell::RefCell, collections::HashMap};

use crate::parser::expression::Literal;

pub struct Environment<'a> {
    pub values: RefCell<HashMap<String, Literal>>,
    pub enclosing: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn define(&self, identifier: String, value: Literal) {
        self.values.borrow_mut().insert(identifier, value);
    }

    pub fn assign(&self, identifier: &String, value: &Literal) -> bool {
        let mut a = self.values.borrow_mut();

        match a.get(identifier) {
            Some(val) => {
                a.insert(identifier.clone(), value.clone());

                true
            }
            None => match self.enclosing {
                Some(enclosing) => {
                    enclosing.values.borrow_mut().insert(identifier.clone(), value.clone());

                    return true;
                }
                None => return false,
            },
        }
    }

    pub fn get(&self, identifier: &String) -> Option<Literal> {
        let values = self.values.borrow_mut();
        let res = values.get(identifier);

        match res {
            Some(_) => {
                return res.cloned()
            },
            None => {}
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get(identifier);
        }

        None
    }
}
