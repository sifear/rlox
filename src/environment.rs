use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::parser::{expression::Literal, method::Callable, statement::FunStmt};

pub struct Environment {
    pub values: RefCell<HashMap<String, (Literal, bool)>>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub global_methods: RefCell<HashMap<String, Callable>>,
    pub local_methods: RefCell<HashMap<String, Rc<FunStmt>>>,
}

impl Environment {
    pub fn new(_values: RefCell<HashMap<String, (Literal, bool)>>, _enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        let local_methods: RefCell<HashMap<String, Rc<FunStmt>>> = RefCell::new(HashMap::new());
        let global_methods: RefCell<HashMap<String, Callable>> = RefCell::new(HashMap::new());

        {
            let mut a = global_methods.borrow_mut();

            let now_name = String::from("now");
            let now_func = Box::new(|| {
                let now = SystemTime::now().duration_since(UNIX_EPOCH);
                match now {
                    Ok(e) => {
                        return Box::new(Literal::String(e.as_secs().to_string()));
                    }
                    Err(_) => {
                        return Box::new(Literal::String(String::from("Error")));
                    }
                }
            });

            let func = Callable::new(now_name.clone(), 0, now_func);
            a.insert(String::from("now"), func);
        }

        Environment {
            enclosing: _enclosing,
            values: _values,
            global_methods,
            local_methods,
        }
    }

    pub fn define(&self, identifier: String, value: Option<Literal>) {
        let initialized = value.is_some();
        let _value = match value {
            Some(literal) => literal,
            None => Literal::Null,
        };

        self.values
            .borrow_mut()
            .insert(identifier, (_value, initialized));
    }

    pub fn define_method(&self, identifier: String, value: Rc<FunStmt>) {
        self.local_methods.borrow_mut().insert(identifier, value);
    }

    pub fn assign(&self, identifier: &String, value: &Literal) -> bool {
        let mut a = self.values.borrow_mut();

        match a.get(identifier) {
            Some(val) => {
                a.insert(identifier.clone(), (value.clone(), true));

                true
            }
            None => match &self.enclosing {
                Some(enclosing) => {
                    enclosing.borrow().assign(identifier, value);

                    return true;
                }
                None => return false,
            },
        }
    }

    pub fn get(&self, identifier: &String) -> Option<(Literal, bool)> {
        let values = self.values.borrow_mut();
        let res = values.get(identifier);

        match res {
            Some(literal) => return Some(literal.clone()),
            None => {}
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(identifier);
        }

        None
    }

    pub fn get_method(&self, identifier: &String) -> Option<Rc<FunStmt>> {
        let values = self.local_methods.borrow();
        let res = values.get(identifier);

        match res {
            Some(func) => return Some(func.clone()),
            None => {}
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get_method(identifier);
        }

        None
    }
}
