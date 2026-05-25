use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use std::ops::{Deref, DerefMut};

use crate::parser::{
    expression::{FnObjectStruct, Literal},
    method::Callable,
    statement::FunStmt,
};

pub struct Environment {
    pub values: RefCell<HashMap<String, (Literal, bool)>>,
    pub enclosing: RefCell<Option<Rc<RefCell<Environment>>>>,
    pub global_methods: RefCell<HashMap<String, Callable>>,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment").finish()
    }
}

impl Environment {
    pub fn new(
        _values: RefCell<HashMap<String, (Literal, bool)>>,
        _enclosing: RefCell<Option<Rc<RefCell<Environment>>>>,
    ) -> Environment {
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

    pub fn define_method(&self, identifier: String, value: Literal) {
        self.values.borrow_mut().insert(identifier, (value, true));
    }

    pub fn assign(&self, identifier: &String, value: &Literal) -> bool {
        let mut a = self.values.borrow_mut();

        match a.get(identifier) {
            Some(val) => {
                a.insert(identifier.clone(), (value.clone(), true));

                true
            }
            None => match self.enclosing.borrow().as_ref() {
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

        if let Some(enclosing) = &self.enclosing.borrow().as_ref() {
            return enclosing.borrow().get(identifier);
        } else {
        }

        None
    }

    pub fn get_method(&self, identifier: &String) -> Option<FnObjectStruct> {
        let values = self.values.borrow();
        let res = values.get(identifier);

        match res {
            Some(func) => match &func.0 {
                Literal::FnObject(f) => {
                    return Some(FnObjectStruct {
                        name: f.name.clone(),
                        args: f.args.clone(),
                        statements: f.statements.clone(),
                        local_env: f.local_env.clone(),
                    })
                }
                _ => {}
            },
            None => {}
        }

        if let Some(enclosing) = &self.enclosing.borrow().as_ref() {
            return enclosing.borrow().get_method(identifier);
        }

        None
    }

    pub fn ls(&self, depth: usize) {
        let tabs = "\t".repeat(depth);
        println!("{}Printing env", tabs);

        for entry in self.values.borrow().iter() {
            println!("{}{}: {:?}", tabs, entry.0, entry.1);
        }

        match &self.enclosing.borrow().as_ref() {
            Some(enc) => {
                println!("{}Enclosing:", tabs);
                enc.borrow().ls(depth + 1);
            }
            None => {}
        }

        println!("{}Printing env end", tabs);
    }
}
