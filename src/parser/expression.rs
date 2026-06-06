use crate::{
    environment::Environment,
    interpreter::{
        is_variable::as_variable,
        runtime_error::{RuntimeError, RuntimeErrorType},
    },
    parser::{
        expression::Literal::FnObject,
        statement::{BlockStmt, FunStmt, Statement},
    },
    scanner::token::{Token, TokenType},
};
use core::fmt;
use core::fmt::Debug;
use std::{
    any::Any,
    borrow::BorrowMut,
    cell::RefCell,
    collections::HashMap,
    fs,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use super::evaluate::{arithmetic, comparison, eq_comparison, plus};
use crate::is_truthy::is_truthy;

pub trait Expr: Any {
    fn to_string(&self) -> String;
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError>;
    fn as_any(&self) -> &dyn Any;
}

impl dyn Expr {
    // Helper method to check the type
    fn is<T: Any>(&self) -> bool {
        self.type_id() == std::any::TypeId::of::<T>()
    }
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref::<T>()
    }
}

impl Debug for dyn Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // write!(f, "Expression {{{}}}", self.to_string())
        write!(f, "")
    }
}

pub struct Empty {}

pub struct Ternery {
    pub predicate: Rc<dyn Expr>,
    pub true_arm: Rc<dyn Expr>,
    pub false_arm: Rc<dyn Expr>,
}

// #[derive(Clone)]
#[derive(Debug)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    FnObject(FnObjectStruct),
    Break,
    Return,
    Null,
}

#[derive(Debug, Clone)]
pub struct FnObjectStruct {
    pub name: String,
    pub args: Vec<Token>,
    pub statements: Rc<BlockStmt>,
    pub local_env: RefCell<Option<Rc<RefCell<Environment>>>>,
}

impl Clone for Literal {
    fn clone(&self) -> Literal {
        match self {
            Literal::String(s) => Literal::String(String::from(s.as_str())),
            Literal::Number(n) => Literal::Number(n.clone()),
            Literal::Boolean(b) => Literal::Boolean(*b),
            Literal::FnObject(f) => Literal::FnObject((*f).clone()),
            Literal::Break => Literal::Break,
            Literal::Return => Literal::Return,
            Literal::Null => Literal::Null,
        }
    }
}

pub struct Unary {
    op: Token,
    right: Rc<dyn Expr>,
}

pub struct Call {
    pub calle: Rc<dyn Expr>,
    pub paren: Token,
    pub arguments: Vec<Rc<dyn Expr>>,
}

pub struct Binary {
    pub left: Rc<dyn Expr>,
    pub op: Token,
    pub right: Rc<dyn Expr>,
}

pub struct Logical {
    pub left: Rc<dyn Expr>,
    pub op: Token,
    pub right: Rc<dyn Expr>,
}

pub struct Variable {
    pub name: Token,
}

pub struct Assign {
    pub l_value: Token,
    pub value: Rc<dyn Expr>,
}

impl Unary {
    pub fn new(right: Rc<dyn Expr>, op: Token) -> Unary {
        Unary { right, op }
    }
}

impl Binary {
    pub fn new(left: Rc<dyn Expr>, right: Rc<dyn Expr>, op: Token) -> Binary {
        Binary { left, right, op }
    }
}

impl fmt::Display for dyn Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr for Call {
    fn to_string(&self) -> String {
        format!("<Call expression>")
    }

    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        // let calle = self.calle.evaluate(env);

        match as_variable(self.calle.as_any()) {
            Some(fn_name) => {
                let env_ref = env.borrow();
                let global_methods_borrow = env_ref.global_methods.borrow();
                let name = &fn_name.name.lexeme.unwrap();
                let b = global_methods_borrow.get(name);
                match b {
                    Some(val) => {
                        if val.arity != self.arguments.len() as u32 {
                            return Err(RuntimeError::new(
                                RuntimeErrorType::ArgumentCountMismatch,
                                self.paren.line,
                            ));
                        }
                        let c = *(val.function)();
                        let d = c.clone();

                        Ok(d)
                    }
                    None => {
                        println!("getting method with name: {}", name);
                        let mut func: Option<FnObjectStruct> = env.borrow().get_method(name);

                        match &mut func {
                            Some(fun) => {
                                println!("get env of function object literal");
                                let local_env = fun.local_env.borrow();
                                let _local_env = local_env.as_ref().unwrap().clone();

                                for (index, arg) in fun.args.iter().enumerate() {
                                    if index <= self.arguments.len() {
                                        let input_value = self.arguments[index]
                                            .evaluate(env.clone(), result_buffer)
                                            .unwrap();
                                        let input_identifier = arg.lexeme.as_ref().unwrap().clone();
                                        println!(
                                            "defining {:?} for {}",
                                            input_value, input_identifier
                                        );

                                        _local_env.borrow().define(input_identifier, Some(input_value.clone()));
                                    }
                                }

                                println!("caling fn object with:");
                                _local_env.borrow().ls(0);


                                let b = fun.statements.evaluate(_local_env, result_buffer);

                                return b;
                            }
                            None => {
                                return Err(RuntimeError::new(
                                    RuntimeErrorType::FunctionNameNotFound,
                                    self.paren.line,
                                ));
                            }
                        }
                    }
                }
            }
            None => {
                return Err(RuntimeError::new(
                    RuntimeErrorType::NotCallableExpression,
                    self.paren.line,
                ));
            }
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Empty {
    fn to_string(&self) -> String {
        format!("<Discarded expression>")
    }

    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        return Ok(Literal::Null);
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Ternery {
    fn to_string(&self) -> String {
        let mut cucc = String::from("(");
        cucc.push_str(&self.predicate.to_string());
        cucc.push_str(" ? ");
        cucc.push_str(&self.true_arm.to_string());
        cucc.push_str(" : ");
        cucc.push_str(&self.false_arm.to_string());
        cucc.push(')');
        return cucc;
    }

    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let res_of_predicate = self.predicate.evaluate(env.clone(), result_buffer);
        if res_of_predicate.is_err() {
            return res_of_predicate;
        }

        if is_truthy(&res_of_predicate.unwrap()) {
            self.true_arm.evaluate(env.clone(), result_buffer)
        } else {
            self.false_arm.evaluate(env.clone(), result_buffer)
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Null => String::from("(Null literal)"),
            Literal::Break => format!("(Break)"),
            Literal::Return => format!("(Return)"),
            Literal::Boolean(true) => String::from("true"),
            Literal::Boolean(false) => String::from("false"),
            Literal::Number(n) => format!("{}", n),
            Literal::FnObject(f) => format!("fn object {}", f.name),
            Literal::String(str_val) => format!("{}", str_val),
        }
    }

    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        match self {
            FnObject(f) => {
                println!("Evaluating literal fn object");

                let local_env = Rc::new(RefCell::new(Environment::new(
                    RefCell::new(HashMap::new()),
                    RefCell::new(Some(env.clone())),
                )));

                f.args.iter().for_each(|a| {
                    local_env.borrow().define(a.lexeme.clone().unwrap(), None);
                });

                f.local_env.replace(Some(local_env.clone()));
            }
            _ => {}
        }

        return Ok(self.clone());
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Unary {
    fn to_string(&self) -> String {
        format!("({} {})", self.op, self.right)
    }

    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        match self.op.token_type {
            TokenType::Minus => {
                let ampl = self.right.evaluate(env, result_buffer);
                match ampl {
                    Ok(res) => match res {
                        Literal::Number(n) => Ok(Literal::Number(-1.0 * n)),
                        _ => Err(RuntimeError::new(RuntimeErrorType::Unknown, 0)),
                    },
                    Err(err) => Err(err),
                }
            }
            TokenType::Bang => {
                let a = self.right.evaluate(env, result_buffer);
                match a {
                    Ok(res) => {
                        let ampl = is_truthy(&res);

                        Ok(Literal::Boolean(!ampl))
                    }
                    Err(err) => Err(err),
                }
            }
            _ => {
                panic!("Unexpected operator token type while evaluating unary.")
            }
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Binary {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        match self.op.token_type {
            TokenType::Minus | TokenType::Star | TokenType::Slash => {
                let res = arithmetic(self, env.clone(), result_buffer);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            TokenType::Plus => {
                let res = plus(&self, env.clone(), result_buffer);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => {
                let res = comparison(self, env, result_buffer);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            TokenType::BangEqual | TokenType::EqualEqual => {
                let res = eq_comparison(self, env, result_buffer);
                match res {
                    Ok(value) => Ok(value),
                    Err(runtime_error) => {
                        println!("{}", runtime_error.to_string());

                        Ok(Literal::Null)
                    }
                }
            }
            _ => Ok(Literal::Null),
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Logical {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let left = self.left.evaluate(env.clone(), result_buffer);
        if left.is_err() {
            return left;
        }
        let _left = left.unwrap();

        if self.op.token_type == TokenType::Or {
            if is_truthy(&_left) {
                return Ok(_left);
            }
        } else {
            if !is_truthy(&_left) {
                return Ok(_left);
            }
        }

        self.right.evaluate(env.clone(), result_buffer)
    }
}

impl Expr for Variable {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        match &self.name.lexeme {
            Some(name) => {
                let a = env.borrow().get(name);
                match a {
                    Some(b) => {
                        if !b.1 {
                            return Err(RuntimeError::new(
                                RuntimeErrorType::AccessToUninitiaizedVariable,
                                self.name.line,
                            ));
                        }

                        Ok(b.0.clone())
                    }
                    None => {
                        let method = env.borrow().get_method(name);
                        match method {
                            Some(a) => Ok(Literal::FnObject(a)),
                            None => Err(RuntimeError::new(
                                RuntimeErrorType::IdentifierNotDefined(String::from(name)),
                                self.name.line,
                            )),
                        }
                    }
                }
            }
            None => Err(RuntimeError::new(
                RuntimeErrorType::IdentifierTokenNotSaved,
                0,
            )),
        }
    }

    fn to_string(&self) -> String {
        format!("(VAR {})", self.name.lexeme.clone().take().unwrap())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expr for Assign {
    fn evaluate(
        &self,
        env: Rc<RefCell<Environment>>,
        result_buffer: &mut String,
    ) -> Result<Literal, RuntimeError> {
        let val = self.value.evaluate(env.clone(), result_buffer);
        let _val = match val {
            Ok(v) => v,
            Err(err) => return Err(err),
        };

        match &self.l_value.lexeme {
            Some(a) => {
                let success = env.borrow().assign(&a, &_val);
                if success {
                    Ok(_val)
                } else {
                    Err(RuntimeError::new(
                        RuntimeErrorType::IdentifierNotDefined(String::from(a)),
                        self.l_value.line,
                    ))
                }
            }
            None => Err(RuntimeError::new(
                RuntimeErrorType::IdentifierNotDefined(String::from("?")),
                self.l_value.line,
            )),
        }
    }

    fn to_string(&self) -> String {
        "".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
