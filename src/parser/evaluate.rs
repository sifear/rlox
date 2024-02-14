use std::{any::TypeId, fmt::format};

use crate::token::TokenType;

use super::{
    expression::{Binary, Expr, Literal},
    runtime_error::{RuntimeError, RuntimeErrorType},
};

pub fn arithmetic(expr: &Binary) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate();
    let right = expr.right.evaluate();

    match left {
        Literal::Number(l) => match right {
            Literal::Number(r) => match expr.op.token_type {
                TokenType::Minus => Ok(Literal::Number(l - r)),
                TokenType::Plus => Ok(Literal::Number(l + r)),
                TokenType::Star => Ok(Literal::Number(l * r)),
                TokenType::Slash => Ok(Literal::Number(l / r)),
                _ => {
                    return Err(RuntimeError::new(
                        RuntimeErrorType::ArithmeticInvalidOperand,
                        0,
                    ));
                }
            },
            _ => {
                return Err(RuntimeError::new(
                    RuntimeErrorType::ArithmeticInvalidOperand,
                    0,
                ));
            }
        },
        _ => {
            return Err(RuntimeError::new(
                RuntimeErrorType::ArithmeticInvalidOperand,
                0,
            ));
        }
    }
}

pub fn plus(expr: &Binary) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate();
    let mut right = expr.right.evaluate();

    match left {
        Literal::Number(..) => match right {
            Literal::String(r_str) => {
                println!("to parse: {}", r_str);
                let parsed = r_str.parse::<f64>();
                match parsed {
                    Ok(res) => {
                        right = Literal::Number(res);
                    }
                    Err(..) => {
                        return Err(RuntimeError::new(
                            RuntimeErrorType::ArithemticInvalidOperandAfterCast,
                            0,
                        ));
                    }
                }
            }
            _ => {}
        },
        _ => {}
    };

    match left {
        Literal::Number(l) => match right {
            Literal::Number(r) => Ok(Literal::Number(l + r)),
            Literal::String(r_str) => {
                let cast_r = r_str.parse::<f64>();
                if cast_r.is_ok() {
                    return Ok(Literal::Number(l + cast_r.unwrap()));
                } else {
                    return Err(RuntimeError::new(
                        RuntimeErrorType::ArithemticInvalidOperandAfterCast,
                        0,
                    ));
                }
            }
            _ => {
                return Err(RuntimeError::new(
                    RuntimeErrorType::OperationNotSupported,
                    0,
                ));
            }
        },
        Literal::String(lstr) => match right {
            Literal::String(rstr) => Ok(Literal::String(format!("{}{}", lstr, rstr))),
            Literal::Number(rstr_num) => {
                Ok(Literal::String(format!("{}{}", lstr, rstr_num.to_string())))
            }
            _ => {
                return Err(RuntimeError::new(
                    RuntimeErrorType::OperationNotSupported,
                    0,
                ));
            }
        },
        _ => {
            return Err(RuntimeError::new(
                RuntimeErrorType::OperationNotSupported,
                0,
            ));
        }
    }
}

pub fn comparison(expr: &Binary) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate();
    let right = expr.right.evaluate();

    match left {
        Literal::Number(lv) => match right {
            Literal::Number(rv) => match expr.op.token_type {
                TokenType::Less => Ok(Literal::Boolean(lv < rv)),
                TokenType::LessEqual => Ok(Literal::Boolean(lv <= rv)),
                TokenType::Greater => Ok(Literal::Boolean(lv > rv)),
                TokenType::GreaterEqual => Ok(Literal::Boolean(lv >= rv)),
                _ => Err(RuntimeError::new(
                    RuntimeErrorType::ComparisonInvalidOperand,
                    0,
                )),
            },
            _ => Err(RuntimeError::new(
                RuntimeErrorType::ComparisonInvalidOperand,
                0,
            )),
        },
        _ => Err(RuntimeError::new(
            RuntimeErrorType::ComparisonInvalidOperand,
            0,
        )),
    }
}

pub fn eq_comparison(expr: &Binary) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate();
    let right = expr.right.evaluate();

    match left {
        Literal::Boolean(lb) => match right {
            Literal::Boolean(rb) => match expr.op.token_type {
                TokenType::BangEqual => Ok(Literal::Boolean(lb != rb)),
                TokenType::EqualEqual => Ok(Literal::Boolean(lb == rb)),
                _ => Err(RuntimeError::new(
                    RuntimeErrorType::OperationNotSupported,
                    0,
                )),
            },
            _ => Err(RuntimeError::new(
                RuntimeErrorType::OperationNotSupported,
                0,
            )),
        },
        Literal::Number(ln) => match right {
            Literal::Number(rn) => match expr.op.token_type {
                TokenType::BangEqual => Ok(Literal::Boolean(ln != rn)),
                TokenType::EqualEqual => Ok(Literal::Boolean(ln == rn)),
                _ => Err(RuntimeError::new(
                    RuntimeErrorType::OperationNotSupported,
                    0,
                )),
            },
            _ => Err(RuntimeError::new(
                RuntimeErrorType::OperationNotSupported,
                0,
            )),
        },
        Literal::String(ls) => match right {
            Literal::String(rs) => match expr.op.token_type {
                TokenType::BangEqual => Ok(Literal::Boolean(ls != rs)),
                TokenType::EqualEqual => Ok(Literal::Boolean(ls == rs)),
                _ => Err(RuntimeError::new(
                    RuntimeErrorType::OperationNotSupported,
                    0,
                )),
            },
            _ => Err(RuntimeError::new(
                RuntimeErrorType::OperationNotSupported,
                0,
            )),
        },
        _ => Err(RuntimeError::new(
            RuntimeErrorType::OperationNotSupported,
            0,
        )),
    }
}
