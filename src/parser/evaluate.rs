use crate::environment::Environment;
use crate::scanner::token::TokenType;

use super::{
    expression::{Binary, Literal},
    runtime_error::{RuntimeError, RuntimeErrorType},
};

pub fn arithmetic(expr: &Binary, env: &Environment) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate(&env);
    if left.is_err() {
        return left;
    }
    let right = expr.right.evaluate(&env);
    if right.is_err() {
        return right;
    }

    let _left = left.unwrap();
    let _right = right.unwrap();

    match _left {
        Literal::Number(l) => match _right {
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

pub fn plus(expr: &Binary, env: &Environment) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate(env);
    if left.is_err() {
        return left;
    }
    let right = expr.right.evaluate(env);
    if right.is_err() {
        return right;
    }

    let _left = left.unwrap();
    let mut _right = right.unwrap();

    match _left {
        Literal::Number(l) => match _right {
            Literal::Number(r) => Ok(Literal::Number(l + r)),
            Literal::String(r_str) => {
                let cast_r = r_str.parse::<f64>();
                if cast_r.is_ok() {
                    Ok(Literal::Number(l + cast_r.unwrap()))
                } else {
                    Err(RuntimeError::new(
                        RuntimeErrorType::ArithemticInvalidOperandAfterCast,
                        0,
                    ))
                }
            }
            _ => {
                return Err(RuntimeError::new(
                    RuntimeErrorType::OperationNotSupported,
                    0,
                ));
            }
        },
        Literal::String(lstr) => match _right {
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

pub fn comparison(expr: &Binary, env: &Environment) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate(env);
    if left.is_err() {
        return left;
    }
    let right = expr.right.evaluate(env);
    if right.is_err() {
        return right;
    }

    let _left = left.unwrap();
    let _right = right.unwrap();

    match _left {
        Literal::Number(lv) => match _right {
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

pub fn eq_comparison(expr: &Binary, env: &Environment) -> Result<Literal, RuntimeError> {
    let left = expr.left.evaluate(env);
    if left.is_err() {
        return left;
    }
    let right = expr.right.evaluate(env);
    if right.is_err() {
        return right;
    }

    let _left = left.unwrap();
    let _right = right.unwrap();

    match _left {
        Literal::Boolean(lb) => match _right {
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
        Literal::Number(ln) => match _right {
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
        Literal::String(ls) => match _right {
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
