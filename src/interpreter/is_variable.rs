use std::any::Any;

use crate::{parser::expression::Variable, scanner::token::Token};

pub fn as_variable(value: &dyn Any) -> Option<Variable> {
    if let Some(var) = value.downcast_ref::<Variable>() {
        Some(Variable {
            name: Token {
                lexeme: var.name.lexeme.clone(),
                line: var.name.line,
                token_type: var.name.token_type.clone(),
            },
        })
    } else {
        None
    }
}
