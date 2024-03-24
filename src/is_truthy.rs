use crate::parser::expression::Literal;

pub fn is_truthy(literal: &Literal) -> bool {
    match literal {
        Literal::Boolean(false) => false,
        Literal::Null => false,
        _ => true,
    }
}