use core::fmt;
use crate::token::{Token, TokenType};

trait Expr {
    fn to_string(&self) -> String;
}

struct Literal {
    value: String,
}
struct Unary {
    op: Token,
    right: Box<dyn Expr>,
}
struct Binary {
    left: Box<dyn Expr>,
    op: Token,
    right: Box<dyn Expr>,
}
struct Grouping {
    expr: Box<dyn Expr>,
}

impl fmt::Display for dyn Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr for Literal {
    fn to_string(&self) -> String {
        format!("({})", self.value)
    }
}
impl Expr for Unary {
    fn to_string(&self) -> String {
        format!("({} {})", self.op, self.right)
    }
}
impl Expr for Binary {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.op, self.left, self.right)
    }
}
impl Expr for Grouping {
    fn to_string(&self) -> String {
        format!("({})", self.expr)
    }
}

pub fn test() {
    let literal = Literal {
        value: String::from("test"),
    };
    let unary_expr = Unary {
        op: Token::new(TokenType::Minus, None, None, 1),
        right: Box::new(literal),
    };

    let unary_box: Box<dyn Expr> = Box::new(unary_expr);

    println!("{}", unary_box);
}
