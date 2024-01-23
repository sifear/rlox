use crate::token::{self, Token, TokenType};
use core::fmt;

pub trait Expr {
    fn to_string(&self) -> String;
}

pub struct Literal {
    pub literal_type: TokenType,
}

pub struct Unary {
    op: Token,
    right: Box<dyn Expr>,
}
pub struct Binary {
    left: Box<dyn Expr>,
    op:Token,
    right: Box<dyn Expr>,
}

impl Unary {
    pub fn new(right: Box<dyn Expr>, op: Token) -> Unary {
        Unary { right, op }
    }
}

impl Binary {
    pub fn new(left: Box<dyn Expr>, right: Box<dyn Expr>, op: Token) -> Binary {
        Binary { left, right, op }
    }
}

pub struct Grouping {
    pub expr: Box<dyn Expr>,
}

impl fmt::Display for dyn Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr for Literal {
    fn to_string(&self) -> String {
        format!("({})", self.literal_type)
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
    let token_type =  TokenType::String(String::from("test"));

    let literal = Literal {
        literal_type: token_type,
    };
    let unary_expr = Unary {
        op: Token::new(TokenType::Minus, None, None, 1),
        right: Box::new(literal),
    };

    let unary_box: Box<dyn Expr> = Box::new(unary_expr);

    println!("{}", unary_box);
}
