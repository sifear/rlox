use crate::token::{self, Token, TokenType};
use core::fmt;
use core::fmt::Debug;
pub trait Expr {
    fn to_string(&self) -> String;
}

impl Debug for dyn Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Expression {{{}}}", self.to_string())
    }
}

pub struct Empty {}

pub struct Ternery {
    pub predicate: Box<dyn Expr>,
    pub true_arm: Box<dyn Expr>,
    pub false_arm: Box<dyn Expr>,
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
    op: Token,
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

#[derive(Debug)]
pub struct Grouping {
    pub exprs: Vec<Box<dyn Expr>>,
}

impl fmt::Display for dyn Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr for Empty {
    fn to_string(&self) -> String {
        format!("<Discarded expression>")
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
        let mut cucc = String::from("(");
        for (i, expr) in self.exprs.iter().enumerate() {
            cucc.push_str(&expr.to_string());

            if i < self.exprs.len() - 1 {
                cucc.push(',');
            }
        }
        cucc.push(')');

        return cucc;
    }
}

pub fn test() {
    let token_type = TokenType::String;

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
