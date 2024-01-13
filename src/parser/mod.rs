use std::borrow::Borrow;

use crate::token::Token;
use crate::token::TokenType;

enum LiteralValue {
    Number(f64),
    String(String),
}

impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Number(num) => write!(f, "{}", *num),
            LiteralValue::String(str) => write!(f, "{}", str),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary { expr, operator } => write!(f, "({} {})", operator, expr),
            Expr::Binary { left, right, operator } => write!(f, "({} {} {})", operator, left, right ),
            Expr::Grouping { expression } => write!(f, "({})", expression),
            Expr::Literal(literal) => write!(f, "{}", literal)
        }
    }
}

enum Expr {
    Unary {
        expr: Box<Expr>,
        operator: Token,
    },
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: Token,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal(LiteralValue),
}


pub fn cucc() {
    let left = Expr::Literal(LiteralValue::Number(45.0));
    let right = Expr::Literal(LiteralValue::Number(100.0));

    let expr = Expr::Binary {
        left: Box::new(left),
        right: Box::new(right),
        operator: Token::new(TokenType::Plus, None, None, 1),
    };

    let left2 = Expr::Literal(LiteralValue::Number(3.0));
    let right2 = Expr::Literal(LiteralValue::Number(4.0));

    let expr2 = Expr::Binary {
        left: Box::new(left2),
        right: Box::new(right2),
        operator: Token::new(TokenType::Plus, None, None, 1),
    };

    let expr3 = Expr::Binary { left: Box::new(expr), right:Box::new(expr2), operator: Token::new(TokenType::Minus, None, None, 0) };

    let expr4 = Expr::Unary { expr: Box::new(expr3), operator: Token::new(TokenType::Minus, None, None, 0) };

    let expr5 = Expr::Grouping { expression: Box::new(expr4) };

    println!("{}", expr5);
}
