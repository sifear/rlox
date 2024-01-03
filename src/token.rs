use core::fmt;

#[derive(Debug)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::And => write!(f, "And"),
            TokenType::Bang => write!(f, "Bang"),
            TokenType::BangEqual => write!(f, "BangEqual"),
            TokenType::Class => write!(f, "Class"),
            TokenType::Comma => write!(f, "Comma"),
            TokenType::Dot => write!(f, "Dot"),
            TokenType::Else => write!(f, "Else"),
            TokenType::Eof => write!(f, "Eof"),
            TokenType::Equal => write!(f, "Equal"),
            TokenType::EqualEqual => write!(f, "EqualEqual"),
            TokenType::False => write!(f, "False"),
            TokenType::For => write!(f, "For"),
            TokenType::Fun => write!(f, "Fun"),
            TokenType::Greater => write!(f, "Greater"),
            TokenType::GreaterEqual => write!(f, "GreaterEqual"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::If => write!(f, "If"),
            TokenType::LeftBrace => write!(f, "LeftBrace"),
            TokenType::LeftParen => write!(f, "LeftParen"),
            TokenType::Less => write!(f, "Less"),
            TokenType::LessEqual => write!(f, "LessEqual"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Nil => write!(f, "Nil"),
            TokenType::Number => write!(f, "Number"),
            TokenType::Or => write!(f, "Or"),
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Print => write!(f, "Print"),
            TokenType::Return => write!(f, "Return"),
            TokenType::RightBrace => write!(f, "RightBrace"),
            TokenType::RightParen => write!(f, "RightParen"),
            TokenType::Semicolon => write!(f, "Semicolon"),
            TokenType::Slash => write!(f, "Slash"),
            TokenType::Star => write!(f, "Star"),
            TokenType::String => write!(f, "String"),
            TokenType::Super => write!(f, "Super"),
            TokenType::This => write!(f, "This"),
            TokenType::True => write!(f, "True"),
            TokenType::Var => write!(f, "Var"),
            TokenType::While => write!(f, "While"),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<String>,
    line: u32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Option<String>, line: u32) -> Token {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        let mut as_string = self.token_type.to_string();
        as_string.push_str(&self.lexeme);
        as_string.push_str(match &self.literal {
            Some(lit) => lit,
            None => &"",
        });

        as_string
    }
}
