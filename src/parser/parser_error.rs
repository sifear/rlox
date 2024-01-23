use core::fmt;

pub enum ParserErrorType {
    UnclosedGroup,
}

pub struct ParserError {
    error_type: ParserErrorType,
    line: u32,
}

impl ParserError {
    pub fn new(error_type: ParserErrorType, line: u32) -> ParserError {
        ParserError { error_type, line }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error_type {
            ParserErrorType::UnclosedGroup => write!(f, "Unclosed group at line {}", self.line),
        }
    }
}
