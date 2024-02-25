use core::fmt;

#[derive(Debug)]
pub enum ParserErrorType {
    UnclosedGroup,
    ExpressionExpected,
    ExpressionListExpected,
    BinaryMissingLHS,
    PredicateMissingTrue,
    PredicateMissingFalse,
}

#[derive(Debug)]
pub struct ParserError {
    error_type: ParserErrorType,
    pub line: u32,
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
            ParserErrorType::ExpressionExpected => write!(f, "Expression expected at line {}", self.line),
            ParserErrorType::ExpressionListExpected => write!(f, "Expression list expected at line {}", self.line),
            ParserErrorType::BinaryMissingLHS => write!(f, "Binary expression is missing left side at line {}", self.line),
            ParserErrorType::PredicateMissingTrue => write!(f, "Ternery expression missing true arm {}", self.line),
            ParserErrorType::PredicateMissingFalse => write!(f, "Ternery expression missing false arm {}", self.line),
        }
    }
}
