use core::fmt;

#[derive(Debug, PartialEq)]
pub enum RuntimeErrorType {
    OperationNotSupported,
    ArithmeticInvalidOperand,
    ComparisonInvalidOperand,
    ComparisonInvalidOperator,
    ArithmeticInvalidOperator,
    ArithemticInvalidOperandAfterCast,
    StatementExpected,
    ExpressionExpected,
    StatementMissingSemicolon,
    VarInitializerExpected,
    IdentifierExpedcted,
    IdentifierNotDefined(String),
    IdentifierTokenNotSaved,
    InvalidAssignmentTarget,
    AccessToUninitiaizedVariable,
    MissingWhileCondStartParenthesis,
    MissingWhileCondEndParenthesis,
    MissingForCondStartParenthesis,
    MissingForCondEndParenthesis,
    UnexpextedBreakStatement,
    NotCallableExpression,
    FunctionNameNotFound,
    ArgumentCountMismatch,
    Unknown,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub error_type: RuntimeErrorType,
    pub line: u32,
}

impl RuntimeError {
    pub fn new(error_type: RuntimeErrorType, line: u32) -> RuntimeError {
        RuntimeError { error_type, line }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.error_type {
            RuntimeErrorType::ArithmeticInvalidOperand => write!(
                f,
                "Invalid operands of arithmetic expression at line {}",
                self.line
            ),
            RuntimeErrorType::ArithmeticInvalidOperator => write!(
                f,
                "Invalid operator of arithmetic expression at line {}",
                self.line
            ),
            RuntimeErrorType::ComparisonInvalidOperator => write!(
                f,
                "Invalid operator of comparison expression at line {}",
                self.line
            ),
            RuntimeErrorType::ComparisonInvalidOperand => write!(
                f,
                "Invalid operator of comparison expression at line {}",
                self.line
            ),
            RuntimeErrorType::OperationNotSupported => {
                write!(f, "Operation is not supported at line {}", self.line)
            }
            RuntimeErrorType::ArithemticInvalidOperandAfterCast => write!(
                f,
                "Unsuccessful cast while evaluating arithmetic expression at line {}",
                self.line
            ),
            RuntimeErrorType::StatementExpected => {
                write!(f, "Statement expected at line {}", self.line)
            }
            RuntimeErrorType::StatementMissingSemicolon => {
                write!(f, "Missing semicolon after line {}", self.line)
            }
            RuntimeErrorType::VarInitializerExpected => write!(
                f,
                "Initilaizer expression is expected at line {}",
                self.line
            ),
            RuntimeErrorType::IdentifierExpedcted => {
                write!(f, "Identifier expression is expected at line {}", self.line)
            }
            RuntimeErrorType::IdentifierNotDefined(ident_name) => {
                write!(
                    f,
                    "Identifier >{ident_name}< not defined at line {}",
                    self.line
                )
            }
            RuntimeErrorType::IdentifierTokenNotSaved => {
                write!(f, "Identifier token not saved at line {}", self.line)
            }
            RuntimeErrorType::InvalidAssignmentTarget => {
                write!(f, "Invalid assignment target at line {}", self.line)
            }
            RuntimeErrorType::AccessToUninitiaizedVariable => {
                write!(f, "Reading uninitialized variable at line {}", self.line)
            }
            RuntimeErrorType::MissingWhileCondStartParenthesis => write!(
                f,
                "Missing start parenthesis before while condition at line {}",
                self.line
            ),
            RuntimeErrorType::MissingWhileCondEndParenthesis => write!(
                f,
                "Missing end parenthesis after while condition at line {}",
                self.line
            ),
            RuntimeErrorType::MissingForCondStartParenthesis => write!(
                f,
                "Missing start parenthesis before for loop condition at line {}",
                self.line
            ),
            RuntimeErrorType::MissingForCondEndParenthesis => write!(
                f,
                "Missing end parenthesis after for loop condition at line {}",
                self.line
            ),
            RuntimeErrorType::ExpressionExpected => {
                write!(f, "Expression expected at line {}", self.line)
            }
            RuntimeErrorType::UnexpextedBreakStatement => {
                write!(f, "Unexpected break sttatement at {}", self.line)
            }
            RuntimeErrorType::Unknown => write!(f, "Unknown error at line {}", self.line),
            RuntimeErrorType::ExpressionExpected => {
                write!(f, "Expression expected at line {}", self.line)
            }
            RuntimeErrorType::NotCallableExpression => {
                write!(f, "Not callable expression at line {}", self.line)
            }
            RuntimeErrorType::FunctionNameNotFound => {
                write!(f, "Function name not found at {}", self.line)
            }
            RuntimeErrorType::ArgumentCountMismatch => {
                write!(
                    f,
                    "Not enough or too much arguments supplied at line {}",
                    self.line
                )
            }
            RuntimeErrorType::Unknown => write!(f, "Unknown error at line {}", self.line),
        }
    }
}
