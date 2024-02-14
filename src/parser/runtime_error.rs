use core::fmt;

pub enum RuntimeErrorType {
    OperationNotSupported,
    ArithmeticInvalidOperand,
    ArithmeticInvalidOperator,
    ArithemticInvalidOperandAfterCast,
}

pub struct RuntimeError {
    error_type: RuntimeErrorType,
    line: u32,
}

impl RuntimeError {
    pub fn new(error_type: RuntimeErrorType, line: u32) -> RuntimeError {
        RuntimeError { error_type, line }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error_type {
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
            RuntimeErrorType::OperationNotSupported => {
                write!(f, "Operation is not supported at line {}", self.line)
            }
            RuntimeErrorType::ArithemticInvalidOperandAfterCast => write!(
                f,
                "Unsuccessful cast while evaluating arithmetic expression at line {}",
                self.line
            ),
        }
    }
}
