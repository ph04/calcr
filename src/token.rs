use std::{error::Error, fmt, f32};
use crate::fraction::Fraction;

/// An enum used to handle operator tokens, used in `Token::Operators`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ops {
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Fac,
}

impl Ops {
    /// A method used to return the precedence level of
    /// any operator. The precedence levels are:
    /// - `Add` and `Sub`: level `0`
    /// - `Mul` and `Div`: level `1`
    /// - `Pow` and `Fac`: level `2`
    pub fn precedence(&self) -> u8 {
        match *self {
            Self::And | Self::Or => 0,
            Self::Add | Self::Sub => 1,
            Self::Mul | Self::Div => 2,
            Self::Pow | Self::Fac => 3,
        }
    }
}

impl fmt::Display for Ops {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Pow => write!(f, "^"),
            Self::Fac => write!(f, "!")
        }
    }
}

/// An enum to handle commands.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cmd {
    Exit,
    Help,
    Clear,
    Debug,
    Ratio,
    Unknown(String),
}

impl fmt::Display for Cmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Exit => write!(f, "exit"),
            Self::Help => write!(f, "help"),
            Self::Clear => write!(f, "clear"),
            Self::Debug => write!(f, "debug"),
            Self::Ratio => write!(f, "ratio"),
            Self::Unknown(name) => write!(f, "{}", name), // should never be printed
        }
    }
}

// pub enum VarState {
//     Known(f32),
//     Unknown,
// }

/// An enum used to handle tokens of the given input.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // Number(f32), // TODO: currently can't handle float numbers
    Number(Fraction),
    // Variable(String, Option<f32>),
    Variable(String),
    // Variable(Option<f32>),
    // Variable(VarState),
    // Variable(String),
    CommandToken,
    LeftBracket,
    RightBracket,
    Operators(Ops),
    Equal,
    Unknown(char),
    Command(Cmd),
}

/// An enum to handle syntax errors in the given input.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenError {
    UnknownToken(char),
    UnknownVariable(String),
    UnknownCommand(String),
    MisplacedBrackets,
    MissingOperands,
    MissingOperators,
    UnimplementedOperator(Ops),
    InvalidCommandSyntax(Cmd),
    InvalidBitwiseOperands,
    InvalidDefinitionSyntax,
    DivisionByZero,
    UnknownError(usize),
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownToken(c) => write!(f, "Unknown token '{}'.", c),
            Self::UnknownVariable(v) => write!(f, "Unknown variable '{}'.", v),
            Self::UnknownCommand(name) => write!(f, "Unknown command '{}'.", name),
            Self::MisplacedBrackets => write!(f, "Brackets are misplaced."),
            Self::MissingOperands => write!(f, "There are not enough operands."),
            Self::MissingOperators => write!(f, "There are not enough operators."),
            Self::UnimplementedOperator(operator) => write!(f, "'{}' is currently unimplemented.", operator),
            Self::InvalidCommandSyntax(cmd) => write!(f, "Invalid syntax for the command '{}'.", cmd),
            Self::InvalidBitwiseOperands => write!(f, "Invalid bitwise operands, integers are required."),
            Self::InvalidDefinitionSyntax => write!(f, "Invalid syntax for defining variables."),
            Self::DivisionByZero => write!(f, "Attempt to divide by 0."),
            Self::UnknownError(code) => write!(f, "An unknown error occured while evaluating the input. Please report this bug to the developer. Error code: #{}.", code),
        }
    }
}

impl Error for TokenError {}