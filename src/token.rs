use std::{error::Error, fmt};
use crate::fraction::{Fraction, consts::*};

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
    // TODO: example
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

/// An enum used to handle command tokens, used in `Token::Command`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cmd {
    Exit,
    Help,
    Clear,
    Debug,
    Ratio,
    Hex,
    Flags,
    Vars,
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
            Self::Hex => write!(f, "hex"),
            Self::Flags => write!(f, "flags"),
            Self::Vars => write!(f, "vars"),
            Self::Unknown(name) => write!(f, "{}", name), // should never be printed
        }
    }
}

/// An enum used to handle constants, used in `Token::Constant`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    Pi,
    E,
    Tau,
}

impl Const {
    pub fn value(&self) -> Fraction {
        match *self {
            Self::Pi => PI_FRACTION,
            Self::E => E_FRACTION,
            Self::Tau => TAU_FRACTION,
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Pi => write!(f, "pi"),
            Self::E => write!(f, "e"),
            Self::Tau => write!(f, "tau"),
        }
    }
}

/// An enum used to handle tokens of the given input.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Number(Fraction), // TODO: currently can't handle float numbers
    Variable(String),
    Constant(Const),
    CommandToken,
    LeftBracket,
    RightBracket,
    Operators(Ops),
    Equal,
    Ans,
    Unknown(char),
    Command(Cmd),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(frac) => write!(f, "{}", frac.float().unwrap()),
            Self::Variable(name) => write!(f, "{}", name),
            Self::Constant(c) => write!(f, "{}", c),
            Self::CommandToken => write!(f, "\\"),
            Self::LeftBracket => write!(f, "("),
            Self::RightBracket => write!(f, ")"),
            Self::Operators(op) => write!(f, "{}", op),
            Self::Equal => write!(f, "="),
            Self::Ans => write!(f, "ans"),
            Self::Unknown(c) => write!(f, "{}", c),
            Self::Command(cmd) => write!(f, "{}", cmd),
        }
    }
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
    InvalidVariableName(Token),
    EmptyBrackets,
    ConstantName(Const),
    DivisionByZero,
    IndeterminateForm,
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
            Self::InvalidVariableName(name) => write!(f, "'{}' is an invalid variable name.", name),
            Self::EmptyBrackets => write!(f, "No instructions."),
            Self::ConstantName(c) => write!(f, "'{}' is an invalid variable name, since it's a constant.", c),
            Self::DivisionByZero => write!(f, "Attempt to divide by 0."),
            Self::IndeterminateForm => write!(f, "Indeterminate form."),
            Self::UnknownError(code) => write!(f, "An unknown error occured while evaluating the input. Please report this bug to the developer. Error code: #{:04}.", code),
        }
    }
}

impl Error for TokenError {}