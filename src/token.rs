use std::{error::Error, fmt};
use crate::fraction::{Fraction, consts::*};

/// An enum used to handle operator tokens, used in `Token::Operators`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ops {
    /// Used to handle the `&` token.
    And,
    
    /// Used to handle the `|` token.
    Or,
    
    /// Used to handle the `+` token.
    Add,

    /// Used to handle the `-` token.
    Sub,

    /// Used to handle the `*` token.
    Mul,

    /// Used to handle the `/` token.
    Div,

    /// Used to handle the `^` token.
    Pow,

    /// Used to handle the `!` token.
    Fac,
}

impl Ops {
    /// A method used to return the precedence level of
    /// any operator. The precedence levels are:
    /// - `And` and `Or`: level `0`
    /// - `Add` and `Sub`: level `1`
    /// - `Mul` and `Div`: level `2`
    /// - `Pow` and `Fac`: level `3`
    ///
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::token::{Token, Ops};
    /// assert_eq!(Ops::Sub.precedence(), 1);
    /// ```
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
/// `\debug` and `\ratio` can be toggled from the command line.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cmd {
    /// Used to handle the `\exit` token.
    Exit,

    /// Used to handle the `\help` token.
    Help,

    /// Used to handle the `\clear` token.
    Clear,

    /// Used to handle the `\debug` token, and can be toggled from the command line by passing `--debug`.
    Debug,

    /// Used to handle the `\ratio` token, and can be toggled from the command line by passing `--ratio`.
    Ratio,

    /// Used to handle the `\hex` token, and it can't be toggled from the command line because it can conflict with the hex flag.
    Hex,

    /// Used to handle the `\flags` token.
    Flags,

    /// Used to handle the `\vars` token.
    Vars,

    /// Used to handle the `\remove` token.
    Remove,

    /// Used to handle unknown commands.
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
            Self::Remove => write!(f, "remove"),
            Self::Unknown(name) => write!(f, "{}", name),
        }
    }
}

/// An enum used to handle constants, used in `Token::Constant`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    /// Used to handle the `pi` token.
    Pi,

    /// Used to handle the `e` token.
    E,

    /// Used to handle the `tau` token.
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
    /// Used to handle numbers.
    Number(Fraction), // TODO: currently can't handle float numbers

    /// Used to handle variables.
    Variable(String),

    /// Used to handle constants.
    Constant(Const),

    /// Used to handle command.
    Command(Cmd),

    /// Used to handle the `\` token.
    CommandToken,

    /// Used to handle the `(` token.
    LeftBracket,

    /// Used to handle the `)` token.
    RightBracket,

    /// Used to handle `&`, `|`, `+`, `-`, `*`, `/`, `^` and `!` tokens.
    Operators(Ops),

    /// Used to handle the `=` token.
    Equal,

    /// Used to handle the ` ` token, but only in special cases.
    Space,

    /// Used to handle the `ans` token.
    Ans,

    /// Used to handle unknown tokens.
    Unknown(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(frac) => write!(f, "{}", frac.float().unwrap()),
            Self::Variable(name) => write!(f, "{}", name),
            Self::Command(cmd) => write!(f, "{}", cmd),
            Self::Constant(c) => write!(f, "{}", c),
            Self::CommandToken => write!(f, "\\"),
            Self::LeftBracket => write!(f, "("),
            Self::RightBracket => write!(f, ")"),
            Self::Operators(op) => write!(f, "{}", op),
            Self::Equal => write!(f, "="),
            Self::Space => write!(f, " "),
            Self::Ans => write!(f, "ans"),
            Self::Unknown(c) => write!(f, "{}", c),
        }
    }
}

/// An enum to handle syntax errors in the given input.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenError {
    /// Used to handle unknown token errors.
    UnknownToken(char),

    /// Used to handle unknown variable errors.
    UnknownVariable(String),

    /// Used to handle unknown command errors.
    UnknownCommand(String),

    /// Used to handle errors when bracket are misplaced.
    MisplacedBrackets,

    /// Used to handle errors when operands are missing.
    MissingOperands,

    /// Used to handle errors when operators are missing.
    MissingOperators,

    /// Used to handles errors when there is a `\` without any command
    MissingCommand,

    /// Used to handle unimplemented operator errors.
    UnimplementedOperator(Ops),

    /// Used to handle errors when invalid syntax for commands is used.
    InvalidCommandSyntax(Cmd),

    /// Used to handle errors when spacing is not allowed.
    WrongSpacing,

    /// Used to handle invalid bitwise operands errors.
    InvalidBitwiseOperands,

    /// Used to handle errors when invalid syntax when defining variables is used.
    InvalidDefinitionSyntax,

    /// Used to handle errors when using invalid names for variables.
    InvalidVariableName(Token),

    /// Used to handle errors when the name of the variable is already taken by a constant.
    ConstantName(Const),

    /// Used when the input is `()`.
    EmptyBrackets,

    /// Used to handle errors when the user performs an attempt to divide by `0`.
    DivisionByZero,

    /// Used to handle errors indeterminate forms.
    IndeterminateForm,

    /// Used to handle errors when a command is called without enough arguments.
    NotEnoughArguments,

    /// Used to handle unknown errors.
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
            Self::MissingCommand => write!(f, "There are no known commands."),
            Self::UnimplementedOperator(operator) => write!(f, "'{}' is currently unimplemented.", operator),
            Self::InvalidCommandSyntax(cmd) => write!(f, "Invalid syntax for the command '{}'.", cmd),
            Self::WrongSpacing => write!(f, "Spaces are not allowed in this context."),
            Self::InvalidBitwiseOperands => write!(f, "Invalid bitwise operands, integers are required."),
            Self::InvalidDefinitionSyntax => write!(f, "Invalid syntax for defining variables."),
            Self::InvalidVariableName(name) => write!(f, "'{}' is an invalid variable name.", name),
            Self::ConstantName(c) => write!(f, "'{}' is an invalid variable name, since it's a constant.", c),
            Self::EmptyBrackets => write!(f, "No instructions."),
            Self::DivisionByZero => write!(f, "Attempt to divide by 0."),
            Self::IndeterminateForm => write!(f, "Indeterminate form."),
            Self::NotEnoughArguments => write!(f, "Not enough arguments passed."),
            Self::UnknownError(code) => write!(f, "An unknown error occured while evaluating the input. Please report this bug to the developer. Error code: #{:04}.", code),
        }
    }
}

impl Error for TokenError {}