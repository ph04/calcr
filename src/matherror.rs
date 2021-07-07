use std::{error, fmt};

/// An enum used to handle math errors.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MathError {
    Infinity,
    IntegerUnwrap,
}

impl fmt::Display for MathError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Infinity => write!(f, "The denominator can't be 0."),
            Self::IntegerUnwrap => write!(f, "The result of the division is not an integer."),
        }
    }
}

impl error::Error for MathError {}