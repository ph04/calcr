use std::{error, fmt};

/// An enum used to handle math errors.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MathError {
    Infinity,
}

impl fmt::Display for MathError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MathError::Infinity => write!(f, "The denominator is 0."),
        }
    }
}

impl error::Error for MathError {}