use std::{error, fmt};

/// An enum used to handle math errors.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MathError {
    /// Used to handle errors when there is an attempt to divide by `0`.
    Infinity,

    /// Used to handle errors from indeterminate forms.
    IndeterminateForm,

    /// Used to handle errors when there is an unsuccessful attempt to `.unwrap()` from `.try_integer()`.
    IntegerUnwrap,

    /// Used to handle errors when there is an attempt to compute the factorial of a negative number.
    InvalidFactorial,
}

impl fmt::Display for MathError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Infinity => write!(f, "The denominator can't be 0."),
            Self::IndeterminateForm => write!(f, "The given fraction is an indeterminate form."),
            Self::IntegerUnwrap => write!(f, "The result of the division is not an integer."),
            Self::InvalidFactorial => write!(f, "If the fraction doesn't yield a positive integer, the factorial can't be computed."),
        }
    }
}

impl error::Error for MathError {}