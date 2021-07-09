#![allow(clippy::suspicious_arithmetic_impl)]

use crate::matherror::MathError;
use std::{convert::TryInto, f32, fmt, num::NonZeroUsize, ops};

pub mod consts {
    use super::*;

    /// A const used to handle `0`, and the fraction is `(0 / 1)`.
    pub const ZERO_FRACTION: Fraction = Fraction {
        numerator: 0,
        denominator: unsafe { NonZeroUsize::new_unchecked(1) },
    };

    /// A const used to handle `PI`, and the fraction is `(245850922 / 78256779)`.
    pub const PI_FRACTION: Fraction = Fraction {
        numerator: 245850922,
        denominator: unsafe { NonZeroUsize::new_unchecked(78256779) }
    };

    /// A const used to handle `E`, and the fraction is `(268876667 / 98914198)`.
    pub const E_FRACTION: Fraction = Fraction {
        numerator: 268876667,
        denominator: unsafe { NonZeroUsize::new_unchecked(98914198) }
    };

    /// A const used to handle `TAU`, and the fraction is `(411557987 / 65501488)`.
    pub const TAU_FRACTION: Fraction = Fraction {
        numerator: 411557987,
        denominator: unsafe { NonZeroUsize::new_unchecked(65501488) }
    };
}

use self::consts::*;

/// A struct used to handle fractions. It can perform additions,
/// subtractions, multiplications, divisions and exponentials
/// on every valid given fraction. The denominator is using a
/// `NonZeroUsize`, to make sure that is valid.
/// 
/// # Examples
/// 
/// ```
/// # pub use calcr::fraction::Fraction;
/// # pub use calcr::matherror::MathError;
/// let fraction1 = Fraction::new(-3, 4).unwrap(); // (-3 / 4)
/// 
/// let fraction2 = Fraction::new(5, 6).unwrap(); // (5 / 6)
/// 
/// assert_eq!(fraction1 + fraction2, Fraction::new(1, 12).unwrap()); // (-3 / 4) + (5 / 6) = (1 / 12)
/// 
/// // the denominator of a fraction can't be zero!
/// assert_eq!(Fraction::new(5, 0), Err(MathError::Infinity)) // can't divide by zero!
/// ```
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Fraction {
    numerator: isize,
    denominator: NonZeroUsize,
}

impl Fraction {
    /// Returns the reciprocal of a fraction, if the numerator is not 0.
    fn reciprocal(&mut self) -> Result<Self, MathError> {
        let sign: isize = if self.numerator > 0 {
            1
        } else {
            -1
        };

        let temp = self.denominator.get() as isize;

        self.denominator = match NonZeroUsize::new(self.numerator.abs() as usize) {
            Some(d) => d,
            None => return Err(MathError::Infinity),
        };

        self.numerator = temp * sign;

        Ok(*self)
    }

    /// Returns the GCD from the two input numbers with the Euclidean algorithm.
    fn gcd(mut x: usize, mut y: usize) -> usize {
        // if the numerator is 0, the GCD would be 0 as well,
        // and there would be a division by zero,
        // so the denominator is returned and the fraction will always be (0 / 1).
        if x == 0 {
            return y;
        }

        while y != 0 {
            let t = y;

            y = x % y;
            x = t;
        }

        x
    }

    /// Reduces the fraction to lowest terms.
    fn simplify(&mut self) {
        let denominator = self.denominator.get();

        let gcd = Self::gcd(self.numerator.abs() as usize, denominator);

        self.numerator /= gcd as isize;

        // since the GCD is always less or equal than the denominator,
        // and since the denominator is more than 0 (if checked),
        // it's not needed to unwrap the Option<NonZeroUsize>.
        unsafe { self.denominator = NonZeroUsize::new_unchecked(denominator / gcd) }
    }

    /// Returns a `Fraction`, and checks if the fields are valid.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use calcr::matherror::MathError;
    /// let fraction1 = Fraction::new(-1, 2); // (-1 / 2)
    /// 
    /// let fraction2 = Fraction::new(3, 0); // (3 / 0)
    /// 
    /// assert_eq!(fraction2, Err(MathError::Infinity)); // can't divide by zero!
    /// ```
    pub fn new(numerator: isize, denominator: usize) -> Result<Self, MathError> {
        match (numerator, NonZeroUsize::new(denominator)) {
            (_, None) => Err(MathError::Infinity),
            (n, Some(d)) => {
                let mut fraction = Self {
                    numerator: n,
                    denominator: d,
                };
                
                fraction.simplify();

                Ok(fraction)
            },
        }
    }

    /// # Safety
    /// 
    /// Returns a `Fraction` without checking if the denominator is not 0;
    /// if it's not the case, the function can cause undefined behaviour.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use calcr::matherror::MathError;
    /// unsafe {
    ///     let fraction1_unsafe = Fraction::new_unchecked(-1, 2); // (-1 / 2)
    /// 
    ///     let fraction2_unsafe = Fraction::new_unchecked(3, 0); // (3 / 0)
    /// }
    /// ```
    /// 
    /// # Panics
    /// 
    /// If the denominator is `0`, the program could panic.
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// unsafe {
    ///     let invalid_fraction = Fraction::new_unchecked(4, 0); // (4 / 0)
    ///     
    ///     // the above expression could make the program panic!
    /// }
    /// ```
    pub unsafe fn new_unchecked(numerator: isize, denominator: usize) -> Self {
        let mut fraction = Self {
            numerator,
            denominator: NonZeroUsize::new_unchecked(denominator),
        };

        fraction.simplify();
        
        fraction
    }

    /// Returs the `Fraction` of the given float number,
    /// performing a binary search with the farey sequence.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// let fraction = Fraction::from(3.692); // 3.692
    /// 
    /// assert_eq!(fraction, Fraction::new(923, 250).unwrap()) // 923 / 250 = 3.692
    /// ```
    pub fn from(number: f32) -> Self {
        // if the number is 0.0, the algorithm loops infinitely
        if number == 0.0 {
            return consts::ZERO_FRACTION
        }

        let error_margin = 0.001;

        let mut left = (0, 1);
        let mut right = (1, 0);

        loop {
            let mediant = (left.0 + right.0, left.1 + right.1);

            let mediant_num = mediant.0 as f32;
            let mediant_den = mediant.1 as f32;

            let product = number.abs() * mediant_den;

            if product > mediant_num {
                left = mediant;
            } else if (product - mediant_num).abs() < error_margin { // clippy
                return unsafe {
                    Self::new_unchecked(if number.is_sign_positive() {
                        mediant.0
                    } else {
                        -mediant.0
                    }, mediant.1)
                }
            } else {
                right = mediant;
            }
        }
    }

    /// If the denominator is valid, returns a `f32` with
    /// the value of the division of the fraction.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use calcr::matherror::MathError;
    /// let fraction1 = Fraction::new(1, 4).unwrap(); // (1 / 4)
    /// 
    /// assert_eq!(fraction1.float(), Ok(0.25)); // 1 / 4 = 0.25
    /// 
    /// unsafe {
    ///     let fraction2 = Fraction::new_unchecked(-6, 0); // (-6 / 0)
    ///     
    ///     assert_eq!(fraction2.float(), Err(MathError::Infinity)); // can't divide by zero!
    /// }
    /// ```
    pub fn float(&self) -> Result<f32, MathError> {
        match self.denominator.get() {
            0 => Err(MathError::Infinity),
            _ => Ok(self.numerator as f32 / self.denominator.get() as f32),
        }
    }

    /// # Safety
    /// 
    /// Returns a `f32` with the value of the division of the
    /// fraction, without checking if the denominator is valid.
    /// 
    /// # Examples
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use calcr::matherror::MathError;
    /// unsafe {
    ///     let fraction1 = Fraction::new_unchecked(-2, 5); // (-2 / 5)
    /// 
    ///     assert_eq!(fraction1.float_unchecked(), -0.4) // -2 / 5 = -0.4
    /// }
    /// ```
    /// 
    /// # Panics
    /// 
    /// If the denominator is `0`, the program could panic.
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use std::num::NonZeroUsize;
    /// unsafe {
    ///     let invalid_fraction = Fraction::new_unchecked(9, 0); // (9 / 0) is invalid!
    /// 
    ///     let invalid_float = invalid_fraction.float_unchecked(); // can't divide by zero!
    /// 
    ///     // the above expression could make the program panic!
    /// }
    /// ```
    pub unsafe fn float_unchecked(&self) -> f32 {
        self.numerator as f32 / self.denominator.get() as f32
    }

    /// If the division of the fraction is an integer,
    /// it returns the result of the division as an `isize`.
    ///
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use calcr::matherror::MathError;
    /// let fraction1 = Fraction::new(10, 5).unwrap(); // (10 / 5) = (2 / 1)
    /// 
    /// assert_eq!(fraction1.try_integer(), Ok(2)); // 2 / 1 = 2
    /// 
    /// let fraction2 = Fraction::new(3, 5).unwrap(); // (3 / 5)
    ///
    /// assert_eq!(fraction2.try_integer(), Err(MathError::IntegerUnwrap)); // 3 / 5 = 0.6
    /// ```
    pub fn try_integer(&self) -> Result<isize, MathError> {
        // this check is enough since
        // whenever a `Fraction` is created
        // it gets reduced to lowest terms
        if self.denominator.get() == 1 {
            Ok(self.numerator)
        } else {
            Err(MathError::IntegerUnwrap)
        }
    }

    // TODO: fix this
    /// Returns the given fraction to the power of `exp`, which is a `u32`.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// let fraction = Fraction::new(2, 3).unwrap(); // (2 / 3)
    /// 
    /// assert_eq!(fraction.pow(3), Fraction::new(8, 27).unwrap()); // (2 / 3) ^ 3 = (8 / 27)
    /// ```
    pub fn pow(&self, exp: u32) -> Self {
        Self {
            numerator: self.numerator.pow(exp),
            denominator: NonZeroUsize::new(
                self.denominator.get().pow(exp)
            ).unwrap(),
        }
    }

    /// Returns the given fraction to the power of `exp`, which is a `f32`.
    /// Returns `Err(MathError::Indeterminate)` if the given fractions
    /// are both `ZERO_FRACTION`.
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::fraction::{Fraction, consts::ZERO_FRACTION};
    /// # pub use calcr::matherror::MathError;
    /// let fraction1 = Fraction::new(3, 5).unwrap(); // (3 / 5)
    /// let fraction2 = Fraction::new(4, 3).unwrap(); // (4 / 3)
    /// 
    /// assert_eq!(fraction1.powf(fraction2), Ok(Fraction::new(167, 330).unwrap())); // (3 / 5) ^ (4 / 3) = (167 / 330)
    /// 
    /// assert_eq!(ZERO_FRACTION.powf(ZERO_FRACTION), Err(MathError::IndeterminateForm)); // 0 ^ 0 is an indeterminate form
    /// ```
    pub fn powf(&self, exp: Self) -> Result<Self, MathError> {
        match (*self, exp) {
            (ZERO_FRACTION, ZERO_FRACTION) => Err(MathError::IndeterminateForm),
            _ => {
                Ok(Self::from(
                    self
                        .float()
                        .unwrap()
                        .powf(exp.float().unwrap())
                ))
            },
        }
    }

    /// Calculates recursively the factorial of the given `usize`.
    fn factorial_internal(n: usize) -> usize {
        match n {
            0 | 1 => 1,
            _ => n * Self::factorial_internal(n - 1),
        }
    }

    /// If the given `Fraction` yields a positive integer,
    /// returns its factorial as a `Fraction`.
    /// 
    /// # Examples
    /// ```
    /// # pub use calcr::fraction::Fraction;
    /// # pub use calcr::matherror::MathError;
    /// let fraction1 = Fraction::from(4.0); // 4.0 = (4 / 1)
    /// 
    /// assert_eq!(fraction1.factorial().unwrap().try_integer().unwrap(), 24); // 4! = 24
    /// 
    /// let fraction2 = Fraction::from(0.5); // 0.5 = (1 / 2)
    ///
    /// assert_eq!(fraction2.factorial(), Err(MathError::InvalidFactorial)); // can't compute 0.5!
    /// 
    /// let fraction3 = Fraction::from(-5.0); // -5.0 = (-5 / 1)
    /// 
    /// assert_eq!(fraction3.factorial(), Err(MathError::InvalidFactorial)); // can't compute -5!
    /// ```
    pub fn factorial(&self) -> Result<Self, MathError> {
        if let Ok(integer) = self.try_integer() {
            if !integer.is_negative()  {
                let factorial = Self::factorial_internal(integer.try_into().unwrap()); // this will never panic

                Ok(Fraction::from(factorial as f32))
            } else {
                Err(MathError::InvalidFactorial)
            }
        } else {
            Err(MathError::InvalidFactorial)
        }
    }
}

impl ops::Add for Fraction {
    type Output = Fraction;

    fn add(self, other: Fraction) -> Fraction {
        let lhs_denominator = self.denominator.get();
        let rhs_denominator = other.denominator.get();

        let gcd = Fraction::gcd(lhs_denominator, rhs_denominator);

        let denominator = lhs_denominator * rhs_denominator / gcd;

        let lhs_numerator = (rhs_denominator / gcd) as isize * self.numerator;
        let rhs_numerator = (lhs_denominator / gcd) as isize * other.numerator;

        unsafe {
            Fraction::new_unchecked(lhs_numerator + rhs_numerator, denominator)
        }
    }
}

impl ops::Sub for Fraction {
    type Output = Fraction;

    fn sub(self, other: Fraction) -> Fraction {
        self + (-other)
    }
}

impl ops::Mul for Fraction {
    type Output = Fraction;

    fn mul(self, other: Fraction) -> Fraction {
        let numerator = self.numerator * other.numerator;

        let denominator = self.denominator.get() * other.denominator.get();

        unsafe {
            Fraction::new_unchecked(numerator, denominator)
        }
    }
}

/// The implementations of the `Div` trait for `Fraction`
/// returns a `Result<Fraction, MathError>` to avoid
/// panics on `engine.rs` while the calculator is running,
/// which would result in making the calculator stop abruptly.
impl ops::Div for Fraction {
    type Output = Result<Fraction, MathError>;

    fn div(self, other: Fraction) -> Self::Output {
        match other.clone().reciprocal() {
            Ok(reciprocal) => Ok(self * reciprocal),
            Err(math_error) => Err(math_error),
        }
    }
}

impl ops::Neg for Fraction {
    type Output = Fraction;
    
    fn neg(self) -> Fraction {
        Fraction {
            numerator: -self.numerator,
            denominator: self.denominator,
        }
    }
}

impl fmt::Display for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} / {})", self.numerator, self.denominator.get())
    }
}

impl Default for Fraction {
    fn default() -> Fraction {
        consts::ZERO_FRACTION
    }
}