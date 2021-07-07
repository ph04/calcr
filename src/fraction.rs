#![allow(clippy::suspicious_arithmetic_impl)]

// TODO: CHECK COMMENTS AND DOCS
use crate::matherror::MathError;

use std::{f32, ops, fmt, num::NonZeroUsize};

pub mod consts {
    use super::*;

    pub const ZERO_FRACTION: Fraction = Fraction {
        numerator: 0,
        denominator: unsafe { NonZeroUsize::new_unchecked(1) },
    };

    // FIXME: CHANGE THESE WITH REAL VALUES
    pub const PI_FRACTION: Fraction = Fraction {
        numerator: 3,
        denominator: unsafe { NonZeroUsize::new_unchecked(1) }
    };

    pub const E_FRACTION: Fraction = Fraction {
        numerator: 3,
        denominator: unsafe { NonZeroUsize::new_unchecked(1) }
    };

    pub const TAU_FRACTION: Fraction = Fraction {
        numerator: 3,
        denominator: unsafe { NonZeroUsize::new_unchecked(1) }
    };
}

/// A struct used to handle rational numbers.
// TODO: examples
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
        // and since the denominator is more than 0,
        // it's not needed to unwrap the Option<NonZeroUsize>.
        unsafe { self.denominator = NonZeroUsize::new_unchecked(denominator / gcd) }
    }

    /// Creates an instance of a `Fraction`, and checks if the fields are valid.
    /// 
    /// Returns a `Result` with `Ok(Fraction)` if it's valid, otherwise returns `Err(MathError)`
    /// from the defined enum `MathError`.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use fraction::rational::Fraction;
    /// # pub use fraction::matherror::MathError;
    /// let fraction1 = Fraction::new(-1, 2); // (-1 / 2)
    /// 
    /// let fraction2 = Fraction::new(3, 0); // (3 / 0)
    /// 
    /// assert_eq!(fraction2, Err(MathError::Infinity));
    /// 
    /// let fraction3 = Fraction::new(0, 0); // (0 / 0)
    /// 
    /// assert_eq!(fraction3, Err(MathError::Indeterminate));
    /// ```
    pub fn new(numerator: isize, denominator: usize) -> Result<Self, MathError> {
        match (numerator, NonZeroUsize::new(denominator)) {
            // (0, None) => Err(MathError::Indeterminate),
            (_, None) => Err(MathError::Infinity),
            (n, Some(d)) => {
                let mut fraction = Fraction {
                    numerator: n,
                    denominator: d,
                };
                
                fraction.simplify();

                Ok(fraction)
            },
        }
    }

    pub fn float(&self) -> Result<f32, MathError> {
        match self.denominator.get() {
            0 => Err(MathError::Infinity),
            _ => Ok(self.numerator as f32 / self.denominator.get() as f32),
        }
    }

    // TODO: docs and examples
    pub unsafe fn float_unchecked(&self) -> f32 {
        self.numerator as f32 / self.denominator.get() as f32
    }

    /// # Safety
    /// Creates an instance of a `Fraction` without checking whether the denominator is 0.
    /// 
    /// If it's not the case, the function causes undefined behaviour.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use fraction::rational::Fraction;
    /// # pub use fraction::matherror::MathError;
    /// unsafe {
    ///     let fraction1_unsafe = Fraction::new_unchecked(-1, 2); // (-1 / 2)
    /// 
    ///     let fraction2_unsafe = Fraction::new_unchecked(3, 0); // (3 / 0)
    /// }
    /// ```
    /// 
    /// # Panics
    /// 
    /// The program panics because there would be divisions by 0.
    /// 
    /// ```should_panic
    /// # pub use fraction::rational::Fraction;
    /// unsafe {
    ///     let fraction_unsafe = Fraction::new_unchecked(0, 0); // (0 / 0)
    ///     
    ///     // The above expression will make the program panic!
    /// }
    /// ```
    pub unsafe fn new_unchecked(numerator: isize, denominator: usize) -> Self {
        let mut fraction = Fraction {
            numerator,
            denominator: NonZeroUsize::new_unchecked(denominator),
        };

        fraction.simplify();
        
        fraction
    }

    /// Returns the fraction to the power of `exp`.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use fraction::rational::Fraction;
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

    // TODO: docs and examples
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

impl ops::AddAssign for Fraction {
    fn add_assign(&mut self, other: Fraction) {
        let result = *self + other;

        *self = Fraction {
            numerator: result.numerator,
            denominator: result.denominator,
        }
    } 
}

impl ops::SubAssign for Fraction {
    fn sub_assign(&mut self, other: Fraction) {
        let result = *self - other;

        *self = Fraction {
            numerator: result.numerator,
            denominator: result.denominator,
        }
    }
}

impl ops::MulAssign for Fraction {
    fn mul_assign(&mut self, other: Fraction) {
        let result = *self * other;

        *self = Fraction {
            numerator: result.numerator,
            denominator: result.denominator,
        }
    } 
}

// impl ops::DivAssign for Fraction {
//     fn div_assign(&mut self, other: Fraction) {
//         let result = *self / other;

//         *self = Fraction {
//             numerator: result.numerator,
//             denominator: result.denominator,
//         }
//     }
// }

impl fmt::Display for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} / {})", self.numerator, self.denominator.get())
    }
}