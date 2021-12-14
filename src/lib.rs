#![allow(clippy::eq_op, clippy::from_over_into, clippy::identity_op)]
#![no_std]
use core::{
    cmp::Ordering,
    ops::{Div, Rem},
};
use ethnum::U256;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Sign {
    Plus,
    Minus,
    NoSign,
}

const SIGN_BIT_MASK: U256 = U256::from_words(
    0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_u128,
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_u128,
);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct I256(pub Sign, pub U256);

impl I256 {
    pub fn min_value() -> Self {
        I256(Sign::Minus, (U256::MAX & SIGN_BIT_MASK) + U256::ONE)
    }

    pub fn zero() -> Self {
        I256(Sign::NoSign, U256::ZERO)
    }
}

impl Ord for I256 {
    fn cmp(&self, other: &I256) -> Ordering {
        match (self.0, other.0) {
            (Sign::NoSign, Sign::NoSign) => Ordering::Equal,
            (Sign::NoSign, Sign::Plus) => Ordering::Less,
            (Sign::NoSign, Sign::Minus) => Ordering::Greater,
            (Sign::Minus, Sign::NoSign) => Ordering::Less,
            (Sign::Minus, Sign::Plus) => Ordering::Less,
            (Sign::Minus, Sign::Minus) => self.1.cmp(&other.1).reverse(),
            (Sign::Plus, Sign::Minus) => Ordering::Greater,
            (Sign::Plus, Sign::NoSign) => Ordering::Greater,
            (Sign::Plus, Sign::Plus) => self.1.cmp(&other.1),
        }
    }
}

impl PartialOrd for I256 {
    fn partial_cmp(&self, other: &I256) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Default for I256 {
    fn default() -> I256 {
        I256::zero()
    }
}
impl From<U256> for I256 {
    fn from(val: U256) -> I256 {
        if val == U256::ZERO {
            I256::zero()
        } else if val & SIGN_BIT_MASK == val {
            I256(Sign::Plus, val)
        } else {
            I256(Sign::Minus, !val + U256::ONE)
        }
    }
}
impl Into<U256> for I256 {
    fn into(self) -> U256 {
        let sign = self.0;
        if sign == Sign::NoSign {
            U256::ZERO
        } else if sign == Sign::Plus {
            self.1
        } else {
            !self.1 + U256::from(1u64)
        }
    }
}

impl Div for I256 {
    type Output = I256;

    fn div(self, other: I256) -> I256 {
        if other == I256::zero() {
            return I256::zero();
        }

        if self == I256::min_value() && other.1 == U256::ONE {
            return I256::min_value();
        }

        let d = (self.1 / other.1) & SIGN_BIT_MASK;

        if d == U256::ZERO {
            return I256::zero();
        }

        match (self.0, other.0) {
            (Sign::NoSign, Sign::Plus)
            | (Sign::Plus, Sign::NoSign)
            | (Sign::NoSign, Sign::NoSign)
            | (Sign::Plus, Sign::Plus)
            | (Sign::Minus, Sign::Minus) => I256(Sign::Plus, d),
            (Sign::NoSign, Sign::Minus)
            | (Sign::Plus, Sign::Minus)
            | (Sign::Minus, Sign::NoSign)
            | (Sign::Minus, Sign::Plus) => I256(Sign::Minus, d),
        }
    }
}

impl Rem for I256 {
    type Output = I256;

    fn rem(self, other: I256) -> I256 {
        let r = (self.1 % other.1) & SIGN_BIT_MASK;

        if r == U256::ZERO {
            return I256::zero();
        }

        I256(self.0, r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::num::Wrapping;

    #[test]
    fn div_i256() {
        // Sanity checks based on i8. Notice that we need to use `Wrapping` here because
        // Rust will prevent the overflow by default whereas the EVM does not.
        assert_eq!(Wrapping(i8::MIN) / Wrapping(-1), Wrapping(i8::MIN));
        assert_eq!(i8::MIN / 1, i8::MIN);
        assert_eq!(i8::MAX / 1, i8::MAX);
        assert_eq!(i8::MAX / -1, -i8::MAX);

        assert_eq!(100i8 / -1, -100i8);
        assert_eq!(100i8 / 2, 50i8);

        // Now the same calculations based on i256
        let one = I256(Sign::NoSign, U256::new(1));
        let one_hundred = I256(Sign::NoSign, U256::new(100));
        let fifty = I256(Sign::Plus, U256::new(50));
        let two = I256(Sign::NoSign, U256::new(2));
        let neg_one_hundred = I256(Sign::Minus, U256::new(100));
        let minus_one = I256(Sign::Minus, U256::new(1));
        let max_value = I256(Sign::Plus, U256::new(2).pow(255) - 1);
        let neg_max_value = I256(Sign::Minus, U256::new(2).pow(255) - 1);

        assert_eq!(I256::min_value() / minus_one, I256::min_value());
        assert_eq!(I256::min_value() / one, I256::min_value());
        assert_eq!(max_value / one, max_value);
        assert_eq!(max_value / minus_one, neg_max_value);

        assert_eq!(one_hundred / minus_one, neg_one_hundred);
        assert_eq!(one_hundred / two, fifty);
    }
}
