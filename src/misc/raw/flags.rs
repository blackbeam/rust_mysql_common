// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use num_traits::{Bounded, PrimInt};

use std::fmt;

use crate::bitflags_ext::Bitflags;

/// Wrapper for raw flags value.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawFlags<T: Bitflags>(pub T::Repr);

impl<T: Bitflags> RawFlags<T> {
    /// Create new flags.
    pub fn new(value: T::Repr) -> Self {
        Self(value)
    }

    /// Returns parsed flags. Unknown bits will be truncated.
    pub fn get(&self) -> T {
        T::from_bits_truncate(self.0)
    }
}

impl<T: fmt::Debug> fmt::Debug for RawFlags<T>
where
    T: Bitflags,
    T::Repr: fmt::Binary,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.get())?;
        let unknown_bits = self.0 & (T::Repr::max_value() ^ T::all().bits());
        if unknown_bits.count_ones() > 0 {
            write!(
                f,
                " (Unknown bits: {:0width$b})",
                unknown_bits,
                width = T::Repr::max_value().count_ones() as usize,
            )?
        }
        Ok(())
    }
}
