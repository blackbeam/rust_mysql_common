// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use num_traits::{Bounded, PrimInt};

use std::{fmt, io, marker::PhantomData, mem::size_of};

use crate::{
    bitflags_ext::Bitflags,
    io::ParseBuf,
    proto::{MyDeserialize, MySerialize},
};

use super::{int::IntRepr, RawInt};

/// Wrapper for raw flags value.
///
/// Deserialization of this type won't lead to an error if value contains unknown flags.
#[derive(Clone, Default, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawFlags<T: Bitflags, U>(pub T::Repr, PhantomData<U>);

impl<T: Bitflags, U> RawFlags<T, U> {
    /// Create new flags.
    pub fn new(value: T::Repr) -> Self {
        Self(value, PhantomData)
    }

    /// Returns parsed flags. Unknown bits will be truncated.
    pub fn get(&self) -> T {
        T::from_bits_truncate(self.0)
    }
}

impl<T: fmt::Debug, U> fmt::Debug for RawFlags<T, U>
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

impl<'de, T: Bitflags, U> MyDeserialize<'de> for RawFlags<T, U>
where
    U: IntRepr<Primitive = T::Repr>,
{
    const SIZE: Option<usize> = Some(size_of::<T::Repr>());
    type Ctx = ();

    fn deserialize_((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let value = buf.parse_unchecked::<RawInt<U>>(())?;
        Ok(Self::new(*value))
    }
}

impl<T: Bitflags, U> MySerialize for RawFlags<T, U>
where
    U: IntRepr<Primitive = T::Repr>,
{
    fn serialize(&self, buf: &mut Vec<u8>) {
        RawInt::<U>::new(self.0).serialize(buf);
    }
}
