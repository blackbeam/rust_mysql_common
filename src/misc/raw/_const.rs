// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{convert::TryFrom, fmt, marker::PhantomData};

/// Wrapper for a raw value of a MySql constant or enum variant.
///
/// * `T` – specifies the raw value,
/// * `U` – specifies the parsed value.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawConst<T, U>(pub T, PhantomData<U>);

impl<T, U> RawConst<T, U> {
    /// Creates a new wrapper.
    pub fn new(t: T) -> Self {
        Self(t, PhantomData)
    }
}

impl<T, U> RawConst<T, U>
where
    T: Copy,
    U: TryFrom<T>,
{
    /// Tries to parse the raw value as `U`.
    pub fn get(&self) -> Result<U, U::Error> {
        U::try_from(self.0)
    }
}

impl<T: fmt::Debug, U: fmt::Debug> fmt::Debug for RawConst<T, U>
where
    T: Copy,
    U: TryFrom<T>,
    U::Error: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match U::try_from(self.0) {
            Ok(u) => u.fmt(f),
            Err(t) => write!(
                f,
                "Unknown value for type {}: {:?}",
                std::any::type_name::<U>(),
                t
            ),
        }
    }
}
