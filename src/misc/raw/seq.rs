// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{borrow::Cow, convert::TryFrom, fmt, marker::PhantomData};

use super::RawConst;

/// Same as `RawCons` but for a sequence of values.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawSeq<'a, T: Clone, U>(pub Cow<'a, [T]>, PhantomData<U>);

impl<'a, T: Clone, U> RawSeq<'a, T, U> {
    /// Creates a new wrapper.
    pub fn new(t: impl Into<Cow<'a, [T]>>) -> Self {
        Self(t.into(), PhantomData)
    }

    /// Returns a length of this sequence.
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a, T, U> RawSeq<'a, T, U>
where
    T: Copy,
    U: TryFrom<T>,
{
    /// Returns raw value at the given position.
    pub fn get(&self, index: usize) -> Option<RawConst<T, U>> {
        self.0.get(index).copied().map(RawConst::new)
    }
}

impl<T: fmt::Debug, U: fmt::Debug> fmt::Debug for RawSeq<'_, T, U>
where
    T: Copy,
    U: TryFrom<T>,
    U::Error: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0
            .iter()
            .copied()
            .map(RawConst::<T, U>::new)
            .collect::<Vec<_>>()
            .fmt(f)
    }
}
