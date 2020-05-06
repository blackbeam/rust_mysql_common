// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use num_traits::{Bounded, PrimInt};
use saturating::Saturating as S;

use std::{
    borrow::Cow,
    cmp::min,
    convert::TryFrom,
    fmt,
    io::{self, Read, Write},
    marker::PhantomData,
};

use crate::bitflags_ext::Bitflags;

/// Returns length of length-encoded-integer representation of `x`.
pub fn lenenc_int_len(x: u64) -> u64 {
    if x < 251 {
        1
    } else if x < 65_536 {
        3
    } else if x < 16_777_216 {
        4
    } else {
        9
    }
}

/// Returns length of lenght-encoded-string representation of `s`.
pub fn lenenc_str_len(s: &[u8]) -> u64 {
    let len = s.len() as u64;
    lenenc_int_len(len) + len
}

pub(crate) fn unexpected_buf_eof() -> io::Error {
    io::Error::new(
        io::ErrorKind::UnexpectedEof,
        "can't parse: buf doesn't have enough data",
    )
}

/// Splits server 'version' string into three numeric pieces.
///
/// It'll return `(0, 0, 0)` in case of error.
pub fn split_version<T: AsRef<[u8]>>(version_str: T) -> (u8, u8, u8) {
    let bytes = version_str.as_ref();
    let mut offset = 0;
    let mut nums = [0_u8; 3];
    for i in 0..=2 {
        match lexical::parse_partial::<u8, _>(&bytes[offset..]) {
            Ok((x, count))
                if count > 0
                    && (i != 0
                        || (bytes.len() > offset + count && bytes[offset + count] == b'.')) =>
            {
                offset += count + 1;
                nums[i] = x;
            }
            _ => {
                nums = [0_u8; 3];
                break;
            }
        }
    }

    (nums[0], nums[1], nums[2])
}

pub(crate) struct LimitedRead<T> {
    limit: S<usize>,
    read: T,
}

impl<T> LimitedRead<T> {
    pub fn new(read: T, limit: S<usize>) -> Self {
        Self { read, limit }
    }

    pub fn get_limit(&self) -> usize {
        self.limit.0
    }
}

impl<T: Read> Read for LimitedRead<T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let limit = min(buf.len(), self.limit.0);
        let count = self.read.read(&mut buf[..limit])?;
        self.limit -= S(count);
        Ok(count)
    }
}

pub(crate) trait LimitRead: Read + Sized {
    fn limit(&mut self, limit: S<usize>) -> LimitedRead<&mut Self> {
        LimitedRead::new(self, limit)
    }
}

impl<T: Read> LimitRead for T {}

pub(crate) struct LimitedWrite<T> {
    limit: S<usize>,
    write: T,
}

impl<T> LimitedWrite<T> {
    pub fn new(write: T, limit: S<usize>) -> Self {
        Self { write, limit }
    }

    pub fn get_limit(&self) -> usize {
        self.limit.0
    }
}

impl<T: Write> Write for LimitedWrite<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let limit = min(buf.len(), self.limit.0);
        let count = self.write.write(&buf[..limit])?;
        self.limit -= S(count);
        Ok(count)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.write.flush()
    }
}

pub(crate) trait LimitWrite: Write + Sized {
    fn limit(&mut self, limit: S<usize>) -> LimitedWrite<&mut Self> {
        LimitedWrite::new(self, limit)
    }
}

impl<T: Write> LimitWrite for T {}

/// Wrapper for a raw value of a particular type.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawField<T, E, V>(pub T, PhantomData<(E, V)>);

impl<T: Copy, U: Into<T>, V: TryFrom<T, Error = U>> RawField<T, U, V> {
    /// Creates a new wrapper.
    pub fn new(t: T) -> Self {
        Self(t, PhantomData)
    }

    /// Returns either parsed value of this field, or raw value in case of an error.
    pub fn get(&self) -> Result<V, U> {
        V::try_from(self.0)
    }
}

impl<T: fmt::Debug, U: fmt::Debug, V: fmt::Debug> fmt::Debug for RawField<T, U, V>
where
    T: Copy,
    U: Into<T>,
    V: TryFrom<T, Error = U>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match V::try_from(self.0) {
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

/// Wrapper for a sequence of values of a particular type.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawSeq<T, U, V>(pub Vec<T>, PhantomData<(U, V)>);

impl<T: Copy, U: Into<T>, V: TryFrom<T, Error = U>> RawSeq<T, U, V> {
    /// Creates a new wrapper.
    pub fn new(t: Vec<T>) -> Self {
        Self(t, PhantomData)
    }

    /// Returns either parsed value at the given index, or raw value in case of an error.
    pub fn get(&self, index: usize) -> Option<Result<V, U>> {
        self.0.get(index).copied().map(V::try_from)
    }

    /// Returns a length of this sequence.
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T: fmt::Debug, U: fmt::Debug, V: fmt::Debug> fmt::Debug for RawSeq<T, U, V>
where
    T: Copy,
    U: Into<T>,
    V: TryFrom<T, Error = U>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0
            .iter()
            .copied()
            .map(RawField::<T, U, V>::new)
            .collect::<Vec<_>>()
            .fmt(f)
    }
}

/// Wrapper for raw flags value.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct RawFlags<T: Bitflags>(pub T::Repr);

impl<T: Bitflags> RawFlags<T> {
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

/// Wrapper for raw text value.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct RawText<T = Vec<u8>>(pub T);

impl<T: AsRef<[u8]>> RawText<T> {
    /// Returns either parsed value of this field, or raw value in case of an error.
    pub fn get(&self) -> Cow<str> {
        let slice = self.0.as_ref();
        match slice.iter().position(|c| *c == 0) {
            Some(position) => String::from_utf8_lossy(&slice[..position]),
            None => String::from_utf8_lossy(slice),
        }
    }
}

impl<T: AsRef<[u8]>> fmt::Debug for RawText<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.get().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_split_version() {
        assert_eq!((1, 2, 3), split_version("1.2.3"));
        assert_eq!((10, 20, 30), split_version("10.20.30foo"));
        assert_eq!((0, 0, 0), split_version("100.200.300foo"));
        assert_eq!((0, 0, 0), split_version("100.200foo"));
        assert_eq!((0, 0, 0), split_version("1,2.3"));
    }
}
