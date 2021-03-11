// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use saturating::Saturating as S;

use std::{
    cmp::min,
    io::{self, Read, Write},
};

pub mod raw;

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
