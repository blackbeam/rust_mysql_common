// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::io::{self};

use crate::io::ParseBuf;

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

// ---------------------------------------------------------------------------
// Variable-length integer encoding (MySQL serialization library format)
// ---------------------------------------------------------------------------

/// Reads a variable-length unsigned integer from the MySQL serialization format.
///
/// # Encoding
///
/// Bytes are stored in little-endian order. The least-significant byte's
/// trailing 1-bits encode the total byte count: `count = trailing_ones + 1`.
///
/// For 1–8 bytes, the encoded value is: `le_uint >> (trailing_ones + 1)`.
/// For 9 bytes (first byte = `0xFF`), the remaining 8 bytes are the raw value.
///
/// # Reference
///
/// <https://dev.mysql.com/doc/dev/mysql-server/latest/PageLibsMysqlSerialization.html>
pub(crate) fn read_varlen_uint(buf: &mut ParseBuf<'_>) -> io::Result<u64> {
    if buf.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "unexpected end of buffer reading varlen uint",
        ));
    }

    let first_byte = buf.0[0];
    let trailing_ones = first_byte.trailing_ones() as usize;

    if trailing_ones == 8 {
        // Special case: 0xFF → 9 bytes total, remaining 8 bytes are the raw u64
        if buf.len() < 9 {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end of buffer reading 9-byte varlen uint",
            ));
        }
        buf.0 = &buf.0[1..]; // skip 0xFF marker
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&buf.0[..8]);
        buf.0 = &buf.0[8..];
        return Ok(u64::from_le_bytes(bytes));
    }

    let num_bytes = trailing_ones + 1;
    if buf.len() < num_bytes {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "unexpected end of buffer reading varlen uint",
        ));
    }

    let mut raw = [0u8; 8];
    raw[..num_bytes].copy_from_slice(&buf.0[..num_bytes]);
    buf.0 = &buf.0[num_bytes..];

    let le_val = u64::from_le_bytes(raw);
    Ok(le_val >> num_bytes)
}

/// Writes a variable-length unsigned integer in the MySQL serialization format.
///
/// # Reference
///
/// <https://dev.mysql.com/doc/dev/mysql-server/latest/PageLibsMysqlSerialization.html>
pub(crate) fn write_varlen_uint(buf: &mut Vec<u8>, value: u64) {
    let num_bytes = varlen_uint_size(value);

    if num_bytes == 9 {
        // Special case: 0xFF marker followed by raw 8-byte LE value
        buf.push(0xFF);
        buf.extend_from_slice(&value.to_le_bytes());
        return;
    }

    // Shift value left by num_bytes to make room for trailing ones + terminator
    let shifted = value << num_bytes;

    // Set trailing ones: the lowest (num_bytes - 1) bits are 1, bit (num_bytes - 1) is 0
    let trailer = (1u64 << (num_bytes - 1)) - 1;
    let encoded = shifted | trailer;

    buf.extend_from_slice(&encoded.to_le_bytes()[..num_bytes]);
}

/// Returns the number of bytes needed to varlen-encode the given unsigned value.
///
/// Mirrors `get_size_integer_varlen_unsigned()` from MySQL:
/// <https://github.com/mysql/mysql-server/blob/trunk/libs/mysql/serialization/variable_length_integers.h>
pub(crate) fn varlen_uint_size(value: u64) -> usize {
    let data_bits = if value == 0 {
        0
    } else {
        64 - value.leading_zeros() as usize
    };
    match data_bits {
        0..=7 => 1,
        8..=14 => 2,
        15..=21 => 3,
        22..=28 => 4,
        29..=35 => 5,
        36..=42 => 6,
        43..=49 => 7,
        50..=56 => 8,
        _ => 9,
    }
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
    split_version_inner(bytes).unwrap_or((0, 0, 0))
}

// Split into its own function for two reasons:
// 1. Generic function will be instantiated for every type, increasing code size
// 2. It allows using Option and ? operator without breaking public API
fn split_version_inner(input: &[u8]) -> Option<(u8, u8, u8)> {
    let mut nums = [0_u8; 3];
    let mut iter = input.split(|c| *c == b'.');
    for (i, chunk) in (&mut iter).take(2).enumerate() {
        nums[i] = btoi::btoi(chunk).ok()?;
    }
    // allow junk at the end of the final part of the version
    let chunk_with_junk = iter.next()?;
    let end_of_digits = chunk_with_junk.iter().position(|c| *c < b'0' || *c > b'9');
    let chunk = match end_of_digits {
        Some(pos) => &chunk_with_junk[..pos],
        None => chunk_with_junk,
    };
    nums[2] = btoi::btoi(chunk).ok()?;

    Some((nums[0], nums[1], nums[2]))
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
        assert_eq!((0, 0, 0), split_version("1"));
        assert_eq!((0, 0, 0), split_version("1.2"));
    }
}
