use std::io;

// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

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
