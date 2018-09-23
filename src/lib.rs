// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate atoi;
extern crate base64;
extern crate bit_vec;
#[macro_use]
extern crate bitflags;
extern crate byteorder;
extern crate checked;
pub extern crate chrono;
#[macro_use]
extern crate lazy_static;
extern crate num_bigint;
extern crate rand;
extern crate regex;
#[cfg(feature = "rustc_serialize")]
pub extern crate rustc_serialize;
pub extern crate serde;
pub extern crate serde_json;
extern crate sha1;
extern crate sha2;
extern crate smallvec;
pub extern crate time;
extern crate twox_hash;
pub extern crate uuid;

/// Macro to conveniently generate named parameters for a statement.
/// Parameter name is `T` where `String: From<T>`, and
///
/// ```ignore
/// params! {
///     "param_name_1" => param_value_1,
///     "param_name_2" => param_value_2,
/// }
/// ```
#[macro_export]
macro_rules! params {
    ($($name:expr => $value:expr),*) => (
        vec![
            $((::std::string::String::from($name), $crate::value::Value::from($value))),*
        ]
    );
    ($($name:expr => $value:expr),*,) => (
        params!($($name => $value),*)
    );
}

/// Non panicking Slice::split_at
macro_rules! split_at_or_err {
    ($reader:expr, $at:expr, $msg:expr) => {
        if $reader.len() >= $at {
            Ok($reader.split_at($at))
        } else {
            Err(io::Error::new(io::ErrorKind::UnexpectedEof, $msg))
        }
    };
}

/// Reads MySql's length-encoded string
#[macro_export]
macro_rules! read_lenenc_str {
    ($reader:expr) => {
        $reader.read_lenenc_int().and_then(|len| {
            let (value, rest) = split_at_or_err!(
                $reader,
                len as usize,
                "EOF while reading length-encoded string"
            )?;
            *$reader = rest;
            Ok(value)
        })
    };
}

pub mod constants;
pub mod crypto;
pub mod io;
pub mod named_params;
#[macro_use]
pub mod packets;
pub mod params;
pub mod row;
pub mod scramble;
pub mod value;
