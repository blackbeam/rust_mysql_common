// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![cfg_attr(feature = "nightly", feature(test, const_fn))]
#[cfg(feature = "nightly")]
extern crate test;

#[macro_use]
extern crate bitflags;

pub use chrono;
#[macro_use]
extern crate lazy_static;

#[cfg(feature = "rustc_serialize")]
pub extern crate rustc_serialize;
pub use serde;
pub use serde_json;

pub use time;

pub use uuid;

/// This macro is a convenient way to pass named parameters to a statement.
///
/// ```ignore
/// let foo = 42;
/// conn.prep_exec("SELECT :foo, :foo2x", params! {
///     foo,
///     "foo2x" => foo * 2,
/// });
/// ```
#[macro_export]
macro_rules! params {
    () => {};
    (@to_pair $name:expr => $value:expr) => (
        (std::string::String::from($name), $crate::value::Value::from($value))
    );
    (@to_pair $name:ident) => (
        (std::string::String::from(stringify!($name)), $crate::value::Value::from($name))
    );
    (@expand $vec:expr;) => {};
    (@expand $vec:expr; $name:expr => $value:expr, $($tail:tt)*) => {
        $vec.push(params!(@to_pair $name => $value));
        params!(@expand $vec; $($tail)*);
    };
    (@expand $vec:expr; $name:expr => $value:expr $(, $tail:tt)*) => {
        $vec.push(params!(@to_pair $name => $value));
        params!(@expand $vec; $($tail)*);
    };
    (@expand $vec:expr; $name:ident, $($tail:tt)*) => {
        $vec.push(params!(@to_pair $name));
        params!(@expand $vec; $($tail)*);
    };
    (@expand $vec:expr; $name:ident $(, $tail:tt)*) => {
        $vec.push(params!(@to_pair $name));
        params!(@expand $vec; $($tail)*);
    };
    ($i:ident, $($tail:tt)*) => {
        {
            let mut output = std::vec::Vec::new();
            params!(@expand output; $i, $($tail)*);
            output
        }
    };
    ($i:expr => $($tail:tt)*) => {
        {
            let mut output = std::vec::Vec::new();
            params!(@expand output; $i => $($tail)*);
            output
        }
    };
    ($i:ident) => {
        {
            let mut output = std::vec::Vec::new();
            params!(@expand output; $i);
            output
        }
    }
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

#[cfg(test)]
#[test]
fn params_macro_test() {
    use crate::value::Value;

    let foo = 42;
    let bar = "bar";

    assert_eq!(vec![(String::from("foo"), Value::Int(42))], params! { foo });
    assert_eq!(
        vec![(String::from("foo"), Value::Int(42))],
        params! { foo, }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { foo, bar }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { foo, bar, }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { "foo" => foo, "bar" => bar }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { "foo" => foo, "bar" => bar, }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { foo, "bar" => bar }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { "foo" => foo, bar }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { foo, "bar" => bar, }
    );
    assert_eq!(
        vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ],
        params! { "foo" => foo, bar, }
    );
}
