// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This crate is an implementation of basic MySql protocol primitives.
//!
//! This crate:
//! * defines basic MySql constants;
//! * implements necessary functionality for MySql `cached_sha2_password`,
//!   `mysql_native_password` and legacy authentication plugins;
//! * implements helper traits for MySql protocol IO;
//! * implements support of named parameters for prepared statements;
//! * implements parsers for a subset of MySql protocol packets (including binlog packets);
//! * defines rust representation of MySql protocol value and row;
//! * implements conversion between MySql values and rust types, between MySql rows and tuples
//!   of rust types.
//!
//! ## Supported rust types
//!
//! Crate offers conversion from/to MySql values for following types (please see MySql documentation
//! on supported ranges for numeric types). Following table refers to MySql protocol types
//! (see `Value` struct) and not to MySql column types. Please see [MySql documentation][1] for
//! column and protocol type correspondence:
//!
//! | Type                                 | Notes                                                     |
//! | ------------------------------------ | -------------------------------------------------------   |
//! | `{i,u}8..{i,u}128`, `{i,u}size`      | MySql int/uint will be converted, bytes will be parsed.<br>⚠️ Note that range of `{i,u}128` is greater than supported by MySql integer types but it'll be serialized anyway (as decimal bytes string). |
//! | `f32`                                | MySql float will be converted to `f32`, bytes will be parsed as `f32`.<br>⚠️ MySql double won't be converted to `f32` to avoid precision loss (see #17) |
//! | `f64`                                | MySql float and double will be converted to `f64`, bytes will be parsed as `f64`. |
//! | `bool`                               | MySql int {`0`, `1`} or bytes {`"0x30"`, `"0x31"`}        |
//! | `Vec<u8>`                            | MySql bytes                                               |
//! | `String`                             | MySql bytes parsed as utf8                                |
//! | `Duration` (`std` and `time`)        | MySql time or bytes parsed as MySql time string           |
//! | [`time::PrimitiveDateTime`] (v0.2.x) | MySql date time or bytes parsed as MySql date time string (⚠️ lossy! microseconds are ignored)           |
//! | [`time::Date`] (v0.2.x)              | MySql date or bytes parsed as MySql date string (⚠️ lossy! microseconds are ignored)           |
//! | [`time::Time`] (v0.2.x)              | MySql time or bytes parsed as MySql time string (⚠️ lossy! microseconds are ignored)           |
//! | [`time::Duration`] (v0.2.x)          | MySql time or bytes parsed as MySql time string           |
//! | [`time::PrimitiveDateTime`] (v0.3.x) | MySql date time or bytes parsed as MySql date time string (⚠️ lossy! microseconds are ignored)           |
//! | [`time::Date`] (v0.3.x)              | MySql date or bytes parsed as MySql date string (⚠️ lossy! microseconds are ignored)           |
//! | [`time::Time`] (v0.3.x)              | MySql time or bytes parsed as MySql time string (⚠️ lossy! microseconds are ignored)           |
//! | [`time::Duration`] (v0.3.x)          | MySql time or bytes parsed as MySql time string           |
//! | [`chrono::NaiveTime`]                | MySql date or bytes parsed as MySql date string           |
//! | [`chrono::NaiveDate`]                | MySql date or bytes parsed as MySql date string           |
//! | [`chrono::NaiveDateTime`]            | MySql date or bytes parsed as MySql date string           |
//! | [`uuid::Uuid`]                       | MySql bytes parsed using `Uuid::from_slice`               |
//! | [`serde_json::Value`]                | MySql bytes parsed using `serde_json::from_str`           |
//! | `mysql_common::Deserialized<T : DeserializeOwned>` | MySql bytes parsed using `serde_json::from_str` |
//! | `Option<T: FromValue>`               | Must be used for nullable columns to avoid errors         |
//! | [`decimal::Decimal`]                 | MySql int, uint or bytes parsed using `Decimal::from_str`.<br>⚠️ Note that this type doesn't support full range of MySql `DECIMAL` type. |
//! | [`bigdecimal::BigDecimal`] (v0.2.x)  | MySql int, uint, floats or bytes parsed using `BigDecimal::parse_bytes`.<br>⚠️ Note that range of this type is greater than supported by MySql `DECIMAL` type but it'll be serialized anyway. |
//! | [`bigdecimal::BigDecimal`] (v0.3.x)  | MySql int, uint, floats or bytes parsed using `BigDecimal::parse_bytes`.<br>⚠️ Note that range of this type is greater than supported by MySql `DECIMAL` type but it'll be serialized anyway. |
//! | `num_bigint::{BigInt, BigUint}`      | MySql int, uint or bytes parsed using `_::parse_bytes`.<br>⚠️ Note that range of this type is greater than supported by MySql integer types but it'll be serialized anyway (as decimal bytes string). |
//!
//! Also crate provides from-row convertion for the following list of types (see `FromRow` trait):
//!
//! | Type                                            | Notes                                             |
//! | ----------------------------------------------- | ------------------------------------------------- |
//! | `Row`                                           | Trivial conversion for `Row` itself.              |
//! | `T: FromValue`                                  | For rows with a single column.                    |
//! | `(T1: FromValue [, ..., T12: FromValue])`       | Row to a tuple of arity 1-12.                     |
//! | [`frunk::Hlist!`] types                         | Usefull to overcome tuple arity limitation        |
//!
//! ### Crate features
//!
//! | Feature        | Description                                          | Default |
//! | -------------- | ---------------------------------------------------- | ------- |
//! | `bigdecimal`   | Enables `bigdecimal` v0.2.x types support            | 🔴      |
//! | `bigdecimal03` | Enables `bigdecimal` v0.3.x types support            | 🟢      |
//! | `chrono`       | Enables `chrono` types support                       | 🔴      |
//! | `rust_decimal` | Enables `rust_decimal` types support                 | 🟢      |
//! | `time`         | Enables `time` v0.2.x types support                  | 🔴      |
//! | `time03`       | Enables `time` v0.3.x types support                  | 🟢      |
//! | `uuid`         | Enables `Uuid` type support                          | 🟢      |
//! | `frunk`        | Enables `FromRow` for `frunk::Hlist!` types          | 🟢      |
//! | `derive`       | Enables [`FromValue` and `FromRow` derive macros][2] | 🔴      |
//!
//! [1]: https://dev.mysql.com/doc/internals/en/binary-protocol-value.html
//! [2]: https://docs.rs/mysql-common-derive
#![cfg_attr(feature = "nightly", feature(test))]
#![cfg_attr(docsrs, feature(doc_cfg))]

// The `test` feature is required to compile tests.
// It'll bind test binaries to an official C++ impl of MySql decimals (see build.rs)
// The idea is to test our rust impl agaist C++ impl.
#[cfg(all(not(feature = "test"), test))]
compile_error!("Please invoke `cargo test` with `--features test` flags");

#[cfg(feature = "nightly")]
extern crate test;

#[macro_use]
pub mod bitflags_ext;

#[cfg(feature = "bigdecimal")]
pub use bigdecimal;

#[cfg(feature = "bigdecimal03")]
pub use bigdecimal03;

#[cfg(feature = "chrono")]
pub use chrono;

#[cfg(feature = "frunk")]
pub use frunk;

#[cfg(feature = "rust_decimal")]
pub use rust_decimal;

#[cfg(feature = "time")]
pub use time;

#[cfg(feature = "time03")]
pub use time03;

#[cfg(feature = "uuid")]
pub use uuid;

#[cfg(feature = "derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate mysql_common_derive;

pub use num_bigint;
pub use serde;
pub use serde_json;

pub use value::convert::FromValueError;
pub use value::Value;

pub use row::convert::FromRowError;
pub use row::Row;

pub use value::json::{Deserialized, Serialized};

pub mod prelude {
    #[cfg(feature = "derive")]
    #[doc(inline)]
    pub use mysql_common_derive::FromValue;

    #[cfg(feature = "derive")]
    #[doc(inline)]
    pub use mysql_common_derive::FromRow;

    pub use crate::row::{convert::FromRow, ColumnIndex};
    pub use crate::value::convert::{FromValue, ToValue};
}

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
    (@to_pair $map:expr, $name:expr => $value:expr) => (
        let entry = $map.entry(std::vec::Vec::<u8>::from($name));
        if let std::collections::hash_map::Entry::Occupied(_) = entry {
            panic!("Redefinition of named parameter `{}'", std::string::String::from_utf8_lossy(entry.key()));
        } else {
            entry.or_insert($crate::value::Value::from($value));
        }
    );
    (@to_pair $map:expr, $name:ident) => (
        let entry = $map.entry(stringify!($name).as_bytes().to_vec());
        if let std::collections::hash_map::Entry::Occupied(_) = entry {
            panic!("Redefinition of named parameter `{}'", std::string::String::from_utf8_lossy(entry.key()));
        } else {
            entry.or_insert($crate::value::Value::from($name));
        }
    );
    (@expand $map:expr;) => {};
    (@expand $map:expr; $name:expr => $value:expr, $($tail:tt)*) => {
        params!(@to_pair $map, $name => $value);
        params!(@expand $map; $($tail)*);
    };
    (@expand $map:expr; $name:expr => $value:expr $(, $tail:tt)*) => {
        params!(@to_pair $map, $name => $value);
        params!(@expand $map; $($tail)*);
    };
    (@expand $map:expr; $name:ident, $($tail:tt)*) => {
        params!(@to_pair $map, $name);
        params!(@expand $map; $($tail)*);
    };
    (@expand $map:expr; $name:ident $(, $tail:tt)*) => {
        params!(@to_pair $map, $name);
        params!(@expand $map; $($tail)*);
    };
    ($i:ident, $($tail:tt)*) => {
        {
            let mut map: std::collections::HashMap<std::vec::Vec<u8>, $crate::value::Value, _> = std::default::Default::default();
            params!(@expand (&mut map); $i, $($tail)*);
            $crate::params::Params::Named(map)
        }
    };
    ($i:expr => $($tail:tt)*) => {
        {
            let mut map: std::collections::HashMap<std::vec::Vec<u8>, $crate::value::Value, _> = std::default::Default::default();
            params!(@expand (&mut map); $i => $($tail)*);
            $crate::params::Params::Named(map)
        }
    };
    ($i:ident) => {
        {
            let mut map: std::collections::HashMap<std::vec::Vec<u8>, $crate::value::Value, _> = std::default::Default::default();
            params!(@expand (&mut map); $i);
            $crate::params::Params::Named(map)
        }
    }
}

pub mod constants;
pub mod crypto;
pub mod io;
pub mod misc;
pub mod named_params;
#[macro_use]
pub mod packets;
pub mod params;
pub mod proto;
pub mod row;
pub mod scramble;
pub mod value;

pub mod binlog;

#[cfg(test)]
#[test]
fn params_macro_test() {
    use crate::{params::Params, value::Value};

    let foo = 42;
    let bar = "bar";

    assert_eq!(
        Params::from(vec![(String::from("foo"), Value::Int(42))]),
        params! { foo }
    );
    assert_eq!(
        Params::from(vec![(String::from("foo"), Value::Int(42))]),
        params! { foo, }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { foo, bar }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { foo, bar, }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { "foo" => foo, "bar" => bar }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { "foo" => foo, "bar" => bar, }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { foo, "bar" => bar }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { "foo" => foo, bar }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { foo, "bar" => bar, }
    );
    assert_eq!(
        Params::from(vec![
            (String::from("foo"), Value::Int(42)),
            (String::from("bar"), Value::Bytes((&b"bar"[..]).into())),
        ]),
        params! { "foo" => foo, bar, }
    );
}

#[test]
#[should_panic(expected = "Redefinition of named parameter `a'")]
fn params_macro_should_panic_on_named_param_redefinition() {
    params! {"a" => 1, "b" => 2, "a" => 3};
}
