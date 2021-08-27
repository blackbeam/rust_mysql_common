[![Gitter](https://badges.gitter.im/rust-mysql/community.svg)](https://gitter.im/rust-mysql/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

[![Crates.io](https://img.shields.io/crates/v/mysql_common.svg)](https://crates.io/crates/cargo-readme)
[![Docs.rs](https://docs.rs/mysql_common/badge.svg)](https://docs.rs/mysql_common)
[![Build Status](https://travis-ci.org/blackbeam/rust_mysql_common.svg?branch=master)](https://travis-ci.org/blackbeam/rust_mysql_common)

# mysql_common

This crate is an implementation of basic MySql protocol primitives.

This crate:
* defines basic MySql constants;
* implements necessary functionality for MySql `cached_sha2_password` and
  `mysql_native_password` authentication plugins;
* implements helper traits for MySql protocol IO;
* implements support of named parameters for prepared statements;
* implements parsers for a subset of MySql protocol packets;
* defines rust representation of MySql protocol value and row;
* implements conversion between MySql values and rust types, between MySql rows and tuples
  of rust types.

### Supported rust types

Crate offers conversion from/to MySql values for following types (please see MySql documentation
on supported ranges for numeric types). Following table refers to MySql protocol types
(see `Value` struct) and not to MySql column types. Please see [MySql documentation][1] for
column and protocol type correspondence:

| Type                                 | Notes                                                     |
| ------------------------------------ | -------------------------------------------------------   |
| `{i,u}8..{i,u}128`, `{i,u}size`      | MySql int/uint will be converted, bytes will be parsed.<br>⚠️ Note that range of `{i,u}128` is greater than supported by MySql integer types but it'll be serialized anyway (as decimal bytes string). |
| `f32`                                | MySql float will be converted to `f32`, bytes will be parsed as `f32`.<br>⚠️ MySql double won't be converted to `f32` to avoid precision loss (see #17) |
| `f64`                                | MySql float and double will be converted to `f64`, bytes will be parsed as `f64`. |
| `bool`                               | MySql int {`0`, `1`} or bytes {`"0x30"`, `"0x31"`}        |
| `Vec<u8>`                            | MySql bytes                                               |
| `String`                             | MySql bytes parsed as utf8                                |
| `Duration` (`std` and `time`)        | MySql time or bytes parsed as MySql time string           |
| [`time::PrimitiveDateTime`] (v0.2.x) | MySql date time or bytes parsed as MySql date time string (⚠️ lossy! microseconds are ignored)           |
| [`time::Date`] (v0.2.x)              | MySql date or bytes parsed as MySql date string (⚠️ lossy! microseconds are ignored)           |
| [`time::Time`] (v0.2.x)              | MySql time or bytes parsed as MySql time string (⚠️ lossy! microseconds are ignored)           |
| [`time::Duration`] (v0.2.x)          | MySql time or bytes parsed as MySql time string           |
| [`time::PrimitiveDateTime`] (v0.3.x) | MySql date time or bytes parsed as MySql date time string (⚠️ lossy! microseconds are ignored)           |
| [`time::Date`] (v0.3.x)              | MySql date or bytes parsed as MySql date string (⚠️ lossy! microseconds are ignored)           |
| [`time::Time`] (v0.3.x)              | MySql time or bytes parsed as MySql time string (⚠️ lossy! microseconds are ignored)           |
| [`time::Duration`] (v0.3.x)          | MySql time or bytes parsed as MySql time string           |
| [`chrono::NaiveTime`]                | MySql date or bytes parsed as MySql date string           |
| [`chrono::NaiveDate`]                | MySql date or bytes parsed as MySql date string           |
| [`chrono::NaiveDateTime`]            | MySql date or bytes parsed as MySql date string           |
| [`uuid::Uuid`]                       | MySql bytes parsed using `Uuid::from_slice`               |
| [`serde_json::Value`]                | MySql bytes parsed using `serde_json::from_str`           |
| `mysql_common::Deserialized<T : DeserializeOwned>` | MySql bytes parsed using `serde_json::from_str` |
| `Option<T: FromValue>`               | Must be used for nullable columns to avoid errors         |
| [`decimal::Decimal`]                 | MySql int, uint or bytes parsed using `Decimal::from_str`.<br>⚠️ Note that this type doesn't support full range of MySql `DECIMAL` type. |
| [`bigdecimal::BigDecimal`] (v0.2.x)  | MySql int, uint, floats or bytes parsed using `BigDecimal::parse_bytes`.<br>⚠️ Note that range of this type is greater than supported by MySql `DECIMAL` type but it'll be serialized anyway. |
| [`bigdecimal::BigDecimal`] (v0.3.x)  | MySql int, uint, floats or bytes parsed using `BigDecimal::parse_bytes`.<br>⚠️ Note that range of this type is greater than supported by MySql `DECIMAL` type but it'll be serialized anyway. |
| `num_bigint::{BigInt, BigUint}`      | MySql int, uint or bytes parsed using `_::parse_bytes`.<br>⚠️ Note that range of this type is greater than supported by MySql integer types but it'll be serialized anyway (as decimal bytes string). |

Also crate provides from-row convertion for the following list of types (see `FromRow` trait):

| Type                                            | Notes                                             |
| ----------------------------------------------- | ------------------------------------------------- |
| `Row`                                           | Trivial conversion for `Row` itself.              |
| `T: FromValue`                                  | For rows with a single column.                    |
| `(T1: FromValue [, ..., T12: FromValue])`       | Row to a tuple of arity 1-12.                     |
| [`frunk::Hlist!`] types                         | Usefull to overcome tuple arity limitation        |

#### Crate features

| Feature        | Description                                 | Default |
| -------------- | ------------------------------------------- | ------- |
| `bigdecimal`   | Enables `bigdecimal` v0.2.x types support   | 🟢      |
| `bigdecimal03` | Enables `bigdecimal` v0.3.x types support   | 🔴      |
| `chrono`       | Enables `chrono` types support              | 🟢      |
| `rust_decimal` | Enables `rust_decimal` types support        | 🟢      |
| `time`         | Enables `time` v0.2.x types support         | 🟢      |
| `time03`       | Enables `time` v0.3.x types support         | 🔴      |
| `uuid`         | Enables `Uuid` type support                 | 🟢      |
| `frunk`        | Enables `FromRow` for `frunk::Hlist!` types | 🟢      |

[1]: https://dev.mysql.com/doc/internals/en/binary-protocol-value.html

## License

Licensed under either of
 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
