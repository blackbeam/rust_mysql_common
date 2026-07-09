// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![doc = include_str!("../CRATE_LEVEL_DOCS.md")]
#![cfg_attr(feature = "nightly", feature(test))]
#![cfg_attr(docsrs, feature(doc_cfg))]

// The `test` feature is required to compile tests.
// It'll bind test binaries to an official C++ impl of MySql decimals (see build.rs)
// The idea is to test our rust impl against C++ impl.
#[cfg(all(not(feature = "test"), test))]
compile_error!("Please invoke `cargo test` with `--features test` flags");

#[cfg(feature = "nightly")]
extern crate test;

#[macro_use]
pub mod bitflags_ext;

#[cfg(feature = "bigdecimal")]
pub use bigdecimal;

#[cfg(feature = "chrono")]
pub use chrono;

#[cfg(feature = "frunk")]
pub use frunk;

#[cfg(feature = "rust_decimal")]
pub use rust_decimal;

#[cfg(feature = "time")]
pub use time;

pub use uuid;

#[cfg(feature = "derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate mysql_common_derive;

pub use num_bigint;
pub use serde;
pub use serde_json;

pub use value::Value;
pub use value::convert::FromValueError;

pub use row::Row;
pub use row::convert::FromRowError;

pub use value::json::{Deserialized, Serialized};

pub mod prelude {
    #[cfg(feature = "derive")]
    #[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
    #[doc(inline)]
    pub use mysql_common_derive::FromValue;

    #[cfg(feature = "derive")]
    #[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
    #[doc(inline)]
    pub use mysql_common_derive::FromRow;

    pub use crate::row::{ColumnIndex, convert::FromRow};
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
    (@to_pair $map:expr_2021, $name:expr_2021 => $value:expr_2021) => (
        let entry = $map.entry(std::vec::Vec::<u8>::from($name));
        if let std::collections::hash_map::Entry::Occupied(_) = entry {
            panic!("Redefinition of named parameter `{}'", std::string::String::from_utf8_lossy(entry.key()));
        } else {
            entry.or_insert($crate::value::Value::from($value));
        }
    );
    (@to_pair $map:expr_2021, $name:ident) => (
        let entry = $map.entry(stringify!($name).as_bytes().to_vec());
        if let std::collections::hash_map::Entry::Occupied(_) = entry {
            panic!("Redefinition of named parameter `{}'", std::string::String::from_utf8_lossy(entry.key()));
        } else {
            entry.or_insert($crate::value::Value::from($name));
        }
    );
    (@expand $map:expr_2021;) => {};
    (@expand $map:expr_2021; $name:expr_2021 => $value:expr_2021, $($tail:tt)*) => {
        params!(@to_pair $map, $name => $value);
        params!(@expand $map; $($tail)*);
    };
    (@expand $map:expr_2021; $name:expr_2021 => $value:expr_2021 $(, $tail:tt)*) => {
        params!(@to_pair $map, $name => $value);
        params!(@expand $map; $($tail)*);
    };
    (@expand $map:expr_2021; $name:ident, $($tail:tt)*) => {
        params!(@to_pair $map, $name);
        params!(@expand $map; $($tail)*);
    };
    (@expand $map:expr_2021; $name:ident $(, $tail:tt)*) => {
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
    ($i:expr_2021 => $($tail:tt)*) => {
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

pub mod auth;
pub mod collations;
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
pub mod value;

#[cfg(feature = "binlog")]
#[cfg_attr(docsrs, doc(cfg(feature = "binlog")))]
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

#[test]
fn issue_88() {
    use crate::{Value, prelude::FromValue};
    #[derive(FromValue, Debug, Eq, PartialEq)]
    #[mysql(is_integer)]
    #[repr(u8)]
    enum SomeType {
        A,
        B = 42,
        C,
    }

    let value = Value::Int(42);
    assert_eq!(SomeType::B, SomeType::from_value(value));

    let value = Value::Int(0);
    assert_eq!(SomeType::A, SomeType::from_value(value));
}

#[test]
fn from_value_is_string() {
    use crate::{Value, prelude::FromValue};
    #[derive(FromValue, Debug, Eq, PartialEq)]
    #[mysql(is_string, rename_all = "snake_case")]
    #[allow(clippy::enum_variant_names)]
    enum SomeTypeIsString {
        FirstVariant = 0,
        SecondVariant = 2,
        ThirdVariant = 3,
    }

    let value = Value::Bytes(b"first_variant".to_vec());
    assert_eq!(
        SomeTypeIsString::FirstVariant,
        SomeTypeIsString::from_value(value)
    );

    let value = Value::Bytes(b"third_variant".to_vec());
    assert_eq!(
        SomeTypeIsString::ThirdVariant,
        SomeTypeIsString::from_value(value)
    );

    assert_eq!(
        Value::from(SomeTypeIsString::FirstVariant),
        Value::Bytes(b"first_variant".to_vec())
    );
    assert_eq!(
        Value::from(SomeTypeIsString::SecondVariant),
        Value::Bytes(b"second_variant".to_vec())
    );
    assert_eq!(
        Value::from(SomeTypeIsString::ThirdVariant),
        Value::Bytes(b"third_variant".to_vec())
    );
}

#[test]
fn from_value_is_integer() {
    use crate::{Value, prelude::FromValue};
    #[derive(FromValue, Debug, Eq, PartialEq)]
    #[mysql(is_integer, rename_all = "snake_case")]
    #[repr(i8)]
    #[allow(clippy::enum_variant_names)]
    enum SomeTypeIsInteger {
        FirstVariant = -1_i8,
        SecondVariant = 2,
        ThirdVariant = 3,
    }

    let value = Value::Int(-1);
    assert_eq!(
        SomeTypeIsInteger::FirstVariant,
        SomeTypeIsInteger::from_value(value)
    );

    let value = Value::Int(3);
    assert_eq!(
        SomeTypeIsInteger::ThirdVariant,
        SomeTypeIsInteger::from_value(value)
    );

    assert_eq!(Value::from(SomeTypeIsInteger::FirstVariant), Value::Int(-1));
    assert_eq!(Value::from(SomeTypeIsInteger::SecondVariant), Value::Int(2));
    assert_eq!(Value::from(SomeTypeIsInteger::ThirdVariant), Value::Int(3));
}

#[cfg(test)]
mod tests {
    use crate::{
        FromValueError,
        constants::ColumnType,
        packets::Column,
        row::{convert::FromRow, new_row},
        value::{Value, convert::from_value},
    };
    use unic_langid::LanguageIdentifier;

    #[derive(FromValue)]
    #[mysql(serialize_with = "from_langid", deserialize_with = "to_langid")]
    struct LangId(LanguageIdentifier);

    impl std::ops::Deref for LangId {
        type Target = LanguageIdentifier;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    fn to_langid(v: Value) -> Result<LanguageIdentifier, FromValueError> {
        match v {
            Value::Bytes(ref b) => match LanguageIdentifier::from_bytes(b) {
                Ok(ident) => Ok(ident),
                Err(_) => Err(FromValueError(v)),
            },
            _ => Err(FromValueError(v)),
        }
    }

    fn from_langid(land_id: LanguageIdentifier) -> Value {
        Value::Bytes(land_id.to_string().into())
    }

    #[test]
    fn newtype_with() {
        let mut value = Value::Bytes(b"en-US".into());

        let ident = from_value::<LangId>(value);

        assert_eq!(ident.language.to_string().as_str(), "en");
        assert_eq!(ident.to_string().as_str(), "en-US");

        value = ident.into();

        assert_eq!(value, Value::Bytes(b"en-US".into()));
    }

    #[test]
    fn from_row_derive() {
        #[derive(FromRow)]
        #[mysql(table_name = "Foos", rename_all = "camelCase")]
        struct Foo {
            id: u64,
            text_data: String,
            #[mysql(json)]
            json_data: serde_json::Value,
            #[mysql(deserialize_with = "from_literal", rename = "custom")]
            custom_bool: bool,
        }

        fn from_literal(value: crate::Value) -> Result<bool, crate::FromValueError> {
            match value {
                crate::Value::Bytes(x) if x == b"true" => Ok(true),
                crate::Value::Bytes(x) if x == b"false" => Ok(false),
                x => Err(crate::FromValueError(x)),
            }
        }

        assert_eq!(Foo::TABLE_NAME, "Foos");
        assert_eq!(Foo::ID_FIELD, "id");
        assert_eq!(Foo::TEXT_DATA_FIELD, "textData");
        assert_eq!(Foo::JSON_DATA_FIELD, "jsonData");
        assert_eq!(Foo::CUSTOM_BOOL_FIELD, "custom");

        let columns = vec![
            Column::new(ColumnType::MYSQL_TYPE_LONGLONG)
                .with_name(b"id")
                .with_org_name(b"id")
                .with_table(b"Foos")
                .with_org_table(b"Foos"),
            Column::new(ColumnType::MYSQL_TYPE_VARCHAR)
                .with_name(b"textData")
                .with_org_name(b"textData")
                .with_table(b"Foos")
                .with_org_table(b"Foos"),
            Column::new(ColumnType::MYSQL_TYPE_JSON)
                .with_name(b"jsonData")
                .with_org_name(b"jsonData")
                .with_table(b"Foos")
                .with_org_table(b"Foos"),
            Column::new(ColumnType::MYSQL_TYPE_VARCHAR)
                .with_name(b"custom")
                .with_org_name(b"custom")
                .with_table(b"Foos")
                .with_org_table(b"Foos"),
        ];

        let row = new_row(
            vec![
                crate::Value::Int(10),
                crate::Value::Bytes(b"bytes".into()),
                crate::Value::Bytes(b"[true,false,\"not found\"]".into()),
                crate::Value::Bytes(b"true".into()),
            ],
            columns.into(),
        );

        let deserialized = Foo::from_row(row);

        assert_eq!(deserialized.id, 10);
        assert_eq!(deserialized.text_data, "bytes");
        assert_eq!(
            deserialized.json_data.to_string(),
            "[true,false,\"not found\"]"
        );
        assert!(deserialized.custom_bool);
    }
}
