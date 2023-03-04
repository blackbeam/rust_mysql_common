extern crate proc_macro;

use proc_macro_error::abort;

use crate::error::Error;
type Result<T> = std::result::Result<T, crate::error::Error>;

mod error;
mod warn;

mod from_row;
mod from_value;

/// Derives `FromValue`.
///
/// Supported derivations:
///
/// *   for enum – you should carefully read the corresponding section of MySql documentation
/// *   for newtypes (see [New Type Idiom][1]) – given that the wrapped type itself satisfies
///     `FromValue`
///
/// ## Enums
///
/// ### Container attributes:
///
/// *  `#[mysql(crate_name = "some_name")]` – overrides an attemt to guess a crate that provides
///    required traits
/// *  `#[mysql(rename_all = ...)]` – rename all the variants according to the given case
///    convention. The possible values are "lowercase", "UPPERCASE", "PascalCase", "camelCase",
///    "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case", "SCREAMING-KEBAB-CASE"
/// *  `#[mysql(is_integer)]` – tells derive macro that the value is an integer rather than MySql
///    ENUM. Macro won't warn if variants are sparse or greater than u16 and will not try to parse
///    textual representation.
/// *  `#[mysql(is_string)]` – tells derive macro that the value is a string rather than MySql
///    ENUM. Macro won't warn if variants are sparse or greater than u16 and will not try to parse
///
/// ### Example
///
/// Given `ENUM('x-small', 'small', 'medium', 'large', 'x-large')` on MySql side:
///
/// ```no_run
/// # use mysql_common_derive::FromValue;
/// # use mysql_common::{row::Row, row::convert::from_row};
/// #[derive(FromValue)]
/// #[mysql(rename_all = "kebab-case")]
/// #[repr(u8)]
/// enum Size {
///     XSmall = 1,
///     Small,
///     Medium,
///     Large,
///     XLarge,
/// }
///
/// fn assert_from_row_works(x: Row) -> Size {
///     from_row(x)
/// }
///
/// # fn main() {}
/// ```
///
/// ## Newtypes
///
/// It is expected, that wrapper value satisfies `FromValue` or `deserialize_with` is given.
/// Also note, that to support `FromRow` the wrapped value must satisfy `Into<Value>` or
/// `serialize_with` must be given.
///
/// ### Container attributes:
///
/// *  `#[mysql(crate_name = "some_name")]` – overrides an attemt to guess a crate to import types from
/// *  `#[mysql(bound = "Foo: Bar, Baz: Quux")]` – use the following additional bounds
/// *  `#[mysql(deserialize_with = "some::path")]` – use the following function to deserialize
///    the wrapped value. Expected signature is `fn (Value) -> Result<Wrapped, FromValueError>`.
/// *  `#[mysql(serialize_with = "some::path")]` – use the following function to serialize
///    the wrapped value. Expected signature is `fn (Wrapped) -> Value`.
///
/// ### Example
///
/// ```no_run
/// # use mysql_common::{row::Row, row::convert::from_row, prelude::FromValue, value::Value, value::convert::{from_value, FromValueError}};
///
/// /// Dummy complex type with additional bounds on FromValue impl.
/// struct ComplexTypeToWrap<'a, 'b, const N: usize, T, U, V>([(&'a T, &'b U, V); N]);
///
/// struct FakeIr;
///
/// impl TryFrom<Value> for FakeIr {
///     // ...
/// #    type Error = FromValueError;
/// #    fn try_from(v: Value) -> Result<Self, Self::Error> {
/// #        unimplemented!();
/// #    }
/// }
///
/// impl<'a, 'b: 'a, const N: usize, T: 'a, U: From<String>, V: From<u64>> From<FakeIr> for ComplexTypeToWrap<'a, 'b, N, T, U, V> {
///     // ...
/// #    fn from(x: FakeIr) -> Self {
/// #        unimplemented!();
/// #    }
/// }
///
/// impl From<FakeIr> for Value {
///     // ...
/// #    fn from(x: FakeIr) -> Self {
/// #        unimplemented!();
/// #    }
/// }
///
/// impl<'a, 'b: 'a, const N: usize, T: 'a, U: From<String>, V: From<u64>> FromValue for ComplexTypeToWrap<'a, 'b, N, T, U, V> {
///     type Intermediate = FakeIr;
/// }
///
/// fn neg_de(v: Value) -> Result<i64, FromValueError> {
///     match v {
///         Value::Int(x) => Ok(-x),
///         Value::UInt(x) => Ok(-(x as i64)),
///         x => Err(FromValueError(x)),
///     }
/// }
///
/// fn neg_ser(x: i64) -> Value {
///     Value::Int(-x)
/// }
///
/// #[derive(FromValue)]
/// #[mysql(deserialize_with = "neg_de", serialize_with = "neg_ser")]
/// struct Neg(i64);
///
/// #[derive(FromValue)]
/// struct Inch(i32);
///
/// #[derive(FromValue)]
/// struct Foo<T>(Option<T>);
///
/// #[derive(FromValue)]
/// #[mysql(bound = "'b: 'a, T: 'a, U: From<String>, V: From<u64>")]
/// struct Bar<'a, 'b, const N: usize, T, U, V>(ComplexTypeToWrap<'a, 'b, N, T, U, V>);
///
/// fn assert_from_row_works<'a, 'b, const N: usize, T, U, V>(x: Row) -> (Inch, Neg, Foo<u8>, Bar<'a, 'b, N, T, U, V>)
/// where 'b: 'a, T: 'a, U: From<String>, V: From<u64>,
/// {
///     from_row(x)
/// }
///
/// # fn main() {}
/// ```
///
/// [1]: https://doc.rust-lang.org/rust-by-example/generics/new_types.html
#[proc_macro_derive(FromValue, attributes(mysql))]
#[proc_macro_error::proc_macro_error]
pub fn from_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    match from_value::impl_from_value(&input) {
        Ok(gen) => gen.into(),
        Err(e) => abort!(e),
    }
}

/// Derives `FromRow`.
///
/// Also defines some constants on the struct:
///
/// *  `const TABLE_NAME: &str` – if `table_name` is given
/// *  `const {}_FIELD: &str` – for each struct field (`{}` is a SCREAMING_SNAKE_CASE representation
///    of a field name (not a column name))
///
/// Supported derivations:
///
/// * for a struct with named fields – field name will be used as a column name to search for a value
///
/// ### Container attributes:
///
/// *  `#[mysql(crate_name = "some_name")]` – overrides an attemt to guess a crate that provides
///    required traits
/// *  `#[mysql(rename_all = ...)]` – rename all column names according to the given case
///    convention. The possible values are "lowercase", "UPPERCASE", "PascalCase", "camelCase",
///    "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case", "SCREAMING-KEBAB-CASE"
/// *  `#[mysql(table_name = "some_name")]` – defines `pub const TABLE_NAME: &str` on the struct
///
/// ### Field attributes:
///
/// *  `#[mysql(rename = "some_name")]` – overrides column name of a field
/// *  `#[mysql(json)]` - column will be interpreted as a JSON string containing
///    a value of a field type
///
/// ### Example
///
/// ```
/// # use mysql_common_derive::FromRow;
/// # use mysql_common::{
/// #     constants::ColumnType,
/// #     packets::Column,
/// #     row::{Row, new_row},
/// #     row::convert::from_row,
/// #     value::Value,
/// # };
/// #[derive(Debug, PartialEq, Eq, FromRow)]
/// #[mysql(table_name = "Foos")]
/// struct Foo {
///     id: u64,
///     #[mysql(json, rename = "def")]
///     definition: Bar,
///     child: Option<u64>,
/// }
///
/// #[derive(Debug, serde::Deserialize, PartialEq, Eq)]
/// enum Bar {
///     Left,
///     Right,
/// }
///
/// /// Returns the following row:
/// ///
/// /// ```
/// /// +----+-----------+-------+
/// /// | id | def       | child |
/// /// +----+-----------+-------+
/// /// | 42 | '"Right"' | NULL  |
/// /// +----+-----------+-------+
/// /// ```
/// fn get_row() -> Row {
///     // ...
/// #   let values = vec![Value::Int(42), Value::Bytes(b"\"Right\"".as_slice().into()), Value::NULL];
/// #   let columns = vec![
/// #       Column::new(ColumnType::MYSQL_TYPE_LONG).with_name(b"id"),
/// #       Column::new(ColumnType::MYSQL_TYPE_BLOB).with_name(b"def"),
/// #       Column::new(ColumnType::MYSQL_TYPE_NULL).with_name(b"child"),
/// #   ];
/// #   new_row(values, columns.into_boxed_slice().into())
/// }
///
/// # fn main() {
/// let foo = from_row::<Foo>(get_row());
/// assert_eq!(foo, Foo { id: 42, definition: Bar::Right, child: None });
/// assert_eq!(Foo::TABLE_NAME, "Foos");
/// assert_eq!(Foo::ID_FIELD, "id");
/// assert_eq!(Foo::DEFINITION_FIELD, "def");
/// assert_eq!(Foo::CHILD_FIELD, "child");
/// # }
/// ```
///
/// ## Newtypes
///
/// It is expected, that wrapper value satisfies `FromValue` or `deserialize_with` is given.
/// Also note, that to support `FromRow` the wrapped value must satisfy `Into<Value>` or
/// `serialize_with` must be given.
///
/// ### Container attributes:
///
/// *  `#[mysql(crate_name = "some_name")]` – overrides an attemt to guess a crate to import types from
/// *  `#[mysql(bound = "Foo: Bar, Baz: Quux")]` – use the following additional bounds
/// *  `#[mysql(deserialize_with = "some::path")]` – use the following function to deserialize
///    the wrapped value. Expected signature is `fn (Value) -> Result<Wrapped, FromValueError>`.
/// *  `#[mysql(serialize_with = "some::path")]` – use the following function to serialize
///    the wrapped value. Expected signature is `fn (Wrapped) -> Value`.
///
/// ### Example
///
/// ```no_run
/// # use mysql_common::{row::Row, row::convert::from_row, prelude::FromValue, value::Value, value::convert::{from_value, FromValueError}};
///
/// /// Dummy complex type with additional bounds on FromValue impl.
/// struct ComplexTypeToWrap<'a, 'b, const N: usize, T, U, V>([(&'a T, &'b U, V); N]);
///
/// struct FakeIr;
///
/// impl TryFrom<Value> for FakeIr {
///     // ...
/// #    type Error = FromValueError;
/// #    fn try_from(v: Value) -> Result<Self, Self::Error> {
/// #        unimplemented!();
/// #    }
/// }
///
/// impl<'a, 'b: 'a, const N: usize, T: 'a, U: From<String>, V: From<u64>> From<FakeIr> for ComplexTypeToWrap<'a, 'b, N, T, U, V> {
///     // ...
/// #    fn from(x: FakeIr) -> Self {
/// #        unimplemented!();
/// #    }
/// }
///
/// impl From<FakeIr> for Value {
///     // ...
/// #    fn from(x: FakeIr) -> Self {
/// #        unimplemented!();
/// #    }
/// }
///
/// impl<'a, 'b: 'a, const N: usize, T: 'a, U: From<String>, V: From<u64>> FromValue for ComplexTypeToWrap<'a, 'b, N, T, U, V> {
///     type Intermediate = FakeIr;
/// }
///
/// fn neg_de(v: Value) -> Result<i64, FromValueError> {
///     match v {
///         Value::Int(x) => Ok(-x),
///         Value::UInt(x) => Ok(-(x as i64)),
///         x => Err(FromValueError(x)),
///     }
/// }
///
/// fn neg_ser(x: i64) -> Value {
///     Value::Int(-x)
/// }
///
/// #[derive(FromValue)]
/// #[mysql(deserialize_with = "neg_de", serialize_with = "neg_ser")]
/// struct Neg(i64);
///
/// #[derive(FromValue)]
/// struct Inch(i32);
///
/// #[derive(FromValue)]
/// struct Foo<T>(Option<T>);
///
/// #[derive(FromValue)]
/// #[mysql(bound = "'b: 'a, T: 'a, U: From<String>, V: From<u64>")]
/// struct Bar<'a, 'b, const N: usize, T, U, V>(ComplexTypeToWrap<'a, 'b, N, T, U, V>);
///
/// fn assert_from_row_works<'a, 'b, const N: usize, T, U, V>(x: Row) -> (Inch, Neg, Foo<u8>, Bar<'a, 'b, N, T, U, V>)
/// where 'b: 'a, T: 'a, U: From<String>, V: From<u64>,
/// {
///     from_row(x)
/// }
///
/// # fn main() {}
/// ```
///
/// [1]: https://doc.rust-lang.org/rust-by-example/generics/new_types.html
#[proc_macro_derive(FromRow, attributes(mysql))]
#[proc_macro_error::proc_macro_error]
pub fn from_row(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    match from_row::impl_from_row(&input) {
        Ok(gen) => gen.into(),
        Err(e) => abort!(e),
    }
}
