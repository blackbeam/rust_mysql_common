extern crate proc_macro;

use proc_macro_error::abort;

use crate::error::Error;
type Result<T> = std::result::Result<T, crate::error::Error>;

mod error;
mod warn;

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
/// *  `#[mysql(crate_name = ...)]` – overrides an attemt to guess a crate that provides required
///    traits
/// *  `#[mysql(rename_all = ...)]` – rename all the variants according to the given case
///    convention. The possible values are "lowercase", "UPPERCASE", "PascalCase", "camelCase",
///    "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case", "SCREAMING-KEBAB-CASE"
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
/// ### Container attributes:
///
/// *  `#[mysql(crate_name = ...)]` – overrides an attemt to guess a crate to import types from
/// *  `#[mysql(bound = ...)]` – use the following additional bounds
///
/// ### Example
///
/// ```no_run
/// # use mysql_common_derive::FromValue;
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
/// fn assert_from_row_works<'a, 'b, const N: usize, T, U, V>(x: Row) -> (Inch, Foo<u8>, Bar<'a, 'b, N, T, U, V>)
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
