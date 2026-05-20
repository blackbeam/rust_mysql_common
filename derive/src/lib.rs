//! Implements [`FromValue`] and [`FromRow`] derive macros.

extern crate proc_macro;

use crate::error::Error;
type Result<T> = std::result::Result<T, crate::error::Error>;

mod error;
mod warn;

mod from_row;
mod from_value;

/// Derives `FromValue`. See `mysql_common` crate-level docs for more info.
#[proc_macro_derive(FromValue, attributes(mysql))]
#[manyhow::manyhow]
pub fn from_value(input: proc_macro::TokenStream) -> manyhow::Result<proc_macro2::TokenStream> {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    from_value::impl_from_value(&input).map_err(manyhow::Error::from)
}

/// Derives `FromRow`. See `mysql_common` crate-level docs for more info.
#[proc_macro_derive(FromRow, attributes(mysql))]
#[manyhow::manyhow]
pub fn from_row(input: proc_macro::TokenStream) -> manyhow::Result<proc_macro2::TokenStream> {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    from_row::impl_from_row(&input).map_err(manyhow::Error::from)
}
