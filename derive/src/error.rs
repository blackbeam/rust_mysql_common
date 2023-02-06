use proc_macro2::Span;
use proc_macro_error::{Diagnostic, Level};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected a struct with a single unnamed field")]
    NotANewTypeStruct(Span),
    #[error("structs with named fields are not supported")]
    NamedFieldsNotSupported(Span),
    #[error("unit structs are not supported")]
    UnitStructsNotSupported(Span),
    #[error("generics support is not implemented")]
    GenericsNotImplemented,
    #[error("union support is not implemented")]
    UnionNotImplemented,
    #[error("non-unit variants are not supported")]
    NonUnitVariant(Span),
    #[error("unsupported discriminant")]
    UnsupportedDiscriminant(Span),
    #[error("add #[mysql(explicit_invalid)] attribute to allow")]
    ExplicitInvalid(Span),
    #[error("no suitable crate found, use #[mysql(crate = \"..\")] to specify the crate name")]
    NoCrateNameFound,
    #[error("multiple crates found, use #[mysql(crate = \"..\")] to specify the particular name")]
    MultipleCratesFound,
    #[error(transparent)]
    Syn(#[from] syn::Error),
    #[error(transparent)]
    Darling(#[from] darling::error::Error),
}

impl From<Error> for Diagnostic {
    fn from(x: Error) -> Diagnostic {
        match x {
            Error::UnionNotImplemented => Diagnostic::new(Level::Error, format!("FromValue: {x}")),
            Error::GenericsNotImplemented => {
                Diagnostic::new(Level::Error, format!("FromValue: {x}"))
            }
            Error::NonUnitVariant(span) => {
                Diagnostic::spanned(span, Level::Error, format!("FromValue: {x}"))
            }
            Error::UnsupportedDiscriminant(span) => {
                Diagnostic::spanned(span, Level::Error, format!("FromValue: {x}"))
            }
            Error::ExplicitInvalid(span) => {
                Diagnostic::spanned(span, Level::Error, format!("FromValue: {x}"))
            }
            Error::Syn(ref e) => {
                Diagnostic::spanned(e.span(), Level::Error, format!("FromValue: {x}"))
            }
            Error::Darling(ref e) => {
                Diagnostic::spanned(e.span(), Level::Error, format!("FromValue: {x}"))
            }
            Error::NoCrateNameFound => Diagnostic::new(Level::Error, format!("FromValue: {x}")),
            Error::MultipleCratesFound => Diagnostic::new(Level::Error, format!("FromValue: {x}")),
            Error::NotANewTypeStruct(span) => {
                Diagnostic::spanned(span, Level::Error, format!("FromValue: {x}"))
            }
            Error::NamedFieldsNotSupported(span) => {
                Diagnostic::spanned(span, Level::Error, format!("FromValue: {x}"))
            }
            Error::UnitStructsNotSupported(span) => {
                Diagnostic::spanned(span, Level::Error, format!("FromValue: {x}"))
            }
        }
    }
}
