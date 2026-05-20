use manyhow::{emit, Emitter, ToTokensError};
use proc_macro2::Span;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected a struct with a single unnamed field")]
    NotANewTypeStruct(Span),
    #[error("structs with named fields are not supported")]
    NamedFieldsNotSupported(Span),
    #[error("unit structs are not supported")]
    UnitStructsNotSupported(Span),
    #[error("structs with unnamed fields are not supported")]
    StructsWithUnnamedFieldsNotSupported(Span),
    #[error("unions are not supported")]
    UnionsNotSupported(Span),
    #[error("enums are not supported")]
    EnumsNotSupported(Span),
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
    #[error("conflicting attributes")]
    FromValueConflictingAttributes(Span, Span),
    #[error("representation won't fit into MySql integer")]
    UnsupportedRepresentation(Span),
    #[error("conflicting attributes")]
    FromRowConflictingAttributes(Span, Span),
}

impl ToTokensError for Error {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Error::UnionsNotSupported(span)
            | Error::EnumsNotSupported(span)
            | Error::NonUnitVariant(span)
            | Error::UnsupportedDiscriminant(span)
            | Error::ExplicitInvalid(span)
            | Error::NotANewTypeStruct(span)
            | Error::NamedFieldsNotSupported(span)
            | Error::UnitStructsNotSupported(span)
            | Error::UnsupportedRepresentation(span)
            | Error::StructsWithUnnamedFieldsNotSupported(span) => {
                manyhow::error_message!(span, "FromValue: {self}").to_tokens(tokens);
            }
            Error::Syn(ref e) => {
                manyhow::error_message!(e.span(), "FromValue: {self}").to_tokens(tokens);
            }
            Error::Darling(ref e) => {
                manyhow::error_message!(e.span(), "FromValue: {self}").to_tokens(tokens);
            }
            Error::NoCrateNameFound | Error::MultipleCratesFound => {
                manyhow::error_message!("FromValue: {self}").to_tokens(tokens);
            }
            Error::FromValueConflictingAttributes(s1, s2) => {
                let mut emitter = Emitter::new();
                emit!(emitter, s1, "FromValue: {self}");
                emit!(emitter, s2, "conflicting attribute");
                if let Err(error) = emitter.into_result() {
                    error.to_tokens(tokens);
                }
            }
            Error::FromRowConflictingAttributes(s1, s2) => {
                let mut emitter = Emitter::new();
                emit!(emitter, s1, "FromRow: {self}");
                emit!(emitter, s2, "conflicting attribute");
                if let Err(error) = emitter.into_result() {
                    error.to_tokens(tokens);
                }
            }
        };
    }
}
