//! This module implements conversion to ISO language-locale identifiers
//! See [unic-locale](https://github.com/zbraniecki/unic-locale)

#![cfg(feature = "unic-langid")]

use std::convert::TryFrom;
use unic_langid::LanguageIdentifier;

use super::{FromValue, FromValueError, ParseIr, Value};

#[cfg_attr(docsrs, doc(cfg(feature = "unic-langid")))]
impl TryFrom<Value> for ParseIr<LanguageIdentifier> {
    type Error = FromValueError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match &value {
            Value::Bytes(b) => match LanguageIdentifier::from_bytes(b) {
                Ok(ident) => Ok(ParseIr(ident, value)),
                Err(_) => Err(FromValueError(value)),
            },
            _ => Err(FromValueError(value)),
        }
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "unic-langid")))]
impl From<ParseIr<LanguageIdentifier>> for LanguageIdentifier {
    fn from(value: ParseIr<LanguageIdentifier>) -> Self {
        value.commit()
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "unic-langid")))]
impl From<ParseIr<LanguageIdentifier>> for Value {
    fn from(value: ParseIr<LanguageIdentifier>) -> Self {
        value.rollback()
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "unic-langid")))]
impl FromValue for LanguageIdentifier {
    type Intermediate = ParseIr<LanguageIdentifier>;
}

#[cfg_attr(docsrs, doc(cfg(feature = "unic-langid")))]
impl From<LanguageIdentifier> for Value {
    fn from(lang_ident: LanguageIdentifier) -> Value {
        Value::Bytes(lang_ident.to_string().into())
    }
}

#[test]
fn can_convert_sql_locale_to_ident() {
    let value = Value::Bytes(String::from("en-US").into_bytes());

    let ident = LanguageIdentifier::from_value(value);

    assert_eq!(ident.language.to_string().as_str(), "en");
    assert_eq!(ident.to_string().as_str(), "en-US");
}
