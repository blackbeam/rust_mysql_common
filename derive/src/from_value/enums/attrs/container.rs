use darling::FromMeta;
use proc_macro2::{Span, TokenStream};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::TokenStreamExt;

#[derive(Default, FromMeta)]
pub struct Mysql {
    #[darling(default)]
    pub crate_name: Crate,
    #[darling(default)]
    pub rename_all: Option<RenameAll>,
    #[darling(default)]
    pub allow_invalid_discriminants: bool,
}

pub enum Crate {
    NotFound,
    Multiple,
    Itself,
    Found(String),
}

impl Default for Crate {
    fn default() -> Self {
        let mysql = crate_name("mysql");
        let mysql_async = crate_name("mysql_async");
        let mysql_common = crate_name("mysql_common");
        match (mysql, mysql_async, mysql_common) {
            (Ok(_), Ok(_), _) | (_, Ok(_), Ok(_)) | (Ok(_), _, Ok(_)) => Self::Multiple,
            (Ok(FoundCrate::Itself), Err(_), Err(_))
            | (Err(_), Ok(FoundCrate::Itself), Err(_))
            | (Err(_), Err(_), Ok(FoundCrate::Itself)) => Self::Itself,
            (Ok(FoundCrate::Name(x)), Err(_), Err(_))
            | (Err(_), Ok(FoundCrate::Name(x)), Err(_))
            | (Err(_), Err(_), Ok(FoundCrate::Name(x))) => Self::Found(x),
            (Err(_), Err(_), Err(_)) => Self::NotFound,
        }
    }
}

impl FromMeta for Crate {
    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(Self::Found(value.into()))
    }
}

pub enum RenameAll {
    Lowercase,
    Uppercase,
    UpperCamelCase,
    LowerCamelCase,
    SnakeCase,
    KebabCase,
    ShoutySnakeCase,
    ShoutyKebabCase,
}
impl RenameAll {
    pub fn rename(&self, name: &str) -> String {
        match self {
            RenameAll::Lowercase => name.to_lowercase(),
            RenameAll::Uppercase => name.to_uppercase(),
            RenameAll::UpperCamelCase => heck::AsUpperCamelCase(name).to_string(),
            RenameAll::LowerCamelCase => heck::AsLowerCamelCase(name).to_string(),
            RenameAll::SnakeCase => heck::AsSnakeCase(name).to_string(),
            RenameAll::KebabCase => heck::AsKebabCase(name).to_string(),
            RenameAll::ShoutySnakeCase => heck::AsShoutySnakeCase(name).to_string(),
            RenameAll::ShoutyKebabCase => heck::AsShoutyKebabCase(name).to_string(),
        }
    }
}

impl FromMeta for RenameAll {
    fn from_string(value: &str) -> darling::Result<Self> {
        match value {
            "lowercase" => Ok(Self::Lowercase),
            "UPPERCASE" => Ok(Self::Uppercase),
            "PascalCase" => Ok(Self::UpperCamelCase),
            "camelCase" => Ok(Self::LowerCamelCase),
            "snake_case" => Ok(Self::SnakeCase),
            "kebab-case" => Ok(Self::KebabCase),
            "SCREAMING_SNAKE_CASE" => Ok(Self::ShoutySnakeCase),
            "SCREAMING-KEBAB-CASE" => Ok(Self::ShoutyKebabCase),
            _ => Err(darling::Error::unknown_value(value)),
        }
    }
}

#[derive(Default, FromMeta)]
#[darling(allow_unknown_fields)]
pub struct Repr(pub EnumRepr);

pub enum EnumRepr {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    ISize,
    USize,
}

impl EnumRepr {
    const I8_IDENT: &str = "i8";
    const U8_IDENT: &str = "u8";
    const I16_IDENT: &str = "i16";
    const U16_IDENT: &str = "u16";
    const I32_IDENT: &str = "i32";
    const U32_IDENT: &str = "u32";
    const I64_IDENT: &str = "i64";
    const U64_IDENT: &str = "u64";
    const I128_IDENT: &str = "i128";
    const U128_IDENT: &str = "u128";
    const ISIZE_IDENT: &str = "isize";
    const USIZE_IDENT: &str = "usize";

    const fn ident(&self) -> &'static str {
        match self {
            EnumRepr::I8 => "i8",
            EnumRepr::U8 => "u8",
            EnumRepr::I16 => "i16",
            EnumRepr::U16 => "u16",
            EnumRepr::I32 => "i32",
            EnumRepr::U32 => "u32",
            EnumRepr::I64 => "i64",
            EnumRepr::U64 => "u64",
            EnumRepr::I128 => "i128",
            EnumRepr::U128 => "u128",
            EnumRepr::ISize => "isize",
            EnumRepr::USize => "usize",
        }
    }

    fn from_ident(ident: &str) -> Option<Self> {
        match ident {
            Self::I8_IDENT => Some(EnumRepr::I8),
            Self::U8_IDENT => Some(EnumRepr::U8),
            Self::I16_IDENT => Some(EnumRepr::I16),
            Self::U16_IDENT => Some(EnumRepr::U16),
            Self::I32_IDENT => Some(EnumRepr::I32),
            Self::U32_IDENT => Some(EnumRepr::U32),
            Self::I64_IDENT => Some(EnumRepr::I64),
            Self::U64_IDENT => Some(EnumRepr::U64),
            Self::I128_IDENT => Some(EnumRepr::I128),
            Self::U128_IDENT => Some(EnumRepr::U128),
            Self::ISIZE_IDENT => Some(EnumRepr::ISize),
            Self::USIZE_IDENT => Some(EnumRepr::USize),
            _ => None,
        }
    }
}

impl quote::ToTokens for EnumRepr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(syn::Ident::new(self.ident(), Span::call_site()))
    }
}

impl Default for EnumRepr {
    fn default() -> Self {
        Self::ISize
    }
}

impl FromMeta for EnumRepr {
    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        Ok(items
            .into_iter()
            .filter_map(|x| match x {
                syn::NestedMeta::Meta(syn::Meta::Path(path)) => Some(path),
                _ => None,
            })
            .filter_map(|x| x.get_ident())
            .find_map(|x| Self::from_ident(&x.to_string()))
            .unwrap_or_default())
    }
}
