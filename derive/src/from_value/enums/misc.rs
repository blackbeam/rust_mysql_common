use num_bigint::BigInt;
use syn::{spanned::Spanned, Token};

pub fn get_discriminant(def: &(Token![=], syn::Expr)) -> Result<BigInt, crate::Error> {
    match def {
        (
            _,
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Byte(x),
                ..
            }),
        ) => Ok(BigInt::from(x.value())),
        (
            _,
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Int(x),
                ..
            }),
        ) => Ok(x.base10_parse().unwrap()),
        (_, expr) => Err(crate::Error::UnsupportedDiscriminant(expr.span())),
    }
}
