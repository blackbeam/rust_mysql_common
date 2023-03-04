use num_bigint::BigInt;
use syn::spanned::Spanned;

pub fn get_discriminant(def: &syn::Expr) -> Result<BigInt, crate::Error> {
    match def {
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Byte(x),
            ..
        }) => Ok(BigInt::from(x.value())),
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(x),
            ..
        }) => Ok(x.base10_parse().unwrap()),
        syn::Expr::Group(syn::ExprGroup { ref expr, .. }) => get_discriminant(expr),
        expr => Err(crate::Error::UnsupportedDiscriminant(expr.span())),
    }
}
