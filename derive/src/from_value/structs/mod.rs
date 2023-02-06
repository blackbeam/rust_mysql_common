use darling::FromMeta;
use heck::AsSnakeCase;
use proc_macro2::{Span, TokenStream};
use proc_macro_error::abort;
use quote::{ToTokens, TokenStreamExt};

use super::enums::attrs::container::Crate;

mod attrs;

pub fn impl_from_value_for_struct(
    attrs: &[syn::Attribute],
    ident: &proc_macro2::Ident,
    generics: &syn::Generics,
    data_struct: &syn::DataStruct,
) -> crate::Result<TokenStream> {
    let fields = match &data_struct.fields {
        syn::Fields::Named(_) => {
            return Err(crate::Error::NamedFieldsNotSupported(
                data_struct.struct_token.span,
            ))
        }
        syn::Fields::Unnamed(fields) => {
            if fields.unnamed.len() != 1 {
                return Err(crate::Error::NotANewTypeStruct(
                    data_struct.struct_token.span,
                ));
            } else {
                fields
            }
        }
        syn::Fields::Unit => {
            return Err(crate::Error::UnitStructsNotSupported(
                data_struct.struct_token.span,
            ))
        }
    };

    let meta = attrs
        .into_iter()
        .filter_map(|attr| attr.parse_meta().ok())
        .collect::<Vec<_>>();

    let item_attrs = meta
        .iter()
        .find_map(|x| match x {
            syn::Meta::List(y) if y.path.is_ident("mysql") => Some(x),
            _ => None,
        })
        .map(|x| <attrs::container::Mysql as FromMeta>::from_meta(x))
        .transpose()?
        .unwrap_or_default();

    if generics.params.is_empty() {
        let derived = NewTypeNoGenerics {
            ident,
            field: fields.unnamed.first().unwrap(),
            item_attrs,
        };
        Ok(quote::quote! { #derived })
    } else {
        let derived = NewType {
            ident,
            field: fields.unnamed.first().unwrap(),
            item_attrs,
            generics,
        };
        Ok(quote::quote! { #derived })
    }
}

struct NewTypeNoGenerics<'a> {
    ident: &'a proc_macro2::Ident,
    item_attrs: attrs::container::Mysql,
    field: &'a syn::Field,
}

impl ToTokens for NewTypeNoGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let container_name = self.ident;
        let field_type = &self.field.ty;

        let crat = match self.item_attrs.crate_name {
            Crate::NotFound => abort!(crate::Error::NoCrateNameFound),
            Crate::Multiple => abort!(crate::Error::MultipleCratesFound),
            Crate::Itself => syn::Ident::new("crate", Span::call_site()),
            Crate::Found(ref name) => syn::Ident::new(name, Span::call_site()),
        };

        let ir_name = syn::Ident::new(&format!("{container_name}Ir"), Span::call_site());
        let ir_mod_name = syn::Ident::new(
            &format!("{}_ir", AsSnakeCase(container_name.to_string())),
            Span::call_site(),
        );

        let new_tokens = quote::quote!(
            mod #ir_mod_name {
                use super::#container_name;
                use #crat::prelude::FromValue;
                use #crat::Value;
                use std::convert::TryFrom;

                #[derive(Debug, Clone, Copy, Eq, PartialEq)]
                pub struct #ir_name<T: FromValue>(T::Intermediate);

                impl TryFrom<Value> for #ir_name<#field_type> {
                    type Error = <<#field_type as FromValue>::Intermediate as TryFrom<Value>>::Error;

                    fn try_from(value: Value) -> Result<Self, Self::Error> {
                        <#field_type as FromValue>::Intermediate::try_from(value).map(Self)
                    }
                }

                impl From<#ir_name<#field_type>> for #container_name {
                    fn from(ir: #ir_name<#field_type>) -> Self {
                        Self(ir.0.into())
                    }
                }

                impl From<#ir_name<#field_type>> for Value
                where
                    <#field_type as FromValue>::Intermediate: Into<Value>,
                {
                    fn from(ir: #ir_name<#field_type>) -> Self {
                        ir.0.into()
                    }
                }
            }

            pub use #ir_mod_name::#ir_name;

            impl #crat::prelude::FromValue for #container_name {
                type Intermediate = #ir_name<#field_type>;
            }
        );

        tokens.append_all(new_tokens);
    }
}

struct NewType<'a> {
    ident: &'a proc_macro2::Ident,
    item_attrs: attrs::container::Mysql,
    field: &'a syn::Field,
    generics: &'a syn::Generics,
}

impl ToTokens for NewType<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let container_name = self.ident;
        let field_type = &self.field.ty;
        let generics = self.generics.params.iter();

        let crat = match self.item_attrs.crate_name {
            Crate::NotFound => abort!(crate::Error::NoCrateNameFound),
            Crate::Multiple => abort!(crate::Error::MultipleCratesFound),
            Crate::Itself => syn::Ident::new("crate", Span::call_site()),
            Crate::Found(ref name) => syn::Ident::new(name, Span::call_site()),
        };

        let ir_name = syn::Ident::new(&format!("{container_name}Ir"), Span::call_site());
        let ir_mod_name = syn::Ident::new(
            &format!("{}_ir", AsSnakeCase(container_name.to_string())),
            Span::call_site(),
        );

        let impl_generics = (generics.len() > 0).then(|| {
            let generics = self.generics.params.iter();
            quote::quote!(#(#generics,)*)
        });
        let ident_generics = (generics.len() > 0).then(|| {
            let generics = self.generics.params.iter().map(|g| match g {
                syn::GenericParam::Type(x) => {
                    let ident = &x.ident;
                    quote::quote!(#ident)
                }
                syn::GenericParam::Lifetime(x) => {
                    let lifetime = &x.lifetime;
                    quote::quote!(#lifetime)
                }
                syn::GenericParam::Const(x) => {
                    let ident = &x.ident;
                    quote::quote!(#ident)
                }
            });
            quote::quote!(#(#generics,)*)
        });

        let additional_bounds = {
            let additional_bounds = self.item_attrs.bound.iter().map(|x| x.0.iter()).flatten();
            quote::quote!(#(#additional_bounds,)*)
        };

        let from_value_bound = quote::quote!(#field_type: #crat::prelude::FromValue,);
        let into_value_bound = quote::quote!(<#field_type as #crat::prelude::FromValue>::Intermediate: Into<#crat::Value>,);

        let new_tokens = quote::quote!(
            mod #ir_mod_name {
                use #crat::prelude::FromValue;

                pub struct #ir_name<T: FromValue>(pub T::Intermediate);
            }

            pub use #ir_mod_name::#ir_name;

            impl<#impl_generics> std::convert::TryFrom<#crat::Value> for #ir_name<#field_type>
            where
                #additional_bounds
                #from_value_bound
            {
                type Error = <<#field_type as #crat::prelude::FromValue>::Intermediate as std::convert::TryFrom<#crat::Value>>::Error;

                fn try_from(value: #crat::Value) -> Result<Self, Self::Error> {
                    <#field_type as #crat::prelude::FromValue>::Intermediate::try_from(value).map(Self)
                }
            }

            impl<#impl_generics> From<#ir_name<#field_type>> for #container_name<#ident_generics>
            where
                #additional_bounds
                #from_value_bound
            {
                fn from(ir: #ir_name<#field_type>) -> Self {
                    Self(ir.0.into())
                }
            }


            impl<#impl_generics> From<#ir_name<#field_type>> for #crat::Value
            where
                #additional_bounds
                #from_value_bound
                #into_value_bound
            {
                fn from(ir: #ir_name<#field_type>) -> Self {
                    ir.0.into()
                }
            }

            impl <#impl_generics> #crat::prelude::FromValue for #container_name <#ident_generics>
            where
                    #additional_bounds
                    #from_value_bound {
                type Intermediate = #ir_name<#field_type>;
            }
        );

        tokens.append_all(new_tokens);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn derive_struct() {
        let code = r#"
            #[derive(FromValue)]
            struct A(i32);
        "#;
        let input = syn::parse_str::<syn::DeriveInput>(code).unwrap();
        let derived = super::super::impl_from_value(&input).unwrap();
        eprintln!("{}", derived);

        let code = r#"
            #[derive(FromValue)]
            struct A<T>(T);
        "#;
        let input = syn::parse_str::<syn::DeriveInput>(code).unwrap();
        let derived = super::super::impl_from_value(&input).unwrap();
        eprintln!("{}", derived);
    }
}
