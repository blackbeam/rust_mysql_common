use darling::FromMeta;

#[derive(Default, FromMeta)]
pub struct Mysql {
    #[darling(default)]
    pub rename: Option<String>,
    #[darling(default)]
    pub explicit_invalid: bool,
    #[darling(default)]
    pub allow_invalid_discriminants: bool,
}
