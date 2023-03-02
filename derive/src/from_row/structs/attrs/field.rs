use darling::FromMeta;

#[derive(Debug, Default, FromMeta)]
pub struct Mysql {
    #[darling(default)]
    pub json: bool,
    #[darling(default)]
    pub rename: Option<String>,
}
