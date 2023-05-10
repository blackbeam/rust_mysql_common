use darling::FromAttributes;

#[derive(Debug, Default, FromAttributes)]
#[darling(attributes(mysql))]
pub struct Mysql {
    #[darling(default)]
    pub json: bool,
    #[darling(default)]
    pub rename: Option<String>,
}
