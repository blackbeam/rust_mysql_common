#[derive(Debug, Default)]
pub struct MysqlClearPassword(());

impl super::ChallengeResponsePlugin for MysqlClearPassword {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        _challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        let pass = ctx.pass();
        let mut cleartext_pass = vec![0_u8; pass.len() + 1];
        cleartext_pass[..pass.len()].copy_from_slice(pass);
        Ok(super::Response::last(cleartext_pass))
    }

    fn password_hash<C: super::Context>(&self, _ctx: C) -> Option<Vec<u8>> {
        None
    }
}
