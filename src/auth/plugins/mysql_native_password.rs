use sha1::Sha1;
use sha2::Digest as _;

use crate::auth::common::xor;

/// Handshake packet scramble length since MySql 4.1
pub const SCRAMBLE_LENGTH: usize = 20;

#[derive(Debug, Default)]
pub struct MysqlNativePassword(());

impl super::ChallengeResponsePlugin for MysqlNativePassword {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        let Ok(nonce) = <&[u8; SCRAMBLE_LENGTH]>::try_from(challenge) else {
            return Err(super::Error::Challenge);
        };

        let response = scramble_native(nonce, ctx.pass())
            .map(|x| x.to_vec())
            .unwrap_or_default();

        Ok(super::Response::last(response))
    }

    fn password_hash<C: super::Context>(&self, ctx: C) -> Option<Vec<u8>> {
        let first = Sha1::digest(ctx.pass());
        Some(Sha1::digest(first).to_vec())
    }
}

/// Scramble algorithm used in mysql_native_password.
///
/// SHA1(password) XOR SHA1(nonce, SHA1(SHA1(password)))
pub fn scramble_native(nonce: &[u8], password: &[u8]) -> Option<[u8; 20]> {
    fn sha1_1(bytes: impl AsRef<[u8]>) -> [u8; 20] {
        Sha1::digest(bytes).into()
    }

    fn sha1_2(bytes1: impl AsRef<[u8]>, bytes2: impl AsRef<[u8]>) -> [u8; 20] {
        let mut hasher = Sha1::new();
        hasher.update(bytes1.as_ref());
        hasher.update(bytes2.as_ref());
        hasher.finalize().into()
    }

    if password.is_empty() {
        return None;
    }

    Some(xor(
        sha1_1(password),
        sha1_2(nonce, sha1_1(sha1_1(password))),
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_compute_scrambled_password() {
        let scr = [
            0x4e, 0x52, 0x33, 0x48, 0x50, 0x3a, 0x71, 0x49, 0x59, 0x61, 0x5f, 0x39, 0x3d, 0x64,
            0x62, 0x3f, 0x53, 0x64, 0x7b, 0x60,
        ];
        let password = [0x47, 0x21, 0x69, 0x64, 0x65, 0x72, 0x32, 0x37];
        let output1 = scramble_native(&scr, &password);
        assert!(output1.is_some());
        assert_eq!(
            output1.unwrap(),
            [
                0x09, 0xcf, 0xf8, 0x85, 0x5e, 0x9e, 0x70, 0x53, 0x40, 0xff, 0x22, 0x70, 0xd8, 0xfb,
                0x9f, 0xad, 0xba, 0x90, 0x6b, 0x70,
            ]
        );
    }
}
