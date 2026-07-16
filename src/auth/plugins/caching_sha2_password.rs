use sha2::{Digest as _, Sha256};

use crate::{
    auth::{
        common::{to_u8_32, xor},
        plugins::mysql_native_password::SCRAMBLE_LENGTH,
    },
    crypto,
};

const FAST_AUTH_SUCCESS: u8 = 0x03;
const PERFORM_FULL_AUTHENTICATION: u8 = 0x04;

#[derive(Debug, Default)]
pub struct CachingSha2Password(State);

impl CachingSha2Password {}

#[derive(Debug, Default, Clone, Copy)]
enum State {
    /// Waiting for initial handshake scramble
    ///
    /// * goes to `Done` on empty password
    /// * goes to `Step2` otherwise
    #[default]
    Init,
    /// Waiting for cache state from the server
    ///
    /// * goes to `Done` if password is cached or if server key is known
    /// * goes to `Step3` otherwise
    Step2,
    /// Waiting for public key from the server
    ///
    /// * goes to `Done`
    Step3,
    /// Authentication done.
    Done,
}

impl super::ChallengeResponsePlugin for CachingSha2Password {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        match self.0 {
            State::Init => {
                let Ok(scramble) = challenge.try_into() else {
                    return Err(super::Error::Challenge);
                };

                if ctx.pass().is_empty() {
                    self.0 = State::Done;
                    return Ok(super::Response::Last {
                        packet: Some(Vec::new()),
                    });
                }

                let response = scramble_sha256(scramble, ctx.pass())
                    .map(|x| x.to_vec())
                    .unwrap_or_default();

                self.0 = State::Step2;
                Ok(super::Response::Next {
                    packet: Some(response),
                })
            }
            State::Step2 => {
                let Ok([cache_state]) = <[u8; 1]>::try_from(challenge) else {
                    return Err(super::Error::Challenge);
                };

                match cache_state {
                    FAST_AUTH_SUCCESS => {
                        // cached
                        self.0 = State::Done;
                        Ok(super::Response::Last { packet: None })
                    }
                    PERFORM_FULL_AUTHENTICATION => {
                        // not cached
                        if ctx.is_secure_transport() {
                            // just send the password in case of a secure transport
                            self.0 = State::Done;
                            let mut pass = ctx.pass().to_vec();
                            pass.push(0x00);
                            Ok(super::Response::Last { packet: Some(pass) })
                        } else {
                            if let Some(key) = ctx.server_key_pem() {
                                let response = encrypt_pass(ctx.pass(), ctx.scramble(), key)?;
                                self.0 = State::Done;
                                Ok(response)
                            } else {
                                // requesting server's public key
                                self.0 = State::Step3;
                                Ok(super::Response::Next {
                                    packet: Some(vec![0x02]),
                                })
                            }
                        }
                    }
                    _ => Err(super::Error::Challenge),
                }
            }
            State::Step3 => {
                let response = encrypt_pass(ctx.pass(), ctx.scramble(), challenge)?;
                self.0 = State::Done;
                Ok(response)
            }
            State::Done => {
                panic!("ChallengeResponsePlugin::run called after Response::Last returned")
            }
        }
    }

    fn password_hash<C: super::Context>(&self, _ctx: C) -> Option<Vec<u8>> {
        None
    }
}

fn encrypt_pass(pass: &[u8], scramble: &[u8], key: &[u8]) -> Result<super::Response, super::Error> {
    let mut pass = pass.to_vec();
    pass.push(0x00);

    for (i, c) in pass.iter_mut().enumerate() {
        *(c) ^= scramble[i % scramble.len()];
    }

    match crypto::encrypt(&pass, key) {
        Ok(encrypted_pass) => Ok(super::Response::Last {
            packet: Some(encrypted_pass),
        }),
        Err(crypto::Error::Key(_)) => Err(super::Error::Challenge),
        Err(crypto::Error::Padding(e)) => Err(super::Error::Other(Box::new(e))),
    }
}

/// Scramble algorithm used in cached_sha2_password fast path.
///
/// XOR(SHA256(password), SHA256(SHA256(SHA256(password)), nonce))
pub fn scramble_sha256(nonce: &[u8; SCRAMBLE_LENGTH], password: &[u8]) -> Option<[u8; 32]> {
    fn sha256_1(bytes: impl AsRef<[u8]>) -> [u8; 32] {
        let mut hasher = Sha256::default();
        hasher.update(bytes.as_ref());
        to_u8_32(hasher.finalize())
    }

    fn sha256_2(bytes1: impl AsRef<[u8]>, bytes2: impl AsRef<[u8]>) -> [u8; 32] {
        let mut hasher = Sha256::default();
        hasher.update(bytes1.as_ref());
        hasher.update(bytes2.as_ref());
        to_u8_32(hasher.finalize())
    }

    if password.is_empty() {
        return None;
    }

    Some(xor(
        sha256_1(password),
        sha256_2(sha256_1(sha256_1(password)), nonce),
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
        let output2 = scramble_sha256(&scr, &password);
        assert!(output2.is_some());
        assert_eq!(
            output2.unwrap(),
            [
                0x4f, 0x97, 0xbb, 0xfd, 0x20, 0x24, 0x01, 0xc4, 0x2a, 0x69, 0xde, 0xaa, 0xe5, 0x3b,
                0xda, 0x07, 0x7e, 0xd7, 0x57, 0x85, 0x63, 0xc1, 0xa8, 0x0e, 0xb8, 0x16, 0xc8, 0x21,
                0x19, 0xb6, 0x8d, 0x2e,
            ]
        );
    }
}
