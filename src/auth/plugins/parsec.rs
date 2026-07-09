#[cfg(feature = "client_parsec")]
use ed25519_dalek::PUBLIC_KEY_LENGTH;

pub const CHALLENGE_SCRAMBLE_LENGTH: usize = 32;
pub const CHALLENGE_SALT_LENGTH: usize = 18;
pub const AUTH_STRING_LENGTH: usize = 2 + CHALLENGE_SALT_LENGTH;

#[cfg(feature = "client_parsec")]
#[derive(Debug, Default)]
pub struct Parsec(State);

#[cfg(feature = "client_parsec")]
#[derive(Debug, Default, Clone, Copy)]
enum State {
    #[default]
    /// Reads server scramble and responds with an empty packet.
    ///
    /// Goes to `Step2`
    Init,
    /// Generates client secret and actual plugin response
    ///
    /// Goes to `Done`
    Step2 {
        server_scramble: [u8; CHALLENGE_SCRAMBLE_LENGTH],
    },
    Done {
        /// In case of the Parsec plugin the "zero-config TLS" shared secret
        /// is auth string followed by the derived public key
        password_hash: [u8; AUTH_STRING_LENGTH + PUBLIC_KEY_LENGTH],
    },
}

#[cfg(not(feature = "client_parsec"))]
#[derive(Debug, Default)]
pub struct Parsec(());

#[cfg(not(feature = "client_parsec"))]

impl super::ChallengeResponsePlugin for Parsec {
    fn run<C: super::Context>(
        &mut self,
        _ctx: C,
        _challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        Err(super::Error::FeatureRequired("mysql_common/client_parsec"))
    }

    fn password_hash<C: super::Context>(&self, _ctx: C) -> Option<Vec<u8>> {
        None
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "client_parsec")))]
#[cfg(feature = "client_parsec")]
impl super::ChallengeResponsePlugin for Parsec {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        use crate::crypto::rsa::{GetRandom, Rng};

        use ed25519_dalek::{SIGNATURE_LENGTH, Signer, SigningKey};
        use pbkdf2::pbkdf2_hmac;
        use sha2::Sha512;

        match self.0 {
            State::Init => {
                let Ok(server_scramble) = <[u8; CHALLENGE_SCRAMBLE_LENGTH]>::try_from(challenge)
                else {
                    return Err(super::Error::Challenge);
                };

                self.0 = State::Step2 { server_scramble };

                Ok(super::Response::next_empty())
            }
            State::Step2 { server_scramble } => {
                let Ok(auth_string) = AuthString::try_from_slice(challenge) else {
                    return Err(super::Error::Challenge);
                };

                // This is the server nonce[0..32] + client nonce[32..64] + signature buffer[64..128]
                // [0..64] - is the message we will need to sign. [32..] - (client nonce + signature)
                // that will be sent to server
                let mut message_and_signature =
                    [0_u8; CHALLENGE_SCRAMBLE_LENGTH * 2 + SIGNATURE_LENGTH];

                // copy server scramble
                message_and_signature[..CHALLENGE_SCRAMBLE_LENGTH]
                    .copy_from_slice(&server_scramble);
                // generate client scramble
                GetRandom
                    .fill(
                        &mut message_and_signature
                            [CHALLENGE_SCRAMBLE_LENGTH..CHALLENGE_SCRAMBLE_LENGTH * 2],
                    )
                    .map_err(|e| super::Error::Other(Box::new(e)))?;

                // derive the key
                let mut derived_key = [0_u8; ed25519_dalek::SECRET_KEY_LENGTH];
                pbkdf2_hmac::<Sha512>(
                    ctx.pass(),
                    auth_string.salt(),
                    auth_string.iterations(),
                    &mut derived_key,
                );

                let signing_key = SigningKey::from_bytes(&derived_key);
                let signature = signing_key.sign(&message_and_signature[..64]);
                message_and_signature[64..].copy_from_slice(&signature.to_bytes());

                let mut password_hash = [0_u8; AUTH_STRING_LENGTH + PUBLIC_KEY_LENGTH];
                password_hash[..AUTH_STRING_LENGTH].copy_from_slice(auth_string.as_bytes());
                password_hash[AUTH_STRING_LENGTH..]
                    .copy_from_slice(signing_key.verifying_key().as_bytes());

                self.0 = State::Done { password_hash };

                Ok(super::Response::Last {
                    packet: Some(message_and_signature[32..].to_vec()),
                })
            }
            State::Done { .. } => {
                panic!("ChallengeResponsePlugin::run called after Response::Last returned")
            }
        }
    }

    fn password_hash<C: super::Context>(&self, _ctx: C) -> Option<Vec<u8>> {
        if let State::Done { password_hash } = self.0 {
            Some(password_hash.to_vec())
        } else {
            None
        }
    }
}

#[cfg(feature = "client_parsec")]
#[derive(Debug, thiserror::Error)]
enum AuthStringError {
    #[error("Size mismatch")]
    SizeMismatch,
    #[error("Unknown algorithm")]
    UnknownAlgorithm,
    #[error("Wrong iterations factor")]
    WrongFactor,
}

#[cfg(feature = "client_parsec")]
#[repr(C, packed)]
struct AuthString {
    algorithm: u8,
    factor: u8,
    salt: [u8; CHALLENGE_SALT_LENGTH],
}

#[cfg(feature = "client_parsec")]
impl AuthString {
    const fn as_bytes(&self) -> &[u8; AUTH_STRING_LENGTH] {
        // SAFETY: Self layout is identical to an array of AUTH_STRING_LENGTH bytes
        unsafe { &*(self as *const _ as *const [u8; AUTH_STRING_LENGTH]) }
    }

    const fn try_from_slice(slice: &[u8]) -> Result<&Self, AuthStringError> {
        const {
            assert!(size_of::<Self>() == AUTH_STRING_LENGTH, "invalid layout");
        }

        if slice.len() != AUTH_STRING_LENGTH {
            return Err(AuthStringError::SizeMismatch);
        }

        if slice[0] != b'P' {
            // Unknown algorithm
            return Err(AuthStringError::UnknownAlgorithm);
        }

        if slice[1] > 3 {
            // Invalid number of iterations
            return Err(AuthStringError::WrongFactor);
        }

        // SAFETY: Self layout is identical to an array of AUTH_STRING_LENGTH bytes
        Ok(unsafe { &*(std::ptr::from_ref(slice) as *const Self) })
    }

    const fn iterations(&self) -> u32 {
        1024_u32 << self.factor
    }

    const fn salt(&self) -> &[u8; CHALLENGE_SALT_LENGTH] {
        &self.salt
    }
}
