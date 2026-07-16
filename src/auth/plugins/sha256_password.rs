use crate::crypto;

const REQUEST_SERVER_KEY: u8 = 0x01;
const SCRAMBLE_LENGTH: usize = 20;

#[derive(Debug, Default)]
pub struct Sha256Password(State);

#[derive(Debug, Default, Clone, Copy)]
enum State {
    #[default]
    /// Expects server nonce
    ///
    /// * goes to `Done` on empty password, TLS transport or known server key
    /// * goes to `Step2` otherwise
    Init,
    /// Expects server key
    ///
    /// * goes to `Done`
    Step2 {
        /// sha256_password preferes plugin scramble over handshake scramble
        plugin_scramble: [u8; SCRAMBLE_LENGTH],
    },
    Done,
}

impl super::ChallengeResponsePlugin for Sha256Password {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        match self.0 {
            State::Init => {
                let Ok(plugin_scramble) = <&[u8; SCRAMBLE_LENGTH]>::try_from(challenge) else {
                    return Err(super::Error::Challenge);
                };

                if ctx.pass().is_empty() {
                    self.0 = State::Done;
                    return Ok(super::Response::last_empty());
                }

                if ctx.is_tls_transport() {
                    // just send the null-terminated password in case of a secure transport
                    self.0 = State::Done;
                    let mut pass = ctx.pass().to_vec();
                    pass.push(0x00);
                    return Ok(super::Response::last(pass));
                }

                if let Some(key) = ctx.server_key_pem() {
                    let response = encrypt_pass(ctx.pass(), plugin_scramble, key)?;
                    self.0 = State::Done;
                    Ok(response)
                } else {
                    // requesting server's public key
                    self.0 = State::Step2 {
                        plugin_scramble: *plugin_scramble,
                    };
                    Ok(super::Response::next(vec![REQUEST_SERVER_KEY]))
                }
            }
            State::Step2 {
                plugin_scramble: server_nonce,
            } => {
                let response = encrypt_pass(ctx.pass(), &server_nonce, challenge)?;
                self.0 = State::Done;
                Ok(response)
            }
            State::Done => Err(super::Error::Logic),
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
        Ok(encrypted_pass) => Ok(super::Response::last(encrypted_pass)),
        Err(crypto::Error::Key(_)) => Err(super::Error::Challenge),
        Err(crypto::Error::Padding(e)) => Err(super::Error::Other(Box::new(e))),
    }
}
