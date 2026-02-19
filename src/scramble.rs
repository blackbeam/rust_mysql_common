// Copyright (c) 2016 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use sha1::Sha1;
use sha2::{Digest, Sha256};

fn xor<T, U>(mut left: T, right: U) -> T
where
    T: AsMut<[u8]>,
    U: AsRef<[u8]>,
{
    left.as_mut()
        .iter_mut()
        .zip(right.as_ref().iter())
        .map(|(l, r)| *l ^= r)
        .last();
    left
}

fn to_u8_32(bytes: impl AsRef<[u8]>) -> [u8; 32] {
    let mut out = [0; 32];
    out[..].copy_from_slice(bytes.as_ref());
    out
}

/// Insecure password hasing used in mysql_old_password.
fn hash_password(output: &mut [u32; 2], password: &[u8]) {
    let mut nr: u32 = 1345345333;
    let mut add: u32 = 7;
    let mut nr2: u32 = 0x12345671;

    let mut tmp: u32;

    for x in password {
        if *x == b' ' || *x == b'\t' {
            continue;
        }

        tmp = *x as u32;
        nr ^= (nr & 63)
            .wrapping_add(add)
            .wrapping_mul(tmp)
            .wrapping_add(nr << 8);
        nr2 = nr2.wrapping_add((nr2 << 8) ^ nr);
        add = add.wrapping_add(tmp);
    }

    output[0] = nr & 0b01111111_11111111_11111111_11111111;
    output[1] = nr2 & 0b01111111_11111111_11111111_11111111;
}

pub fn scramble_323(nonce: &[u8], password: &[u8]) -> Option<[u8; 8]> {
    struct Rand323 {
        seed1: u32,
        seed2: u32,
        max_value: u32,
        max_value_dbl: f64,
    }

    impl Rand323 {
        fn init(seed1: u32, seed2: u32) -> Self {
            Self {
                max_value: 0x3FFFFFFF,
                max_value_dbl: 0x3FFFFFFF as f64,
                seed1: seed1 % 0x3FFFFFFF,
                seed2: seed2 % 0x3FFFFFFF,
            }
        }

        fn my_rnd(&mut self) -> f64 {
            self.seed1 = (self.seed1 * 3 + self.seed2) % self.max_value;
            self.seed2 = (self.seed1 + self.seed2 + 33) % self.max_value;
            (self.seed1 as f64) / self.max_value_dbl
        }
    }

    let mut hash_pass = [0_u32; 2];
    let mut hash_message = [0_u32; 2];

    if password.is_empty() {
        return None;
    }

    let mut output = [0_u8; 8];

    hash_password(&mut hash_pass, password);
    hash_password(&mut hash_message, nonce);

    let mut rand_st = Rand323::init(
        hash_pass[0] ^ hash_message[0],
        hash_pass[1] ^ hash_message[1],
    );

    for x in output.iter_mut() {
        *x = ((rand_st.my_rnd() * 31_f64).floor() + 64_f64) as u8;
    }

    let extra = (rand_st.my_rnd() * 31_f64).floor() as u8;

    for x in output.iter_mut() {
        *x ^= extra;
    }

    Some(output)
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

/// Scramble algorithm used in cached_sha2_password fast path.
///
/// XOR(SHA256(password), SHA256(SHA256(SHA256(password)), nonce))
pub fn scramble_sha256(nonce: &[u8], password: &[u8]) -> Option<[u8; 32]> {
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

/// Crafting a signature according to EdDSA for message using the pass.
///
/// This will only work if `client_ed25519` feature is enabled and otherwise will panic at runtime.
#[cfg_attr(docsrs, doc(cfg(feature = "client_ed25519")))]
pub fn create_response_for_ed25519(_pass: &[u8], _message: &[u8]) -> [u8; 64] {
    #[cfg(not(feature = "client_ed25519"))]
    {
        panic!(
            "Can't create response for `ed25519` authentication plugin — `mysql_common/client_ed25519` feature is disabled."
        )
    }

    #[cfg(feature = "client_ed25519")]
    {
        use curve25519_dalek::scalar::clamp_integer;
        use curve25519_dalek::{EdwardsPoint, Scalar};
        use sha2::Sha512;
        use std::convert::TryInto;

        // Following reference implementation at https://github.com/mysql-net/MySqlConnector/blob/master/src/MySqlConnector.Authentication.Ed25519/Ed25519AuthenticationPlugin.cs#L35
        // with additional guidance from https://www.rfc-editor.org/rfc/rfc8032#section-5.1.5
        // Relying on functions provided by curve25519_dalek

        // hash the provided password
        let hashed_pass = Sha512::default().chain_update(_pass).finalize();

        // the hashed password is split into secret and hash_prefix
        let secret: &[u8; 32] = &hashed_pass[..32].try_into().unwrap();
        let hash_prefix: &[u8; 32] = &hashed_pass[32..].try_into().unwrap();

        // The public key A is the encoding of the point [s]B, where s is the clamped secret
        let small_s = clamp_integer(*secret);
        let reduced_small_s = Scalar::from_bytes_mod_order(small_s);
        let capital_a = EdwardsPoint::mul_base(&reduced_small_s).compress();

        // Compute SHA-512(dom2(F, C) || prefix || PH(M)),
        // dom2 is the empty string,
        // PH(M) = M is the provided message
        let small_r = Sha512::default()
            .chain_update(hash_prefix)
            .chain_update(_message)
            .finalize();

        // Interpret the 64-octet digest as a little-endian integer r.
        let reduced_small_r = Scalar::from_bytes_mod_order_wide(small_r.as_ref());

        // Let R be the encoding of the point [r]B
        let capital_r = EdwardsPoint::mul_base(&reduced_small_r).compress();

        // Compute SHA512(dom2(F, C) || R || A || PH(M))
        // dom2 is the empty string,
        // PH(M) = M is the provided message
        let small_k = Sha512::default()
            .chain_update(&capital_r.to_bytes())
            .chain_update(&capital_a.to_bytes())
            .chain_update(_message)
            .finalize();

        // interpret the 64-octet-digest as a little-endian integer k.
        let reduced_small_k = Scalar::from_bytes_mod_order_wide(small_k.as_ref());

        let capital_s = reduced_small_k * reduced_small_s;
        let capital_s = capital_s + reduced_small_r;

        // Form the signature of the concatenation of R (32 octets) and the
        // little-endian encoding of S (32 octets; the three most
        // significant bits of the final octet are always zero).

        let mut result = [0; 64];
        result[..32].copy_from_slice(capital_r.as_bytes());
        result[32..].copy_from_slice(capital_s.as_bytes());

        result
    }
}

/// Crafting response message for parsec authentication. It consists of a client nonce and signature.
///
/// This will only work if `parsec` feature is enabled and otherwise will panic at runtime.
#[cfg_attr(docsrs, doc(cfg(feature = "client_parsec")))]
pub fn create_response_for_parsec(
    _pass: &[u8],
    _server_nonce: &[u8; 32],
    _iterations: u32,
    _server_salt: &[u8],
) -> [u8; 96] {
    #[cfg(not(feature = "client_parsec"))]
    {
        panic!(
            "Can't create response for `parsec` authentication plugin — `mysql_common/client_parsec` feature is disabled."
        )
    }

    #[cfg(feature = "client_parsec")]
    {
        use ed25519_dalek::{Signer, SigningKey};
        use pbkdf2::pbkdf2_hmac;
        use rand::Rng;
        use sha2::Sha512;

        // Generating derived key using iterations and user specific salt received in parsec ext-salt packet
        let mut derived_key = [0u8; 32];
        pbkdf2_hmac::<Sha512>(_pass, _server_salt, _iterations, &mut derived_key);

        let mut rng = rand::rng();
        // This is the server nonce[0..32] + client nonce[32..64] + signature buffer[64..128]
        // [0..64] - is the message we will need to sign. [32..] - (client nonce + signature) that will be sent to server
        let mut msg_and_signature = [0u8; 128];
        msg_and_signature[..32].copy_from_slice(_server_nonce);
        // First 32 bytes is random client nonce
        let client_nonce = &mut msg_and_signature[32..64];
        rng.fill(client_nonce);

        // The rest 64 bytes is the ed25519 signature of server_nonce + client_nonce using derived key
        let signing_key = SigningKey::from_bytes(&derived_key);

        let signature = signing_key.sign(&msg_and_signature[..64]);
        msg_and_signature[64..].copy_from_slice(&signature.to_bytes());
        msg_and_signature[32..].try_into().expect("infallible")
    }
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
        let output2 = scramble_sha256(&scr, &password);
        assert!(output1.is_some());
        assert!(output2.is_some());
        assert_eq!(
            output1.unwrap(),
            [
                0x09, 0xcf, 0xf8, 0x85, 0x5e, 0x9e, 0x70, 0x53, 0x40, 0xff, 0x22, 0x70, 0xd8, 0xfb,
                0x9f, 0xad, 0xba, 0x90, 0x6b, 0x70,
            ]
        );
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
