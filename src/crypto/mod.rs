// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

pub mod der;
pub mod rsa;

/// Helper function to encrypt mysql password using a public key loaded from a server.
///
/// It will use OAEP padding, so MySql versions prior to 8.0.5 are not supported.
pub fn encrypt(pass: &[u8], key: &[u8]) -> Vec<u8> {
    let pub_key = self::rsa::PublicKey::from_pem(key);
    let pad = self::rsa::Pkcs1OaepPadding::new(self::rsa::GetRandom);
    pub_key.encrypt_block(pass, pad)
}

/// Verifies MariaDB shared secret generated for zero-config TLS validation.
///
/// The expected shared secret is computed as:
/// `SHA256(password_hash || scramble || leaf_cert_fingerprint)`.
pub fn verify_mariadb_shared_secret(
    server_shared_secret: &[u8],
    password_hash: &[u8],
    scramble: &[u8],
    leaf_cert_fingerprint: &[u8],
) -> bool {
    use sha2::{Digest, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(password_hash);
    hasher.update(scramble);
    hasher.update(leaf_cert_fingerprint);
    let expected = hasher.finalize();

    // Server sends uppercase hex string - getting the same and comparing.
    let expected_hex = hex::encode_upper(expected);
    expected_hex.as_bytes() == server_shared_secret
}

/// The class to check if standard TLS validation failed, requiring a fallback
/// to MariaDB's "zero-config TLS" shared secret verification. It also holds the required piece of
/// info for that - the fingerprint of the server's leaf certificate
pub struct MariaDbZeroConfigCheck {
    leaf_cert_fingerprint: Option<Vec<u8>>,
}

impl MariaDbZeroConfigCheck {
    /// Creates a new `MariaDbZeroConfigCheck` instance.
    pub fn new(leaf_cert_fingerprint: Option<Vec<u8>>) -> Self {
        Self {
            leaf_cert_fingerprint,
        }
    }

    /// Returns true if standard verification failed
    pub fn requires_zeroconfig_fallback(&self) -> bool {
        self.leaf_cert_fingerprint.is_some()
    }

    /// Returns the fingerprint of the server's leaf certificate
    pub fn leaf_cert_fingerprint(&self) -> &[u8] {
        self.leaf_cert_fingerprint
            .as_deref()
            .expect("leaf_cert_fingerprint must not be requested if standard validation passed")
    }
}
