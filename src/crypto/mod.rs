pub mod der;
pub mod rsa;

/// Helper function to encrypt mysql password using a public key loaded from a server.
pub fn encrypt(pass: &[u8], key: &[u8]) -> Vec<u8> {
    let pub_key = self::rsa::PublicKey::from_pem(key);
    let rng = ::rand::OsRng::new().unwrap();
    let pad = self::rsa::Pkcs1OaepPadding::new(rng);
    pub_key.encrypt_block(pass, pad)
}
