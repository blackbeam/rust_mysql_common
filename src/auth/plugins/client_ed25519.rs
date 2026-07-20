pub const NONCE_BYTES: usize = 32;

#[derive(Debug, Default)]
pub struct ClientEd25519(());

#[cfg_attr(docsrs, doc(cfg(feature = "client_ed25519")))]
#[cfg(feature = "client_ed25519")]
impl super::ChallengeResponsePlugin for ClientEd25519 {
    fn run<C: super::Context>(
        &mut self,
        ctx: C,
        challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        use curve25519_dalek::scalar::clamp_integer;
        use curve25519_dalek::{EdwardsPoint, Scalar};
        use sha2::{Digest as _, Sha512};
        use std::convert::TryInto;

        let Ok(nonce) = <[u8; NONCE_BYTES]>::try_from(challenge) else {
            return Err(super::Error::Challenge);
        };

        // Following reference implementation at https://github.com/mysql-net/MySqlConnector/blob/master/src/MySqlConnector.Authentication.Ed25519/Ed25519AuthenticationPlugin.cs#L35
        // with additional guidance from https://www.rfc-editor.org/rfc/rfc8032#section-5.1.5
        // Relying on functions provided by curve25519_dalek

        // hash the provided password
        let hashed_pass = Sha512::default().chain_update(ctx.pass()).finalize();

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
            .chain_update(nonce)
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
            .chain_update(nonce)
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

        Ok(super::Response::last(result.to_vec()))
    }

    fn password_hash<C: super::Context>(&self, ctx: C) -> Option<Vec<u8>> {
        use curve25519_dalek::{EdwardsPoint, Scalar, scalar::clamp_integer};
        use sha2::{Digest, Sha512};

        let az = Sha512::default().chain_update(ctx.pass()).finalize();
        let secret: [u8; 32] = az[..32].try_into().unwrap();
        let small_s = Scalar::from_bytes_mod_order(clamp_integer(secret));
        Some(
            EdwardsPoint::mul_base(&small_s)
                .compress()
                .to_bytes()
                .to_vec(),
        )
    }
}

#[cfg(not(feature = "client_ed25519"))]
impl super::ChallengeResponsePlugin for ClientEd25519 {
    fn run<C: super::Context>(
        &mut self,
        _ctx: C,
        _challenge: &[u8],
    ) -> Result<super::Response, super::Error> {
        Err(super::Error::FeatureRequired("mysql_common/client_ed25519"))
    }

    fn password_hash<C: super::Context>(&self, _ctx: C) -> Option<Vec<u8>> {
        None
    }
}
