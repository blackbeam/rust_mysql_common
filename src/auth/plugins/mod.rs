use crate::{
    auth::plugins::{
        caching_sha2_password::CachingSha2Password, client_ed25519::ClientEd25519,
        mysql_clear_password::MysqlClearPassword, mysql_native_password::MysqlNativePassword,
        mysql_old_password::MysqlOldPassword, parsec::Parsec, sha256_password::Sha256Password,
    },
    packets::AuthPlugin,
};

pub mod caching_sha2_password;
pub mod client_ed25519;
pub mod mysql_clear_password;
pub mod mysql_native_password;
pub mod mysql_old_password;
pub mod parsec;
pub mod sha256_password;

/// Initialized authentication procedure.
///
/// Proxy for all the supproted plugins.
#[derive(Debug)]
pub enum AuthProc {
    CachingSha2Password(CachingSha2Password),
    #[cfg_attr(docsrs, doc(cfg(feature = "client_ed25519")))]
    ClientEd25519(ClientEd25519),
    MysqlClearPassword(MysqlClearPassword),
    MysqlNativePassword(MysqlNativePassword),
    MysqlOldPassword(MysqlOldPassword),
    Sha256Password(Sha256Password),
    #[cfg_attr(docsrs, doc(cfg(feature = "client_parsec")))]
    Parsec(Parsec),
}

#[derive(Debug, thiserror::Error)]
pub enum PluginInitError {
    #[error("Unsopported plugin `{}`", String::from_utf8_lossy(&_0))]
    UnsupportedPlugin(Vec<u8>),
}

impl AuthProc {
    pub fn init(plugin: &AuthPlugin<'_>) -> Result<AuthProc, PluginInitError> {
        match plugin {
            AuthPlugin::MysqlOldPassword => Ok(Self::MysqlOldPassword(Default::default())),
            AuthPlugin::MysqlClearPassword => Ok(Self::MysqlClearPassword(Default::default())),
            AuthPlugin::MysqlNativePassword => Ok(Self::MysqlNativePassword(Default::default())),
            AuthPlugin::CachingSha2Password => Ok(Self::CachingSha2Password(Default::default())),
            AuthPlugin::Ed25519 => Ok(Self::ClientEd25519(Default::default())),
            AuthPlugin::Parsec => Ok(Self::Parsec(Default::default())),
            AuthPlugin::Sha256Password => Ok(Self::Sha256Password(Default::default())),
            AuthPlugin::Other(name) => {
                Err(PluginInitError::UnsupportedPlugin(name.as_ref().to_owned()))
            }
        }
    }
}

impl ChallengeResponsePlugin for AuthProc {
    fn run<C: Context>(&mut self, ctx: C, challenge: &[u8]) -> Result<Response, Error> {
        match self {
            AuthProc::CachingSha2Password(x) => x.run(ctx, challenge),
            AuthProc::ClientEd25519(x) => x.run(ctx, challenge),
            AuthProc::MysqlClearPassword(x) => x.run(ctx, challenge),
            AuthProc::MysqlNativePassword(x) => x.run(ctx, challenge),
            AuthProc::MysqlOldPassword(x) => x.run(ctx, challenge),
            AuthProc::Sha256Password(x) => x.run(ctx, challenge),
            AuthProc::Parsec(x) => x.run(ctx, challenge),
        }
    }

    fn password_hash<C: Context>(&self, ctx: C) -> Option<Vec<u8>> {
        match self {
            AuthProc::CachingSha2Password(x) => x.password_hash(ctx),
            AuthProc::ClientEd25519(x) => x.password_hash(ctx),
            AuthProc::MysqlClearPassword(x) => x.password_hash(ctx),
            AuthProc::MysqlNativePassword(x) => x.password_hash(ctx),
            AuthProc::MysqlOldPassword(x) => x.password_hash(ctx),
            AuthProc::Sha256Password(x) => x.password_hash(ctx),
            AuthProc::Parsec(x) => x.password_hash(ctx),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("The order of challenges/responses given does not match the plugin logic")]
    Logic,
    #[error("Password does not match plugin requirements")]
    Password,
    #[error("Bad auth plugin challenge")]
    Challenge,
    #[error("`{_0}` feature must be enabled for this plugin to work")]
    FeatureRequired(&'static str),
    #[error(transparent)]
    Other(Box<dyn std::error::Error + Send + Sync + 'static>),
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FeatureRequired(l0), Self::FeatureRequired(r0)) => l0 == r0,
            (Self::Other(_), Self::Other(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

pub trait Context {
    /// User password
    fn pass(&self) -> &[u8];
    /// Whether the transport is IPC.
    fn is_ipc_transport(&self) -> bool;
    /// Whether the transport is TLS
    fn is_tls_transport(&self) -> bool;
    /// Initial handshake scramble.
    fn scramble(&self) -> &[u8];
    /// The public key server uses for RSA-based key exchange
    fn server_key_pem(&self) -> Option<&[u8]>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Response {
    /// Plugin response asking for the next challenge
    Next {
        /// Optional packet to send to the server.
        packet: Option<Vec<u8>>,
    },
    /// Last plugin response
    Last {
        /// Optional packet to send to the server.
        packet: Option<Vec<u8>>,
    },
}

impl Response {
    /// Convenience function returning empty packet response asking for anothe challenge.
    pub fn next_empty() -> Self {
        Self::Next {
            packet: Some(Vec::new()),
        }
    }

    /// Convenience function asking for another challenge.
    pub fn next(data: Vec<u8>) -> Self {
        Self::Next { packet: Some(data) }
    }

    /// Convenience function returning the last empty packet response.
    pub fn last_empty() -> Self {
        Self::Last {
            packet: Some(Vec::new()),
        }
    }

    /// Convenience function returning the last response.
    pub fn last(data: Vec<u8>) -> Self {
        Self::Last { packet: Some(data) }
    }

    /// Convenience function returning no response.
    pub fn last_none() -> Self {
        Self::Last { packet: None }
    }

    /// Returns the data to send to the server (if any).
    pub fn data(&self) -> Option<&[u8]> {
        match self {
            Response::Next { packet } | Response::Last { packet } => packet.as_deref(),
        }
    }
}

pub trait ChallengeResponsePlugin {
    /// Runs a single step of a challenge-response authentication.
    ///
    /// Note that the caller must properly preprocess the challenge, namely:
    ///
    /// * abort on error packet
    /// * handle auth switch request
    /// * remove leading 0x01 byte added by the server to escape the binary data
    fn run<C: Context>(&mut self, ctx: C, challenge: &[u8]) -> Result<Response, Error>;

    /// Returns a password hash for MariaDb "zero-config TLS" feature.
    /// This only makes sense if authentication was completes successfully.
    ///
    /// Returns `None` if called mid-authentication or if plugin does not support the feature.
    fn password_hash<C: Context>(&self, ctx: C) -> Option<Vec<u8>>;
}
