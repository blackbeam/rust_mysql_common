// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{error::Error, io};

use crate::io::ParseBuf;

pub mod codec;
pub mod sync_framed;

/// Text protocol marker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Text;

/// Binary protocol marker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Binary;

#[derive(Debug, thiserror::Error)]
#[error("Error deserializing `{}`: {}", type_name, cause)]
pub struct DeserializeError {
    pub type_name: &'static str,
    #[source]
    pub cause: Box<dyn std::error::Error + Send + Sync + 'static>,
}

impl DeserializeError {
    pub fn new<T, E: std::error::Error + Send + Sync + 'static>(e: E) -> io::Error {
        io::Error::new(
            io::ErrorKind::Other,
            Self {
                type_name: std::any::type_name::<T>(),
                cause: Box::new(e),
            },
        )
    }

    pub fn actual_kind(mut err: &io::Error) -> io::ErrorKind {
        let mut kind;
        loop {
            kind = err.kind();
            match err.source() {
                Some(e) => match e.downcast_ref::<io::Error>() {
                    Some(e) => {
                        err = e;
                    }
                    None => return kind,
                },
                None => return kind,
            }
        }
    }
}

/// Serialization for various MySql types.
pub trait MySerialize {
    /// Serializes self into the `buf`.
    fn serialize(&self, buf: &mut Vec<u8>);
}

/// Deserialization for various MySql types.
pub trait MyDeserialize<'de>: Sized {
    /// Size hint of a serialized value (in bytes), if it's constant.
    const SIZE: Option<usize>;

    /// Some structs defines deserialization in the context of another value.
    ///
    /// Use `()` here if the deserialization procedure is defined without premises.
    type Ctx;

    /// Deserializes self from the given `buf`.
    ///
    /// Imlementation must consume corresponding amount of bytes from the `buf`.
    ///
    /// # Panic
    ///
    /// Implementation must panic on insufficient buffer length if `Self::SIZE.is_some()`.
    /// One should use `ParseBuf::checked_parse` for checked deserialization.
    fn deserialize(ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        Self::deserialize_(ctx, buf).map_err(DeserializeError::new::<Self, _>)
    }

    fn deserialize_(ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self>;
}
