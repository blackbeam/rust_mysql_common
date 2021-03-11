// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{borrow::Cow, cmp::min, fmt, io, marker::PhantomData};

use bytes::BufMut;

use crate::{
    io::{BufMutExt, ParseBuf},
    misc::unexpected_buf_eof,
};

/// Wrapper for a raw text value, that came from a server.
///
/// `T` encodes the serialized representation.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct RawText<'a, T>(pub Cow<'a, [u8]>, PhantomData<T>);

impl<'a, T> RawText<'a, T> {
    /// Wraps the given value.
    pub fn new(text: impl Into<Cow<'a, [u8]>>) -> Self {
        Self(text.into(), PhantomData)
    }

    /// Converts self to a 'static version.
    pub fn into_owned(self) -> RawText<'static, T> {
        RawText(Cow::Owned(self.0.into_owned()), PhantomData)
    }

    /// Returns the value as a UTF-8 string (lossy contverted).
    pub fn as_str(&'a self) -> Cow<'a, str> {
        let slice = self.0.as_ref();
        match slice.iter().position(|c| *c == 0) {
            Some(position) => String::from_utf8_lossy(&slice[..position]),
            None => String::from_utf8_lossy(slice),
        }
    }
}

impl<T> fmt::Debug for RawText<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

/// Serialized text representation.
pub trait TextRepr {
    type Ctx;

    fn serialize(text: &[u8], buf: &mut Vec<u8>);
    fn deserialize<'de>(ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>>;
}

/// Length-encoded text.
///
/// Text prepended with it's length as a length-encoded integer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LengthEncodedText;

impl TextRepr for LengthEncodedText {
    type Ctx = ();

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        buf.put_lenenc_int(text.len() as u64);
        buf.put_slice(text);
    }

    fn deserialize<'de>((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        let len = buf
            .checked_eat_lenenc_int()
            .ok_or_else(unexpected_buf_eof)?;
        buf.checked_eat(len as usize)
            .map(Cow::Borrowed)
            .ok_or_else(unexpected_buf_eof)
    }
}

/// A text prepended by it's u8 length.
///
/// Truncates text if its too length.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct U8Text;

impl TextRepr for U8Text {
    type Ctx = ();

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        buf.put_u8_str(text);
    }

    fn deserialize<'de>((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        buf.checked_eat_u8_str()
            .map(Cow::Borrowed)
            .ok_or_else(unexpected_buf_eof)
    }
}

/// A text prepended by it's u32 length.
///
/// Truncates text if its too length.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct U32Text;

impl TextRepr for U32Text {
    type Ctx = ();

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        buf.put_u32_str(text);
    }

    fn deserialize<'de>((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        buf.checked_eat_u32_str()
            .map(Cow::Borrowed)
            .ok_or_else(unexpected_buf_eof)
    }
}

/// Null-terminated text.
///
/// Deserialization will error with `InvalidData` if there is no `0`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NullText;

impl TextRepr for NullText {
    type Ctx = ();

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        let last = text
            .iter()
            .position(|x| *x == 0)
            .unwrap_or_else(|| text.len());
        buf.put_slice(&text[..last]);
        buf.put_u8(0);
    }

    fn deserialize<'de>((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        match buf.0.iter().position(|x| *x == 0) {
            Some(i) => {
                let out = buf.eat(i);
                buf.eat_u8();
                Ok(Cow::Borrowed(out))
            }
            None => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "no null terminator for null-terminated string",
            )),
        }
    }
}

/// Text stored from the current position to the end of the buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EofText;

impl TextRepr for EofText {
    type Ctx = ();

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        buf.put_slice(text);
    }

    fn deserialize<'de>((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        Ok(Cow::Borrowed(buf.eat_all()))
    }
}

/// Text without length.
///
/// Its length is stored somewhere else.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BareText;

impl TextRepr for BareText {
    type Ctx = usize;

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        buf.put_slice(text);
    }

    fn deserialize<'de>(len: usize, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        buf.checked_eat(len)
            .ok_or_else(unexpected_buf_eof)
            .map(Cow::Borrowed)
    }
}

/// Fixed length text (zero-padded to the left).
///
/// Truncates text if its too length.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FixedLengthText<const LEN: usize>;

impl<const LEN: usize> TextRepr for FixedLengthText<LEN> {
    type Ctx = ();

    fn serialize(text: &[u8], buf: &mut Vec<u8>) {
        let len = min(LEN, text.len());
        buf.put_slice(&text[..len]);
        for _ in 0..(LEN - len) {
            buf.put_u8(0);
        }
    }

    fn deserialize<'de>((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Cow<'de, [u8]>> {
        buf.checked_eat(LEN)
            .map(Cow::Borrowed)
            .ok_or_else(unexpected_buf_eof)
    }
}
