// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{borrow::Cow, convert::TryFrom, io};

use crate::{
    io::ParseBuf,
    misc::{
        raw::{text::LengthEncodedText, RawText},
        unexpected_buf_eof,
    },
    proto::MyDeserialize,
};

use super::jsonb;

/// An operation kind of a JsonDiff object.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum JsonDiffOperation {
    /// The JSON value in the given path is replaced with a new value.
    REPLACE,
    /// Add a new element at the given path.
    INSERT,
    /// The JSON value at the given path is removed from an array or object.
    REMOVE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
#[error("Unknown JsonDiff operation {}", _0)]
#[repr(transparent)]
pub struct UnknownJsonDiffOperation(pub u8);

impl From<UnknownJsonDiffOperation> for u8 {
    fn from(x: UnknownJsonDiffOperation) -> Self {
        x.0
    }
}

impl TryFrom<u8> for JsonDiffOperation {
    type Error = UnknownJsonDiffOperation;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::REPLACE),
            1 => Ok(Self::INSERT),
            2 => Ok(Self::REMOVE),
            x => Err(UnknownJsonDiffOperation(x)),
        }
    }
}

/// A class that represents a logical change to a JSON document.
///
/// It is used by row-based replication to send information about changes in JSON documents
/// without sending the whole updated document.
#[derive(Debug, Clone, PartialEq)]
pub struct JsonDiff<'a> {
    path: RawText<'a, LengthEncodedText>,
    operation: JsonDiffOperation,
    value: Option<jsonb::Value<'a>>,
}

impl<'a> JsonDiff<'a> {
    /// Returns JsonDiff path as a slice of bytes.
    pub fn path_ref(&self) -> &[u8] {
        self.path.0.as_ref()
    }

    /// Returns JsonDiff path as a string (lossy converted).
    pub fn path_str(&'a self) -> Cow<'a, str> {
        self.path.as_str()
    }

    /// Returns JsonDiff operation.
    pub fn operation(&self) -> JsonDiffOperation {
        self.operation
    }

    /// Returns JsonDiff value (if any).
    pub fn value(&'a self) -> Option<&'a jsonb::Value<'a>> {
        self.value.as_ref()
    }

    /// Returns a `'static` version of `self`.
    pub fn into_owned(self) -> JsonDiff<'static> {
        JsonDiff {
            path: self.path.into_owned(),
            operation: self.operation,
            value: self.value.map(|x| x.into_owned()),
        }
    }
}

impl<'de> MyDeserialize<'de> for JsonDiff<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let operation = buf
            .checked_eat_u8()
            .ok_or_else(unexpected_buf_eof)
            .and_then(|op| {
                JsonDiffOperation::try_from(op)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
            })?;
        let path = RawText::<'_, LengthEncodedText>::deserialize((), &mut *buf)?;
        let value = if operation != JsonDiffOperation::REMOVE {
            let value_len = buf
                .checked_eat_lenenc_int()
                .ok_or_else(unexpected_buf_eof)? as usize;
            let mut value_buf = buf
                .checked_eat_buf(value_len)
                .ok_or_else(unexpected_buf_eof)?;
            Some(jsonb::Value::deserialize((), &mut value_buf)?)
        } else {
            None
        };

        Ok(Self {
            path,
            operation,
            value,
        })
    }
}
