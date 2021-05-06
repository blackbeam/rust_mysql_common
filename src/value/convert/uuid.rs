// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module implements conversion from/to `Value` for `Uuid`.

#![cfg(feature = "uuid")]

use uuid::Uuid;

use crate::value::Value;

use super::{ConvIr, FromValue, FromValueError};

impl From<Uuid> for Value {
    fn from(uuid: Uuid) -> Value {
        Value::Bytes(uuid.as_bytes().to_vec())
    }
}

/// Intermediate result of a Value-to-Uuid conversion.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UuidIr {
    val: Uuid,
    bytes: Vec<u8>,
}

impl ConvIr<Uuid> for UuidIr {
    fn new(v: Value) -> Result<UuidIr, FromValueError> {
        match v {
            Value::Bytes(bytes) => match Uuid::from_slice(bytes.as_slice()) {
                Ok(val) => Ok(UuidIr { val, bytes }),
                Err(_) => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Uuid {
        self.val
    }
    fn rollback(self) -> Value {
        Value::Bytes(self.bytes)
    }
}

impl FromValue for Uuid {
    type Intermediate = UuidIr;
}
