// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use super::{Deserialized, DeserializedIr, Serialized};
use rustc_serialize::json::{self, Json};
use rustc_serialize::{Decodable, Encodable};
use std::str::{from_utf8, from_utf8_unchecked};
use value::convert::{ConvIr, FromValue, FromValueError};
use value::Value;

impl From<Json> for Value {
    fn from(x: Json) -> Value {
        Value::Bytes(json::encode(&x).unwrap().into())
    }
}

impl<T: Encodable> From<Serialized<T>> for Value {
    fn from(x: Serialized<T>) -> Value {
        Value::Bytes(json::encode(&x.0).unwrap().into())
    }
}

impl<T: Decodable> ConvIr<Deserialized<T>> for DeserializedIr<T> {
    fn new(v: Value) -> Result<DeserializedIr<T>, FromValueError> {
        let (output, bytes) = {
            let bytes = match v {
                Value::Bytes(bytes) => match from_utf8(&*bytes) {
                    Ok(_) => bytes,
                    Err(_) => return Err(FromValueError(Value::Bytes(bytes))),
                },
                v => return Err(FromValueError(v)),
            };
            let output = {
                match json::decode(unsafe { from_utf8_unchecked(&*bytes) }) {
                    Ok(output) => output,
                    Err(_) => return Err(FromValueError(Value::Bytes(bytes))),
                }
            };
            (output, bytes)
        };
        Ok(DeserializedIr {
            bytes: bytes,
            output: Deserialized(output),
        })
    }

    fn commit(self) -> Deserialized<T> {
        self.output
    }

    fn rollback(self) -> Value {
        Value::Bytes(self.bytes)
    }
}

impl<T: Decodable> FromValue for Deserialized<T> {
    type Intermediate = DeserializedIr<T>;
}

/// Intermediate result of a Value-to-Json conversion.
#[derive(Debug)]
pub struct JsonIr {
    bytes: Vec<u8>,
    output: Json,
}

impl ConvIr<Json> for JsonIr {
    fn new(v: Value) -> Result<JsonIr, FromValueError> {
        let (output, bytes) = {
            let bytes = match v {
                Value::Bytes(bytes) => match from_utf8(&*bytes) {
                    Ok(_) => bytes,
                    Err(_) => return Err(FromValueError(Value::Bytes(bytes))),
                },
                v => return Err(FromValueError(v)),
            };
            let output = {
                match Json::from_str(unsafe { from_utf8_unchecked(&*bytes) }) {
                    Ok(output) => output,
                    Err(_) => return Err(FromValueError(Value::Bytes(bytes))),
                }
            };
            (output, bytes)
        };
        Ok(JsonIr {
            bytes: bytes,
            output: output,
        })
    }

    fn commit(self) -> Json {
        self.output
    }

    fn rollback(self) -> Value {
        Value::Bytes(self.bytes)
    }
}

impl FromValue for Json {
    type Intermediate = JsonIr;
}
