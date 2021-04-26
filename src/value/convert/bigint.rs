// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module implements conversion from/to `Value` for `BigInt` and `BigUint` types.

use num_bigint::{BigInt, BigUint};
use num_traits::{FromPrimitive, ToPrimitive};

use super::{ConvIr, FromValue, FromValueError, ParseIr, Value};

impl ConvIr<BigInt> for ParseIr<BigInt> {
    fn new(v: Value) -> Result<Self, FromValueError> {
        match v {
            Value::Int(x) => Ok(ParseIr {
                value: Value::Int(x),
                output: x.into(),
            }),
            Value::UInt(x) => Ok(ParseIr {
                value: Value::UInt(x),
                output: x.into(),
            }),
            Value::Bytes(bytes) => match BigInt::parse_bytes(&*bytes, 10) {
                Some(x) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output: x,
                }),
                None => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> BigInt {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl FromValue for BigInt {
    type Intermediate = ParseIr<BigInt>;
    fn from_value(v: Value) -> BigInt {
        <_>::from_value_opt(v).expect("Could not retrieve BigInt from Value")
    }
}

impl From<BigInt> for Value {
    fn from(x: BigInt) -> Value {
        if let Some(x) = x.to_i64() {
            Value::Int(x)
        } else if let Some(x) = x.to_u64() {
            Value::UInt(x)
        } else {
            Value::Bytes(x.to_string().into())
        }
    }
}

impl ConvIr<BigUint> for ParseIr<BigUint> {
    fn new(v: Value) -> Result<Self, FromValueError> {
        match v {
            Value::Int(x) => {
                if let Some(parsed) = <_>::from_i64(x) {
                    Ok(ParseIr {
                        value: Value::Int(x),
                        output: parsed,
                    })
                } else {
                    Err(FromValueError(Value::Int(x)))
                }
            }
            Value::UInt(x) => Ok(ParseIr {
                value: Value::UInt(x),
                output: x.into(),
            }),
            Value::Bytes(bytes) => match BigUint::parse_bytes(&*bytes, 10) {
                Some(x) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output: x,
                }),
                None => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> BigUint {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl FromValue for BigUint {
    type Intermediate = ParseIr<BigUint>;
    fn from_value(v: Value) -> BigUint {
        <_>::from_value_opt(v).expect("Could not retrieve BigUint from Value")
    }
}

impl From<BigUint> for Value {
    fn from(x: BigUint) -> Value {
        if let Some(x) = x.to_u64() {
            Value::UInt(x)
        } else {
            Value::Bytes(x.to_string().into())
        }
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::{BigInt, BigUint};
    use proptest::prelude::*;

    use crate::value::{convert::from_value, Value};

    proptest! {
        #[test]
        fn big_int_roundtrip(
            bytes_pos in r"[1-9][0-9]{31}",
            bytes_neg in r"-[1-9][0-9]{31}",
            uint in (i64::max_value() as u64 + 1)..u64::max_value(),
            int: i64,
        ) {
            let val_bytes_pos = Value::Bytes(bytes_pos.as_bytes().into());
            let val_bytes_neg = Value::Bytes(bytes_neg.as_bytes().into());
            let val_uint = Value::UInt(uint);
            let val_int = Value::Int(int);

            assert_eq!(Value::from(from_value::<BigInt>(val_bytes_pos.clone())), val_bytes_pos);
            assert_eq!(Value::from(from_value::<BigInt>(val_bytes_neg.clone())), val_bytes_neg);
            assert_eq!(Value::from(from_value::<BigInt>(val_uint.clone())), val_uint);
            assert_eq!(Value::from(from_value::<BigInt>(val_int.clone())), val_int);
        }

        #[test]
        fn big_uint_roundtrip(
            bytes in r"[1-9][0-9]{31}",
            uint: u64,
            int in 0i64..i64::max_value(),
        ) {
            let val_bytes = Value::Bytes(bytes.as_bytes().into());
            let val_uint = Value::UInt(uint);
            let val_int = Value::Int(int);

            assert_eq!(Value::from(from_value::<BigUint>(val_bytes.clone())), val_bytes);
            assert_eq!(Value::from(from_value::<BigUint>(val_uint.clone())), val_uint);
            assert_eq!(Value::from(from_value::<BigUint>(val_int)), Value::UInt(int as u64));
        }
    }
}
