// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module implements conversion from/to `Value` for `Decimal` type.

#![cfg(feature = "rust_decimal")]

use rust_decimal::Decimal;

use std::str::{from_utf8, FromStr};

use super::{ConvIr, FromValue, FromValueError, ParseIr, Value};

impl ConvIr<Decimal> for ParseIr<Decimal> {
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
            Value::Bytes(bytes) => match from_utf8(&*bytes) {
                Ok(x) => match Decimal::from_str(x) {
                    Ok(x) => Ok(ParseIr {
                        value: Value::Bytes(bytes),
                        output: x,
                    }),
                    Err(_) => Err(FromValueError(Value::Bytes(bytes))),
                },
                Err(_) => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Decimal {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl FromValue for Decimal {
    type Intermediate = ParseIr<Decimal>;
    fn from_value(v: Value) -> Decimal {
        <_>::from_value_opt(v).expect("Could not retrieve Decimal from Value")
    }
}

impl From<Decimal> for Value {
    fn from(decimal: Decimal) -> Value {
        Value::Bytes(decimal.to_string().into())
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rust_decimal::Decimal;

    use super::super::*;

    proptest! {
        #[test]
        fn decimal_roundtrip(
            sign in r"-?",
            m in r"[0-7][0-9]{1,13}",
            d in r"[0-9]{0,14}",
        ) {
            let m = match m.trim_start_matches('0') {
                "" => "0",
                m => m,
            };
            let sign = if m == "0" && d.chars().all(|b| b == '0') {
                String::new()
            } else {
                sign
            };
            let d = if d.is_empty() {
                String::new()
            } else {
                format!(".{}", d)
            };
            let num = format!("{}{}{}", sign, m , d);
            let val = Value::Bytes(num.as_bytes().to_vec());
            let decimal = from_value::<Decimal>(val.clone());
            let val2 = Value::from(decimal);
            assert_eq!(val, val2);
        }
    }
}
