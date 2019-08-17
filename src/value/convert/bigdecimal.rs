// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module implements conversion from/to `Value` for `BigDecimal` type.

use bigdecimal::BigDecimal;

use super::{ConvIr, FromValue, FromValueError, ParseIr, Value};

impl ConvIr<BigDecimal> for ParseIr<BigDecimal> {
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
            Value::Float(x) => Ok(ParseIr {
                value: Value::Float(x),
                output: x.into(),
            }),
            Value::Bytes(bytes) => match BigDecimal::parse_bytes(&*bytes, 10) {
                Some(x) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output: x,
                }),
                None => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> BigDecimal {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl FromValue for BigDecimal {
    type Intermediate = ParseIr<BigDecimal>;
    fn from_value(v: Value) -> BigDecimal {
        <_>::from_value_opt(v)
            .ok()
            .expect("Could not retrieve BigDecimal from Value")
    }
}

impl From<BigDecimal> for Value {
    fn from(big_decimal: BigDecimal) -> Value {
        Value::Bytes(big_decimal.to_string().into())
    }
}

#[cfg(test)]
mod tests {
    use bigdecimal::BigDecimal;
    use proptest::prelude::*;

    use crate::value::convert::from_value;
    use crate::value::Value;

    proptest! {
        #[test]
        fn big_decimal_roundtrip(
            sign in r"-?",
            m in r"[0-9]{1,38}",
            d in r"[0-9]{0,38}",
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
            let decimal = from_value::<BigDecimal>(val.clone());
            let val2 = Value::from(decimal);
            assert_eq!(val, val2);
        }
    }
}
