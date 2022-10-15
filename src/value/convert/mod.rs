// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use lexical::parse;
use num_traits::{FromPrimitive, ToPrimitive};
use regex::bytes::Regex;

use std::{
    any::type_name, borrow::Cow, convert::TryInto, error::Error, fmt, str::from_utf8,
    time::Duration,
};

use crate::value::Value;

macro_rules! impl_from_value {
    ($ty:ty, $ir:ty) => {
        impl crate::value::convert::FromValue for $ty {
            type Intermediate = $ir;
        }
    };
}

pub mod bigdecimal;
pub mod bigdecimal03;
pub mod bigint;
pub mod chrono;
pub mod decimal;
pub mod time;
pub mod time03;
pub mod uuid;

lazy_static::lazy_static! {
    static ref DATETIME_RE_YMD: Regex = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    static ref DATETIME_RE_YMD_HMS: Regex =
        Regex::new(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$").unwrap();
    static ref DATETIME_RE_YMD_HMS_NS: Regex =
        Regex::new(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{1,6}$").unwrap();
    static ref TIME_RE_HH_MM_SS: Regex = Regex::new(r"^\d{2}:[0-5]\d:[0-5]\d$").unwrap();
    static ref TIME_RE_HH_MM_SS_MS: Regex =
        Regex::new(r"^\d{2}:[0-5]\d:[0-5]\d\.\d{1,6}$").unwrap();
    static ref TIME_RE_HHH_MM_SS: Regex = Regex::new(r"^[0-8]\d\d:[0-5]\d:[0-5]\d$").unwrap();
    static ref TIME_RE_HHH_MM_SS_MS: Regex =
        Regex::new(r"^[0-8]\d\d:[0-5]\d:[0-5]\d\.\d{1,6}$").unwrap();
}

/// Returns (year, month, day, hour, minute, second, micros)
#[cfg(any(feature = "chrono", all(feature = "time", test)))]
fn parse_mysql_datetime_string(bytes: &[u8]) -> Option<(u32, u32, u32, u32, u32, u32, u32)> {
    let len = bytes.len();

    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    #[repr(u8)]
    enum DateTimeKind {
        Ymd = 0,
        YmdHms,
        YmdHmsMs,
    }

    let kind = if len == 10 && DATETIME_RE_YMD.is_match(bytes) {
        DateTimeKind::Ymd
    } else if len == 19 && DATETIME_RE_YMD_HMS.is_match(bytes) {
        DateTimeKind::YmdHms
    } else if 20 < len && len < 27 && DATETIME_RE_YMD_HMS_NS.is_match(bytes) {
        DateTimeKind::YmdHmsMs
    } else {
        return None;
    };

    let (year, month, day, hour, minute, second, micros) = match kind {
        DateTimeKind::Ymd => (..4, 5..7, 8..10, None, None, None, None),
        DateTimeKind::YmdHms => (
            ..4,
            5..7,
            8..10,
            Some(11..13),
            Some(14..16),
            Some(17..19),
            None,
        ),
        DateTimeKind::YmdHmsMs => (
            ..4,
            5..7,
            8..10,
            Some(11..13),
            Some(14..16),
            Some(17..19),
            Some(20..),
        ),
    };

    Some((
        parse(&bytes[year]).unwrap(),
        parse(&bytes[month]).unwrap(),
        parse(&bytes[day]).unwrap(),
        hour.map(|pos| parse(&bytes[pos]).unwrap()).unwrap_or(0),
        minute.map(|pos| parse(&bytes[pos]).unwrap()).unwrap_or(0),
        second.map(|pos| parse(&bytes[pos]).unwrap()).unwrap_or(0),
        micros.map(|pos| parse_micros(&bytes[pos])).unwrap_or(0),
    ))
}

/// `FromValue` conversion error.
#[derive(Debug, Clone, PartialEq)]
pub struct FromValueError(pub Value);

impl fmt::Display for FromValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Couldn't convert the value `{:?}` to a desired type",
            self.0
        )
    }
}

impl Error for FromValueError {
    fn description(&self) -> &str {
        "Couldn't convert the value to a desired type"
    }
}

/// Basic operations on `FromValue` conversion intermediate result.
///
/// See [`FromValue`](trait.FromValue.html)
pub trait ConvIr<T>: Sized {
    fn new(v: Value) -> Result<Self, FromValueError>;
    fn commit(self) -> T;
    fn rollback(self) -> Value;
}

/// Implement this trait to convert value to something.
///
/// `FromRow` requires ability to cheaply rollback `FromValue` conversion. This ability is
/// provided via `Intermediate` associated type.
///
/// Example implementation:
///
/// ```ignore
/// #[derive(Debug)]
/// pub struct StringIr {
///     bytes: Vec<u8>,
/// }
///
/// impl ConvIr<String> for StringIr {
///     fn new(v: Value) -> MyResult<StringIr> {
///         match v {
///             Value::Bytes(bytes) => match from_utf8(&*bytes) {
///                 Ok(_) => Ok(StringIr { bytes: bytes }),
///                 Err(_) => Err(Error::FromValueError(Value::Bytes(bytes))),
///             },
///             v => Err(Error::FromValueError(v)),
///         }
///     }
///     fn commit(self) -> String {
///         unsafe { String::from_utf8_unchecked(self.bytes) }
///     }
///     fn rollback(self) -> Value {
///         Value::Bytes(self.bytes)
///     }
/// }
///
/// impl FromValue for String {
///     type Intermediate = StringIr;
/// }
/// ```
pub trait FromValue: Sized {
    type Intermediate: ConvIr<Self>;

    /// Will panic if could not convert `v` to `Self`.
    fn from_value(v: Value) -> Self {
        match Self::from_value_opt(v) {
            Ok(this) => this,
            Err(_) => panic!("Could not retrieve {} from Value", type_name::<Self>()),
        }
    }

    /// Will return `Err(Error::FromValueError(v))` if could not convert `v` to `Self`.
    fn from_value_opt(v: Value) -> Result<Self, FromValueError> {
        let ir = Self::Intermediate::new(v)?;
        Ok(ir.commit())
    }

    /// Will return `Err(Error::FromValueError(v))` if `v` is not convertible to `Self`.
    fn get_intermediate(v: Value) -> Result<Self::Intermediate, FromValueError> {
        Self::Intermediate::new(v)
    }
}

/// Will panic if could not convert `v` to `T`
pub fn from_value<T: FromValue>(v: Value) -> T {
    FromValue::from_value(v)
}

/// Will return `Err(FromValueError(v))` if could not convert `v` to `T`
pub fn from_value_opt<T: FromValue>(v: Value) -> Result<T, FromValueError> {
    FromValue::from_value_opt(v)
}

macro_rules! impl_from_value_num {
    ($t:ident) => {
        impl ConvIr<$t> for ParseIr<$t> {
            fn new(v: Value) -> Result<ParseIr<$t>, FromValueError> {
                match v {
                    Value::Int(x) => {
                        if let Some(output) = $t::from_i64(x) {
                            Ok(ParseIr {
                                value: Value::Int(x),
                                output,
                            })
                        } else {
                            Err(FromValueError(Value::Int(x)))
                        }
                    }
                    Value::UInt(x) => {
                        if let Some(output) = $t::from_u64(x) {
                            Ok(ParseIr {
                                value: Value::UInt(x),
                                output,
                            })
                        } else {
                            Err(FromValueError(Value::UInt(x)))
                        }
                    }
                    Value::Bytes(bytes) => match parse(&*bytes) {
                        Ok(x) => Ok(ParseIr {
                            value: Value::Bytes(bytes),
                            output: x,
                        }),
                        _ => Err(FromValueError(Value::Bytes(bytes))),
                    },
                    v => Err(FromValueError(v)),
                }
            }
            fn commit(self) -> $t {
                self.output
            }
            fn rollback(self) -> Value {
                self.value
            }
        }

        impl_from_value!($t, ParseIr<$t>);
    };
}

/// Intermediate result of a Value-to-Option<T> conversion.
#[derive(Debug, Clone, PartialEq)]
pub struct OptionIr<T> {
    value: Option<Value>,
    ir: Option<T>,
}

impl<T, Ir> ConvIr<Option<T>> for OptionIr<Ir>
where
    T: FromValue<Intermediate = Ir>,
    Ir: ConvIr<T>,
{
    fn new(v: Value) -> Result<OptionIr<Ir>, FromValueError> {
        match v {
            Value::NULL => Ok(OptionIr {
                value: Some(Value::NULL),
                ir: None,
            }),
            v => match T::get_intermediate(v) {
                Ok(ir) => Ok(OptionIr {
                    value: None,
                    ir: Some(ir),
                }),
                Err(err) => Err(err),
            },
        }
    }
    fn commit(self) -> Option<T> {
        self.ir.map(|ir| ir.commit())
    }
    fn rollback(self) -> Value {
        let OptionIr { value, ir } = self;
        match value {
            Some(v) => v,
            None => match ir {
                Some(ir) => ir.rollback(),
                None => unreachable!(),
            },
        }
    }
}

impl<T> FromValue for Option<T>
where
    T: FromValue,
{
    type Intermediate = OptionIr<T::Intermediate>;
}

impl ConvIr<Value> for Value {
    fn new(v: Value) -> Result<Self, FromValueError> {
        Ok(v)
    }

    fn commit(self) -> Value {
        self
    }

    fn rollback(self) -> Value {
        self
    }
}

impl FromValue for Value {
    type Intermediate = Value;
    fn from_value(v: Value) -> Value {
        v
    }
    fn from_value_opt(v: Value) -> Result<Value, FromValueError> {
        Ok(v)
    }
}

impl<'a> ConvIr<Cow<'a, str>> for Vec<u8> {
    fn new(v: Value) -> Result<Self, FromValueError> {
        match v {
            Value::Bytes(bytes) => match from_utf8(&*bytes) {
                Ok(_) => Ok(bytes),
                Err(_) => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }

    fn commit(self) -> Cow<'a, str> {
        unsafe { Cow::Owned(String::from_utf8_unchecked(self)) }
    }

    fn rollback(self) -> Value {
        Value::Bytes(self)
    }
}

impl ConvIr<String> for Vec<u8> {
    fn new(v: Value) -> Result<Vec<u8>, FromValueError> {
        match v {
            Value::Bytes(bytes) => match from_utf8(&*bytes) {
                Ok(_) => Ok(bytes),
                Err(_) => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> String {
        unsafe { String::from_utf8_unchecked(self) }
    }
    fn rollback(self) -> Value {
        Value::Bytes(self)
    }
}

/// Intermediate result of a Value-to-Integer conversion.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseIr<T> {
    value: Value,
    output: T,
}

impl ConvIr<i64> for ParseIr<i64> {
    fn new(v: Value) -> Result<ParseIr<i64>, FromValueError> {
        match v {
            Value::Int(x) => Ok(ParseIr {
                value: Value::Int(x),
                output: x,
            }),
            Value::UInt(x) if x <= ::std::i64::MAX as u64 => Ok(ParseIr {
                value: Value::UInt(x),
                output: x as i64,
            }),
            Value::Bytes(bytes) => match parse(&*bytes) {
                Ok(x) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output: x,
                }),
                _ => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> i64 {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<u64> for ParseIr<u64> {
    fn new(v: Value) -> Result<ParseIr<u64>, FromValueError> {
        match v {
            Value::Int(x) if x >= 0 => Ok(ParseIr {
                value: Value::Int(x),
                output: x as u64,
            }),
            Value::UInt(x) => Ok(ParseIr {
                value: Value::UInt(x),
                output: x,
            }),
            Value::Bytes(bytes) => match parse(&*bytes) {
                Ok(x) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output: x,
                }),
                _ => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> u64 {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<f32> for ParseIr<f32> {
    fn new(v: Value) -> Result<ParseIr<f32>, FromValueError> {
        match v {
            Value::Float(x) => Ok(ParseIr {
                value: Value::Float(x),
                output: x,
            }),
            Value::Bytes(bytes) => {
                let val = parse(&*bytes).ok();
                match val {
                    Some(x) => Ok(ParseIr {
                        value: Value::Bytes(bytes),
                        output: x,
                    }),
                    None => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> f32 {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<f64> for ParseIr<f64> {
    fn new(v: Value) -> Result<ParseIr<f64>, FromValueError> {
        match v {
            Value::Double(x) => Ok(ParseIr {
                value: Value::Double(x),
                output: x,
            }),
            Value::Float(x) => {
                let double = x.into();
                Ok(ParseIr {
                    value: Value::Double(double),
                    output: double,
                })
            }
            Value::Bytes(bytes) => {
                let val = parse(&*bytes).ok();
                match val {
                    Some(x) => Ok(ParseIr {
                        value: Value::Bytes(bytes),
                        output: x,
                    }),
                    _ => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> f64 {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<bool> for ParseIr<bool> {
    fn new(v: Value) -> Result<ParseIr<bool>, FromValueError> {
        match v {
            Value::Int(0) => Ok(ParseIr {
                value: Value::Int(0),
                output: false,
            }),
            Value::Int(1) => Ok(ParseIr {
                value: Value::Int(1),
                output: true,
            }),
            Value::Bytes(bytes) => {
                if bytes.len() == 1 {
                    match bytes[0] {
                        0x30 => Ok(ParseIr {
                            value: Value::Bytes(bytes),
                            output: false,
                        }),
                        0x31 => Ok(ParseIr {
                            value: Value::Bytes(bytes),
                            output: true,
                        }),
                        _ => Err(FromValueError(Value::Bytes(bytes))),
                    }
                } else {
                    Err(FromValueError(Value::Bytes(bytes)))
                }
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> bool {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<Vec<u8>> for Vec<u8> {
    fn new(v: Value) -> Result<Vec<u8>, FromValueError> {
        match v {
            Value::Bytes(bytes) => Ok(bytes),
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Vec<u8> {
        self
    }
    fn rollback(self) -> Value {
        Value::Bytes(self)
    }
}

impl<'a> ConvIr<Cow<'a, [u8]>> for Vec<u8> {
    fn new(v: Value) -> Result<Vec<u8>, FromValueError> {
        match v {
            Value::Bytes(bytes) => Ok(bytes),
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Cow<'a, [u8]> {
        Cow::Owned(self)
    }
    fn rollback(self) -> Value {
        Value::Bytes(self)
    }
}

impl<const N: usize> ConvIr<[u8; N]> for [u8; N] {
    fn new(v: Value) -> Result<Self, FromValueError> {
        match v {
            Value::Bytes(bytes) => bytes
                .try_into()
                .map_err(|x| FromValueError(Value::Bytes(x))),
            v => Err(FromValueError(v)),
        }
    }

    fn commit(self) -> [u8; N] {
        self
    }

    fn rollback(self) -> Value {
        Value::Bytes(self.into())
    }
}

impl<const N: usize> FromValue for [u8; N] {
    type Intermediate = [u8; N];
}

fn parse_micros(micros_bytes: &[u8]) -> u32 {
    let mut micros = parse(micros_bytes).unwrap();

    let mut pad_zero_cnt = 0;
    for b in micros_bytes.iter() {
        if *b == b'0' {
            pad_zero_cnt += 1;
        } else {
            break;
        }
    }

    for _ in 0..(6 - pad_zero_cnt - (micros_bytes.len() - pad_zero_cnt)) {
        micros *= 10;
    }
    micros
}

/// Returns (is_neg, hours, minutes, seconds, microseconds)
fn parse_mysql_time_string(mut bytes: &[u8]) -> Option<(bool, u32, u32, u32, u32)> {
    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    #[repr(u8)]
    enum TimeKind {
        HhMmSs = 0,
        HhhMmSs,
        HhMmSsMs,
        HhhMmSsMs,
    }

    if bytes.len() < 8 {
        return None;
    }

    let is_neg = bytes[0] == b'-';
    if is_neg {
        bytes = &bytes[1..];
    }

    let len = bytes.len();

    let kind = if len == 8 && TIME_RE_HH_MM_SS.is_match(bytes) {
        TimeKind::HhMmSs
    } else if len == 9 && TIME_RE_HHH_MM_SS.is_match(bytes) {
        TimeKind::HhhMmSs
    } else if TIME_RE_HH_MM_SS_MS.is_match(bytes) {
        TimeKind::HhMmSsMs
    } else if TIME_RE_HHH_MM_SS_MS.is_match(bytes) {
        TimeKind::HhhMmSsMs
    } else {
        return None;
    };

    let (hour_pos, min_pos, sec_pos, micros_pos) = match kind {
        TimeKind::HhMmSs => (..2, 3..5, 6..8, None),
        TimeKind::HhMmSsMs => (..2, 3..5, 6..8, Some(9..)),
        TimeKind::HhhMmSs => (..3, 4..6, 7..9, None),
        TimeKind::HhhMmSsMs => (..3, 4..6, 7..9, Some(10..)),
    };

    Some((
        is_neg,
        parse(&bytes[hour_pos]).unwrap(),
        parse(&bytes[min_pos]).unwrap(),
        parse(&bytes[sec_pos]).unwrap(),
        micros_pos.map(|pos| parse_micros(&bytes[pos])).unwrap_or(0),
    ))
}

impl ConvIr<Duration> for ParseIr<Duration> {
    fn new(v: Value) -> Result<ParseIr<Duration>, FromValueError> {
        match v {
            Value::Time(false, days, hours, minutes, seconds, microseconds) => {
                let nanos = (microseconds as u32) * 1000;
                let secs = u64::from(seconds)
                    + u64::from(minutes) * 60
                    + u64::from(hours) * 60 * 60
                    + u64::from(days) * 60 * 60 * 24;
                Ok(ParseIr {
                    value: Value::Time(false, days, hours, minutes, seconds, microseconds),
                    output: Duration::new(secs, nanos),
                })
            }
            Value::Bytes(val_bytes) => {
                let duration = match parse_mysql_time_string(&*val_bytes) {
                    Some((false, hours, minutes, seconds, microseconds)) => {
                        let nanos = microseconds * 1000;
                        let secs = u64::from(seconds)
                            + u64::from(minutes) * 60
                            + u64::from(hours) * 60 * 60;
                        Duration::new(secs, nanos)
                    }
                    _ => return Err(FromValueError(Value::Bytes(val_bytes))),
                };
                Ok(ParseIr {
                    value: Value::Bytes(val_bytes),
                    output: duration,
                })
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Duration {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl From<Duration> for Value {
    fn from(x: Duration) -> Value {
        let mut secs_total = x.as_secs();
        let micros = (f64::from(x.subsec_nanos()) / 1000_f64).round() as u32;
        let seconds = (secs_total % 60) as u8;
        secs_total -= u64::from(seconds);
        let minutes = ((secs_total % (60 * 60)) / 60) as u8;
        secs_total -= u64::from(minutes) * 60;
        let hours = ((secs_total % (60 * 60 * 24)) / (60 * 60)) as u8;
        secs_total -= u64::from(hours) * 60 * 60;
        Value::Time(
            false,
            (secs_total / (60 * 60 * 24)) as u32,
            hours,
            minutes,
            seconds,
            micros,
        )
    }
}

impl_from_value!(String, Vec<u8>);
impl_from_value!(Vec<u8>, Vec<u8>);
impl_from_value!(Cow<'_, str>, Vec<u8>);
impl_from_value!(Cow<'_, [u8]>, Vec<u8>);
impl_from_value!(bool, ParseIr<bool>);
impl_from_value!(i64, ParseIr<i64>);
impl_from_value!(u64, ParseIr<u64>);
impl_from_value!(f32, ParseIr<f32>);
impl_from_value!(f64, ParseIr<f64>);
impl_from_value!(Duration, ParseIr<Duration>);
impl_from_value_num!(i8);
impl_from_value_num!(u8);
impl_from_value_num!(i16);
impl_from_value_num!(u16);
impl_from_value_num!(i32);
impl_from_value_num!(u32);
impl_from_value_num!(isize);
impl_from_value_num!(usize);
impl_from_value_num!(i128);
impl_from_value_num!(u128);

pub trait ToValue {
    fn to_value(&self) -> Value;
}

impl<T: Into<Value> + Clone> ToValue for T {
    fn to_value(&self) -> Value {
        self.clone().into()
    }
}

impl<'a, T: ToValue> From<&'a T> for Value {
    fn from(x: &'a T) -> Value {
        x.to_value()
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(x: Option<T>) -> Value {
        match x {
            None => Value::NULL,
            Some(x) => x.into(),
        }
    }
}

macro_rules! into_value_impl (
    (signed $t:ty) => (
        impl From<$t> for Value {
            fn from(x: $t) -> Value {
                Value::Int(x as i64)
            }
        }
    );
    (unsigned $t:ty) => (
        impl From<$t> for Value {
            fn from(x: $t) -> Value {
                Value::UInt(x as u64)
            }
        }
    );
);

into_value_impl!(signed i8);
into_value_impl!(signed i16);
into_value_impl!(signed i32);
into_value_impl!(signed i64);
into_value_impl!(signed isize);
into_value_impl!(unsigned u8);
into_value_impl!(unsigned u16);
into_value_impl!(unsigned u32);
into_value_impl!(unsigned u64);
into_value_impl!(unsigned usize);

impl From<i128> for Value {
    fn from(x: i128) -> Value {
        if let Some(x) = x.to_i64() {
            Value::Int(x)
        } else if let Some(x) = x.to_u64() {
            Value::UInt(x)
        } else {
            Value::Bytes(x.to_string().into())
        }
    }
}

impl From<u128> for Value {
    fn from(x: u128) -> Value {
        if let Some(x) = x.to_u64() {
            Value::UInt(x)
        } else {
            Value::Bytes(x.to_string().into())
        }
    }
}

impl From<f32> for Value {
    fn from(x: f32) -> Value {
        Value::Float(x)
    }
}

impl From<f64> for Value {
    fn from(x: f64) -> Value {
        Value::Double(x)
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value::Int(if x { 1 } else { 0 })
    }
}

impl<'a> From<&'a [u8]> for Value {
    fn from(x: &'a [u8]) -> Value {
        Value::Bytes(x.into())
    }
}

impl From<Vec<u8>> for Value {
    fn from(x: Vec<u8>) -> Value {
        Value::Bytes(x)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(x: &'a str) -> Value {
        let string: String = x.into();
        Value::Bytes(string.into_bytes())
    }
}

impl<'a, T: ToOwned> From<Cow<'a, T>> for Value
where
    T::Owned: Into<Value>,
    &'a T: Into<Value>,
{
    fn from(x: Cow<'a, T>) -> Value {
        match x {
            Cow::Borrowed(x) => x.into(),
            Cow::Owned(x) => x.into(),
        }
    }
}

impl From<String> for Value {
    fn from(x: String) -> Value {
        Value::Bytes(x.into_bytes())
    }
}

impl<const N: usize> From<[u8; N]> for Value {
    fn from(x: [u8; N]) -> Value {
        Value::Bytes(x.to_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    macro_rules! signed_primitive_roundtrip {
        ($t:ty, $name:ident) => {
            proptest! {
                #[test]
                fn $name(n: $t) {
                    let val = Value::Int(n as i64);
                    let val_bytes = Value::Bytes(n.to_string().into());
                    assert_eq!(Value::from(from_value::<$t>(val.clone())), val);
                    assert_eq!(Value::from(from_value::<$t>(val_bytes.clone())), val);
                    if n >= 0 {
                        let val_uint = Value::UInt(n as u64);
                        assert_eq!(Value::from(from_value::<$t>(val_uint.clone())), val);
                    }
                }
            }
        };
    }

    macro_rules! unsigned_primitive_roundtrip {
        ($t:ty, $name:ident) => {
            proptest! {
                #[test]
                fn $name(n: $t) {
                    let val = Value::UInt(n as u64);
                    let val_bytes = Value::Bytes(n.to_string().into());
                    assert_eq!(Value::from(from_value::<$t>(val.clone())), val);
                    assert_eq!(Value::from(from_value::<$t>(val_bytes.clone())), val);
                    if n as u64 <= i64::max_value() as u64 {
                        let val_int = Value::Int(n as i64);
                        assert_eq!(Value::from(from_value::<$t>(val_int.clone())), val);
                    }
                }
            }
        };
    }

    proptest! {
        #[test]
        fn bytes_roundtrip(s: Vec<u8>) {
            let val = Value::Bytes(s);
            assert_eq!(Value::from(from_value::<Vec<u8>>(val.clone())), val);
        }

        #[test]
        fn string_roundtrip(s: String) {
            let val = Value::Bytes(s.as_bytes().to_vec());
            assert_eq!(Value::from(from_value::<String>(val.clone())), val);
        }

        #[test]
        fn parse_mysql_time_string_parses_valid_time(
            s in r"-?[0-8][0-9][0-9]:[0-5][0-9]:[0-5][0-9](\.[0-9]{1,6})?"
        ) {
            parse_mysql_time_string(s.as_bytes()).unwrap();
            // Don't test `parse_mysql_time_string_with_time` here,
            // as this tests valid MySQL TIME values, not valid time ranges within a day.
            // Due to that, `time::parse` will return an Err for invalid time strings.
        }

        #[test]
        fn parse_mysql_time_string_parses_correctly(
            sign in 0..2,
            h in 0u32..900,
            m in 0u32..59,
            s in 0u32..59,
            have_us in 0..2,
            us in 0u32..1000000,
        ) {
            let time_string = format!(
                "{}{:02}:{:02}:{:02}{}",
                if sign == 1 { "-" } else { "" },
                h, m, s,
                if have_us == 1 {
                    format!(".{:06}", us)
                } else {
                    "".into()
                }
            );
            let time = parse_mysql_time_string(time_string.as_bytes()).unwrap();
            assert_eq!(time, (sign == 1, h, m, s, if have_us == 1 { us } else { 0 }));

            // Don't test `parse_mysql_time_string_with_time` here,
            // as this tests valid MySQL TIME values, not valid time ranges within a day.
            // Due to that, `time::parse` will return an Err for invalid time strings.
        }

        #[test]
        #[cfg(all(feature = "time", test))]
        fn parse_mysql_datetime_string_parses_valid_time(
            s in r"[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{1,6})?"
        ) {
            parse_mysql_datetime_string(s.as_bytes()).unwrap();
        }

        #[test]
        #[cfg(all(feature = "time", test))]
        fn parse_mysql_datetime_string_doesnt_crash(s in "\\PC*") {
            parse_mysql_datetime_string(s.as_bytes());
            let _ = super::time::parse_mysql_datetime_string_with_time(s.as_bytes());
        }

        #[test]
        fn i128_roundtrip(
            bytes_pos in r"16[0-9]{37}",
            bytes_neg in r"-16[0-9]{37}",
            uint in (i64::max_value() as u64 + 1)..u64::max_value(),
            int: i64,
        ) {
            let val_bytes_pos = Value::Bytes(bytes_pos.as_bytes().into());
            let val_bytes_neg = Value::Bytes(bytes_neg.as_bytes().into());
            let val_uint = Value::UInt(uint);
            let val_int = Value::Int(int);

            assert_eq!(Value::from(from_value::<i128>(val_bytes_pos.clone())), val_bytes_pos);
            assert_eq!(Value::from(from_value::<i128>(val_bytes_neg.clone())), val_bytes_neg);
            assert_eq!(Value::from(from_value::<i128>(val_uint.clone())), val_uint);
            assert_eq!(Value::from(from_value::<i128>(val_int.clone())), val_int);
        }

        #[test]
        fn u128_roundtrip(
            bytes in r"16[0-9]{37}",
            uint: u64,
            int in 0i64..i64::max_value(),
        ) {
            let val_bytes = Value::Bytes(bytes.as_bytes().into());
            let val_uint = Value::UInt(uint);
            let val_int = Value::Int(int);

            assert_eq!(Value::from(from_value::<u128>(val_bytes.clone())), val_bytes);
            assert_eq!(Value::from(from_value::<u128>(val_uint.clone())), val_uint);
            assert_eq!(Value::from(from_value::<u128>(val_int)), Value::UInt(int as u64));
        }

        #[test]
        fn f32_roundtrip(n: f32) {
            let val = Value::Float(n);
            let val_bytes = Value::Bytes(n.to_string().into());
            assert_eq!(Value::from(from_value::<f32>(val.clone())), val);
            assert_eq!(Value::from(from_value::<f32>(val_bytes)), val);
        }

        #[test]
        fn f64_roundtrip(n: f64) {
            let val = Value::Double(n);
            let val_bytes = Value::Bytes(n.to_string().into());
            assert_eq!(Value::from(from_value::<f64>(val.clone())), val);
            assert_eq!(Value::from(from_value::<f64>(val_bytes)), val);
        }
    }

    signed_primitive_roundtrip!(i8, i8_roundtrip);
    signed_primitive_roundtrip!(i16, i16_roundtrip);
    signed_primitive_roundtrip!(i32, i32_roundtrip);
    signed_primitive_roundtrip!(i64, i64_roundtrip);

    unsigned_primitive_roundtrip!(u8, u8_roundtrip);
    unsigned_primitive_roundtrip!(u16, u16_roundtrip);
    unsigned_primitive_roundtrip!(u32, u32_roundtrip);
    unsigned_primitive_roundtrip!(u64, u64_roundtrip);

    #[test]
    fn from_value_should_fail_on_integer_overflow() {
        let value = Value::Bytes(b"340282366920938463463374607431768211456"[..].into());
        assert!(from_value_opt::<u8>(value.clone()).is_err());
        assert!(from_value_opt::<i8>(value.clone()).is_err());
        assert!(from_value_opt::<u16>(value.clone()).is_err());
        assert!(from_value_opt::<i16>(value.clone()).is_err());
        assert!(from_value_opt::<u32>(value.clone()).is_err());
        assert!(from_value_opt::<i32>(value.clone()).is_err());
        assert!(from_value_opt::<u64>(value.clone()).is_err());
        assert!(from_value_opt::<i64>(value.clone()).is_err());
        assert!(from_value_opt::<u128>(value.clone()).is_err());
        assert!(from_value_opt::<i128>(value).is_err());
    }

    #[test]
    fn from_value_should_fail_on_integer_underflow() {
        let value = Value::Bytes(b"-170141183460469231731687303715884105729"[..].into());
        assert!(from_value_opt::<u8>(value.clone()).is_err());
        assert!(from_value_opt::<i8>(value.clone()).is_err());
        assert!(from_value_opt::<u16>(value.clone()).is_err());
        assert!(from_value_opt::<i16>(value.clone()).is_err());
        assert!(from_value_opt::<u32>(value.clone()).is_err());
        assert!(from_value_opt::<i32>(value.clone()).is_err());
        assert!(from_value_opt::<u64>(value.clone()).is_err());
        assert!(from_value_opt::<i64>(value.clone()).is_err());
        assert!(from_value_opt::<u128>(value.clone()).is_err());
        assert!(from_value_opt::<i128>(value).is_err());
    }

    #[cfg(feature = "nightly")]
    #[cfg(feature = "chrono")]
    #[bench]
    fn bench_parse_mysql_datetime_string(bencher: &mut test::Bencher) {
        let text = "1234-12-12 12:12:12.123456";
        bencher.bytes = text.len() as u64;
        bencher.iter(|| {
            parse_mysql_datetime_string(text.as_bytes()).unwrap();
        });
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_parse_mysql_time_string(bencher: &mut test::Bencher) {
        let text = "-012:34:56.012345";
        bencher.bytes = text.len() as u64;
        bencher.iter(|| {
            parse_mysql_time_string(text.as_bytes()).unwrap();
        });
    }

    #[test]
    fn value_float_read_conversions_work() {
        let original_f32 = std::f32::consts::PI;
        let float_value = Value::Float(original_f32);

        // Reading an f32 from a MySQL float works.
        let converted_f32: f32 = f32::from_value_opt(float_value.clone()).unwrap();
        assert_eq!(converted_f32, original_f32);

        // Reading an f64 from a MySQL float also works (lossless cast).
        let converted_f64: f64 = f64::from_value_opt(float_value).unwrap();
        assert_eq!(converted_f64, original_f32 as f64);
    }

    #[test]
    fn value_double_read_conversions_work() {
        let original_f64 = std::f64::consts::PI;
        let double_value = Value::Double(original_f64);

        // Reading an f64 from a MySQL double works.
        let converted_f64: f64 = f64::from_value_opt(double_value.clone()).unwrap();
        assert_eq!(converted_f64, original_f64);

        // Reading an f32 from a MySQL double fails (precision loss).
        assert!(f32::from_value_opt(double_value).is_err());
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_parse_mysql_datetime_string_with_time(bencher: &mut test::Bencher) {
        let text = "1234-12-12 12:12:12.123456";
        bencher.bytes = text.len() as u64;
        bencher.iter(|| {
            parse_mysql_datetime_string_with_time(text.as_bytes()).unwrap();
        });
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_parse_mysql_time_string_with_time(bencher: &mut test::Bencher) {
        let text = "12:34:56.012345";
        bencher.bytes = text.len() as u64;
        bencher.iter(|| {
            parse_mysql_time_string_with_time(text.as_bytes()).unwrap();
        });
    }
}
