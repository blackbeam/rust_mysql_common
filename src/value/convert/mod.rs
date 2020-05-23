// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, Timelike};
use lexical::parse;
use num_traits::{FromPrimitive, ToPrimitive};
use regex::bytes::Regex;
use time::{Date, ParseError, PrimitiveDateTime, Time};
use uuid::Uuid;

use std::{any::type_name, error::Error, fmt, str::from_utf8, time::Duration};

use crate::value::Value;

mod bigdecimal;
mod bigint;
mod decimal;

lazy_static! {
    static ref DATETIME_RE_YMD: Regex = { Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap() };
    static ref DATETIME_RE_YMD_HMS: Regex =
        { Regex::new(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$").unwrap() };
    static ref DATETIME_RE_YMD_HMS_NS: Regex =
        { Regex::new(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{1,6}$").unwrap() };
    static ref TIME_RE_HH_MM_SS: Regex = { Regex::new(r"^\d{2}:[0-5]\d:[0-5]\d$").unwrap() };
    static ref TIME_RE_HH_MM_SS_MS: Regex =
        { Regex::new(r"^\d{2}:[0-5]\d:[0-5]\d\.\d{1,6}$").unwrap() };
    static ref TIME_RE_HHH_MM_SS: Regex = { Regex::new(r"^[0-8]\d\d:[0-5]\d:[0-5]\d$").unwrap() };
    static ref TIME_RE_HHH_MM_SS_MS: Regex =
        { Regex::new(r"^[0-8]\d\d:[0-5]\d:[0-5]\d\.\d{1,6}$").unwrap() };
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
#[inline]
pub fn from_value<T: FromValue>(v: Value) -> T {
    FromValue::from_value(v)
}

/// Will return `Err(FromValueError(v))` if could not convert `v` to `T`
#[inline]
pub fn from_value_opt<T: FromValue>(v: Value) -> Result<T, FromValueError> {
    FromValue::from_value_opt(v)
}

macro_rules! impl_from_value {
    ($ty:ty, $ir:ty) => {
        impl FromValue for $ty {
            type Intermediate = $ir;
        }
    };
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
        match self.ir {
            Some(ir) => Some(ir.commit()),
            None => None,
        }
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

impl ConvIr<PrimitiveDateTime> for ParseIr<PrimitiveDateTime> {
    fn new(value: Value) -> Result<ParseIr<PrimitiveDateTime>, FromValueError> {
        match value {
            Value::Date(y, m, d, h, i, s, u) => Ok(ParseIr {
                value: Value::Date(y, m, d, h, i, s, u),
                output: match create_primitive_date_time(y, m, d, h, i, s, u) {
                    Some(datetime) => datetime,
                    None => return Err(FromValueError(value)),
                },
            }),
            Value::Bytes(bytes) => match parse_mysql_datetime_string_with_time(&*bytes) {
                Ok(output) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output,
                }),
                Err(_) => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> PrimitiveDateTime {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

/// Converts a MySQL `DATE` value to a `time::Date`.
impl ConvIr<Date> for ParseIr<Date> {
    fn new(value: Value) -> Result<ParseIr<Date>, FromValueError> {
        match value {
            Value::Date(y, m, d, h, i, s, u) => Ok(ParseIr {
                value: Value::Date(y, m, d, h, i, s, u),
                output: match Date::try_from_ymd(y as i32, m, d) {
                    Ok(date) => date,
                    Err(_) => return Err(FromValueError(value)),
                },
            }),
            Value::Bytes(bytes) => {
                match from_utf8(&*bytes)
                    .ok()
                    .and_then(|s| time::parse(s, "%Y-%m-%d").ok())
                {
                    Some(output) => Ok(ParseIr {
                        value: Value::Bytes(bytes),
                        output,
                    }),
                    None => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Date {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

/// Converts a MySQL `TIME` value to a `time::Time`.
/// Note: `time::Time` only allows for time values in the 00:00:00 - 23:59:59 range.
/// If you're expecting `TIME` values in MySQL's `TIME` value range of -838:59:59 - 838:59:59,
/// use time::Duration instead.
impl ConvIr<Time> for ParseIr<Time> {
    fn new(value: Value) -> Result<ParseIr<Time>, FromValueError> {
        match value {
            Value::Time(false, 0, h, m, s, u) => Ok(ParseIr {
                value: Value::Time(false, 0, h, m, s, u),
                output: match Time::try_from_hms_micro(h, m, s, u) {
                    Ok(time) => time,
                    Err(_) => return Err(FromValueError(value)),
                },
            }),
            Value::Bytes(bytes) => match parse_mysql_time_string_with_time(&*bytes) {
                Ok(output) => Ok(ParseIr {
                    value: Value::Bytes(bytes),
                    output,
                }),
                Err(_) => Err(FromValueError(Value::Bytes(bytes))),
            },
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Time {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

#[inline]
fn create_primitive_date_time(
    y: u16,
    m: u8,
    d: u8,
    h: u8,
    i: u8,
    s: u8,
    u: u32,
) -> Option<PrimitiveDateTime> {
    if let Ok(date) = Date::try_from_ymd(y as i32, m, d) {
        if let Ok(time) = Time::try_from_hms_micro(h, i, s, u) {
            return Some(PrimitiveDateTime::new(date, time));
        }
    }

    None
}

#[inline]
fn parse_mysql_datetime_string_with_time(bytes: &[u8]) -> Result<PrimitiveDateTime, ParseError> {
    from_utf8(&*bytes)
        .map_err(|_| ParseError::InsufficientInformation)
        .and_then(|s| time::parse(s, "%Y-%m-%d %H:%M:%S").or_else(|_| time::parse(s, "%Y-%m-%d")))
}

#[inline]
fn parse_mysql_time_string_with_time(bytes: &[u8]) -> Result<Time, ParseError> {
    from_utf8(&*bytes)
        .map_err(|_| ParseError::InsufficientInformation)
        .and_then(|s| Time::parse(s, "%H:%M:%S"))
}

impl ConvIr<NaiveDateTime> for ParseIr<NaiveDateTime> {
    fn new(value: Value) -> Result<ParseIr<NaiveDateTime>, FromValueError> {
        let result = match value {
            Value::Date(y, m, d, h, i, s, u) => {
                let date = NaiveDate::from_ymd_opt(y.into(), m.into(), d.into());
                let time = NaiveTime::from_hms_micro_opt(h.into(), i.into(), s.into(), u);
                Ok((date, time, Value::Date(y, m, d, h, i, s, u)))
            }
            Value::Bytes(bytes) => {
                if let Some((y, m, d, h, i, s, u)) = parse_mysql_datetime_string(&*bytes) {
                    let date = NaiveDate::from_ymd_opt(y as i32, m, d);
                    let time = NaiveTime::from_hms_micro_opt(h, i, s, u);
                    Ok((date, time, Value::Bytes(bytes)))
                } else {
                    Err(FromValueError(Value::Bytes(bytes)))
                }
            }
            v => Err(FromValueError(v)),
        };

        let (date, time, value) = result?;

        if date.is_some() && time.is_some() {
            Ok(ParseIr {
                value,
                output: NaiveDateTime::new(date.unwrap(), time.unwrap()),
            })
        } else {
            Err(FromValueError(value))
        }
    }
    fn commit(self) -> NaiveDateTime {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<NaiveDate> for ParseIr<NaiveDate> {
    fn new(value: Value) -> Result<ParseIr<NaiveDate>, FromValueError> {
        let result = match value {
            Value::Date(y, m, d, h, i, s, u) => {
                let date = NaiveDate::from_ymd_opt(y.into(), m.into(), d.into());
                Ok((date, Value::Date(y, m, d, h, i, s, u)))
            }
            Value::Bytes(bytes) => {
                if let Some((y, m, d, _, _, _, _)) = parse_mysql_datetime_string(&*bytes) {
                    let date = NaiveDate::from_ymd_opt(y as i32, m, d);
                    Ok((date, Value::Bytes(bytes)))
                } else {
                    Err(FromValueError(Value::Bytes(bytes)))
                }
            }
            v => Err(FromValueError(v)),
        };

        let (date, value) = result?;

        if date.is_some() {
            Ok(ParseIr {
                value,
                output: date.unwrap(),
            })
        } else {
            Err(FromValueError(value))
        }
    }
    fn commit(self) -> NaiveDate {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

#[inline]
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

/// Returns (year, month, day, hour, minute, second, micros)
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

impl ConvIr<NaiveTime> for ParseIr<NaiveTime> {
    fn new(value: Value) -> Result<ParseIr<NaiveTime>, FromValueError> {
        let result = match value {
            Value::Time(false, 0, h, m, s, u) => {
                let time = NaiveTime::from_hms_micro_opt(h.into(), m.into(), s.into(), u);
                Ok((time, Value::Time(false, 0, h, m, s, u)))
            }
            Value::Bytes(bytes) => {
                if let Some((false, h, m, s, u)) = parse_mysql_time_string(&*bytes) {
                    let time = NaiveTime::from_hms_micro_opt(h, m, s, u);
                    Ok((time, Value::Bytes(bytes)))
                } else {
                    Err(FromValueError(Value::Bytes(bytes)))
                }
            }
            v => Err(FromValueError(v)),
        };

        let (time, value) = result?;

        if time.is_some() {
            Ok(ParseIr {
                value,
                output: time.unwrap(),
            })
        } else {
            Err(FromValueError(value))
        }
    }
    fn commit(self) -> NaiveTime {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
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

impl ConvIr<time::Duration> for ParseIr<time::Duration> {
    fn new(v: Value) -> Result<ParseIr<time::Duration>, FromValueError> {
        match v {
            Value::Time(is_neg, days, hours, minutes, seconds, microseconds) => {
                let duration = time::Duration::days(days.into())
                    + time::Duration::hours(hours.into())
                    + time::Duration::minutes(minutes.into())
                    + time::Duration::seconds(seconds.into())
                    + time::Duration::microseconds(microseconds.into());
                Ok(ParseIr {
                    value: Value::Time(is_neg, days, hours, minutes, seconds, microseconds),
                    output: if is_neg { -duration } else { duration },
                })
            }
            Value::Bytes(val_bytes) => {
                // Parse the string using `parse_mysql_time_string`
                // instead of `parse_mysql_time_string_with_time` here,
                // as it may contain an hour value that's outside of a day's normal 0-23 hour range.
                let duration = match parse_mysql_time_string(&*val_bytes) {
                    Some((is_neg, hours, minutes, seconds, microseconds)) => {
                        let duration = time::Duration::hours(hours.into())
                            + time::Duration::minutes(minutes.into())
                            + time::Duration::seconds(seconds.into())
                            + time::Duration::microseconds(microseconds.into());
                        if is_neg {
                            -duration
                        } else {
                            duration
                        }
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
    fn commit(self) -> time::Duration {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<chrono::Duration> for ParseIr<chrono::Duration> {
    fn new(v: Value) -> Result<ParseIr<chrono::Duration>, FromValueError> {
        match v {
            Value::Time(is_neg, days, hours, minutes, seconds, microseconds) => {
                let duration = chrono::Duration::days(days.into())
                    + chrono::Duration::hours(hours.into())
                    + chrono::Duration::minutes(minutes.into())
                    + chrono::Duration::seconds(seconds.into())
                    + chrono::Duration::microseconds(microseconds.into());
                Ok(ParseIr {
                    value: Value::Time(is_neg, days, hours, minutes, seconds, microseconds),
                    output: if is_neg { -duration } else { duration },
                })
            }
            Value::Bytes(val_bytes) => {
                // Parse the string using `parse_mysql_time_string`
                // instead of `parse_mysql_time_string_with_time` here,
                // as it may contain an hour value that's outside of a day's normal 0-23 hour range.
                let duration = match parse_mysql_time_string(&*val_bytes) {
                    Some((is_neg, hours, minutes, seconds, microseconds)) => {
                        let duration = chrono::Duration::hours(hours.into())
                            + chrono::Duration::minutes(minutes.into())
                            + chrono::Duration::seconds(seconds.into())
                            + chrono::Duration::microseconds(microseconds.into());
                        if is_neg {
                            -duration
                        } else {
                            duration
                        }
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
    fn commit(self) -> chrono::Duration {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl_from_value!(NaiveDateTime, ParseIr<NaiveDateTime>);
impl_from_value!(NaiveDate, ParseIr<NaiveDate>);
impl_from_value!(NaiveTime, ParseIr<NaiveTime>);
impl_from_value!(PrimitiveDateTime, ParseIr<PrimitiveDateTime>);
impl_from_value!(Date, ParseIr<Date>);
impl_from_value!(Time, ParseIr<Time>);
impl_from_value!(Duration, ParseIr<Duration>);
impl_from_value!(time::Duration, ParseIr<time::Duration>);
impl_from_value!(chrono::Duration, ParseIr<chrono::Duration>);
impl_from_value!(String, Vec<u8>);
impl_from_value!(Vec<u8>, Vec<u8>);
impl_from_value!(bool, ParseIr<bool>);
impl_from_value!(i64, ParseIr<i64>);
impl_from_value!(u64, ParseIr<u64>);
impl_from_value!(f32, ParseIr<f32>);
impl_from_value!(f64, ParseIr<f64>);
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
        Value::Float(x.into())
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

impl From<String> for Value {
    fn from(x: String) -> Value {
        Value::Bytes(x.into_bytes())
    }
}

impl From<NaiveDateTime> for Value {
    fn from(x: NaiveDateTime) -> Value {
        if 1000 > x.year() || x.year() > 9999 {
            panic!("Year `{}` not in supported range [1000, 9999]", x.year())
        }
        Value::Date(
            x.year() as u16,
            x.month() as u8,
            x.day() as u8,
            x.hour() as u8,
            x.minute() as u8,
            x.second() as u8,
            x.nanosecond() / 1000,
        )
    }
}

impl From<NaiveDate> for Value {
    fn from(x: NaiveDate) -> Value {
        if 1000 > x.year() || x.year() > 9999 {
            panic!("Year `{}` not in supported range [1000, 9999]", x.year())
        }
        Value::Date(x.year() as u16, x.month() as u8, x.day() as u8, 0, 0, 0, 0)
    }
}

impl From<NaiveTime> for Value {
    fn from(x: NaiveTime) -> Value {
        Value::Time(
            false,
            0,
            x.hour() as u8,
            x.minute() as u8,
            x.second() as u8,
            x.nanosecond() / 1000,
        )
    }
}

impl From<PrimitiveDateTime> for Value {
    fn from(x: PrimitiveDateTime) -> Value {
        Value::Date(
            x.year() as u16,
            x.month(),
            x.day(),
            x.hour(),
            x.minute(),
            x.second(),
            x.microsecond(),
        )
    }
}

impl From<Date> for Value {
    fn from(x: Date) -> Value {
        Value::Date(x.year() as u16, x.month(), x.day(), 0, 0, 0, 0)
    }
}

impl From<Time> for Value {
    fn from(x: Time) -> Value {
        Value::Time(
            false,
            0,
            x.hour() as u8,
            x.minute() as u8,
            x.second() as u8,
            x.microsecond(),
        )
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

impl From<time::Duration> for Value {
    fn from(mut x: time::Duration) -> Value {
        let negative = x < time::Duration::zero();

        if negative {
            x = -x;
        }

        let days = x.whole_days() as u32;
        x = x - time::Duration::days(x.whole_days());
        let hours = x.whole_hours() as u8;
        x = x - time::Duration::hours(x.whole_hours());
        let minutes = x.whole_minutes() as u8;
        x = x - time::Duration::minutes(x.whole_minutes());
        let seconds = x.whole_seconds() as u8;
        x = x - time::Duration::seconds(x.whole_seconds());
        let microseconds = x.whole_microseconds() as u32;

        Value::Time(negative, days, hours, minutes, seconds, microseconds)
    }
}

macro_rules! from_array_impl {
    ($n:expr) => {
        impl From<[u8; $n]> for Value {
            fn from(x: [u8; $n]) -> Value {
                Value::from(&x[..])
            }
        }
    };
}

from_array_impl!(0);
from_array_impl!(1);
from_array_impl!(2);
from_array_impl!(3);
from_array_impl!(4);
from_array_impl!(5);
from_array_impl!(6);
from_array_impl!(7);
from_array_impl!(8);
from_array_impl!(9);
from_array_impl!(10);
from_array_impl!(11);
from_array_impl!(12);
from_array_impl!(13);
from_array_impl!(14);
from_array_impl!(15);
from_array_impl!(16);
from_array_impl!(17);
from_array_impl!(18);
from_array_impl!(19);
from_array_impl!(20);
from_array_impl!(21);
from_array_impl!(22);
from_array_impl!(23);
from_array_impl!(24);
from_array_impl!(25);
from_array_impl!(26);
from_array_impl!(27);
from_array_impl!(28);
from_array_impl!(29);
from_array_impl!(30);
from_array_impl!(31);
from_array_impl!(32);

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
        fn parse_mysql_time_string_doesnt_crash(s in r"\PC*") {
            parse_mysql_time_string(s.as_bytes());
            let _ = parse_mysql_time_string_with_time(s.as_bytes());
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
        fn parse_mysql_datetime_string_doesnt_crash(s in "\\PC*") {
            parse_mysql_datetime_string(s.as_bytes());
            let _ = parse_mysql_datetime_string_with_time(s.as_bytes());
        }

        #[test]
        fn parse_mysql_datetime_string_parses_valid_time(
            s in r"[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{1,6})?"
        ) {
            parse_mysql_datetime_string(s.as_bytes()).unwrap();
        }

        #[test]
        fn parse_mysql_datetime_string_parses_correctly(
            y in 0u32..10000,
            m in 1u32..13,
            d in 1u32..32,
            h in 0u32..60,
            i in 0u32..60,
            s in 0u32..60,
            have_us in 0..2,
            us in 0u32..1000000,
        ) {
            let time_string = format!(
                "{:04}-{:02}-{:02} {:02}:{:02}:{:02}{}",
                y, m, d, h, i, s,
                if have_us == 1 {
                    format!(".{:06}", us)
                } else {
                    "".into()
                }
            );

            let datetime = parse_mysql_datetime_string(time_string.as_bytes()).unwrap();
            assert_eq!(datetime, (y, m, d, h, i, s, if have_us == 1 { us } else { 0 }));

            match parse_mysql_datetime_string_with_time(time_string.as_bytes()) {
                Ok(datetime) => {
                    // If `time` successfully parsed the string,
                    // then let's ensure it matches the values used to create that time string.

                    // `time` and other C-like `strptime` based parsers have no way of parsing
                    // microseconds from the time string. As such, we ignore them entirely.
                    assert_eq!(
                        (
                            datetime.year() as u32,
                            datetime.month() as u32,
                            datetime.day() as u32,
                            datetime.hour() as u32,
                            datetime.minute() as u32,
                            datetime.second() as u32,
                            0,
                        ),
                        (y, m, d, h, i, s, 0));
                },
                Err(err) => {
                    // If `time` failed to parse the string,
                    // then let's check if we passed an invalid value based on the error received,
                    // and fail the test if the string should have parsed successfully.
                    // Any commented out checks are simply to avoid having the compiler show
                    // 'comparison is useless due to type limits' warnings.
                    match err {
                        ParseError::InvalidSecond => assert!(/*s < 0 || */s > 59),
                        ParseError::InvalidMinute => assert!(/*i < 0 || */i > 59),
                        // For InvalidHour, only check if the randomized hour value is within
                        // 0-23 instead of MySQL `TIME`'s full range of -838-838,
                        // since we don't generate values that low or high,
                        // and should be parsed with time::Duration instead.
                        ParseError::InvalidHour => assert!(/*h < 0 || */h > 23),
                        ParseError::InvalidDayOfMonth => assert!(d < 1 || d > 31),
                        ParseError::InvalidMonth => assert!(m < 1000 || m <= 12),
                        // For InvalidYear, ensure that the year isn't also 0,
                        // which is a valid value with non-strict-mode MySQL.
                        ParseError::InvalidYear => assert!(y != 0 && (y < 1000 || y > 9999)),
                        ParseError::ComponentOutOfRange(_) |
                        ParseError::InsufficientInformation => {
                            // We may receive an ComponentOutOfRange or InsufficientInformation
                            // error for a few reasons, such as the format string being incorrect,
                            // the format of the time string being incorrect,
                            // or in some cases, when a value is out of range,
                            // such as when trying to parse a hour value of
                            // less than zero or greater than 23.

                            // Try creating `Date` and `Time` values from the values directly,
                            // and catch any `ComponentRangeError` that they might return.

                            // Seeing as we have no way to tell which value
                            // is rejected if we pass them in all at once,
                            // we call `try_from_ymd` and `try_from_hms_micro`
                            // for each value separately.

                            if Date::try_from_ymd(y as i32, 1, 1).is_err() {
                                assert!(y != 0 && (y < 1000 || y > 9999));
                            } else if Date::try_from_ymd(0, m as u8, 1).is_err() {
                                assert!(m < 1000 || m <= 12);
                            } else if Date::try_from_ymd(0, 1, d as u8).is_err() {
                                assert!(d < 1 || d > 31);
                            } else if Time::try_from_hms_micro(h as u8, 0, 0, 0).is_err() {
                                assert!(/*h < 0 || */h > 23);
                            } else if Time::try_from_hms_micro(0, i as u8, 0, 0).is_err() {
                                assert!(/*i < 0 || */i > 59);
                            } else if Time::try_from_hms_micro(0, 0, s as u8, 0).is_err() {
                                assert!(/*i < 0 || */i > 59);
                            }

                            // If each of the values passed separately, then the only reason we
                            // were given an error is because the date or time itself is invalid,
                            // i.e. February 30th, November 31st, etc.
                            // We have no way of validating if the date or time is actually
                            // invalid or not, so we just assume it's handled correctly
                            // within `time` if all values could be handled separately.
                        },
                        err => {
                            // Panic for any other error as well, seeing as the others either
                            // would never happen, or the time string format must be incorrect,
                            // neither of which should ever happen.
                            panic!("Failed to parse time due to an unknown reason. {}", err);
                        }
                    }
                }
            }
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
            assert_eq!(Value::from(from_value::<u128>(val_int.clone())), Value::UInt(int as u64));
        }

        #[test]
        fn f32_roundtrip(n: f32) {
            let val = Value::Float(n);
            let val_bytes = Value::Bytes(n.to_string().into());
            assert_eq!(Value::from(from_value::<f32>(val.clone())), val);
            assert_eq!(Value::from(from_value::<f32>(val_bytes.clone())), val);
        }

        #[test]
        fn f64_roundtrip(n: f64) {
            let val = Value::Double(n);
            let val_bytes = Value::Bytes(n.to_string().into());
            assert_eq!(Value::from(from_value::<f64>(val.clone())), val);
            assert_eq!(Value::from(from_value::<f64>(val_bytes.clone())), val);
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
        assert!(from_value_opt::<i128>(value.clone()).is_err());
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
        assert!(from_value_opt::<i128>(value.clone()).is_err());
    }

    #[cfg(feature = "nightly")]
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
        let original_f32 = 3.14;
        let float_value = Value::Float(original_f32);

        // Reading an f32 from a MySQL float works.
        let converted_f32: f32 = f32::from_value_opt(float_value.clone()).unwrap();
        assert_eq!(converted_f32, original_f32);

        // Reading an f64 from a MySQL float also works (lossless cast).
        let converted_f64: f64 = f64::from_value_opt(float_value.clone()).unwrap();
        assert_eq!(converted_f64, original_f32 as f64);
    }

    #[test]
    fn value_double_read_conversions_work() {
        let original_f64 = 3.14159265359;
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
