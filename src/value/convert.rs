use atoi::atoi;
use chrono::{NaiveDate, NaiveTime, NaiveDateTime, Datelike, Timelike};
use regex::bytes::Regex;
use std::error::Error;
use std::fmt;
use std::str::{FromStr, from_utf8};
use std::time::Duration;
use time::{self, Timespec, Tm, at, strptime};
use uuid::Uuid;
use value::Value;

lazy_static! {
    static ref DATETIME_RE_YMD: Regex = {
        Regex::new(r"^(\d{4})-(\d{2})-(\d{2})$").unwrap()
    };

    static ref DATETIME_RE_YMD_HMS: Regex = {
        Regex::new(r"^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})$").unwrap()
    };

    static ref DATETIME_RE_YMD_HMS_NS: Regex = {
        Regex::new(r"^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})\.(\d{1,6})$").unwrap()
    };

    static ref TIME_RE_HH_MM_SS: Regex = {
        Regex::new(r"^(\d{2}):([0-5]\d):([0-5]\d)$").unwrap()
    };

    static ref TIME_RE_HH_MM_SS_MS: Regex = {
        Regex::new(r"^(\d{2}):([0-5]\d):([0-5]\d)\.(\d{1,6})$").unwrap()
    };

    static ref TIME_RE_HHH_MM_SS: Regex = {
        Regex::new(r"^([0-8]\d\d):([0-5]\d):([0-5]\d)$").unwrap()
    };

    static ref TIME_RE_HHH_MM_SS_MS: Regex = {
        Regex::new(r"^([0-8]\d\d):([0-5]\d):([0-5]\d)\.(\d{1,6})$").unwrap()
    };
}

/// `FromValue` conversion error.
#[derive(Debug)]
pub struct FromValueError(pub Value);

impl fmt::Display for FromValueError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
        Self::from_value_opt(v).ok().expect(
            "Could not retrieve Self from Value",
        )
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
    ($ty:ty, $ir:ty, $msg:expr) => (
        impl FromValue for $ty {
            type Intermediate = $ir;
            fn from_value(v: Value) -> $ty {
                <Self as FromValue>::from_value_opt(v).ok().expect($msg)
            }
        }
    );
}

macro_rules! impl_from_value_num {
    ($t:ident, $msg:expr) => (
        impl ConvIr<$t> for ParseIr<$t> {
            fn new(v: Value) -> Result<ParseIr<$t>, FromValueError> {
                match v {
                    Value::Int(x) => {
                        let min = ::std::$t::MIN as i64;
                        let mut max = ::std::$t::MAX as i64;
                        if max < 0 {
                            max = ::std::i64::MAX;
                        }
                        if min <= x && x <= max {
                            Ok(ParseIr {
                                value: Value::Int(x),
                                output: x as $t,
                            })
                        } else {
                            Err(FromValueError(Value::Int(x)))
                        }
                    },
                    Value::UInt(x) if x <= ::std::$t::MAX as u64 => Ok(ParseIr {
                        value: Value::UInt(x),
                        output: x as $t,
                    }),
                    Value::Bytes(bytes) => {
                        match atoi(&*bytes) {
                            Some(x) => Ok(ParseIr {
                                value: Value::Bytes(bytes),
                                output: x,
                            }),
                            None => Err(FromValueError(Value::Bytes(bytes))),
                        }
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

        impl_from_value!($t, ParseIr<$t>, $msg);
    );
}

/// Intermediate result of a Value-to-Option<T> conversion.
#[derive(Debug)]
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
            Value::NULL => Ok(OptionIr { value: Some(Value::NULL), ir: None }),
            v => {
                match T::get_intermediate(v) {
                    Ok(ir) => Ok(OptionIr { value: None, ir: Some(ir) }),
                    Err(err) => Err(err),
                }
            }
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
            None => {
                match ir {
                    Some(ir) => ir.rollback(),
                    None => unreachable!(),
                }
            }
        }
    }
}

impl<T> FromValue for Option<T>
where
    T: FromValue,
{
    type Intermediate = OptionIr<T::Intermediate>;
    fn from_value(v: Value) -> Option<T> {
        <Self as FromValue>::from_value_opt(v).ok().expect(
            "Could not retrieve Option<T> from Value",
        )
    }
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

/// Intermediate result of a Value-to-String conversion.
#[derive(Debug)]
pub struct StringIr {
    bytes: Vec<u8>,
}

impl ConvIr<String> for StringIr {
    fn new(v: Value) -> Result<StringIr, FromValueError> {
        match v {
            Value::Bytes(bytes) => {
                match from_utf8(&*bytes) {
                    Ok(_) => Ok(StringIr { bytes: bytes }),
                    Err(_) => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> String {
        unsafe { String::from_utf8_unchecked(self.bytes) }
    }
    fn rollback(self) -> Value {
        Value::Bytes(self.bytes)
    }
}

/// Intermediate result of a Value-to-Integer conversion.
#[derive(Debug)]
pub struct ParseIr<T> {
    value: Value,
    output: T,
}

impl ConvIr<i64> for ParseIr<i64> {
    fn new(v: Value) -> Result<ParseIr<i64>, FromValueError> {
        match v {
            Value::Int(x) => Ok(ParseIr { value: Value::Int(x), output: x }),
            Value::UInt(x) if x <= ::std::i64::MAX as u64 => Ok(ParseIr {
                value: Value::UInt(x),
                output: x as i64,
            }),
            Value::Bytes(bytes) => {
                match atoi(&*bytes) {
                    Some(x) => Ok(ParseIr { value: Value::Bytes(bytes), output: x }),
                    None => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
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
            Value::Int(x) if x >= 0 => Ok(ParseIr { value: Value::Int(x), output: x as u64 }),
            Value::UInt(x) => Ok(ParseIr { value: Value::UInt(x), output: x }),
            Value::Bytes(bytes) => {
                match atoi(&*bytes) {
                    Some(x) => Ok(ParseIr { value: Value::Bytes(bytes), output: x }),
                    None => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
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
            Value::Float(x) if x >= ::std::f32::MIN as f64 && x <= ::std::f32::MAX as f64 => {
                Ok(ParseIr { value: Value::Float(x), output: x as f32 })
            }
            Value::Bytes(bytes) => {
                let val = from_utf8(&*bytes).ok().and_then(|x| f32::from_str(x).ok());
                match val {
                    Some(x) => Ok(ParseIr { value: Value::Bytes(bytes), output: x }),
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
            Value::Float(x) => Ok(ParseIr { value: Value::Float(x), output: x }),
            Value::Bytes(bytes) => {
                let val = from_utf8(&*bytes).ok().and_then(|x| f64::from_str(x).ok());
                match val {
                    Some(x) => Ok(ParseIr { value: Value::Bytes(bytes), output: x }),
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
            Value::Int(0) => Ok(ParseIr { value: Value::Int(0), output: false }),
            Value::Int(1) => Ok(ParseIr { value: Value::Int(1), output: true }),
            Value::Bytes(bytes) => {
                if bytes.len() == 1 {
                    match bytes[0] {
                        0x30 => Ok(ParseIr { value: Value::Bytes(bytes), output: false }),
                        0x31 => Ok(ParseIr { value: Value::Bytes(bytes), output: true }),
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

/// Intermediate result of a Value-to-Vec<u8> conversion.
#[derive(Debug)]
pub struct BytesIr {
    bytes: Vec<u8>,
}

impl ConvIr<Vec<u8>> for BytesIr {
    fn new(v: Value) -> Result<BytesIr, FromValueError> {
        match v {
            Value::Bytes(bytes) => Ok(BytesIr { bytes: bytes }),
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Vec<u8> {
        self.bytes
    }
    fn rollback(self) -> Value {
        Value::Bytes(self.bytes)
    }
}

impl ConvIr<Timespec> for ParseIr<Timespec> {
    fn new(v: Value) -> Result<ParseIr<Timespec>, FromValueError> {
        let tm_utcoff = at(Timespec::new(0, 0)).tm_utcoff;
        match v {
            Value::Date(y, m, d, h, i, s, u) => Ok(ParseIr {
                value: Value::Date(y, m, d, h, i, s, u),
                output: Tm {
                    tm_year: y as i32 - 1_900,
                    tm_mon: m as i32 - 1,
                    tm_mday: d as i32,
                    tm_hour: h as i32,
                    tm_min: i as i32,
                    tm_sec: s as i32,
                    tm_nsec: u as i32 * 1_000,
                    tm_utcoff: tm_utcoff,
                    tm_wday: 0,
                    tm_yday: 0,
                    tm_isdst: -1,
                }.to_timespec(),
            }),
            Value::Bytes(bytes) => {
                let val = from_utf8(&*bytes)
                    .ok()
                    .and_then(|s| {
                        strptime(s, "%Y-%m-%d %H:%M:%S")
                            .or(strptime(s, "%Y-%m-%d"))
                            .ok()
                    })
                    .map(|mut tm| {
                        tm.tm_utcoff = tm_utcoff;
                        tm.tm_isdst = -1;
                        tm.to_timespec()
                    });
                match val {
                    Some(timespec) => Ok(ParseIr { value: Value::Bytes(bytes), output: timespec }),
                    None => Err(FromValueError(Value::Bytes(bytes))),
                }
            }
            v => Err(FromValueError(v)),
        }
    }
    fn commit(self) -> Timespec {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl ConvIr<NaiveDateTime> for ParseIr<NaiveDateTime> {
    fn new(v: Value) -> Result<ParseIr<NaiveDateTime>, FromValueError> {
        let result = match v {
            Value::Date(y, m, d, h, i, s, u) => {
                let date = NaiveDate::from_ymd_opt(y as i32, m as u32, d as u32);
                let time = NaiveTime::from_hms_micro_opt(h as u32, i as u32, s as u32, u);
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
                value: value,
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
    fn new(v: Value) -> Result<ParseIr<NaiveDate>, FromValueError> {
        let result = match v {
            Value::Date(y, m, d, h, i, s, u) => {
                let date = NaiveDate::from_ymd_opt(y as i32, m as u32, d as u32);
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
            Ok(ParseIr { value: value, output: date.unwrap() })
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

/// Returns (year, month, day, hour, minute, second, micros)
fn parse_mysql_datetime_string(bytes: &[u8]) -> Option<(u32, u32, u32, u32, u32, u32, u32)> {
    if bytes.len() == 0 {
        return None;
    }
    DATETIME_RE_YMD_HMS.captures(bytes)
        .or_else(|| DATETIME_RE_YMD.captures(bytes))
        .or_else(|| DATETIME_RE_YMD_HMS_NS.captures(bytes))
        .map(|cts| {
            // shouldn't panic because content is validated by regex
            let year = atoi(cts.get(1).unwrap().as_bytes()).unwrap();
            let month = atoi(cts.get(2).unwrap().as_bytes()).unwrap();
            let day = atoi(cts.get(3).unwrap().as_bytes()).unwrap();

            let (hour, minute, second, micros) = if cts.len() > 4 {
                // shouldn't panic because content is validated by regex
                let hour = atoi(cts.get(4).unwrap().as_bytes()).unwrap();
                let minute = atoi(cts.get(5).unwrap().as_bytes()).unwrap();
                let second = atoi(cts.get(6).unwrap().as_bytes()).unwrap();
                let micros = if cts.len() == 8 {
                    // shouldn't panic because content is validated by regex
                    let micros_bytes = cts.get(7).unwrap().as_bytes();
                    let mut micros = atoi(micros_bytes).unwrap();

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
                } else {
                    0
                };
                (hour, minute, second, micros)
            } else {
                (0, 0, 0, 0)
            };
            (year, month, day, hour, minute, second, micros)
        })
}

/// Returns (is_neg, hours, minutes, seconds, microseconds)
fn parse_mysql_time_string(mut bytes: &[u8]) -> Option<(bool, u32, u32, u32, u32)> {
    if bytes.len() == 0 {
        return None;
    }
    let is_neg = bytes[0] == b'-';
    if is_neg {
        bytes = &bytes[1..];
    }
    TIME_RE_HHH_MM_SS.captures(bytes)
        .or_else(|| TIME_RE_HHH_MM_SS_MS.captures(bytes))
        .or_else(|| TIME_RE_HH_MM_SS.captures(bytes))
        .or_else(|| TIME_RE_HH_MM_SS_MS.captures(bytes))
        .map(|cts| {
            // shouldn't panic because content is validated by regex
            let hours = atoi(cts.get(1).unwrap().as_bytes()).unwrap();
            let minutes = atoi(cts.get(2).unwrap().as_bytes()).unwrap();
            let seconds = atoi(cts.get(3).unwrap().as_bytes()).unwrap();

            let microseconds = if cts.len() == 5 {
                // shouldn't panic because content is validated by regex
                let micros_bytes = cts.get(4).unwrap().as_bytes();
                let mut micros = atoi(micros_bytes).unwrap();

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
            } else {
                0
            };
            (is_neg, hours, minutes, seconds, microseconds)
        })
}

impl ConvIr<NaiveTime> for ParseIr<NaiveTime> {
    fn new(v: Value) -> Result<ParseIr<NaiveTime>, FromValueError> {
        let result = match v {
            Value::Time(false, 0, h, m, s, u) => {
                let time = NaiveTime::from_hms_micro_opt(h as u32, m as u32, s as u32, u);
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
            Ok(ParseIr { value: value, output: time.unwrap() })
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
                let secs = seconds as u64 + minutes as u64 * 60 + hours as u64 * 60 * 60 +
                    days as u64 * 60 * 60 * 24;
                Ok(ParseIr {
                    value: Value::Time(false, days, hours, minutes, seconds, microseconds),
                    output: Duration::new(secs, nanos),
                })
            }
            Value::Bytes(val_bytes) => {
                let duration = match parse_mysql_time_string(&*val_bytes) {
                    Some((false, hours, minutes, seconds, microseconds)) => {
                        let nanos = microseconds * 1000;
                        let secs = seconds as u64 + minutes as u64 * 60 + hours as u64 * 60 * 60;
                        Duration::new(secs, nanos)
                    }
                    _ => return Err(FromValueError(Value::Bytes(val_bytes))),
                };
                Ok(ParseIr { value: Value::Bytes(val_bytes), output: duration })
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
                let duration = time::Duration::days(days as i64) +
                    time::Duration::hours(hours as i64) +
                    time::Duration::minutes(minutes as i64) +
                    time::Duration::seconds(seconds as i64) +
                    time::Duration::microseconds(microseconds as i64);
                Ok(ParseIr {
                    value: Value::Time(is_neg, days, hours, minutes, seconds, microseconds),
                    output: if is_neg { -duration } else { duration },
                })
            }
            Value::Bytes(val_bytes) => {
                let duration = match parse_mysql_time_string(&*val_bytes) {
                    Some((is_neg, hours, minutes, seconds, microseconds)) => {
                        let duration = time::Duration::hours(hours as i64) +
                            time::Duration::minutes(minutes as i64) +
                            time::Duration::seconds(seconds as i64) +
                            time::Duration::microseconds(microseconds as i64);
                        if is_neg { -duration } else { duration }
                    }
                    _ => return Err(FromValueError(Value::Bytes(val_bytes))),
                };
                Ok(ParseIr { value: Value::Bytes(val_bytes), output: duration })
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

impl_from_value!(NaiveDateTime, ParseIr<NaiveDateTime>,
                 "Could not retrieve NaiveDateTime from Value");
impl_from_value!(NaiveDate, ParseIr<NaiveDate>, "Could not retrieve NaiveDate from Value");
impl_from_value!(NaiveTime, ParseIr<NaiveTime>, "Could not retrieve NaiveTime from Value");
impl_from_value!(Timespec, ParseIr<Timespec>, "Could not retrieve Timespec from Value");
impl_from_value!(Duration, ParseIr<Duration>, "Could not retrieve Duration from Value");
impl_from_value!(time::Duration, ParseIr<time::Duration>,
                 "Could not retrieve time::Duration from Value");
impl_from_value!(String, StringIr, "Could not retrieve String from Value");
impl_from_value!(Vec<u8>, BytesIr, "Could not retrieve Vec<u8> from Value");
impl_from_value!(bool, ParseIr<bool>, "Could not retrieve bool from Value");
impl_from_value!(i64, ParseIr<i64>, "Could not retrieve i64 from Value");
impl_from_value!(u64, ParseIr<u64>, "Could not retrieve u64 from Value");
impl_from_value!(f32, ParseIr<f32>, "Could not retrieve f32 from Value");
impl_from_value!(f64, ParseIr<f64>, "Could not retrieve f64 from Value");
impl_from_value_num!(i8, "Could not retrieve i8 from Value");
impl_from_value_num!(u8, "Could not retrieve u8 from Value");
impl_from_value_num!(i16, "Could not retrieve i16 from Value");
impl_from_value_num!(u16, "Could not retrieve u16 from Value");
impl_from_value_num!(i32, "Could not retrieve i32 from Value");
impl_from_value_num!(u32, "Could not retrieve u32 from Value");
impl_from_value_num!(isize, "Could not retrieve isize from Value");
impl_from_value_num!(usize, "Could not retrieve usize from Value");

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
    (u64) => (
        impl From<u64> for Value {
            fn from(x: u64) -> Value {
                Value::UInt(x)
            }
        }
    );
    (i64) => (
        impl From<i64> for Value {
            fn from(x: i64) -> Value {
                Value::Int(x)
            }
        }
    );
    ($t:ty) => (
        impl From<$t> for Value {
            fn from(x: $t) -> Value {
                Value::Int(x as i64)
            }
        }
    );
);

into_value_impl!(i8);
into_value_impl!(u8);
into_value_impl!(i16);
into_value_impl!(u16);
into_value_impl!(i32);
into_value_impl!(u32);
into_value_impl!(i64);
into_value_impl!(u64);
into_value_impl!(isize);

impl From<usize> for Value {
    fn from(x: usize) -> Value {
        if x as u64 <= ::std::i64::MAX as u64 {
            Value::Int(x as i64)
        } else {
            Value::UInt(x as u64)
        }
    }
}

impl From<f32> for Value {
    fn from(x: f32) -> Value {
        Value::Float(x as f64)
    }
}

impl From<f64> for Value {
    fn from(x: f64) -> Value {
        Value::Float(x)
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value::Int(if x {1} else {0})
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
        Value::Date(
            x.year() as u16,
            x.month() as u8,
            x.day() as u8,
            0,
            0,
            0,
            0
        )
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

impl From<Timespec> for Value {
    fn from(x: Timespec) -> Value {
        let t = at(x);
        Value::Date(
            t.tm_year as u16 + 1_900,
            (t.tm_mon + 1) as u8,
            t.tm_mday as u8,
            t.tm_hour as u8,
            t.tm_min as u8,
            t.tm_sec as u8,
            t.tm_nsec as u32 / 1000)
    }
}

impl From<Duration> for Value {
    fn from(x: Duration) -> Value {
        let mut secs_total = x.as_secs();
        let micros = (x.subsec_nanos() as f64 / 1000_f64).round() as u32;
        let seconds = (secs_total % 60) as u8;
        secs_total -= seconds as u64;
        let minutes = ((secs_total % (60 * 60)) / 60) as u8;
        secs_total -= (minutes as u64) * 60;
        let hours = ((secs_total % (60 * 60 * 24)) / (60 * 60)) as u8;
        secs_total -= (hours as u64) * 60 * 60;
        Value::Time(false, (secs_total / (60 * 60 * 24)) as u32, hours, minutes, seconds, micros)
    }
}

impl From<time::Duration> for Value {
    fn from(mut x: time::Duration) -> Value {
        let negative = x < time::Duration::zero();
        if negative {
            x = -x;
        }
        let days = x.num_days() as u32;
        x = x - time::Duration::days(x.num_days());
        let hours = x.num_hours() as u8;
        x = x - time::Duration::hours(x.num_hours());
        let minutes = x.num_minutes() as u8;
        x = x - time::Duration::minutes(x.num_minutes());
        let seconds = x.num_seconds() as u8;
        x = x - time::Duration::seconds(x.num_seconds());
        let microseconds = x.num_microseconds().unwrap_or(0) as u32;
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

impl Into<Value> for Uuid {
    fn into(self) -> Value {
        Value::Bytes(self.as_bytes().to_vec())
    }
}

/// Intermediate result of a Value-to-Uuid conversion.
#[derive(Debug)]
pub struct UuidIr {
    val: Uuid,
    bytes: Vec<u8>,
}

impl ConvIr<Uuid> for UuidIr {
    fn new(v: Value) -> Result<UuidIr, FromValueError> {
        match v {
            Value::Bytes(bytes) => match Uuid::from_bytes(bytes.as_slice()) {
                Ok(val) => Ok(UuidIr { val: val, bytes: bytes }),
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
