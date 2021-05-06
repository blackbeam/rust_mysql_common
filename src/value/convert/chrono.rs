// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module implements conversion from/to `Value` for `chrono` types.

#![cfg(feature = "chrono")]

use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, Timelike};

use crate::value::Value;

use super::{
    parse_mysql_datetime_string, parse_mysql_time_string, ConvIr, FromValueError, ParseIr,
};

impl ConvIr<NaiveDateTime> for ParseIr<NaiveDateTime> {
    fn new(value: Value) -> Result<ParseIr<NaiveDateTime>, FromValueError> {
        let result = match value {
            Value::Date(year, month, day, hour, minute, second, micros) => {
                let date = NaiveDate::from_ymd_opt(year.into(), month.into(), day.into());
                let time = NaiveTime::from_hms_micro_opt(
                    hour.into(),
                    minute.into(),
                    second.into(),
                    micros,
                );
                Ok((
                    date,
                    time,
                    Value::Date(year, month, day, hour, minute, second, micros),
                ))
            }
            Value::Bytes(bytes) => {
                if let Some((year, month, day, hour, minute, second, micros)) =
                    parse_mysql_datetime_string(&*bytes)
                {
                    let date = NaiveDate::from_ymd_opt(year as i32, month, day);
                    let time = NaiveTime::from_hms_micro_opt(hour, minute, second, micros);
                    Ok((date, time, Value::Bytes(bytes)))
                } else {
                    Err(FromValueError(Value::Bytes(bytes)))
                }
            }
            v => Err(FromValueError(v)),
        };

        let (date, time, value) = result?;

        if let (Some(date), Some(time)) = (date, time) {
            Ok(ParseIr {
                value,
                output: NaiveDateTime::new(date, time),
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
            Value::Date(year, month, day, hour, minute, second, micros) => {
                let date = NaiveDate::from_ymd_opt(year.into(), month.into(), day.into());
                Ok((
                    date,
                    Value::Date(year, month, day, hour, minute, second, micros),
                ))
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

        if let Some(output) = date {
            Ok(ParseIr { value, output })
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

        if let Some(output) = time {
            Ok(ParseIr { value, output })
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

#[cfg(feature = "chrono")]
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

impl_from_value!(NaiveDateTime, ParseIr<NaiveDateTime>);
impl_from_value!(NaiveDate, ParseIr<NaiveDate>);
impl_from_value!(NaiveTime, ParseIr<NaiveTime>);
impl_from_value!(chrono::Duration, ParseIr<chrono::Duration>);
