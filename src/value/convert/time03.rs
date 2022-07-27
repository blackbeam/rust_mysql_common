// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module implements conversion from/to `Value` for `time` v0.3.x crate types.

#![cfg(feature = "time03")]

use std::{cmp::Ordering, convert::TryFrom, str::from_utf8};

use time03::{
    error::{Parse, TryFromParsed},
    format_description::{
        modifier::{self, Subsecond},
        Component, FormatItem,
    },
    Date, PrimitiveDateTime, Time,
};

use crate::value::Value;

use super::{parse_mysql_time_string, ConvIr, FromValueError, ParseIr};

lazy_static::lazy_static! {
    static ref FULL_YEAR: modifier::Year = {
        let mut year_modifier = modifier::Year::default();
        year_modifier.padding = modifier::Padding::Zero;
        year_modifier.repr = modifier::YearRepr::Full;
        year_modifier.iso_week_based = false;
        year_modifier.sign_is_mandatory = false;

        year_modifier
    };

    static ref ZERO_PADDED_MONTH: modifier::Month = {
        let mut month_modifier = modifier::Month::default();
        month_modifier.padding = modifier::Padding::Zero;
        month_modifier.repr = modifier::MonthRepr::Numerical;
        month_modifier.case_sensitive = true;

        month_modifier
    };

    static ref ZERO_PADDED_DAY: modifier::Day = {
        let mut day_modifier = modifier::Day::default();
        day_modifier.padding = modifier::Padding::Zero;

        day_modifier
    };

    static ref ZERO_PADDED_HOUR: modifier::Hour = {
        let mut hour_modifier = modifier::Hour::default();
        hour_modifier.padding = modifier::Padding::Zero;
        hour_modifier.is_12_hour_clock = false;

        hour_modifier
    };

    static ref ZERO_PADDED_MINUTE: modifier::Minute = {
        let mut minute_modifier = modifier::Minute::default();
        minute_modifier.padding = modifier::Padding::Zero;

        minute_modifier
    };

    static ref ZERO_PADDED_SECOND: modifier::Second = {
        let mut second_modifier = modifier::Second::default();
        second_modifier.padding = modifier::Padding::Zero;

        second_modifier
    };

    static ref DATE_FORMAT: Vec<FormatItem<'static>> = {
        vec![
            FormatItem::Component(Component::Year(*FULL_YEAR)),
            FormatItem::Literal(b"-"),
            FormatItem::Component(Component::Month(*ZERO_PADDED_MONTH)),
            FormatItem::Literal(b"-"),
            FormatItem::Component(Component::Day(*ZERO_PADDED_DAY)),
        ]
    };

    static ref TIME_FORMAT: Vec<FormatItem<'static>> = {
        vec![
            FormatItem::Component(Component::Hour(*ZERO_PADDED_HOUR)),
            FormatItem::Literal(b":"),
            FormatItem::Component(Component::Minute(*ZERO_PADDED_MINUTE)),
            FormatItem::Literal(b":"),
            FormatItem::Component(Component::Second(*ZERO_PADDED_SECOND)),
        ]
    };

    static ref TIME_FORMAT_MICRO: Vec<FormatItem<'static>> = {
        vec![
            FormatItem::Component(Component::Hour(*ZERO_PADDED_HOUR)),
            FormatItem::Literal(b":"),
            FormatItem::Component(Component::Minute(*ZERO_PADDED_MINUTE)),
            FormatItem::Literal(b":"),
            FormatItem::Component(Component::Second(*ZERO_PADDED_SECOND)),
            FormatItem::Literal(b"."),
            FormatItem::Component(Component::Subsecond(Subsecond::default())),
        ]
    };

    static ref DATE_TIME_FORMAT: Vec<FormatItem<'static>> = {
        let mut format = DATE_FORMAT.clone();
        format.push(FormatItem::Literal(b" "));
        format.extend_from_slice(&*TIME_FORMAT);
        format
    };

    static ref DATE_TIME_FORMAT_MICRO: Vec<FormatItem<'static>> = {
        let mut format = DATE_FORMAT.clone();
        format.push(FormatItem::Literal(b" "));
        format.extend_from_slice(&*TIME_FORMAT_MICRO);
        format
    };
}

impl ConvIr<PrimitiveDateTime> for ParseIr<PrimitiveDateTime> {
    fn new(value: Value) -> Result<ParseIr<PrimitiveDateTime>, FromValueError> {
        match value {
            Value::Date(year, month, day, hour, minute, second, micros) => Ok(ParseIr {
                value: Value::Date(year, month, day, hour, minute, second, micros),
                output: match create_primitive_date_time(
                    year, month, day, hour, minute, second, micros,
                ) {
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

/// Converts a MySQL `DATE` value to a `time03::Date`.
impl ConvIr<Date> for ParseIr<Date> {
    fn new(value: Value) -> Result<ParseIr<Date>, FromValueError> {
        match value {
            Value::Date(year, month, day, hour, minute, second, micros) => {
                let mon = match time03::Month::try_from(month) {
                    Ok(month) => month,
                    Err(_) => return Err(FromValueError(value)),
                };
                Ok(ParseIr {
                    value: Value::Date(year, month, day, hour, minute, second, micros),
                    output: match Date::from_calendar_date(year as i32, mon, day) {
                        Ok(date) => date,
                        Err(_) => return Err(FromValueError(value)),
                    },
                })
            }
            Value::Bytes(bytes) => {
                match from_utf8(&*bytes)
                    .ok()
                    .and_then(|s| Date::parse(s, &*DATE_FORMAT).ok())
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

/// Converts a MySQL `TIME` value to a `time03::Time`.
/// Note: `time03::Time` only allows for time values in the 00:00:00 - 23:59:59 range.
/// If you're expecting `TIME` values in MySQL's `TIME` value range of -838:59:59 - 838:59:59,
/// use time03::Duration instead.
impl ConvIr<Time> for ParseIr<Time> {
    fn new(value: Value) -> Result<ParseIr<Time>, FromValueError> {
        match value {
            Value::Time(false, 0, h, m, s, u) => Ok(ParseIr {
                value: Value::Time(false, 0, h, m, s, u),
                output: match Time::from_hms_micro(h, m, s, u) {
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

fn create_primitive_date_time(
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    micros: u32,
) -> Option<PrimitiveDateTime> {
    let mon = time03::Month::try_from(month).ok()?;
    if let Ok(date) = Date::from_calendar_date(year as i32, mon, day) {
        if let Ok(time) = Time::from_hms_micro(hour, minute, second, micros) {
            return Some(PrimitiveDateTime::new(date, time));
        }
    }

    None
}

pub(crate) fn parse_mysql_datetime_string_with_time(
    bytes: &[u8],
) -> Result<PrimitiveDateTime, Parse> {
    from_utf8(&*bytes)
        .map_err(|_| Parse::TryFromParsed(TryFromParsed::InsufficientInformation))
        .and_then(|s| {
            if s.len() > 19 {
                PrimitiveDateTime::parse(s, &*DATE_TIME_FORMAT_MICRO)
            } else if s.len() == 19 {
                PrimitiveDateTime::parse(&s[..19], &*DATE_TIME_FORMAT)
            } else if s.len() >= 10 {
                PrimitiveDateTime::parse(s, &*DATE_FORMAT)
            } else {
                Err(Parse::TryFromParsed(TryFromParsed::InsufficientInformation))
            }
        })
}

fn parse_mysql_time_string_with_time(bytes: &[u8]) -> Result<Time, Parse> {
    from_utf8(&*bytes)
        .map_err(|_| Parse::TryFromParsed(TryFromParsed::InsufficientInformation))
        .and_then(|s| match s.len().cmp(&8) {
            Ordering::Less => Err(Parse::TryFromParsed(TryFromParsed::InsufficientInformation)),
            Ordering::Equal => Time::parse(s, &*TIME_FORMAT),
            Ordering::Greater => Time::parse(s, &*TIME_FORMAT_MICRO),
        })
}

impl ConvIr<time03::Duration> for ParseIr<time03::Duration> {
    fn new(v: Value) -> Result<ParseIr<time03::Duration>, FromValueError> {
        match v {
            Value::Time(is_neg, days, hours, minutes, seconds, microseconds) => {
                let duration = time03::Duration::days(days.into())
                    + time03::Duration::hours(hours.into())
                    + time03::Duration::minutes(minutes.into())
                    + time03::Duration::seconds(seconds.into())
                    + time03::Duration::microseconds(microseconds.into());
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
                        let duration = time03::Duration::hours(hours.into())
                            + time03::Duration::minutes(minutes.into())
                            + time03::Duration::seconds(seconds.into())
                            + time03::Duration::microseconds(microseconds.into());
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
    fn commit(self) -> time03::Duration {
        self.output
    }
    fn rollback(self) -> Value {
        self.value
    }
}

impl From<PrimitiveDateTime> for Value {
    fn from(x: PrimitiveDateTime) -> Value {
        Value::Date(
            x.year() as u16,
            x.month() as u8,
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
        Value::Date(x.year() as u16, x.month() as u8, x.day(), 0, 0, 0, 0)
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

impl From<time03::Duration> for Value {
    fn from(mut x: time03::Duration) -> Value {
        let negative = x < time03::Duration::ZERO;

        if negative {
            x = -x;
        }

        let days = x.whole_days() as u32;
        x = x - time03::Duration::days(x.whole_days());
        let hours = x.whole_hours() as u8;
        x = x - time03::Duration::hours(x.whole_hours());
        let minutes = x.whole_minutes() as u8;
        x = x - time03::Duration::minutes(x.whole_minutes());
        let seconds = x.whole_seconds() as u8;
        x = x - time03::Duration::seconds(x.whole_seconds());
        let microseconds = x.whole_microseconds() as u32;

        Value::Time(negative, days, hours, minutes, seconds, microseconds)
    }
}

impl_from_value!(PrimitiveDateTime, ParseIr<PrimitiveDateTime>);
impl_from_value!(Date, ParseIr<Date>);
impl_from_value!(Time, ParseIr<Time>);
impl_from_value!(time03::Duration, ParseIr<time03::Duration>);

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use time03::error::ParseFromDescription;

    use super::*;

    proptest! {
        #[test]
        fn parse_mysql_time_string_doesnt_crash(s in r"\PC*") {
            let _ = parse_mysql_time_string_with_time(s.as_bytes());
        }

        #[test]
        fn parse_mysql_datetime_string_doesnt_crash(s in r"\PC*") {
            let _ = parse_mysql_datetime_string_with_time(s.as_bytes());
        }

        #[test]
        fn parse_mysql_time_string_parses_correctly(
            h in 0u32..60,
            i in 0u32..60,
            s in 0u32..60,
            have_us in 0..2,
            us in 0u32..1000000,
        ) {
            let time_string = format!(
                "{:02}:{:02}:{:02}{}",
                h, i, s,
                if have_us == 1 {
                    format!(".{:06}", us).trim_end_matches('0').to_owned()
                } else {
                    "".into()
                }
            );

            match parse_mysql_time_string_with_time(time_string.as_bytes()) {
                Ok(time) => {
                    // If `time` successfully parsed the string,
                    // then let's ensure it matches the values used to create that time string.

                    // `time` and other C-like `strptime` based parsers have no way of parsing
                    // microseconds from the time string. As such, we ignore them entirely.
                    assert_eq!(
                        (
                            time.hour() as u32,
                            time.minute() as u32,
                            time.second() as u32,
                            time.microsecond() as u32,
                        ),
                        (h, i, s, if have_us == 1 { us } else { 0 }));
                },
                Err(err) => {
                    // If `time` failed to parse the string,
                    // then let's check if we passed an invalid value based on the error received,
                    // and fail the test if the string should have parsed successfully.
                    // Any commented out checks are simply to avoid having the compiler show
                    // 'comparison is useless due to type limits' warnings.
                    match err {
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("second")) => assert!(/*s < 0 || */s > 59),
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("minute")) => assert!(/*i < 0 || */i > 59),
                        // For InvalidHour, only check if the randomized hour value is within
                        // 0-23 instead of MySQL `TIME`'s full range of -838-838,
                        // since we don't generate values that low or high,
                        // and should be parsed with time::Duration instead.
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("hour")) => assert!(/*h < 0 || */h > 23),
                        Parse::TryFromParsed(TryFromParsed::ComponentRange(_))  |
                        Parse::TryFromParsed(TryFromParsed::InsufficientInformation) => {
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

                            if Time::from_hms_micro(h as u8, 0, 0, 0).is_err() {
                                assert!(/*h < 0 || */h > 23);
                            } else if Time::from_hms_micro(0, i as u8, 0, 0).is_err() {
                                assert!(/*i < 0 || */i > 59);
                            } else if Time::from_hms_micro(0, 0, s as u8, 0).is_err() {
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
                            panic!("Failed to parse time `{}` due to an unknown reason. {}", time_string, err);
                        }
                    }
                }
            }
        }

        #[test]
        fn parse_mysql_datetime_string_parses_correctly(
            y in 0u32..10000,
            m in 0u32..12,
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
                    format!(".{:06}", us).trim_end_matches('0').to_owned()
                } else {
                    "".into()
                }
            );

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
                            datetime.microsecond() as u32,
                        ),
                        (y, m, d, h, i, s, if have_us == 1 { us } else { 0 }));
                },
                Err(err) => {
                    // If `time` failed to parse the string,
                    // then let's check if we passed an invalid value based on the error received,
                    // and fail the test if the string should have parsed successfully.
                    // Any commented out checks are simply to avoid having the compiler show
                    // 'comparison is useless due to type limits' warnings.
                    match err {
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("second")) => assert!(/*s < 0 || */s > 59),
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("minute")) => assert!(/*i < 0 || */i > 59),
                        // For InvalidHour, only check if the randomized hour value is within
                        // 0-23 instead of MySQL `TIME`'s full range of -838-838,
                        // since we don't generate values that low or high,
                        // and should be parsed with time::Duration instead.
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("hour")) => assert!(/*h < 0 || */h > 23),
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("dayofmonth")) => assert!(!(1..=31).contains(&d)),
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("month")) => assert!(m < 1000 || m <= 12),
                        // For InvalidYear, ensure that the year isn't also 0,
                        // which is a valid value with non-strict-mode MySQL.
                        Parse::ParseFromDescription(ParseFromDescription::InvalidComponent("year")) => assert!(y != 0 && !(1000..=9999).contains(&y)),
                        Parse::TryFromParsed(TryFromParsed::ComponentRange(_))  |
                        Parse::TryFromParsed(TryFromParsed::InsufficientInformation) => {
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

                            if Date::from_calendar_date(y as i32, time03::Month::January, 1).is_err() {
                                assert!(y != 0 && !(1000..=9999).contains(&y));
                            } else if time03::Month::try_from(m as u8).is_err() {
                                assert!(m < 1000 || m <= 12);
                            } else if Date::from_calendar_date(0, time03::Month::January, d as u8).is_err() {
                                assert!(!(1..=31).contains(&d));
                            } else if Time::from_hms_micro(h as u8, 0, 0, 0).is_err() {
                                assert!(/*h < 0 || */h > 23);
                            } else if Time::from_hms_micro(0, i as u8, 0, 0).is_err() {
                                assert!(/*i < 0 || */i > 59);
                            } else if Time::from_hms_micro(0, 0, s as u8, 0).is_err() {
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
                            panic!("Failed to parse time `{}` due to an unknown reason. {}", time_string, err);
                        }
                    }
                }
            }
        }
    }
}
