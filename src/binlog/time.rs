use std::{
    cmp::min,
    fmt::{self, Write},
};

use super::misc::{my_packed_time_get_frac_part, my_packed_time_get_int_part};

/// Server-side mysql time representation.
#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct MysqlTime {
    pub year: u32,
    pub month: u32,
    pub day: u32,
    pub hour: u32,
    pub minute: u32,
    pub second: u32,
    /// microseconds
    pub second_part: u32,
    pub neg: bool,
    pub time_type: MysqlTimestampType,
    pub time_zone_displacement: i32,
}

impl MysqlTime {
    /// Convert time packed numeric representation to [`MysqlTime`].
    pub fn from_int64_time_packed(mut packed_value: i64) -> Self {
        let neg = packed_value < 0;
        if neg {
            packed_value = -packed_value
        }

        let hms: i64 = my_packed_time_get_int_part(packed_value);

        let hour = (hms >> 12) as u32 % (1 << 10); /* 10 bits starting at 12th */
        let minute = (hms >> 6) as u32 % (1 << 6); /* 6 bits starting at 6th   */
        let second = hms as u32 % (1 << 6); /* 6 bits starting at 0th   */
        let second_part = my_packed_time_get_frac_part(packed_value);

        Self {
            year: 0,
            month: 0,
            day: 0,
            hour,
            minute,
            second,
            second_part: second_part as u32,
            neg,
            time_type: MysqlTimestampType::MYSQL_TIMESTAMP_TIME,
            time_zone_displacement: 0,
        }
    }

    /// Convert packed numeric date representation to [`MysqlTime`].
    pub fn from_int64_date_packed(packed_value: i64) -> Self {
        let mut this = Self::from_int64_datetime_packed(packed_value);
        this.time_type = MysqlTimestampType::MYSQL_TIMESTAMP_DATE;
        this
    }

    /// Convert packed numeric datetime representation to [`MysqlTime`].
    pub fn from_int64_datetime_packed(mut packed_value: i64) -> Self {
        let neg = packed_value < 0;
        if neg {
            packed_value = -packed_value
        }

        let second_part = my_packed_time_get_frac_part(packed_value);
        let ymdhms: i64 = my_packed_time_get_int_part(packed_value);

        let ymd: i64 = ymdhms >> 17;
        let ym: i64 = ymd >> 5;
        let hms: i64 = ymdhms % (1 << 17);

        let day = ymd % (1 << 5);
        let month = ym % 13;
        let year = (ym / 13) as _;

        let second = hms % (1 << 6);
        let minute = (hms >> 6) % (1 << 6);
        let hour = (hms >> 12) as _;

        Self {
            year,
            month: month as _,
            day: day as _,
            hour,
            minute: minute as _,
            second: second as _,
            second_part: second_part as _,
            neg,
            time_type: MysqlTimestampType::MYSQL_TIMESTAMP_DATETIME,
            time_zone_displacement: 0,
        }
    }
}

impl fmt::Display for MysqlTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.time_type {
            MysqlTimestampType::MYSQL_TIMESTAMP_DATETIME
            | MysqlTimestampType::MYSQL_TIMESTAMP_DATETIME_TZ => format_datetime(self, f),
            MysqlTimestampType::MYSQL_TIMESTAMP_DATE => format_date(self, f),
            MysqlTimestampType::MYSQL_TIMESTAMP_TIME => format_time(self, f),
            MysqlTimestampType::MYSQL_TIMESTAMP_NONE
            | MysqlTimestampType::MYSQL_TIMESTAMP_ERROR => Ok(()),
        }
    }
}

fn trim_two_digits(value: u32) -> u32 {
    if value >= 100 { 0 } else { value }
}

/// Formats a time value as `HH:MM:SS[.fraction]`.
fn format_time(time: &MysqlTime, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if time.neg {
        f.write_char('-')?;
    }

    write!(
        f,
        "{:02}:{:02}:{:02}",
        time.hour,
        trim_two_digits(time.minute),
        trim_two_digits(time.second),
    )?;
    format_useconds(time.second_part, f)?;
    Ok(())
}

/// Formats a datetime value with an optional fractional part (if formatter precision is given).
fn format_datetime(time: &MysqlTime, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    format_date_and_time(time, f)?;
    format_useconds(time.second_part, f)?;
    if time.time_type == MysqlTimestampType::MYSQL_TIMESTAMP_DATETIME_TZ {
        format_tz(time.time_zone_displacement, f)?;
    }
    Ok(())
}

/// Formats date and time part as 'YYYY-MM-DD hh:mm:ss'
fn format_date_and_time(time: &MysqlTime, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
        f,
        "{:02}{:02}-{:02}-{:02} {:02}:{:02}:{:02}",
        trim_two_digits(time.year / 100),
        trim_two_digits(time.year % 100),
        trim_two_digits(time.month),
        trim_two_digits(time.day),
        trim_two_digits(time.hour),
        trim_two_digits(time.minute),
        trim_two_digits(time.second),
    )
}

/// Formats a date value as 'YYYY-MM-DD'.
fn format_date(time: &MysqlTime, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
        f,
        "{:02}{:02}-{:02}-{:02}",
        trim_two_digits(time.year / 100),
        trim_two_digits(time.year % 100),
        trim_two_digits(time.month),
        trim_two_digits(time.day),
    )
}

/// Only formats useconds if formatter precision is given (will be truncated to 6)
fn format_useconds(mut useconds: u32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Some(dec) = f.precision().map(|x| min(x, 6)) else {
        return Ok(());
    };

    if dec == 0 {
        return Ok(());
    }

    useconds %= 1_000_000;

    for _ in 0..(6 - dec) {
        useconds /= 10;
    }

    write!(f, ".{:0width$}", useconds, width = dec)
}

fn format_tz(tzd: i32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "+{:02}:{:02}", tzd / 3600, tzd.abs() / 60 % 60)
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum MysqlTimestampType {
    /// Textual representation of this value is an empty string
    MYSQL_TIMESTAMP_NONE = -2,
    /// Textual representation of this value is an empty string
    MYSQL_TIMESTAMP_ERROR = -1,
    MYSQL_TIMESTAMP_DATE = 0,
    MYSQL_TIMESTAMP_DATETIME = 1,
    MYSQL_TIMESTAMP_TIME = 2,
    MYSQL_TIMESTAMP_DATETIME_TZ = 3,
}
