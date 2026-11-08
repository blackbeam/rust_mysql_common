// Copyright (c) 2026 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{borrow::Cow, cmp::Reverse, io};

use bytes::BufMut;

use crate::{
    constants::{
        ColumnFlags, ColumnType, Command, CursorType, MAX_PAYLOAD_LEN, StmtExecuteParamFlags,
    },
    io::{BufMutExt, ParseBuf},
    misc::{
        lenenc_str_len,
        raw::{Const, RawInt, int::LeU32},
        unexpected_buf_eof,
    },
    packets::ComStmtSendLongData,
    proto::{MyDeserialize, MySerialize},
    value::{BinValue, Value, ValueDeserializer},
};

mod tests;

/// A single parameter of a prepared statement.
///
/// `StmtParam` keeps the value in its logical form together with the wire type that should be
/// sent to the server. The actual encoding is performed by [`StmtExecuteParams`].
#[derive(Debug, Clone, PartialEq)]
pub struct StmtParam<'a> {
    data: StmtParamData<'a>,
    kind: StmtParamKind,
    long_data: LongDataMode,
}

#[derive(Debug, Clone, PartialEq)]
enum StmtParamKind {
    /// Logical type; the wire type depends on [`ServerFlavor`].
    Typed(StmtParamType),
    /// Escape hatch: `value` is already in the wire format expected for `column_type`.
    Raw {
        column_type: ColumnType,
        flags: StmtExecuteParamFlags,
    },
}

/// Value of a [`StmtParam`].
#[derive(Debug, Clone, PartialEq)]
pub enum StmtParamData<'a> {
    Null,
    Bytes(Cow<'a, [u8]>),
    Int(i64),
    UInt(u64),
    Float(f32),
    Double(f64),
    Date {
        year: u16,
        month: u8,
        day: u8,
    },
    DateTime {
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
        micros: u32,
    },
    Time {
        negative: bool,
        days: u32,
        hours: u8,
        minutes: u8,
        seconds: u8,
        micros: u32,
    },
}

/// Logical type of a [`StmtParam`].
///
/// This is a curated subset of [`ColumnType`]: the server accepts only some type codes for
/// parameters, and for numeric codes the width of the value must match the code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StmtParamType {
    Null,

    Tiny,
    Short,
    Long,
    LongLong,

    UTiny,
    UShort,
    ULong,
    ULongLong,

    Float,
    Double,

    /// `MYSQL_TYPE_NEWDECIMAL`; the value is a length-encoded string.
    Decimal,

    /// `MYSQL_TYPE_DATE`; the value is a length-prefixed date.
    Date,
    /// `MYSQL_TYPE_DATETIME`.
    DateTime,
    /// `MYSQL_TYPE_TIMESTAMP`.
    Timestamp,
    /// `MYSQL_TYPE_TIME`.
    Time,

    /// `MYSQL_TYPE_VAR_STRING`.
    VarString,
    /// `MYSQL_TYPE_STRING`.
    String,
    /// `MYSQL_TYPE_BLOB` (binary collation).
    Blob,
    /// `MYSQL_TYPE_JSON` on MySQL; mapped to `VAR_STRING` on MariaDB.
    Json,
}

/// Long data policy of a single parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LongDataMode {
    /// Inline if it fits into the packet budget; otherwise `COM_STMT_SEND_LONG_DATA`.
    #[default]
    Auto,
    /// Always use `COM_STMT_SEND_LONG_DATA`.
    Always,
    /// Inline only; fails if the parameter does not fit.
    Never,
    /// Data was already sent by the user via `COM_STMT_SEND_LONG_DATA`.
    ///
    /// The driver sends no chunks and excludes the value from the inline section.
    External,
}

/// Server behavior that affects the wire type mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ServerFlavor {
    #[default]
    MySql,
    MariaDb,
}

/// Errors of [`StmtParam::as_type`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum StmtParamError {
    #[error("type {ty:?} is incompatible with value kind {kind}")]
    IncompatibleType {
        ty: StmtParamType,
        kind: &'static str,
    },

    #[error("value is out of range for type {ty:?}")]
    OutOfRange { ty: StmtParamType },
}

/// Errors of [`StmtExecuteRequestBuilder::build`].
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum StmtExecuteError {
    #[error("too many parameters: {count} > {}", u16::MAX)]
    TooManyParams { count: usize },

    #[error("max_allowed_packet is too small: {value}")]
    PacketSizeTooSmall { value: usize },

    #[error("parameters payload is too large: needed {needed}, limit {limit}")]
    PayloadTooLarge { needed: usize, limit: usize },

    #[error("parameter {index}: type {ty:?} does not support long data")]
    LongDataNotSupported { index: usize, ty: StmtParamType },

    #[error("parameter {index}: NULL cannot be sent as long data")]
    NullWithLongData { index: usize },

    #[error("parameter {index}: raw parameters cannot use long data")]
    RawWithLongData { index: usize },

    #[error("parameter {index}: External long data parameters must not carry a value")]
    ExternalWithData { index: usize },

    #[error("parameter {index}: field `{field}` is invalid")]
    InvalidParam { index: usize, field: &'static str },
}

/// Errors of [`StmtExecuteRequest::reuse_types`] and
/// [`StmtExecuteRequest::reuse_types_unchecked`].
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum StmtExecuteMetadataError {
    #[error("parameter count mismatch: expected {expected}, actual {actual}")]
    ParamCountMismatch { expected: usize, actual: usize },

    #[error("parameter {index} type mismatch")]
    TypeMismatch {
        index: usize,
        expected: (ColumnType, StmtExecuteParamFlags),
        actual: (ColumnType, StmtExecuteParamFlags),
    },
}

impl<'a> StmtParam<'a> {
    /// SQL `NULL`; the wire type is `MYSQL_TYPE_NULL`.
    pub fn null() -> Self {
        Self {
            data: StmtParamData::Null,
            kind: StmtParamKind::Typed(StmtParamType::Null),
            long_data: LongDataMode::Auto,
        }
    }

    /// SQL `NULL` with an explicit wire type.
    ///
    /// The server ignores the type of a `NULL` parameter, but `NEW_PARAMS_BOUND = 0` reuses the
    /// type from the previous execute. Use this method when a nullable column must keep stable
    /// metadata across a stream (for example, `null_as(StmtParamType::VarString)`).
    pub fn null_as(ty: StmtParamType) -> Self {
        Self {
            data: StmtParamData::Null,
            kind: StmtParamKind::Typed(ty),
            long_data: LongDataMode::Auto,
        }
    }

    /// String in the connection encoding (`MYSQL_TYPE_VAR_STRING`).
    pub fn bytes(value: impl Into<Cow<'a, [u8]>>) -> Self {
        Self::with_bytes(StmtParamType::VarString, value)
    }

    /// Binary data (`MYSQL_TYPE_BLOB`, binary collation).
    pub fn blob(value: impl Into<Cow<'a, [u8]>>) -> Self {
        Self::with_bytes(StmtParamType::Blob, value)
    }

    /// JSON value; `MYSQL_TYPE_JSON` on MySQL, `MYSQL_TYPE_VAR_STRING` on MariaDB.
    pub fn json(value: impl Into<Cow<'a, [u8]>>) -> Self {
        Self::with_bytes(StmtParamType::Json, value)
    }

    fn with_bytes(ty: StmtParamType, value: impl Into<Cow<'a, [u8]>>) -> Self {
        Self {
            data: StmtParamData::Bytes(value.into()),
            kind: StmtParamKind::Typed(ty),
            long_data: LongDataMode::Auto,
        }
    }

    /// Signed integer; `LONGLONG` by default.
    pub fn int(value: i64) -> Self {
        Self {
            data: StmtParamData::Int(value),
            kind: StmtParamKind::Typed(StmtParamType::LongLong),
            long_data: LongDataMode::Auto,
        }
    }

    /// Unsigned integer; `LONGLONG` with the `UNSIGNED` flag by default.
    pub fn uint(value: u64) -> Self {
        Self {
            data: StmtParamData::UInt(value),
            kind: StmtParamKind::Typed(StmtParamType::ULongLong),
            long_data: LongDataMode::Auto,
        }
    }

    pub fn float(value: f32) -> Self {
        Self {
            data: StmtParamData::Float(value),
            kind: StmtParamKind::Typed(StmtParamType::Float),
            long_data: LongDataMode::Auto,
        }
    }

    pub fn double(value: f64) -> Self {
        Self {
            data: StmtParamData::Double(value),
            kind: StmtParamKind::Typed(StmtParamType::Double),
            long_data: LongDataMode::Auto,
        }
    }

    /// Decimal value as a string (`MYSQL_TYPE_NEWDECIMAL`).
    pub fn decimal(value: impl Into<Cow<'a, [u8]>>) -> Self {
        Self::with_bytes(StmtParamType::Decimal, value)
    }

    pub fn date(year: u16, month: u8, day: u8) -> Self {
        Self {
            data: StmtParamData::Date { year, month, day },
            kind: StmtParamKind::Typed(StmtParamType::Date),
            long_data: LongDataMode::Auto,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn datetime(
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
        micros: u32,
    ) -> Self {
        Self {
            data: StmtParamData::DateTime {
                year,
                month,
                day,
                hour,
                minute,
                second,
                micros,
            },
            kind: StmtParamKind::Typed(StmtParamType::DateTime),
            long_data: LongDataMode::Auto,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn time(
        negative: bool,
        days: u32,
        hours: u8,
        minutes: u8,
        seconds: u8,
        micros: u32,
    ) -> Self {
        Self {
            data: StmtParamData::Time {
                negative,
                days,
                hours,
                minutes,
                seconds,
                micros,
            },
            kind: StmtParamKind::Typed(StmtParamType::Time),
            long_data: LongDataMode::Auto,
        }
    }

    /// Changes the logical type, checking compatibility and the value range.
    ///
    /// For example, `StmtParam::int(42).as_type(StmtParamType::Long)` re-encodes the value as
    /// 4 bytes, while `StmtParam::int(1 << 40).as_type(StmtParamType::Long)` returns
    /// [`StmtParamError::OutOfRange`].
    pub fn as_type(self, ty: StmtParamType) -> Result<Self, StmtParamError> {
        let Self {
            data,
            kind,
            long_data,
        } = self;

        let data = match kind {
            StmtParamKind::Typed(current) => convert_data(data, current, ty)?,
            StmtParamKind::Raw { .. } => {
                return Err(StmtParamError::IncompatibleType { ty, kind: "raw" });
            }
        };

        Ok(Self {
            data,
            kind: StmtParamKind::Typed(ty),
            long_data,
        })
    }

    /// Sets the long data policy.
    ///
    /// [`LongDataMode::External`] is only valid for parameters that carry no value; use
    /// [`StmtParam::external_long_data`] to create such a parameter. Calling this method with
    /// `External` on a parameter that has a value makes [`StmtExecuteRequestBuilder::build`]
    /// fail with [`StmtExecuteError::ExternalWithData`], because the value would otherwise be
    /// silently omitted both from the inline section and from the generated chunks.
    pub fn long_data(mut self, mode: LongDataMode) -> Self {
        self.long_data = mode;
        self
    }

    /// Marks the parameter as already sent via `COM_STMT_SEND_LONG_DATA`.
    ///
    /// The parameter carries no value; [`StmtExecuteParams::long_data`] emits no chunks for it,
    /// and [`StmtExecuteParams`] excludes it from the inline section. The user must send the data
    /// before execute.
    pub fn external_long_data(ty: StmtParamType) -> Self {
        Self {
            data: StmtParamData::Bytes(Cow::Borrowed(&[])),
            kind: StmtParamKind::Typed(ty),
            long_data: LongDataMode::External,
        }
    }

    /// Escape hatch: the bytes are written to the values section as is.
    ///
    /// `value` must already be encoded exactly as the server expects for `column_type`,
    /// including the length prefix for strings and temporal types. Long data is not supported
    /// for raw parameters: a chunk boundary would be ambiguous.
    pub fn raw(
        column_type: ColumnType,
        flags: StmtExecuteParamFlags,
        value: impl Into<Cow<'a, [u8]>>,
    ) -> Self {
        Self {
            data: StmtParamData::Bytes(value.into()),
            kind: StmtParamKind::Raw { column_type, flags },
            long_data: LongDataMode::Auto,
        }
    }

    /// Returns the parameter data.
    ///
    /// Parameters created by [`StmtParam::external_long_data`] carry no data and return an empty
    /// `Bytes` variant.
    pub fn data(&self) -> &StmtParamData<'a> {
        &self.data
    }

    /// Returns the logical type, or `None` for a raw parameter.
    pub fn param_type(&self) -> Option<StmtParamType> {
        match &self.kind {
            StmtParamKind::Typed(ty) => Some(*ty),
            StmtParamKind::Raw { .. } => None,
        }
    }

    pub fn long_data_policy(&self) -> LongDataMode {
        self.long_data
    }

    /// Converts the parameter into a [`Value`].
    ///
    /// Typed parameters are converted directly. Raw parameters (including the ones produced by
    /// [`ComStmtExecute`] deserialization) are decoded from their wire bytes using the stored
    /// column type; unsupported types and malformed bytes are reported as errors.
    ///
    /// A parameter whose value was sent with `COM_STMT_SEND_LONG_DATA`
    /// ([`LongDataMode::External`]) carries no value and yields an error.
    pub fn value(&self) -> io::Result<Value> {
        if self.long_data == LongDataMode::External {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "parameter value was sent as long data",
            ));
        }

        match &self.data {
            StmtParamData::Null => Ok(Value::NULL),
            StmtParamData::Bytes(data) => match &self.kind {
                StmtParamKind::Raw { column_type, flags } => {
                    decode_raw_value(*column_type, *flags, data)
                }
                StmtParamKind::Typed(_) => Ok(Value::Bytes(data.to_vec())),
            },
            StmtParamData::Int(value) => Ok(Value::Int(*value)),
            StmtParamData::UInt(value) => Ok(Value::UInt(*value)),
            StmtParamData::Float(value) => Ok(Value::Float(*value)),
            StmtParamData::Double(value) => Ok(Value::Double(*value)),
            StmtParamData::Date { year, month, day } => {
                Ok(Value::Date(*year, *month, *day, 0, 0, 0, 0))
            }
            StmtParamData::DateTime {
                year,
                month,
                day,
                hour,
                minute,
                second,
                micros,
            } => Ok(Value::Date(
                *year, *month, *day, *hour, *minute, *second, *micros,
            )),
            StmtParamData::Time {
                negative,
                days,
                hours,
                minutes,
                seconds,
                micros,
            } => Ok(Value::Time(
                *negative, *days, *hours, *minutes, *seconds, *micros,
            )),
        }
    }

    fn is_raw(&self) -> bool {
        matches!(&self.kind, StmtParamKind::Raw { .. })
    }

    fn wire_type(&self, flavor: ServerFlavor) -> (ColumnType, StmtExecuteParamFlags) {
        match &self.kind {
            StmtParamKind::Typed(ty) => ty.wire_type(flavor),
            StmtParamKind::Raw { column_type, flags } => (*column_type, *flags),
        }
    }

    fn inline_len(&self, column_type: ColumnType) -> usize {
        match &self.kind {
            StmtParamKind::Raw { .. } => match &self.data {
                StmtParamData::Bytes(data) => data.len(),
                _ => 0,
            },
            StmtParamKind::Typed(_) => self.data.encoded_len(column_type),
        }
    }

    fn validate(&self, index: usize) -> Result<(), StmtExecuteError> {
        let invalid = |field| StmtExecuteError::InvalidParam { index, field };

        match &self.data {
            StmtParamData::Date { month, day, .. } => {
                validate_date(*month, *day).map_err(invalid)?;
            }
            StmtParamData::DateTime {
                month,
                day,
                hour,
                minute,
                second,
                micros,
                ..
            } => {
                validate_date(*month, *day).map_err(invalid)?;
                if *hour > 23 {
                    return Err(invalid("hour"));
                }
                if *minute > 59 {
                    return Err(invalid("minute"));
                }
                if *second > 59 {
                    return Err(invalid("second"));
                }
                if *micros >= 1_000_000 {
                    return Err(invalid("micros"));
                }
            }
            StmtParamData::Time {
                hours,
                minutes,
                seconds,
                micros,
                ..
            } => {
                if *hours > 23 {
                    return Err(invalid("hours"));
                }
                if *minutes > 59 {
                    return Err(invalid("minutes"));
                }
                if *seconds > 59 {
                    return Err(invalid("seconds"));
                }
                if *micros >= 1_000_000 {
                    return Err(invalid("micros"));
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn serialize_value(&self, column_type: ColumnType, buf: &mut Vec<u8>) {
        match &self.data {
            StmtParamData::Null => {}
            StmtParamData::Bytes(data) if self.is_raw() => buf.put_slice(data),
            StmtParamData::Bytes(data) => buf.put_lenenc_str(data),
            StmtParamData::Int(value) => match int_width(column_type) {
                Some(1) => buf.put_i8(*value as i8),
                Some(2) => buf.put_i16_le(*value as i16),
                Some(4) => buf.put_i32_le(*value as i32),
                Some(8) => buf.put_i64_le(*value),
                _ => debug_assert!(false, "invalid integer column type {column_type:?}"),
            },
            StmtParamData::UInt(value) => match int_width(column_type) {
                Some(1) => buf.put_u8(*value as u8),
                Some(2) => buf.put_u16_le(*value as u16),
                Some(4) => buf.put_u32_le(*value as u32),
                Some(8) => buf.put_u64_le(*value),
                _ => debug_assert!(false, "invalid integer column type {column_type:?}"),
            },
            StmtParamData::Float(value) => {
                if column_type == ColumnType::MYSQL_TYPE_DOUBLE {
                    buf.put_f64_le(*value as f64);
                } else {
                    buf.put_f32_le(*value);
                }
            }
            StmtParamData::Double(value) => buf.put_f64_le(*value),
            StmtParamData::Date { year, month, day } => {
                put_date(*year, *month, *day, buf);
            }
            StmtParamData::DateTime {
                year,
                month,
                day,
                hour,
                minute,
                second,
                micros,
            } => {
                put_datetime(*year, *month, *day, *hour, *minute, *second, *micros, buf);
            }
            StmtParamData::Time {
                negative,
                days,
                hours,
                minutes,
                seconds,
                micros,
            } => {
                put_time(*negative, *days, *hours, *minutes, *seconds, *micros, buf);
            }
        }
    }
}

impl<'a> From<&'a Value> for StmtParam<'a> {
    fn from(value: &'a Value) -> Self {
        match value {
            Value::NULL => Self::null(),
            Value::Bytes(value) => Self::bytes(value.as_slice()),
            Value::Int(value) => Self::int(*value),
            Value::UInt(value) => Self::uint(*value),
            Value::Float(value) => Self::float(*value),
            Value::Double(value) => Self::double(*value),
            Value::Date(year, month, day, hour, minute, second, micros) => {
                Self::datetime(*year, *month, *day, *hour, *minute, *second, *micros)
            }
            Value::Time(negative, days, hours, minutes, seconds, micros) => {
                Self::time(*negative, *days, *hours, *minutes, *seconds, *micros)
            }
        }
    }
}

impl From<Value> for StmtParam<'static> {
    fn from(value: Value) -> Self {
        match value {
            Value::NULL => Self::null(),
            Value::Bytes(value) => Self::bytes(value),
            Value::Int(value) => Self::int(value),
            Value::UInt(value) => Self::uint(value),
            Value::Float(value) => Self::float(value),
            Value::Double(value) => Self::double(value),
            Value::Date(year, month, day, hour, minute, second, micros) => {
                Self::datetime(year, month, day, hour, minute, second, micros)
            }
            Value::Time(negative, days, hours, minutes, seconds, micros) => {
                Self::time(negative, days, hours, minutes, seconds, micros)
            }
        }
    }
}

impl StmtParamType {
    fn wire_type(self, flavor: ServerFlavor) -> (ColumnType, StmtExecuteParamFlags) {
        use ColumnType::*;
        use StmtExecuteParamFlags as Flags;

        match self {
            StmtParamType::Null => (MYSQL_TYPE_NULL, Flags::empty()),
            StmtParamType::Tiny => (MYSQL_TYPE_TINY, Flags::empty()),
            StmtParamType::Short => (MYSQL_TYPE_SHORT, Flags::empty()),
            StmtParamType::Long => (MYSQL_TYPE_LONG, Flags::empty()),
            StmtParamType::LongLong => (MYSQL_TYPE_LONGLONG, Flags::empty()),
            StmtParamType::UTiny => (MYSQL_TYPE_TINY, Flags::UNSIGNED),
            StmtParamType::UShort => (MYSQL_TYPE_SHORT, Flags::UNSIGNED),
            StmtParamType::ULong => (MYSQL_TYPE_LONG, Flags::UNSIGNED),
            StmtParamType::ULongLong => (MYSQL_TYPE_LONGLONG, Flags::UNSIGNED),
            StmtParamType::Float => (MYSQL_TYPE_FLOAT, Flags::empty()),
            StmtParamType::Double => (MYSQL_TYPE_DOUBLE, Flags::empty()),
            StmtParamType::Decimal => (MYSQL_TYPE_NEWDECIMAL, Flags::empty()),
            StmtParamType::Date => (MYSQL_TYPE_DATE, Flags::empty()),
            StmtParamType::DateTime => (MYSQL_TYPE_DATETIME, Flags::empty()),
            StmtParamType::Timestamp => (MYSQL_TYPE_TIMESTAMP, Flags::empty()),
            StmtParamType::Time => (MYSQL_TYPE_TIME, Flags::empty()),
            StmtParamType::VarString => (MYSQL_TYPE_VAR_STRING, Flags::empty()),
            StmtParamType::String => (MYSQL_TYPE_STRING, Flags::empty()),
            StmtParamType::Blob => (MYSQL_TYPE_BLOB, Flags::empty()),
            StmtParamType::Json => match flavor {
                ServerFlavor::MySql => (MYSQL_TYPE_JSON, Flags::empty()),
                ServerFlavor::MariaDb => (MYSQL_TYPE_VAR_STRING, Flags::empty()),
            },
        }
    }
}

/// Validated parameter section of `COM_STMT_EXECUTE`.
///
/// Contains the null bitmap, the type array, inline values and long data decisions. Can only be
/// constructed by [`StmtExecuteRequestBuilder`].
#[derive(Debug, Clone, PartialEq)]
pub struct StmtExecuteParams<'a> {
    bitmap: Vec<u8>,
    types: Vec<(ColumnType, StmtExecuteParamFlags)>,
    params: Vec<StmtParam<'a>>,
    long_data: Vec<bool>,
    new_params_bound: bool,
    chunk_size: usize,
    /// Total length of the inline values.
    inline_len: usize,
}

impl<'a> StmtExecuteParams<'a> {
    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    pub fn is_null(&self, index: usize) -> bool {
        let offset = index;
        self.bitmap[offset / 8] & (1 << (offset % 8)) != 0
    }

    pub fn is_long_data(&self, index: usize) -> bool {
        self.long_data[index]
    }

    /// Client-side null bitmap (offset 0).
    pub fn bitmap(&self) -> &[u8] {
        &self.bitmap
    }

    /// Whether `MySerialize` will write the type array.
    pub fn new_params_bound(&self) -> bool {
        self.new_params_bound
    }

    /// Whether the parameter at `index` is marked [`LongDataMode::External`].
    ///
    /// Unlike [`StmtExecuteParams::is_long_data`], this returns `true` only for parameters
    /// whose data was sent by the user, not for `Auto`/`Always` ones.
    ///
    /// # Panic
    ///
    /// Panics if `index` is out of bounds.
    pub fn is_external_long_data(&self, index: usize) -> bool {
        self.params[index].long_data_policy() == LongDataMode::External
    }

    /// Indices of parameters marked [`LongDataMode::External`].
    pub fn external_long_data_params(&self) -> impl Iterator<Item = u16> + '_ {
        self.params.iter().enumerate().filter_map(|(index, param)| {
            (param.long_data_policy() == LongDataMode::External).then_some(index as u16)
        })
    }

    /// Exact number of bytes that [`MySerialize`] writes for the params section.
    ///
    /// The `COM_STMT_EXECUTE` header is not included.
    pub fn serialized_len(&self) -> usize {
        if self.params.is_empty() {
            return 0;
        }

        let mut len = self.bitmap.len() + 1 + self.inline_len;
        if self.new_params_bound {
            len += 2 * self.types.len();
        }
        len
    }

    pub fn column_type(&self, index: usize) -> Option<(ColumnType, StmtExecuteParamFlags)> {
        self.types.get(index).copied()
    }

    /// Wire types of the parameters.
    pub fn types(&self) -> &[(ColumnType, StmtExecuteParamFlags)] {
        &self.types
    }

    /// Returns the parameter at `index`, or `None` if it is out of bounds.
    pub fn param(&self, index: usize) -> Option<&StmtParam<'a>> {
        self.params.get(index)
    }

    /// Converts all parameters into [`Value`]s.
    ///
    /// See [`StmtParam::value`]: parameters whose values were sent with
    /// `COM_STMT_SEND_LONG_DATA` yield an error.
    pub fn values(&self) -> io::Result<Vec<Value>> {
        self.params.iter().map(StmtParam::value).collect()
    }

    /// Switches `NEW_PARAMS_BOUND` off after checking that the types match.
    ///
    /// Returns an error if types mismatch.
    pub(crate) fn reuse_types(
        &mut self,
        types: &[(ColumnType, StmtExecuteParamFlags)],
    ) -> Result<(), StmtExecuteMetadataError> {
        if self.types.len() != types.len() {
            return Err(StmtExecuteMetadataError::ParamCountMismatch {
                expected: types.len(),
                actual: self.types.len(),
            });
        }

        for (index, (expected, actual)) in types.iter().zip(&self.types).enumerate() {
            if expected != actual {
                return Err(StmtExecuteMetadataError::TypeMismatch {
                    index,
                    expected: *expected,
                    actual: *actual,
                });
            }
        }

        self.new_params_bound = false;
        Ok(())
    }

    /// Same as [`StmtExecuteParams::reuse_types`] without the element-wise type check.
    pub(crate) fn reuse_types_unchecked(&mut self) {
        self.new_params_bound = false;
    }

    /// Iterator over `COM_STMT_SEND_LONG_DATA` packets.
    ///
    /// The iterator is lazy and allocates nothing: it scans parameters on demand and borrows
    /// chunk bytes from the request. Packets are yielded in parameter order, chunks in payload
    /// order.
    ///
    /// An empty value yields one empty chunk: the server has to enter the `LONG_DATA_VALUE` state
    /// even for an empty value. [`LongDataMode::External`] parameters produce no chunks.
    ///
    /// This is the low-level primitive: `stmt_id` is supplied by the caller. Normal use goes
    /// through [`ComStmtExecute::long_data`], which takes the id from the command.
    pub fn long_data(&self, stmt_id: u32) -> impl Iterator<Item = ComStmtSendLongData<'_>> + '_ {
        LongDataIter {
            params: self,
            stmt_id,
            index: 0,
            offset: 0,
        }
    }
}

/// Lazy [`StmtExecuteParams::long_data`] iterator.
struct LongDataIter<'p, 'a> {
    params: &'p StmtExecuteParams<'a>,
    stmt_id: u32,
    /// Parameter being emitted.
    index: usize,
    /// Offset of the next chunk inside the current parameter; `1` marks the empty chunk as sent.
    offset: usize,
}

impl<'p, 'a> Iterator for LongDataIter<'p, 'a> {
    type Item = ComStmtSendLongData<'p>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.params.params.len() {
            let param = &self.params.params[self.index];
            let index = self.index;

            if !self.params.long_data[index] || param.long_data_policy() == LongDataMode::External {
                self.index += 1;
                self.offset = 0;
                continue;
            }

            let data = match param.data() {
                StmtParamData::Bytes(data) => data.as_ref(),
                _ => {
                    self.index += 1;
                    self.offset = 0;
                    continue;
                }
            };

            if data.is_empty() {
                if self.offset == 0 {
                    self.offset = 1;
                    return Some(ComStmtSendLongData::new(
                        self.stmt_id,
                        index as u16,
                        &[][..],
                    ));
                }
                self.index += 1;
                self.offset = 0;
                continue;
            }

            if self.offset < data.len() {
                let end = (self.offset + self.params.chunk_size).min(data.len());
                let chunk = &data[self.offset..end];
                self.offset = end;
                return Some(ComStmtSendLongData::new(self.stmt_id, index as u16, chunk));
            }

            self.index += 1;
            self.offset = 0;
        }

        None
    }
}

impl MySerialize for StmtExecuteParams<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        if self.params.is_empty() {
            return;
        }

        buf.reserve(self.serialized_len());

        buf.put_slice(&self.bitmap);
        buf.put_u8(u8::from(self.new_params_bound));

        if self.new_params_bound {
            for (column_type, flags) in &self.types {
                buf.put_u8(*column_type as u8);
                buf.put_u8(flags.bits());
            }
        }

        for (index, param) in self.params.iter().enumerate() {
            if self.is_null(index) || self.long_data[index] {
                continue;
            }
            param.serialize_value(self.types[index].0, buf);
        }
    }
}

/// Snapshot of parameter types for a safe `NEW_PARAMS_BOUND = 0`.
///
/// The snapshot is client-side: it proves nothing about the types the server stored for a
/// statement. Reuse is only valid after a successful execute of the target statement with these
/// types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamsMetadata {
    types: Vec<(ColumnType, StmtExecuteParamFlags)>,
}

impl ParamsMetadata {
    /// Wire types and flags captured from a request.
    pub fn types(&self) -> &[(ColumnType, StmtExecuteParamFlags)] {
        &self.types
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

/// Parameters part of a `COM_STMT_EXECUTE` data.
#[derive(Debug, Clone, PartialEq)]
pub struct StmtExecuteRequest<'a> {
    flags: CursorType,
    iteration_count: u32,
    params: StmtExecuteParams<'a>,
}

impl<'a> StmtExecuteRequest<'a> {
    pub fn flags(&self) -> CursorType {
        self.flags
    }

    pub fn iteration_count(&self) -> u32 {
        self.iteration_count
    }

    pub fn params(&self) -> &StmtExecuteParams<'a> {
        &self.params
    }

    /// Snapshot of the types for a later [`StmtExecuteRequest::reuse_types`].
    ///
    /// This is a *client-side* snapshot: it is not proof that the server accepted these types.
    /// `NEW_PARAMS_BOUND = 0` is only safe after a successful execute of the target statement
    /// with the same parameter types (see [`StmtExecuteRequest::reuse_types`]).
    pub fn metadata(&self) -> ParamsMetadata {
        ParamsMetadata {
            types: self.params.types.clone(),
        }
    }

    /// Same as [`StmtExecuteRequest::metadata`], but moves the type array out of the request.
    ///
    /// Avoids the allocation; the request can no longer be reused afterwards.
    pub fn into_metadata(self) -> ParamsMetadata {
        ParamsMetadata {
            types: self.params.types,
        }
    }

    /// Switches `NEW_PARAMS_BOUND` off after checking that the types match `metadata`.
    pub fn reuse_types(
        &mut self,
        metadata: &ParamsMetadata,
    ) -> Result<(), StmtExecuteMetadataError> {
        self.check_metadata(metadata)?;
        self.params.reuse_types(&metadata.types)
    }

    /// Switches `NEW_PARAMS_BOUND` off without the element-wise type check.
    ///
    /// Only the parameter count is checked. Intended for `TypeReuse::AssumeSame`; a violated
    /// guarantee can desynchronize the server's packet parser.
    pub fn reuse_types_unchecked(
        &mut self,
        metadata: &ParamsMetadata,
    ) -> Result<(), StmtExecuteMetadataError> {
        self.check_metadata(metadata)?;
        debug_assert_eq!(self.params.types, metadata.types);
        self.params.reuse_types_unchecked();
        Ok(())
    }

    fn check_metadata(&self, metadata: &ParamsMetadata) -> Result<(), StmtExecuteMetadataError> {
        if self.params.len() != metadata.len() {
            return Err(StmtExecuteMetadataError::ParamCountMismatch {
                expected: metadata.len(),
                actual: self.params.len(),
            });
        }

        Ok(())
    }
}

define_header!(
    ComStmtExecuteHeader,
    COM_STMT_EXECUTE,
    InvalidComStmtExecuteHeader
);

/// A fully formed `COM_STMT_EXECUTE` command.
#[derive(Debug, Clone, PartialEq)]
pub struct ComStmtExecute<'a> {
    __header: ComStmtExecuteHeader,
    stmt_id: u32,
    request: StmtExecuteRequest<'a>,
}

impl<'a> ComStmtExecute<'a> {
    pub fn new(stmt_id: u32, request: StmtExecuteRequest<'a>) -> Self {
        Self {
            __header: ComStmtExecuteHeader::new(),
            stmt_id,
            request,
        }
    }

    pub fn stmt_id(&self) -> u32 {
        self.stmt_id
    }

    pub fn request(&self) -> &StmtExecuteRequest<'a> {
        &self.request
    }

    pub fn params(&self) -> &StmtExecuteParams<'a> {
        self.request.params()
    }

    pub fn into_request(self) -> StmtExecuteRequest<'a> {
        self.request
    }

    /// Long data packets that must be sent before the command itself.
    pub fn long_data(&self) -> impl Iterator<Item = ComStmtSendLongData<'_>> + '_ {
        self.request.params.long_data(self.stmt_id)
    }

    /// Exact number of bytes that [`MySerialize`] writes, including the command byte.
    pub fn serialized_len(&self) -> usize {
        10 + self.request.params.serialized_len()
    }
}

impl MySerialize for ComStmtExecute<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        buf.reserve(self.serialized_len());
        self.__header.serialize(buf);
        buf.put_u32_le(self.stmt_id);
        buf.put_u8(self.request.flags.bits());
        buf.put_u32_le(self.request.iteration_count);
        self.request.params.serialize(buf);
    }
}

/// Context for [`ComStmtExecute`] deserialization.
///
/// A `COM_STMT_EXECUTE` packet does not carry the parameter count, omits the parameter types
/// when `NEW_PARAMS_BOUND = 0`, and omits values of the parameters that were sent with
/// `COM_STMT_SEND_LONG_DATA`.
#[derive(Debug, Clone, Copy, Default)]
pub struct ComStmtExecuteCtx<'a> {
    /// Number of parameters of the executed statement.
    pub num_params: u16,
    /// Parameter types for `NEW_PARAMS_BOUND = 0`; ignored when the packet carries types.
    pub types: Option<&'a [(ColumnType, StmtExecuteParamFlags)]>,
    /// Parameters whose values were sent with `COM_STMT_SEND_LONG_DATA`.
    pub long_data: &'a [u16],
}

impl<'a> ComStmtExecuteCtx<'a> {
    pub fn new(num_params: u16) -> Self {
        Self {
            num_params,
            ..Default::default()
        }
    }
}

impl<'de> MyDeserialize<'de> for ComStmtExecute<'de> {
    const SIZE: Option<usize> = None;
    type Ctx = ComStmtExecuteCtx<'de>;

    fn deserialize(ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut sbuf: ParseBuf<'_> = buf.parse(10)?;

        Ok(Self {
            __header: sbuf.parse_unchecked(())?,
            stmt_id: sbuf.parse_unchecked::<RawInt<LeU32>>(())?.0,
            request: StmtExecuteRequest {
                flags: sbuf.parse_unchecked::<Const<CursorType, u8>>(())?.0,
                iteration_count: sbuf.parse_unchecked::<RawInt<LeU32>>(())?.0,
                params: deserialize_params(ctx, buf)?,
            },
        })
    }
}

/// Parses the parameters section of `COM_STMT_EXECUTE`.
fn deserialize_params<'de>(
    ctx: ComStmtExecuteCtx<'_>,
    buf: &mut ParseBuf<'de>,
) -> io::Result<StmtExecuteParams<'de>> {
    if let Some(&index) = ctx.long_data.iter().find(|&&index| index >= ctx.num_params) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("long data parameter index {index} is out of bounds"),
        ));
    }

    if ctx.num_params == 0 {
        if !buf.is_empty() {
            return Err(trailing_params_bytes());
        }

        return Ok(StmtExecuteParams {
            bitmap: Vec::new(),
            types: Vec::new(),
            params: Vec::new(),
            long_data: Vec::new(),
            new_params_bound: true,
            // Not known for a parsed packet; chunking does not apply to it.
            chunk_size: MAX_PAYLOAD_LEN - 7,
            inline_len: 0,
        });
    }

    let num_params = usize::from(ctx.num_params);

    let bitmap = buf
        .checked_eat(num_params.div_ceil(8))
        .ok_or_else(unexpected_buf_eof)?
        .to_vec();

    let new_params_bound = buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)? != 0;

    let types = if new_params_bound {
        let mut types_buf = buf
            .checked_eat_buf(num_params * 2)
            .ok_or_else(unexpected_buf_eof)?;
        let mut types = Vec::with_capacity(num_params);

        for _ in 0..num_params {
            let column_type = ColumnType::try_from(types_buf.eat_u8())
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;
            let flags = StmtExecuteParamFlags::from_bits_retain(types_buf.eat_u8());
            types.push((column_type, flags));
        }

        types
    } else {
        let types = ctx.types.ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "parameter types are required when NEW_PARAMS_BOUND is not set",
            )
        })?;

        if types.len() != num_params {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "parameter types count mismatch",
            ));
        }

        types.to_vec()
    };

    let mut long_data = vec![false; num_params];
    for &index in ctx.long_data {
        long_data[index as usize] = true;
    }

    let mut params = Vec::with_capacity(num_params);
    let mut inline_len = 0;

    for (index, &(column_type, flags)) in types.iter().enumerate() {
        let is_null = bitmap[index / 8] & (1 << (index % 8)) != 0;

        let (data, mode) = if long_data[index] {
            (
                StmtParamData::Bytes(Cow::Borrowed(&[])),
                LongDataMode::External,
            )
        } else if is_null || column_type == ColumnType::MYSQL_TYPE_NULL {
            (StmtParamData::Null, LongDataMode::Never)
        } else {
            let value = eat_param_value(column_type, buf)?;
            inline_len += value.len();
            (
                StmtParamData::Bytes(Cow::Borrowed(value)),
                LongDataMode::Never,
            )
        };

        params.push(StmtParam {
            data,
            kind: StmtParamKind::Raw { column_type, flags },
            long_data: mode,
        });
    }

    if !buf.is_empty() {
        return Err(trailing_params_bytes());
    }

    Ok(StmtExecuteParams {
        bitmap,
        types,
        params,
        long_data,
        new_params_bound,
        chunk_size: MAX_PAYLOAD_LEN - 7,
        inline_len,
    })
}

fn trailing_params_bytes() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        "trailing bytes after the parameters section",
    )
}

/// Consumes one parameter value and returns its wire bytes.
fn eat_param_value<'de>(column_type: ColumnType, buf: &mut ParseBuf<'de>) -> io::Result<&'de [u8]> {
    let start = buf.0;

    let ok = match column_type {
        ColumnType::MYSQL_TYPE_TINY => buf.checked_eat(1).is_some(),
        ColumnType::MYSQL_TYPE_SHORT | ColumnType::MYSQL_TYPE_YEAR => buf.checked_eat(2).is_some(),
        ColumnType::MYSQL_TYPE_LONG | ColumnType::MYSQL_TYPE_INT24 => buf.checked_eat(4).is_some(),
        ColumnType::MYSQL_TYPE_LONGLONG => buf.checked_eat(8).is_some(),
        ColumnType::MYSQL_TYPE_FLOAT => buf.checked_eat(4).is_some(),
        ColumnType::MYSQL_TYPE_DOUBLE => buf.checked_eat(8).is_some(),
        ColumnType::MYSQL_TYPE_TIMESTAMP
        | ColumnType::MYSQL_TYPE_DATE
        | ColumnType::MYSQL_TYPE_NEWDATE
        | ColumnType::MYSQL_TYPE_DATETIME
        | ColumnType::MYSQL_TYPE_TIME => match buf.checked_eat_u8() {
            Some(len) => buf.checked_eat(len as usize).is_some(),
            None => false,
        },
        ColumnType::MYSQL_TYPE_STRING
        | ColumnType::MYSQL_TYPE_VAR_STRING
        | ColumnType::MYSQL_TYPE_VARCHAR
        | ColumnType::MYSQL_TYPE_BLOB
        | ColumnType::MYSQL_TYPE_TINY_BLOB
        | ColumnType::MYSQL_TYPE_MEDIUM_BLOB
        | ColumnType::MYSQL_TYPE_LONG_BLOB
        | ColumnType::MYSQL_TYPE_DECIMAL
        | ColumnType::MYSQL_TYPE_NEWDECIMAL
        | ColumnType::MYSQL_TYPE_JSON
        | ColumnType::MYSQL_TYPE_ENUM
        | ColumnType::MYSQL_TYPE_SET
        | ColumnType::MYSQL_TYPE_BIT
        | ColumnType::MYSQL_TYPE_GEOMETRY
        | ColumnType::MYSQL_TYPE_VECTOR => buf.checked_eat_lenenc_str().is_some(),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unsupported parameter type: {column_type:?}"),
            ));
        }
    };

    if !ok {
        return Err(unexpected_buf_eof());
    }

    let consumed = start.len() - buf.0.len();
    Ok(&start[..consumed])
}

/// Decodes raw parameter wire bytes into a [`Value`].
fn decode_raw_value(
    column_type: ColumnType,
    flags: StmtExecuteParamFlags,
    data: &[u8],
) -> io::Result<Value> {
    let mut column_flags = ColumnFlags::empty();
    column_flags.set(
        ColumnFlags::UNSIGNED_FLAG,
        flags.contains(StmtExecuteParamFlags::UNSIGNED),
    );

    let mut buf = ParseBuf(data);
    let value =
        ValueDeserializer::<BinValue>::deserialize((column_type, column_flags), &mut buf)?.0;

    if !buf.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "trailing bytes after the parameter value",
        ));
    }

    Ok(value)
}

/// Builder for [`StmtExecuteRequest`].
#[derive(Debug, Clone)]
pub struct StmtExecuteRequestBuilder {
    flags: CursorType,
    iteration_count: u32,
    flavor: ServerFlavor,
}

impl Default for StmtExecuteRequestBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl StmtExecuteRequestBuilder {
    pub fn new() -> Self {
        Self {
            flags: CursorType::CURSOR_TYPE_NO_CURSOR,
            iteration_count: 1,
            flavor: ServerFlavor::MySql,
        }
    }

    /// Cursor flags; `CURSOR_TYPE_NO_CURSOR` by default.
    ///
    /// The low-level API accepts any value. Drivers that do not implement `COM_STMT_FETCH`
    /// should reject cursor flags.
    pub fn cursor_type(mut self, flags: CursorType) -> Self {
        self.flags = flags;
        self
    }

    /// Server flavor; `ServerFlavor::MySql` by default.
    pub fn server_flavor(mut self, flavor: ServerFlavor) -> Self {
        self.flavor = flavor;
        self
    }

    /// Builds and validates the plan.
    ///
    /// `max_allowed_packet` is the client-side limit for the whole command packet, including the
    /// command byte.
    pub fn build<'a, P>(
        self,
        params: P,
        max_allowed_packet: usize,
    ) -> Result<StmtExecuteRequest<'a>, StmtExecuteError>
    where
        P: IntoIterator<Item = StmtParam<'a>>,
    {
        let params = params.into_iter().collect::<Vec<_>>();
        let len = params.len();

        if len > u16::MAX as usize {
            return Err(StmtExecuteError::TooManyParams { count: len });
        }

        if max_allowed_packet < 8 {
            return Err(StmtExecuteError::PacketSizeTooSmall {
                value: max_allowed_packet,
            });
        }

        let bitmap_len = len.div_ceil(8);
        // 1 command + 4 stmt_id + 1 flags + 4 iteration_count.
        let mut fixed_len = 10;
        if len > 0 {
            // null bitmap + new_params_bound + type array.
            fixed_len += bitmap_len + 1 + 2 * len;
        }

        let mut types = Vec::with_capacity(len);
        let mut long_data = vec![false; len];
        let mut inline_lens = vec![0; len];

        for (index, param) in params.iter().enumerate() {
            param.validate(index)?;

            let wire_type = param.wire_type(self.flavor);
            types.push(wire_type);

            match param.long_data_policy() {
                LongDataMode::Always | LongDataMode::External => {
                    if param.is_raw() {
                        return Err(StmtExecuteError::RawWithLongData { index });
                    }
                    if param.data().is_null() || param.param_type() == Some(StmtParamType::Null) {
                        return Err(StmtExecuteError::NullWithLongData { index });
                    }
                    if !supports_long_data(wire_type.0) {
                        return Err(StmtExecuteError::LongDataNotSupported {
                            index,
                            ty: param.param_type().unwrap_or(StmtParamType::Null),
                        });
                    }
                    // `External` is a marker: the value was sent by the user, so a carried value
                    // would be silently dropped by both the inline section and the chunk iterator.
                    if param.long_data_policy() == LongDataMode::External
                        && !matches!(param.data(), StmtParamData::Bytes(data) if data.is_empty())
                    {
                        return Err(StmtExecuteError::ExternalWithData { index });
                    }
                    long_data[index] = true;
                }
                LongDataMode::Auto | LongDataMode::Never => {}
            }

            if !long_data[index] && !param.data().is_null() {
                inline_lens[index] = param.inline_len(wire_type.0);
            }
        }

        let mut inline_total = inline_lens.iter().sum::<usize>();
        let mut total = fixed_len.saturating_add(inline_total);

        if total > max_allowed_packet {
            let mut candidates = (0..len)
                .filter(|&index| {
                    !long_data[index]
                        && !params[index].data().is_null()
                        && !params[index].is_raw()
                        && params[index].long_data_policy() == LongDataMode::Auto
                        && supports_long_data(types[index].0)
                })
                .collect::<Vec<_>>();

            // Demote the largest values first to keep small ones inline.
            candidates.sort_by_key(|&index| Reverse(inline_lens[index]));

            for index in candidates {
                long_data[index] = true;
                inline_total -= inline_lens[index];
                total = fixed_len.saturating_add(inline_total);

                if total <= max_allowed_packet {
                    break;
                }
            }
        }

        if total > max_allowed_packet {
            return Err(StmtExecuteError::PayloadTooLarge {
                needed: total,
                limit: max_allowed_packet,
            });
        }

        let mut bitmap = vec![0; bitmap_len];
        for (index, param) in params.iter().enumerate() {
            if param.data().is_null() && param.long_data_policy() != LongDataMode::External {
                bitmap[index / 8] |= 1 << (index % 8);
            }
        }

        let chunk_size = max_allowed_packet.min(MAX_PAYLOAD_LEN) - 7;

        Ok(StmtExecuteRequest {
            flags: self.flags,
            iteration_count: self.iteration_count,
            params: StmtExecuteParams {
                bitmap,
                types,
                params,
                long_data,
                new_params_bound: true,
                chunk_size,
                inline_len: inline_total,
            },
        })
    }
}

define_header!(
    ComStmtResetHeader,
    COM_STMT_RESET,
    InvalidComStmtResetHeader
);

/// `COM_STMT_RESET`.
///
/// Used to drop long data accumulated for a statement. The server replies with OK, or with ERR if
/// the statement is unknown.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ComStmtReset {
    __header: ComStmtResetHeader,
    pub stmt_id: u32,
}

impl ComStmtReset {
    pub fn new(stmt_id: u32) -> Self {
        Self {
            __header: ComStmtResetHeader::new(),
            stmt_id,
        }
    }
}

impl MySerialize for ComStmtReset {
    fn serialize(&self, buf: &mut Vec<u8>) {
        self.__header.serialize(buf);
        buf.put_u32_le(self.stmt_id);
    }
}

impl<'de> MyDeserialize<'de> for ComStmtReset {
    const SIZE: Option<usize> = None;
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut sbuf: ParseBuf<'_> = buf.parse(5)?;

        Ok(Self {
            __header: sbuf.parse_unchecked(())?,
            stmt_id: sbuf.parse_unchecked::<RawInt<LeU32>>(())?.0,
        })
    }
}

impl StmtParamData<'_> {
    fn is_null(&self) -> bool {
        matches!(self, StmtParamData::Null)
    }

    fn encoded_len(&self, column_type: ColumnType) -> usize {
        match self {
            StmtParamData::Null => 0,
            StmtParamData::Bytes(data) => lenenc_str_len(data) as usize,
            StmtParamData::Int(_) | StmtParamData::UInt(_) => {
                int_width(column_type).unwrap_or_default()
            }
            StmtParamData::Float(_) => {
                if column_type == ColumnType::MYSQL_TYPE_DOUBLE {
                    8
                } else {
                    4
                }
            }
            StmtParamData::Double(_) => 8,
            StmtParamData::Date { year, month, day } => {
                if *year == 0 && *month == 0 && *day == 0 {
                    1
                } else {
                    5
                }
            }
            StmtParamData::DateTime {
                year,
                month,
                day,
                hour,
                minute,
                second,
                micros,
            } => match datetime_form(*year, *month, *day, *hour, *minute, *second, *micros) {
                0 => 1,
                4 => 5,
                7 => 8,
                _ => 12,
            },
            StmtParamData::Time {
                days,
                hours,
                minutes,
                seconds,
                micros,
                ..
            } => match time_form(*days, *hours, *minutes, *seconds, *micros) {
                0 => 1,
                8 => 9,
                _ => 13,
            },
        }
    }
}

fn convert_data<'a>(
    data: StmtParamData<'a>,
    current: StmtParamType,
    ty: StmtParamType,
) -> Result<StmtParamData<'a>, StmtParamError> {
    use StmtParamData::*;
    use StmtParamType as T;

    let kind = data.kind_name();
    let incompatible = || StmtParamError::IncompatibleType { ty, kind };

    match data {
        Null => match ty {
            // Keep `null_as(ty)` idempotent.
            _ if ty == current || ty == T::Null => Ok(Null),
            _ => Err(incompatible()),
        },
        Bytes(value) => match ty {
            T::VarString | T::String | T::Blob | T::Json | T::Decimal => Ok(Bytes(value)),
            _ => Err(incompatible()),
        },
        Int(value) => match ty {
            T::Tiny => {
                check_range(value, i8::MIN as i64, i8::MAX as i64, ty)?;
                Ok(Int(value))
            }
            T::Short => {
                check_range(value, i16::MIN as i64, i16::MAX as i64, ty)?;
                Ok(Int(value))
            }
            T::Long => {
                check_range(value, i32::MIN as i64, i32::MAX as i64, ty)?;
                Ok(Int(value))
            }
            T::LongLong => Ok(Int(value)),
            T::UTiny => {
                check_range(value, 0, u8::MAX as i64, ty)?;
                Ok(UInt(value as u64))
            }
            T::UShort => {
                check_range(value, 0, u16::MAX as i64, ty)?;
                Ok(UInt(value as u64))
            }
            T::ULong => {
                check_range(value, 0, u32::MAX as i64, ty)?;
                Ok(UInt(value as u64))
            }
            T::ULongLong => {
                check_range(value, 0, i64::MAX, ty)?;
                Ok(UInt(value as u64))
            }
            _ => Err(incompatible()),
        },
        UInt(value) => match ty {
            T::UTiny => {
                check_range(value, 0, u8::MAX as u64, ty)?;
                Ok(UInt(value))
            }
            T::UShort => {
                check_range(value, 0, u16::MAX as u64, ty)?;
                Ok(UInt(value))
            }
            T::ULong => {
                check_range(value, 0, u32::MAX as u64, ty)?;
                Ok(UInt(value))
            }
            T::ULongLong => Ok(UInt(value)),
            _ => Err(incompatible()),
        },
        Float(value) => match ty {
            T::Float => Ok(Float(value)),
            T::Double => Ok(Double(value as f64)),
            _ => Err(incompatible()),
        },
        Double(value) => match ty {
            T::Double => Ok(Double(value)),
            _ => Err(incompatible()),
        },
        Date { year, month, day } => match ty {
            T::Date => Ok(Date { year, month, day }),
            T::DateTime | T::Timestamp => Ok(DateTime {
                year,
                month,
                day,
                hour: 0,
                minute: 0,
                second: 0,
                micros: 0,
            }),
            _ => Err(incompatible()),
        },
        DateTime {
            year,
            month,
            day,
            hour,
            minute,
            second,
            micros,
        } => match ty {
            T::DateTime | T::Timestamp => Ok(DateTime {
                year,
                month,
                day,
                hour,
                minute,
                second,
                micros,
            }),
            T::Date if hour == 0 && minute == 0 && second == 0 && micros == 0 => {
                Ok(Date { year, month, day })
            }
            _ => Err(incompatible()),
        },
        Time {
            negative,
            days,
            hours,
            minutes,
            seconds,
            micros,
        } => match ty {
            T::Time => Ok(Time {
                negative,
                days,
                hours,
                minutes,
                seconds,
                micros,
            }),
            _ => Err(incompatible()),
        },
    }
}

impl StmtParamData<'_> {
    fn kind_name(&self) -> &'static str {
        match self {
            StmtParamData::Null => "null",
            StmtParamData::Bytes(_) => "bytes",
            StmtParamData::Int(_) => "int",
            StmtParamData::UInt(_) => "uint",
            StmtParamData::Float(_) => "float",
            StmtParamData::Double(_) => "double",
            StmtParamData::Date { .. } => "date",
            StmtParamData::DateTime { .. } => "datetime",
            StmtParamData::Time { .. } => "time",
        }
    }
}

fn check_range<T>(value: T, min: T, max: T, ty: StmtParamType) -> Result<(), StmtParamError>
where
    T: PartialOrd,
{
    if value < min || value > max {
        Err(StmtParamError::OutOfRange { ty })
    } else {
        Ok(())
    }
}

fn validate_date(month: u8, day: u8) -> Result<(), &'static str> {
    if month > 12 {
        return Err("month");
    }
    if day > 31 {
        return Err("day");
    }
    if month == 0 && day != 0 {
        return Err("day");
    }
    Ok(())
}

fn int_width(column_type: ColumnType) -> Option<usize> {
    use ColumnType::*;

    match column_type {
        MYSQL_TYPE_TINY => Some(1),
        MYSQL_TYPE_SHORT => Some(2),
        MYSQL_TYPE_LONG => Some(4),
        MYSQL_TYPE_LONGLONG => Some(8),
        _ => None,
    }
}

fn supports_long_data(column_type: ColumnType) -> bool {
    (ColumnType::MYSQL_TYPE_TINY_BLOB as u8..=ColumnType::MYSQL_TYPE_STRING as u8)
        .contains(&(column_type as u8))
}

fn datetime_form(
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    micros: u32,
) -> u8 {
    if year == 0 && month == 0 && day == 0 && hour == 0 && minute == 0 && second == 0 && micros == 0
    {
        0
    } else if hour == 0 && minute == 0 && second == 0 && micros == 0 {
        4
    } else if micros == 0 {
        7
    } else {
        11
    }
}

fn time_form(days: u32, hours: u8, minutes: u8, seconds: u8, micros: u32) -> u8 {
    if days == 0 && hours == 0 && minutes == 0 && seconds == 0 && micros == 0 {
        0
    } else if micros == 0 {
        8
    } else {
        12
    }
}

fn put_date(year: u16, month: u8, day: u8, buf: &mut Vec<u8>) {
    if year == 0 && month == 0 && day == 0 {
        buf.put_u8(0);
    } else {
        buf.put_u8(4);
        buf.put_u16_le(year);
        buf.put_u8(month);
        buf.put_u8(day);
    }
}

#[allow(clippy::too_many_arguments)]
fn put_datetime(
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    micros: u32,
    buf: &mut Vec<u8>,
) {
    let form = datetime_form(year, month, day, hour, minute, second, micros);
    buf.put_u8(form);

    match form {
        0 => {}
        4 => {
            buf.put_u16_le(year);
            buf.put_u8(month);
            buf.put_u8(day);
        }
        7 => {
            buf.put_u16_le(year);
            buf.put_u8(month);
            buf.put_u8(day);
            buf.put_u8(hour);
            buf.put_u8(minute);
            buf.put_u8(second);
        }
        _ => {
            buf.put_u16_le(year);
            buf.put_u8(month);
            buf.put_u8(day);
            buf.put_u8(hour);
            buf.put_u8(minute);
            buf.put_u8(second);
            buf.put_u32_le(micros);
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn put_time(
    negative: bool,
    days: u32,
    hours: u8,
    minutes: u8,
    seconds: u8,
    micros: u32,
    buf: &mut Vec<u8>,
) {
    let form = time_form(days, hours, minutes, seconds, micros);
    buf.put_u8(form);

    match form {
        0 => {}
        8 => {
            buf.put_u8(u8::from(negative));
            buf.put_u32_le(days);
            buf.put_u8(hours);
            buf.put_u8(minutes);
            buf.put_u8(seconds);
        }
        _ => {
            buf.put_u8(u8::from(negative));
            buf.put_u32_le(days);
            buf.put_u8(hours);
            buf.put_u8(minutes);
            buf.put_u8(seconds);
            buf.put_u32_le(micros);
        }
    }
}
