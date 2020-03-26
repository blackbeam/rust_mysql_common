// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use byteorder::{LittleEndian as LE, ReadBytesExt};

use std::fmt;
use std::io;
use std::str::from_utf8;

use crate::constants::{ColumnFlags, ColumnType};
use crate::io::ReadMysqlExt;
use crate::misc::lenenc_int_len;
use crate::packets::{Column, NullBitmap};
use crate::value::Value::*;

pub mod convert;
pub mod json;

/// Side of MySql value serialization.
pub trait SerializationSide {
    /// Null-bitmap offset of this side.
    const BIT_OFFSET: usize;
}

/// Server side serialization. Null-bitmap bit offset: `2`.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ServerSide;

impl SerializationSide for ServerSide {
    const BIT_OFFSET: usize = 2;
}

/// Client side serialization. Null-bitmap bit offset: `0`.
pub struct ClientSide;

impl SerializationSide for ClientSide {
    const BIT_OFFSET: usize = 0;
}

/// Client side representation of a value of MySql column.
///
/// The `Value` is also used as a parameter to a prepared statement.
#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    NULL,
    Bytes(Vec<u8>),
    Int(i64),
    UInt(u64),
    Float(f32),
    Double(f64),
    /// year, month, day, hour, minutes, seconds, micro seconds
    Date(u16, u8, u8, u8, u8, u8, u32),
    /// is negative, days, hours, minutes, seconds, micro seconds
    Time(bool, u32, u8, u8, u8, u32),
}

/// Reads value in text format.
pub fn read_text_value(input: &mut &[u8]) -> io::Result<Value> {
    Value::read_text(input)
}

/// Reads multiple values in text format.
pub fn read_text_values(input: &[u8], count: usize) -> io::Result<Vec<Value>> {
    Value::read_text_many(input, count)
}

/// Reads value in binary format.
pub fn read_bin_value(
    input: &mut &[u8],
    column_type: ColumnType,
    unsigned: bool,
) -> io::Result<Value> {
    Value::read_bin(input, column_type, unsigned)
}

/// Reads multiple values in binary format.
pub fn read_bin_values<T: SerializationSide>(
    input: &[u8],
    columns: &[Column],
) -> io::Result<Vec<Value>> {
    Value::read_bin_many::<T>(input, columns)
}

/// Will escape string for SQL depending on `no_backslash_escape` flag.
fn escaped(input: &str, no_backslash_escape: bool) -> String {
    let mut output = String::with_capacity(input.len());
    output.push('\'');
    if no_backslash_escape {
        for c in input.chars() {
            if c == '\'' {
                output.push('\'');
                output.push('\'');
            } else {
                output.push(c);
            }
        }
    } else {
        for c in input.chars() {
            if c == '\x00' {
                output.push('\\');
                output.push('0');
            } else if c == '\n' {
                output.push('\\');
                output.push('n');
            } else if c == '\r' {
                output.push('\\');
                output.push('r');
            } else if c == '\\' || c == '\'' || c == '"' {
                output.push('\\');
                output.push(c);
            } else if c == '\x1a' {
                output.push('\\');
                output.push('Z');
            } else {
                output.push(c);
            }
        }
    }
    output.push('\'');
    output
}

impl Value {
    /// Returns length in binary serialized form.
    #[inline]
    pub fn bin_len(&self) -> usize {
        match self {
            Value::NULL => 0,
            Value::Bytes(x) => lenenc_int_len(x.len()) + x.len(),
            Value::Int(_) => 8,
            Value::UInt(_) => 8,
            Value::Float(_) => 4,
            Value::Double(_) => 8,
            Value::Date(0u16, 0u8, 0u8, 0u8, 0u8, 0u8, 0u32) => 1,
            Value::Date(_, _, _, 0u8, 0u8, 0u8, 0u32) => 5,
            Value::Date(_, _, _, _, _, _, 0u32) => 8,
            Value::Date(_, _, _, _, _, _, _) => 12,
            Value::Time(_, 0u32, 0u8, 0u8, 0u8, 0u32) => 1,
            Value::Time(_, _, _, _, _, 0u32) => 9,
            Value::Time(_, _, _, _, _, _) => 13,
        }
    }

    pub fn as_sql(&self, no_backslash_escape: bool) -> String {
        match *self {
            Value::NULL => "NULL".into(),
            Value::Int(x) => format!("{}", x),
            Value::UInt(x) => format!("{}", x),
            Value::Float(x) => format!("{}", x),
            Value::Double(x) => format!("{}", x),
            Value::Date(y, m, d, 0, 0, 0, 0) => format!("'{:04}-{:02}-{:02}'", y, m, d),
            Value::Date(y, m, d, h, i, s, 0) => {
                format!("'{:04}-{:02}-{:02} {:02}:{:02}:{:02}'", y, m, d, h, i, s)
            }
            Value::Date(y, m, d, h, i, s, u) => format!(
                "'{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:06}'",
                y, m, d, h, i, s, u
            ),
            Value::Time(neg, d, h, i, s, 0) => {
                if neg {
                    format!("'-{:03}:{:02}:{:02}'", d * 24 + u32::from(h), i, s)
                } else {
                    format!("'{:03}:{:02}:{:02}'", d * 24 + u32::from(h), i, s)
                }
            }
            Value::Time(neg, d, h, i, s, u) => {
                if neg {
                    format!("'-{:03}:{:02}:{:02}.{:06}'", d * 24 + u32::from(h), i, s, u)
                } else {
                    format!("'{:03}:{:02}:{:02}.{:06}'", d * 24 + u32::from(h), i, s, u)
                }
            }
            Value::Bytes(ref bytes) => match from_utf8(&*bytes) {
                Ok(string) => escaped(string, no_backslash_escape),
                Err(_) => {
                    let mut s = String::from("0x");
                    for c in bytes.iter() {
                        s.extend(format!("{:02X}", *c).chars())
                    }
                    s
                }
            },
        }
    }

    fn read_text(input: &mut &[u8]) -> io::Result<Value> {
        if input.is_empty() {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Unexpected EOF while reading Value",
            ))
        } else if input[0] == 0xfb {
            let _ = input.read_u8();
            Ok(Value::NULL)
        } else {
            Ok(Value::Bytes(read_lenenc_str!(input)?.into()))
        }
    }

    fn read_text_many(mut input: &[u8], count: usize) -> io::Result<Vec<Value>> {
        let mut output = Vec::<Value>::new();
        loop {
            if input.is_empty() {
                if output.len() != count {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "Unexpected EOF while reading Values",
                    ));
                } else {
                    break;
                }
            } else {
                output.push(read_text_value(&mut input)?)
            }
        }
        Ok(output)
    }

    fn read_bin(input: &mut &[u8], column_type: ColumnType, unsigned: bool) -> io::Result<Value> {
        match column_type {
            ColumnType::MYSQL_TYPE_STRING
            | ColumnType::MYSQL_TYPE_VAR_STRING
            | ColumnType::MYSQL_TYPE_BLOB
            | ColumnType::MYSQL_TYPE_TINY_BLOB
            | ColumnType::MYSQL_TYPE_MEDIUM_BLOB
            | ColumnType::MYSQL_TYPE_LONG_BLOB
            | ColumnType::MYSQL_TYPE_SET
            | ColumnType::MYSQL_TYPE_ENUM
            | ColumnType::MYSQL_TYPE_DECIMAL
            | ColumnType::MYSQL_TYPE_VARCHAR
            | ColumnType::MYSQL_TYPE_BIT
            | ColumnType::MYSQL_TYPE_NEWDECIMAL
            | ColumnType::MYSQL_TYPE_GEOMETRY
            | ColumnType::MYSQL_TYPE_JSON => Ok(Bytes(read_lenenc_str!(input)?.into())),
            ColumnType::MYSQL_TYPE_TINY => {
                if unsigned {
                    Ok(Int(input.read_u8()?.into()))
                } else {
                    Ok(Int(input.read_i8()?.into()))
                }
            }
            ColumnType::MYSQL_TYPE_SHORT | ColumnType::MYSQL_TYPE_YEAR => {
                if unsigned {
                    Ok(Int(input.read_u16::<LE>()?.into()))
                } else {
                    Ok(Int(input.read_i16::<LE>()?.into()))
                }
            }
            ColumnType::MYSQL_TYPE_LONG | ColumnType::MYSQL_TYPE_INT24 => {
                if unsigned {
                    Ok(Int(input.read_u32::<LE>()?.into()))
                } else {
                    Ok(Int(input.read_i32::<LE>()?.into()))
                }
            }
            ColumnType::MYSQL_TYPE_LONGLONG => {
                if unsigned {
                    Ok(UInt(input.read_u64::<LE>()?))
                } else {
                    Ok(Int(input.read_i64::<LE>()?))
                }
            }
            ColumnType::MYSQL_TYPE_FLOAT => Ok(Float(input.read_f32::<LE>()?.into())),
            ColumnType::MYSQL_TYPE_DOUBLE => Ok(Double(input.read_f64::<LE>()?)),
            ColumnType::MYSQL_TYPE_TIMESTAMP
            | ColumnType::MYSQL_TYPE_DATE
            | ColumnType::MYSQL_TYPE_DATETIME => {
                let len = input.read_u8()?;
                let mut year = 0u16;
                let mut month = 0u8;
                let mut day = 0u8;
                let mut hour = 0u8;
                let mut minute = 0u8;
                let mut second = 0u8;
                let mut micro_second = 0u32;
                if len >= 4u8 {
                    year = input.read_u16::<LE>()?;
                    month = input.read_u8()?;
                    day = input.read_u8()?;
                }
                if len >= 7u8 {
                    hour = input.read_u8()?;
                    minute = input.read_u8()?;
                    second = input.read_u8()?;
                }
                if len == 11u8 {
                    micro_second = input.read_u32::<LE>()?;
                }
                Ok(Date(year, month, day, hour, minute, second, micro_second))
            }
            ColumnType::MYSQL_TYPE_TIME => {
                let len = input.read_u8()?;
                let mut is_negative = false;
                let mut days = 0u32;
                let mut hours = 0u8;
                let mut minutes = 0u8;
                let mut seconds = 0u8;
                let mut micro_seconds = 0u32;
                if len >= 8u8 {
                    is_negative = input.read_u8()? == 1u8;
                    days = input.read_u32::<LE>()?;
                    hours = input.read_u8()?;
                    minutes = input.read_u8()?;
                    seconds = input.read_u8()?;
                }
                if len == 12u8 {
                    micro_seconds = input.read_u32::<LE>()?;
                }
                Ok(Time(
                    is_negative,
                    days,
                    hours,
                    minutes,
                    seconds,
                    micro_seconds,
                ))
            }
            ColumnType::MYSQL_TYPE_NULL => Ok(NULL),
            x => unimplemented!("Unsupported column type {:?}", x),
        }
    }

    fn read_bin_many<T: SerializationSide>(
        mut input: &[u8],
        columns: &[Column],
    ) -> io::Result<Vec<Value>> {
        input.read_u8()?;

        let bitmap = NullBitmap::<T>::read(&mut input, columns.len());
        let mut values = Vec::with_capacity(columns.len());

        for (i, column) in columns.iter().enumerate() {
            if bitmap.is_null(i) {
                values.push(NULL)
            } else {
                values.push(read_bin_value(
                    &mut input,
                    column.column_type(),
                    column.flags().contains(ColumnFlags::UNSIGNED_FLAG),
                )?)
            }
        }

        Ok(values)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::NULL => f.debug_tuple("Null").finish(),
            Value::Bytes(ref bytes) => {
                let mut debug = f.debug_tuple("Bytes");
                if bytes.len() <= 8 {
                    debug
                        .field(&String::from_utf8_lossy(&*bytes).replace("\n", "\\n"))
                        .finish()
                } else {
                    let bytes = String::from_utf8_lossy(&bytes[..8]).replace("\n", "\\n");
                    debug.field(&format!("{}..", bytes)).finish()
                }
            }
            Value::Int(ref val) => f.debug_tuple("Int").field(val).finish(),
            Value::UInt(ref val) => f.debug_tuple("UInt").field(val).finish(),
            Value::Float(ref val) => f.debug_tuple("Float").field(val).finish(),
            Value::Double(ref val) => f.debug_tuple("Double").field(val).finish(),
            Value::Date(y, m, d, 0, 0, 0, 0) => {
                let format = format!("'{:04}-{:02}-{:02}'", y, m, d);
                f.debug_tuple("Date").field(&format).finish()
            }
            Value::Date(y, m, d, h, i, s, 0) => {
                let format = format!("'{:04}-{:02}-{:02} {:02}:{:02}:{:02}'", y, m, d, h, i, s);
                f.debug_tuple("Date").field(&format).finish()
            }
            Value::Date(y, m, d, h, i, s, u) => {
                let format = format!(
                    "'{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:06}'",
                    y, m, d, h, i, s, u
                );
                f.debug_tuple("Date").field(&format).finish()
            }
            Value::Time(neg, d, h, i, s, 0) => {
                let format = if neg {
                    format!("'-{:03}:{:02}:{:02}'", d * 24 + u32::from(h), i, s)
                } else {
                    format!("'{:03}:{:02}:{:02}'", d * 24 + u32::from(h), i, s)
                };
                f.debug_tuple("Time").field(&format).finish()
            }
            Value::Time(neg, d, h, i, s, u) => {
                let format = if neg {
                    format!("'-{:03}:{:02}:{:02}.{:06}'", d * 24 + u32::from(h), i, s, u)
                } else {
                    format!("'{:03}:{:02}:{:02}.{:06}'", d * 24 + u32::from(h), i, s, u)
                };
                f.debug_tuple("Time").field(&format).finish()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::value::Value;

    #[test]
    fn should_escape_string() {
        assert_eq!(r"'?p??\\\\?p??'", Value::from("?p??\\\\?p??").as_sql(false));
        assert_eq!(r#"'?p??\"?p??'"#, Value::from("?p??\"?p??").as_sql(false));
        assert_eq!(r"'?p??\'?p??'", Value::from("?p??'?p??").as_sql(false));
        assert_eq!(r"'?p??\n?p??'", Value::from("?p??\n?p??").as_sql(false));
        assert_eq!(r"'?p??\r?p??'", Value::from("?p??\r?p??").as_sql(false));
        assert_eq!(r"'?p??\0?p??'", Value::from("?p??\x00?p??").as_sql(false));
    }

    #[cfg(feature = "nightly")]
    mod benches {
        use crate::constants::ColumnType;
        use crate::io::WriteMysqlExt;
        use crate::packets::{column_from_payload, Column};
        use crate::packets::{ComStmtExecuteRequestBuilder, NullBitmap};
        use crate::value::{ClientSide, Value};

        #[bench]
        fn bench_build_stmt_execute_request(bencher: &mut test::Bencher) {
            let values = vec![
                Value::Bytes(b"12.3456789".to_vec()),
                Value::Int(0xF0),
                Value::Int(0xF000),
                Value::Int(0xF0000000),
                Value::Float(std::f32::MAX),
                Value::Double(std::f64::MAX),
                Value::NULL,
                Value::Date(2019, 11, 27, 12, 30, 0, 123456),
                Value::UInt(0xF000000000000000),
                Value::Int(0xF00000),
                Value::Date(2019, 11, 27, 0, 0, 0, 0),
                Value::Time(true, 300, 8, 8, 8, 123456),
                Value::Date(2019, 11, 27, 12, 30, 0, 123456),
                Value::Int(2019),
                Value::Bytes(b"varchar".to_vec()),
                Value::Bytes(b"1000000110000001".to_vec()),
                Value::Bytes(br#"{"foo":"bar","baz":42345.6777}"#.to_vec()),
                Value::Bytes(b"12.3456789".to_vec()),
                Value::Bytes(b"Variant".to_vec()),
                Value::Bytes(b"Element".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_TINY_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_MEDIUM_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_LONG_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_VAR_STRING".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_STRING".to_vec()),
                Value::NULL,
                Value::Bytes(b"MYSQL_TYPE_GEOMETRY".to_vec()),
            ];

            let (body, _) = ComStmtExecuteRequestBuilder::new(0).build(&*values);

            bencher.bytes = body.len() as u64;
            bencher.iter(|| ComStmtExecuteRequestBuilder::new(0).build(&*values));
        }

        #[cfg(feature = "nightly")]
        #[bench]
        fn bench_parse_bin_row(bencher: &mut test::Bencher) {
            fn col(name: &str, ty: ColumnType) -> Column {
                let mut payload = b"\x00def".to_vec();
                for _ in 0..5 {
                    payload.write_lenenc_str(name.as_bytes()).unwrap();
                }
                payload.extend_from_slice(&b"_\x2d\x00\xff\xff\xff\xff"[..]);
                payload.push(ty as u8);
                payload.extend_from_slice(&b"\x00\x00\x00"[..]);
                column_from_payload(payload).unwrap()
            }

            let values = vec![
                Value::Bytes(b"12.3456789".to_vec()),
                Value::Int(0xF0),
                Value::Int(0xF000),
                Value::Int(0xF0000000),
                Value::Float(std::f32::MAX),
                Value::Double(std::f64::MAX),
                Value::NULL,
                Value::Date(2019, 11, 27, 12, 30, 0, 123456),
                Value::UInt(0xF000000000000000),
                Value::Int(0xF00000),
                Value::Date(2019, 11, 27, 0, 0, 0, 0),
                Value::Time(true, 300, 8, 8, 8, 123456),
                Value::Date(2019, 11, 27, 12, 30, 0, 123456),
                Value::Int(2019),
                Value::Bytes(b"varchar".to_vec()),
                Value::Bytes(b"1000000110000001".to_vec()),
                Value::Bytes(br#"{"foo":"bar","baz":42345.6777}"#.to_vec()),
                Value::Bytes(b"12.3456789".to_vec()),
                Value::Bytes(b"Variant".to_vec()),
                Value::Bytes(b"Element".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_TINY_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_MEDIUM_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_LONG_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_BLOB".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_VAR_STRING".to_vec()),
                Value::Bytes(b"MYSQL_TYPE_STRING".to_vec()),
                Value::NULL,
                Value::Bytes(b"MYSQL_TYPE_GEOMETRY".to_vec()),
            ];

            let (body, _) = ComStmtExecuteRequestBuilder::new(0).build(&*values);

            let bitmap_len = NullBitmap::<ClientSide>::bitmap_len(values.len());

            let meta_offset = ComStmtExecuteRequestBuilder::NULL_BITMAP_OFFSET + bitmap_len + 1;
            let meta_len = values.len() * 2;
            let columns = body[meta_offset..(meta_offset + meta_len)]
                .chunks(2)
                .map(|meta| col("foo", ColumnType::from(meta[0])))
                .collect::<Vec<_>>();

            let mut data = vec![0x00];
            data.extend_from_slice(
                &body[ComStmtExecuteRequestBuilder::NULL_BITMAP_OFFSET
                    ..(ComStmtExecuteRequestBuilder::NULL_BITMAP_OFFSET + bitmap_len)],
            );
            data.extend_from_slice(
                &body[(ComStmtExecuteRequestBuilder::NULL_BITMAP_OFFSET
                    + bitmap_len
                    + 1
                    + 2 * values.len())..],
            );

            bencher.bytes = data.len() as u64;
            bencher.iter(|| Value::read_bin_many::<ClientSide>(&*data, &*columns).unwrap());
        }
    }
}
