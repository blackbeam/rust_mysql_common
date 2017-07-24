use bit_vec::BitVec;
use byteorder::{LittleEndian as LE, ReadBytesExt};
use constants::{UNSIGNED_FLAG, ColumnType};
use io::{ReadMysqlExt, WriteMysqlExt};
use packets::Column;
use smallvec::SmallVec;
use std::fmt;
use std::io;

use self::Value::*;

pub mod convert;
pub mod json;

/// Client side representation of a value of MySql column.
///
/// The `Value` is also used as a parameter to a prepared statement.
#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    NULL,
    Bytes(Vec<u8>),
    Int(i64),
    UInt(u64),
    Float(f64),
    /// year, month, day, hour, minutes, seconds, micro seconds
    Date(u16, u8, u8, u8, u8, u8, u32),
    /// is negative, days, hours, minutes, seconds, micro seconds
    Time(bool, u32, u8, u8, u8, u32),
}

pub fn read_text_value(input: &mut &[u8]) -> io::Result<Value> {
    Value::read_text(input)
}

pub fn read_text_values(input: &[u8], count: usize) -> io::Result<SmallVec<[Value; 12]>> {
    Value::read_text_many(input, count)
}

pub fn read_bin_value(
    input: &mut &[u8],
    column_type: ColumnType,
    unsigned: bool,
) -> io::Result<Value> {
    Value::read_bin(input, column_type, unsigned)
}

pub fn read_bin_values(input: &[u8], columns: &[Column]) -> io::Result<SmallVec<[Value; 12]>> {
    Value::read_bin_many(input, columns)
}

pub fn serialize_bin_many(
    params: &[Column],
    values: &[Value],
    max_allowed_packet: usize,
) -> io::Result<(Vec<u8>, BitVec<u8>, BitVec<u8>)> {
    Value::serialize_bin_many(params, values, max_allowed_packet)
}

impl Value {
    fn read_text(input: &mut &[u8]) -> io::Result<Value> {
        if input.len() == 0 {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Unexpected EOF while reading Value",
            ))
        } else {
            if input[0] == 0xfb {
                let _ = input.read_u8();
                Ok(Value::NULL)
            } else {
                Ok(Value::Bytes(read_lenenc_str!(input)?.into()))
            }
        }
    }

    fn read_text_many(mut input: &[u8], count: usize) -> io::Result<SmallVec<[Value; 12]>> {
        let mut output = SmallVec::<[Value; 12]>::new();
        loop {
            if input.len() == 0 {
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
            ColumnType::MYSQL_TYPE_STRING |
            ColumnType::MYSQL_TYPE_VAR_STRING |
            ColumnType::MYSQL_TYPE_BLOB |
            ColumnType::MYSQL_TYPE_TINY_BLOB |
            ColumnType::MYSQL_TYPE_MEDIUM_BLOB |
            ColumnType::MYSQL_TYPE_LONG_BLOB |
            ColumnType::MYSQL_TYPE_SET |
            ColumnType::MYSQL_TYPE_ENUM |
            ColumnType::MYSQL_TYPE_DECIMAL |
            ColumnType::MYSQL_TYPE_VARCHAR |
            ColumnType::MYSQL_TYPE_BIT |
            ColumnType::MYSQL_TYPE_NEWDECIMAL |
            ColumnType::MYSQL_TYPE_GEOMETRY |
            ColumnType::MYSQL_TYPE_JSON => Ok(Bytes(read_lenenc_str!(input)?.into())),
            ColumnType::MYSQL_TYPE_TINY => {
                if unsigned {
                    Ok(Int(input.read_u8()? as i64))
                } else {
                    Ok(Int(input.read_i8()? as i64))
                }
            }
            ColumnType::MYSQL_TYPE_SHORT |
            ColumnType::MYSQL_TYPE_YEAR => {
                if unsigned {
                    Ok(Int(input.read_u16::<LE>()? as i64))
                } else {
                    Ok(Int(input.read_i16::<LE>()? as i64))
                }
            }
            ColumnType::MYSQL_TYPE_LONG |
            ColumnType::MYSQL_TYPE_INT24 => {
                if unsigned {
                    Ok(Int(input.read_u32::<LE>()? as i64))
                } else {
                    Ok(Int(input.read_i32::<LE>()? as i64))
                }
            }
            ColumnType::MYSQL_TYPE_LONGLONG => {
                if unsigned {
                    Ok(UInt(input.read_u64::<LE>()?))
                } else {
                    Ok(Int(input.read_i64::<LE>()?))
                }
            }
            ColumnType::MYSQL_TYPE_FLOAT => Ok(Float(input.read_f32::<LE>()? as f64)),
            ColumnType::MYSQL_TYPE_DOUBLE => Ok(Float(input.read_f64::<LE>()?)),
            ColumnType::MYSQL_TYPE_TIMESTAMP |
            ColumnType::MYSQL_TYPE_DATE |
            ColumnType::MYSQL_TYPE_DATETIME => {
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
            _ => Ok(NULL),
        }
    }

    fn read_bin_many(mut input: &[u8], columns: &[Column]) -> io::Result<SmallVec<[Value; 12]>> {
        input.read_u8()?;

        static BIT_OFFSET: usize = 2; // http://dev.mysql.com/doc/internals/en/null-bitmap.html
        let bitmap_len = (columns.len() + 7 + BIT_OFFSET) / 8;

        let mut bitmap = BitVec::<u8>::default();
        for i in 0..columns.len() {
            bitmap.push(input[(i + BIT_OFFSET) / 8] & (1 << ((i + BIT_OFFSET) % 8)) > 0)
        }

        let mut values = SmallVec::<[Value; 12]>::new();

        input = &input[bitmap_len..];

        for (i, column) in columns.iter().enumerate() {
            // Should not panic because bitmap.len() is always >= columns.len()
            if bitmap.get(i).unwrap() {
                values.push(NULL)
            } else {
                values.push(read_bin_value(
                    &mut input,
                    column.column_type(),
                    column.flags().contains(UNSIGNED_FLAG),
                )?)
            }
        }

        Ok(values)
    }

    fn serialize_bin_many(
        params: &[Column],
        values: &[Value],
        max_allowed_packet: usize,
    ) -> io::Result<(Vec<u8>, BitVec<u8>, BitVec<u8>)> {
        static MAX_NON_BYTES_VALUE_BINARY_SIZE: usize = 13;

        let bitmap_len = (params.len() + 7) / 8;
        let cap = max_allowed_packet - bitmap_len - values.len() * MAX_NON_BYTES_VALUE_BINARY_SIZE;

        let mut output = Vec::with_capacity(512);
        let mut written = 0;
        let mut null_bitmap = BitVec::<u8>::default();
        let mut large_bitmap = BitVec::<u8>::default();

        null_bitmap.reserve(params.len());
        large_bitmap.reserve(params.len());

        for value in values.iter() {
            match *value {
                Value::NULL => {
                    null_bitmap.push(true);
                    large_bitmap.push(false);
                },
                Value::Bytes(ref bytes) => {
                    null_bitmap.push(false);
                    if bytes.len() as u64 >= cap as u64 - written {
                        large_bitmap.push(true);
                    } else {
                        large_bitmap.push(false);
                        written += output.write_bin_value(value)?;
                    }
                },
                _ => {
                    null_bitmap.push(false);
                    if cap as u64 - written < MAX_NON_BYTES_VALUE_BINARY_SIZE as u64 {
                        large_bitmap.push(true);
                    } else {
                        large_bitmap.push(false);
                        written += output.write_bin_value(value)?;
                    }
                }
            }
        }

        Ok((output, null_bitmap, large_bitmap))
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
                    y,
                    m,
                    d,
                    h,
                    i,
                    s,
                    u
                );
                f.debug_tuple("Date").field(&format).finish()
            }
            Value::Time(neg, d, h, i, s, 0) => {
                let format = if neg {
                    format!("'-{:03}:{:02}:{:02}'", d * 24 + h as u32, i, s)
                } else {
                    format!("'{:03}:{:02}:{:02}'", d * 24 + h as u32, i, s)
                };
                f.debug_tuple("Time").field(&format).finish()
            }
            Value::Time(neg, d, h, i, s, u) => {
                let format = if neg {
                    format!("'-{:03}:{:02}:{:02}.{:06}'", d * 24 + h as u32, i, s, u)
                } else {
                    format!("'{:03}:{:02}:{:02}.{:06}'", d * 24 + h as u32, i, s, u)
                };
                f.debug_tuple("Time").field(&format).finish()
            }
        }
    }
}
