// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::value::Value;
use byteorder::{LittleEndian as LE, ReadBytesExt, WriteBytesExt};
use std::io;

pub trait ReadMysqlExt: ReadBytesExt {
    /// Reads MySql's length-encoded integer.
    fn read_lenenc_int(&mut self) -> io::Result<u64> {
        match self.read_u8()? {
            x if x < 0xfc => Ok(x.into()),
            0xfc => self.read_uint::<LE>(2),
            0xfd => self.read_uint::<LE>(3),
            0xfe => self.read_uint::<LE>(8),
            0xff => Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid length-encoded integer value",
            )),
            _ => unreachable!(),
        }
    }
}

pub trait WriteMysqlExt: WriteBytesExt {
    /// Writes MySql's length-encoded integer.
    fn write_lenenc_int(&mut self, x: u64) -> io::Result<u64> {
        if x < 251 {
            self.write_u8(x as u8)?;
            Ok(1)
        } else if x < 65_536 {
            self.write_u8(0xFC)?;
            self.write_uint::<LE>(x, 2)?;
            Ok(3)
        } else if x < 16_777_216 {
            self.write_u8(0xFD)?;
            self.write_uint::<LE>(x, 3)?;
            Ok(4)
        } else {
            self.write_u8(0xFE)?;
            self.write_uint::<LE>(x, 8)?;
            Ok(9)
        }
    }

    /// Writes MySql's length-encoded string.
    fn write_lenenc_str(&mut self, bytes: &[u8]) -> io::Result<u64> {
        let written = self.write_lenenc_int(bytes.len() as u64)?;
        self.write_all(bytes)?;
        Ok(written + bytes.len() as u64)
    }

    /// Writes MySql's value in binary value format.
    fn write_bin_value(&mut self, value: &Value) -> io::Result<u64> {
        match *value {
            Value::NULL => Ok(0),
            Value::Bytes(ref x) => self.write_lenenc_str(&x[..]),
            Value::Int(x) => {
                self.write_i64::<LE>(x)?;
                Ok(8)
            }
            Value::UInt(x) => {
                self.write_u64::<LE>(x)?;
                Ok(8)
            }
            Value::F32(x) => {
                self.write_f32::<LE>(x)?;
                Ok(8)
            }
            Value::F64(x) => {
                self.write_f64::<LE>(x)?;
                Ok(8)
            }
            Value::Date(0u16, 0u8, 0u8, 0u8, 0u8, 0u8, 0u32) => {
                self.write_u8(0u8)?;
                Ok(1)
            }
            Value::Date(y, m, d, 0u8, 0u8, 0u8, 0u32) => {
                self.write_u8(4u8)?;
                self.write_u16::<LE>(y)?;
                self.write_u8(m)?;
                self.write_u8(d)?;
                Ok(5)
            }
            Value::Date(y, m, d, h, i, s, 0u32) => {
                self.write_u8(7u8)?;
                self.write_u16::<LE>(y)?;
                self.write_u8(m)?;
                self.write_u8(d)?;
                self.write_u8(h)?;
                self.write_u8(i)?;
                self.write_u8(s)?;
                Ok(8)
            }
            Value::Date(y, m, d, h, i, s, u) => {
                self.write_u8(11u8)?;
                self.write_u16::<LE>(y)?;
                self.write_u8(m)?;
                self.write_u8(d)?;
                self.write_u8(h)?;
                self.write_u8(i)?;
                self.write_u8(s)?;
                self.write_u32::<LE>(u)?;
                Ok(12)
            }
            Value::Time(_, 0u32, 0u8, 0u8, 0u8, 0u32) => {
                self.write_u8(0u8)?;
                Ok(1)
            }
            Value::Time(neg, d, h, m, s, 0u32) => {
                self.write_u8(8u8)?;
                self.write_u8(if neg { 1u8 } else { 0u8 })?;
                self.write_u32::<LE>(d)?;
                self.write_u8(h)?;
                self.write_u8(m)?;
                self.write_u8(s)?;
                Ok(9)
            }
            Value::Time(neg, d, h, m, s, u) => {
                self.write_u8(12u8)?;
                self.write_u8(if neg { 1u8 } else { 0u8 })?;
                self.write_u32::<LE>(d)?;
                self.write_u8(h)?;
                self.write_u8(m)?;
                self.write_u8(s)?;
                self.write_u32::<LE>(u)?;
                Ok(13)
            }
        }
    }
}

impl<T> ReadMysqlExt for T where T: ReadBytesExt {}
impl<T> WriteMysqlExt for T where T: WriteBytesExt {}
