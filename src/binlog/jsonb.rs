// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! MySql internal binary JSON representation.

use std::{
    borrow::Cow,
    convert::{TryFrom, TryInto},
    fmt, io,
    marker::PhantomData,
    str::{from_utf8, Utf8Error},
};

use bytes::BufMut;

use crate::{
    constants::ColumnType,
    io::ParseBuf,
    misc::{
        raw::{
            text::{BareText, VarLenText},
            RawText,
        },
        unexpected_buf_eof,
    },
    proto::MyDeserialize,
};

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(x) => write!(f, "{}", x),
            Value::I16(x) => write!(f, "{}", x),
            Value::U16(x) => write!(f, "{}", x),
            Value::I32(x) => write!(f, "{}", x),
            Value::U32(x) => write!(f, "{}", x),
            Value::I64(x) => write!(f, "{}", x),
            Value::U64(x) => write!(f, "{}", x),
            Value::F64(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{:?}", x),
            Value::SmallArray(a) => f.debug_list().entries(a.iter()).finish(),
            Value::LargeArray(a) => f.debug_list().entries(a.iter()).finish(),
            Value::SmallObject(o) => f
                .debug_map()
                .entries(o.iter().filter_map(|x| x.ok()))
                .finish(),
            Value::LargeObject(o) => f
                .debug_map()
                .entries(o.iter().filter_map(|x| x.ok()))
                .finish(),
            Value::Opaque(x) => write!(f, "<{:?}>", x),
        }
    }
}

/// An iterator over keys of an object.
#[derive(Copy, Clone, PartialEq)]
pub struct ArrayIter<'a, T> {
    cur: u32,
    arr: &'a ComplexValue<'a, T, Array>,
}

impl<'a, T: StorageFormat> Iterator for ArrayIter<'a, T> {
    type Item = io::Result<Value<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.arr.elem_at(self.cur).transpose() {
            Some(x) => {
                self.cur += 1;
                Some(x)
            }
            None => None,
        }
    }
}

/// An iterator over entries of an object.
#[derive(Copy, Clone, PartialEq)]
pub struct ObjectIter<'a, T> {
    cur: u32,
    obj: &'a ComplexValue<'a, T, Object>,
}

impl<'a, T: StorageFormat> Iterator for ObjectIter<'a, T> {
    type Item = io::Result<(RawText<'a, BareText>, Value<'a>)>;

    fn next(&mut self) -> Option<Self::Item> {
        let key = self.obj.key_at(self.cur).transpose();
        let val = self.obj.elem_at(self.cur).transpose();
        match key {
            Some(key) => match val {
                Some(val) => {
                    self.cur += 1;
                    Some(key.and_then(|key| val.map(|val| (key, val))))
                }
                None => None,
            },
            None => None,
        }
    }
}

/// An iterator over keys of an object.
#[derive(Copy, Clone, PartialEq)]
pub struct ObjectKeys<'a, T> {
    cur: u32,
    obj: &'a ComplexValue<'a, T, Object>,
}

impl<'a, T: StorageFormat> Iterator for ObjectKeys<'a, T> {
    type Item = io::Result<RawText<'a, BareText>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.obj.key_at(self.cur).transpose() {
            Some(x) => {
                self.cur += 1;
                Some(x)
            }
            None => None,
        }
    }
}

/// JSONB array or object.
#[derive(Clone, PartialEq)]
pub struct ComplexValue<'a, T, U> {
    element_count: u32,
    data: Cow<'a, [u8]>,
    __phantom: PhantomData<(T, U)>,
}

impl<'a, T, U> ComplexValue<'a, T, U> {
    /// Returns a `'static` version of `self`.
    pub fn into_owned(self) -> ComplexValue<'static, T, U> {
        ComplexValue {
            element_count: self.element_count,
            data: Cow::Owned(self.data.into_owned()),
            __phantom: PhantomData,
        }
    }

    /// Returns the number of lements.
    pub fn element_count(&self) -> u32 {
        self.element_count
    }
}

impl<'a, T: StorageFormat> ComplexValue<'a, T, Object> {
    /// Returns a key with the given index.
    ///
    /// Returns `None` if `pos >= self.element_count()`.
    pub fn key_at<'b>(&'b self, pos: u32) -> io::Result<Option<RawText<'b, BareText>>> {
        if pos >= self.element_count {
            return Ok(None);
        }

        let entry_offset = 2 * T::OFFSET_SIZE as usize + T::KEY_ENTRY_SIZE as usize * pos as usize;

        let mut entry_buf = ParseBuf(&self.data[entry_offset..]);
        let key_offset = T::eat_offset(&mut entry_buf) as usize;
        let key_length = entry_buf.eat_u16_le() as usize;

        if (key_offset
            < entry_offset
                + ((self.element_count - pos) as usize) * T::KEY_ENTRY_SIZE as usize
                + (self.element_count as usize) * T::VALUE_ENTRY_SIZE as usize)
            || (self.data.len() < key_offset + key_length)
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "malformed jsonb: invalid key offset pointer",
            ));
        }

        Ok(Some(RawText::new(
            &self.data[key_offset..(key_offset + key_length)],
        )))
    }

    /// Returns an iterator over keys of an object.
    pub fn keys<'b>(&'b self) -> ObjectKeys<'b, T> {
        ObjectKeys { cur: 0, obj: self }
    }

    /// Returns an iterator over entries of an object.
    pub fn iter<'b>(&'b self) -> ObjectIter<'b, T> {
        ObjectIter { cur: 0, obj: self }
    }
}

impl<'a, T: StorageFormat> ComplexValue<'a, T, Array> {
    /// Returns an iterator over entries of an array.
    pub fn iter<'b>(&'b self) -> ArrayIter<'b, T> {
        ArrayIter { cur: 0, arr: self }
    }
}

impl<'a, T: StorageFormat, U: ComplexType> ComplexValue<'a, T, U> {
    /// Returns an element at the given position.
    ///
    /// * for arrays returns an element at the given position in an arrary,
    /// * for objects returns an element with the given key index.
    ///
    /// Returns `None` if `pos >= self.element_count()`.
    pub fn elem_at<'b>(&'b self, pos: u32) -> io::Result<Option<Value<'b>>> {
        if pos >= self.element_count {
            return Ok(None);
        }

        let entry_size = T::VALUE_ENTRY_SIZE as usize;
        let entry_offset = U::value_entry_offset::<T>(self.element_count, pos);

        let value_type = JsonbType::try_from(self.data[entry_offset])
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        if value_type.is_inlined::<T>() {
            let start = entry_offset + 1;
            let end = start + entry_size - 1;
            return Value::deserialize_simple(value_type, &mut ParseBuf(&self.data[start..end]))
                .map(Some);
        }

        let value_offset = T::eat_offset(&mut ParseBuf(&self.data[(entry_offset + 1)..])) as usize;

        if self.data.len() < value_offset || value_offset < entry_offset + entry_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "malformed jsonb: invalid value offset pointer",
            ));
        }

        Value::deserialize_value(value_type, &mut ParseBuf(&self.data[value_offset..])).map(Some)
    }
}

impl<'a, T: StorageFormat> TryInto<serde_json::Value> for ComplexValue<'a, T, Array> {
    type Error = JsonbToJsonError;

    fn try_into(self) -> Result<serde_json::Value, Self::Error> {
        Ok(serde_json::Value::Array(
            self.iter()
                .map(|x| {
                    x.map_err(|e| Self::Error::InvalidJsonb(e))
                        .and_then(TryInto::try_into)
                })
                .collect::<Result<Vec<_>, Self::Error>>()?,
        ))
    }
}

impl<'a, T: StorageFormat> TryInto<serde_json::Value> for ComplexValue<'a, T, Object> {
    type Error = JsonbToJsonError;

    fn try_into(self) -> Result<serde_json::Value, Self::Error> {
        Ok(serde_json::Value::Object(
            self.iter()
                .map(|x| {
                    x.map_err(|e| Self::Error::InvalidJsonb(e))
                        .and_then(|(k, v)| Ok((from_utf8(k.0.as_ref())?.to_owned(), v.try_into()?)))
                })
                .collect::<Result<serde_json::Map<_, _>, Self::Error>>()?,
        ))
    }
}

impl<'de, T: StorageFormat, U: ComplexType> MyDeserialize<'de> for ComplexValue<'de, T, U> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, whole_buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut sbuf = ParseBuf(whole_buf.0)
            .checked_eat_buf((T::OFFSET_SIZE * 2) as usize)
            .ok_or_else(unexpected_buf_eof)?;

        let element_count = T::eat_offset(&mut sbuf);
        let bytes = T::eat_offset(&mut sbuf);

        if bytes as usize > whole_buf.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "jsonb array/object size is larger than the whole array/object",
            ));
        }

        let header_size = U::header_size::<T>(element_count);

        if header_size > bytes as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "jsonb array/object header is larger than the whole array/object",
            ));
        }

        let data = whole_buf
            .checked_eat(bytes as usize)
            .ok_or_else(unexpected_buf_eof)?;

        Ok(Self {
            element_count,
            data: Cow::Borrowed(data),
            __phantom: PhantomData,
        })
    }
}

/// JSONB opaque value
#[derive(Debug, Clone, PartialEq)]
pub struct OpaqueValue<'a> {
    pub value_type: ColumnType,
    pub data: Cow<'a, [u8]>,
}

impl<'a> OpaqueValue<'a> {
    pub fn into_owned(self) -> OpaqueValue<'static> {
        OpaqueValue {
            value_type: self.value_type,
            data: Cow::Owned(self.data.into_owned()),
        }
    }
}

/// Jsonb Value.
#[derive(Clone, PartialEq)]
pub enum Value<'a> {
    Null,
    Bool(bool),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F64(f64),
    String(RawText<'a, VarLenText>),
    SmallArray(ComplexValue<'a, Small, Array>),
    LargeArray(ComplexValue<'a, Large, Array>),
    SmallObject(ComplexValue<'a, Small, Object>),
    LargeObject(ComplexValue<'a, Large, Object>),
    Opaque(OpaqueValue<'a>),
}

impl<'a> Value<'a> {
    /// Returns a `'static` version of `self`.
    pub fn into_owned(self) -> Value<'static> {
        match self {
            Value::Null => Value::Null,
            Value::Bool(x) => Value::Bool(x),
            Value::I16(x) => Value::I16(x),
            Value::U16(x) => Value::U16(x),
            Value::I32(x) => Value::I32(x),
            Value::U32(x) => Value::U32(x),
            Value::I64(x) => Value::I64(x),
            Value::U64(x) => Value::U64(x),
            Value::F64(x) => Value::F64(x),
            Value::String(x) => Value::String(x.into_owned()),
            Value::SmallArray(x) => Value::SmallArray(x.into_owned()),
            Value::LargeArray(x) => Value::LargeArray(x.into_owned()),
            Value::SmallObject(x) => Value::SmallObject(x.into_owned()),
            Value::LargeObject(x) => Value::LargeObject(x.into_owned()),
            Value::Opaque(x) => Value::Opaque(x.into_owned()),
        }
    }

    fn deserialize_value(value_type: JsonbType, buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        match value_type {
            JsonbType::JSONB_TYPE_SMALL_OBJECT => {
                Ok(Value::SmallObject(ComplexValue::deserialize((), buf)?))
            }
            JsonbType::JSONB_TYPE_LARGE_OBJECT => {
                Ok(Value::LargeObject(ComplexValue::deserialize((), buf)?))
            }
            JsonbType::JSONB_TYPE_SMALL_ARRAY => {
                Ok(Value::SmallArray(ComplexValue::deserialize((), buf)?))
            }
            JsonbType::JSONB_TYPE_LARGE_ARRAY => {
                Ok(Value::LargeArray(ComplexValue::deserialize((), buf)?))
            }
            _ => Value::deserialize_simple(value_type, buf),
        }
    }

    fn deserialize_simple(value_type: JsonbType, buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        match value_type {
            JsonbType::JSONB_TYPE_LITERAL => Value::deserialize_literal(buf),
            JsonbType::JSONB_TYPE_INT16 => buf
                .checked_eat_i16_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::I16),
            JsonbType::JSONB_TYPE_UINT16 => buf
                .checked_eat_u16_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::U16),
            JsonbType::JSONB_TYPE_INT32 => buf
                .checked_eat_i32_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::I32),
            JsonbType::JSONB_TYPE_UINT32 => buf
                .checked_eat_u32_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::U32),
            JsonbType::JSONB_TYPE_INT64 => buf
                .checked_eat_i64_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::I64),
            JsonbType::JSONB_TYPE_UINT64 => buf
                .checked_eat_u64_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::U64),
            JsonbType::JSONB_TYPE_DOUBLE => buf
                .checked_eat_f64_le()
                .ok_or_else(unexpected_buf_eof)
                .map(Value::F64),
            JsonbType::JSONB_TYPE_STRING => Value::deserialize_string(buf),
            JsonbType::JSONB_TYPE_OPAQUE => Value::deserialize_opaque(buf),
            JsonbType::JSONB_TYPE_SMALL_OBJECT
            | JsonbType::JSONB_TYPE_LARGE_OBJECT
            | JsonbType::JSONB_TYPE_SMALL_ARRAY
            | JsonbType::JSONB_TYPE_LARGE_ARRAY => unreachable!(),
        }
    }

    fn deserialize_literal(buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        let literal_type = buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)?;

        match LiteralType::try_from(literal_type)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?
        {
            LiteralType::JSONB_NULL_LITERAL => Ok(Value::Null),
            LiteralType::JSONB_TRUE_LITERAL => Ok(Value::Bool(true)),
            LiteralType::JSONB_FALSE_LITERAL => Ok(Value::Bool(false)),
        }
    }

    fn deserialize_string(buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        let string = RawText::<'a, VarLenText>::deserialize((), buf)?;
        Ok(Value::String(string))
    }

    fn deserialize_opaque(buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        let column_type = buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)?;
        let column_type = ColumnType::try_from(column_type)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let len = deserialize_variable_length(buf)? as usize;
        let data = buf.checked_eat(len).ok_or_else(unexpected_buf_eof)?;
        Ok(Value::Opaque(OpaqueValue {
            value_type: column_type,
            data: Cow::Borrowed(data),
        }))
    }

    /// Returns true if this value is an array.
    pub fn is_array(&self) -> bool {
        match self {
            Value::SmallArray { .. } | Value::LargeArray { .. } => true,
            _ => false,
        }
    }

    /// Returns true if this value is an object.
    pub fn is_object(&self) -> bool {
        match self {
            Value::SmallObject { .. } | Value::LargeObject { .. } => true,
            _ => false,
        }
    }

    /// Returns true if this value is an int (i16, i32 or i64).
    pub fn is_int(&self) -> bool {
        match self {
            Value::I16(_) | Value::I32(_) | Value::I64(_) => true,
            _ => false,
        }
    }

    /// Returns true if this value is an int (u16, u32 or u64).
    pub fn is_uint(&self) -> bool {
        match self {
            Value::U16(_) | Value::U32(_) | Value::U64(_) => true,
            _ => false,
        }
    }

    /// Returns true if this value is f64.
    pub fn is_double(&self) -> bool {
        match self {
            Value::F64(_) => true,
            _ => false,
        }
    }

    /// Returns the number of lements in array or buffer.
    pub fn element_count(&self) -> Option<u32> {
        match self {
            Value::SmallArray(x) => Some(x.element_count()),
            Value::LargeArray(x) => Some(x.element_count()),
            Value::SmallObject(x) => Some(x.element_count()),
            Value::LargeObject(x) => Some(x.element_count()),
            _ => None,
        }
    }

    /// Returns the field type of an opaque value.
    pub fn field_type(&self) -> Option<ColumnType> {
        match self {
            Value::Opaque(OpaqueValue { value_type, .. }) => Some(*value_type),
            _ => None,
        }
    }
}

impl<'a> TryInto<serde_json::Value> for Value<'a> {
    type Error = JsonbToJsonError;

    fn try_into(self) -> Result<serde_json::Value, Self::Error> {
        match self {
            Value::Null => Ok(serde_json::Value::Null),
            Value::Bool(x) => Ok(serde_json::Value::Bool(x)),
            Value::I16(x) => Ok(x.into()),
            Value::U16(x) => Ok(x.into()),
            Value::I32(x) => Ok(x.into()),
            Value::U32(x) => Ok(x.into()),
            Value::I64(x) => Ok(x.into()),
            Value::U64(x) => Ok(x.into()),
            Value::F64(x) => Ok(x.into()),
            Value::String(s) => Ok(from_utf8(s.as_ref())?.into()),
            Value::SmallArray(x) => x.try_into(),
            Value::LargeArray(x) => x.try_into(),
            Value::SmallObject(x) => x.try_into(),
            Value::LargeObject(x) => x.try_into(),
            Value::Opaque(_) => Err(Self::Error::Opaque),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum JsonbToJsonError {
    #[error("JSONB value is invalid: {}", _0)]
    InvalidJsonb(#[from] io::Error),
    #[error("JSONB contains an opaque value")]
    Opaque,
    #[error("JSONB contains invalid UTF-8 char sequences: {}", _0)]
    InvalidUtf8(#[from] Utf8Error),
}

impl<'de> MyDeserialize<'de> for Value<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        if buf.is_empty() {
            // We'll interpret an empty jsonb value as null (MySql does so)
            return Ok(Value::Null);
        }

        let value_type = JsonbType::try_from(buf.eat_u8())
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        Value::deserialize_value(value_type, buf)
    }
}

/// Type of a complex jsonb value (array or object).
pub trait ComplexType {
    const IS_ARRAY: bool;

    /// Calculates header size for this type.
    fn header_size<T: StorageFormat>(element_count: u32) -> usize;

    /// Calculates an offset to the given value entry.
    fn value_entry_offset<T: StorageFormat>(element_count: u32, pos: u32) -> usize;
}

/// An Object (see [`ComplexType`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Object;

impl ComplexType for Object {
    const IS_ARRAY: bool = false;

    fn header_size<T: StorageFormat>(element_count: u32) -> usize {
        let mut header_size = 2 * T::OFFSET_SIZE as usize;
        header_size += element_count as usize * T::KEY_ENTRY_SIZE as usize;
        header_size += element_count as usize * T::VALUE_ENTRY_SIZE as usize;
        header_size
    }

    fn value_entry_offset<T: StorageFormat>(element_count: u32, pos: u32) -> usize {
        let mut first_entry_offset = 2 * T::OFFSET_SIZE as usize;
        first_entry_offset += element_count as usize * T::KEY_ENTRY_SIZE as usize;
        first_entry_offset + T::VALUE_ENTRY_SIZE as usize * pos as usize
    }
}

/// An Array (see [`ComplexType`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Array;

impl ComplexType for Array {
    const IS_ARRAY: bool = true;

    fn header_size<T: StorageFormat>(element_count: u32) -> usize {
        let mut header_size = 2 * T::OFFSET_SIZE as usize;
        header_size += element_count as usize * T::VALUE_ENTRY_SIZE as usize;
        header_size
    }

    fn value_entry_offset<T: StorageFormat>(_element_count: u32, pos: u32) -> usize {
        let first_entry_offset = 2 * T::OFFSET_SIZE as usize;
        first_entry_offset + T::VALUE_ENTRY_SIZE as usize * pos as usize
    }
}

/// JSONB storage format for objects and arrays.
pub trait StorageFormat {
    const IS_LARGE: bool;
    /// The size of offset or size fields.
    const OFFSET_SIZE: u8;
    const KEY_ENTRY_SIZE: u8 = Self::OFFSET_SIZE + 2;
    const VALUE_ENTRY_SIZE: u8 = Self::OFFSET_SIZE + 1;

    fn eat_offset(buf: &mut ParseBuf<'_>) -> u32;
}

/// Small array/object storage format. See [`StorageFormat`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Small;

impl StorageFormat for Small {
    const IS_LARGE: bool = false;
    const OFFSET_SIZE: u8 = 2;

    fn eat_offset(buf: &mut ParseBuf<'_>) -> u32 {
        buf.eat_u16_le() as u32
    }
}

/// Large array/object storage format. See [`StorageFormat`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Large;

impl StorageFormat for Large {
    const IS_LARGE: bool = true;
    const OFFSET_SIZE: u8 = 4;

    fn eat_offset(buf: &mut ParseBuf<'_>) -> u32 {
        buf.eat_u32_le()
    }
}

/// 1-byte JSONB type marker
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum JsonbType {
    JSONB_TYPE_SMALL_OBJECT = 0x0,
    JSONB_TYPE_LARGE_OBJECT = 0x1,
    JSONB_TYPE_SMALL_ARRAY = 0x2,
    JSONB_TYPE_LARGE_ARRAY = 0x3,
    JSONB_TYPE_LITERAL = 0x4,
    JSONB_TYPE_INT16 = 0x5,
    JSONB_TYPE_UINT16 = 0x6,
    JSONB_TYPE_INT32 = 0x7,
    JSONB_TYPE_UINT32 = 0x8,
    JSONB_TYPE_INT64 = 0x9,
    JSONB_TYPE_UINT64 = 0xA,
    JSONB_TYPE_DOUBLE = 0xB,
    JSONB_TYPE_STRING = 0xC,
    JSONB_TYPE_OPAQUE = 0xF,
}

impl JsonbType {
    fn is_inlined<T: StorageFormat>(&self) -> bool {
        match self {
            JsonbType::JSONB_TYPE_LITERAL
            | JsonbType::JSONB_TYPE_INT16
            | JsonbType::JSONB_TYPE_UINT16 => true,
            JsonbType::JSONB_TYPE_INT32 | JsonbType::JSONB_TYPE_UINT32 => T::IS_LARGE,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
#[error("Unknown JSONB type {}", _0)]
#[repr(transparent)]
pub struct UnknownJsonbType(pub u8);

impl From<UnknownJsonbType> for u8 {
    fn from(x: UnknownJsonbType) -> Self {
        x.0
    }
}

impl TryFrom<u8> for JsonbType {
    type Error = UnknownJsonbType;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x0 => Ok(Self::JSONB_TYPE_SMALL_OBJECT),
            0x1 => Ok(Self::JSONB_TYPE_LARGE_OBJECT),
            0x2 => Ok(Self::JSONB_TYPE_SMALL_ARRAY),
            0x3 => Ok(Self::JSONB_TYPE_LARGE_ARRAY),
            0x4 => Ok(Self::JSONB_TYPE_LITERAL),
            0x5 => Ok(Self::JSONB_TYPE_INT16),
            0x6 => Ok(Self::JSONB_TYPE_UINT16),
            0x7 => Ok(Self::JSONB_TYPE_INT32),
            0x8 => Ok(Self::JSONB_TYPE_UINT32),
            0x9 => Ok(Self::JSONB_TYPE_INT64),
            0xA => Ok(Self::JSONB_TYPE_UINT64),
            0xB => Ok(Self::JSONB_TYPE_DOUBLE),
            0xC => Ok(Self::JSONB_TYPE_STRING),
            0xF => Ok(Self::JSONB_TYPE_OPAQUE),
            x => Err(UnknownJsonbType(x)),
        }
    }
}

/// 1-byte JSONB literal type
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum LiteralType {
    JSONB_NULL_LITERAL = 0x0,
    JSONB_TRUE_LITERAL = 0x1,
    JSONB_FALSE_LITERAL = 0x2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
#[error("Unknown JSONB literal type {}", _0)]
#[repr(transparent)]
pub struct UnknownLiteralType(pub u8);

impl From<UnknownLiteralType> for u8 {
    fn from(x: UnknownLiteralType) -> Self {
        x.0
    }
}

impl TryFrom<u8> for LiteralType {
    type Error = UnknownLiteralType;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x0 => Ok(Self::JSONB_NULL_LITERAL),
            0x1 => Ok(Self::JSONB_TRUE_LITERAL),
            0x2 => Ok(Self::JSONB_FALSE_LITERAL),
            x => Err(UnknownLiteralType(x)),
        }
    }
}

/// Deserializes varialbe-length (an ineger) that is used within JSONB.
pub fn deserialize_variable_length<'de>(buf: &mut ParseBuf<'de>) -> io::Result<u32> {
    // variable-length takes up to 5 bytes
    const MAX_REPR_LEN: usize = 5;

    let mut len = 0_u64;
    for i in 0..MAX_REPR_LEN {
        let byte = buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)? as u64;
        len |= (byte & 0x7f) << (7 * i);
        if byte & 0x80 == 0 {
            if len > (u32::MAX as u64) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "invalid variable-length value (> u32::MAX)",
                ));
            }
            return Ok(len as u32);
        }
    }

    return Err(io::Error::new(
        io::ErrorKind::InvalidData,
        "invalid variable-length value (more than 5 bytes)",
    ));
}

/// Serializes variable-length (an integer) that is used within JSONB.
pub fn serialize_variable_lenght(mut length: u32, buf: &mut Vec<u8>) {
    loop {
        let mut byte = (length & 0x7F) as u8;
        length >>= 7;
        if length != 0 {
            byte |= 0x80;
            buf.put_u8(byte);
            break;
        } else {
            buf.put_u8(byte);
        }
    }
}
