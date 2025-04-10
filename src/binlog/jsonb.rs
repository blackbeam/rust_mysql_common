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
    collections::BTreeMap,
    convert::{TryFrom, TryInto},
    fmt, io,
    iter::FromIterator,
    marker::PhantomData,
    str::{Utf8Error, from_utf8},
};

use base64::{Engine, prelude::BASE64_STANDARD};
use serde_json::Number;

use crate::{
    constants::ColumnType,
    io::ParseBuf,
    misc::{
        raw::{Const, RawBytes, bytes::BareU16Bytes, int::*},
        unexpected_buf_eof,
    },
    proto::{MyDeserialize, MySerialize},
};

use super::{decimal::Decimal, time::MysqlTime};

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

/// A key of a JSONB object.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct ObjectKey<'a>(RawBytes<'a, BareU16Bytes>);

impl<'a> ObjectKey<'a> {
    pub fn new(key: impl Into<Cow<'a, [u8]>>) -> Self {
        Self(RawBytes::new(key))
    }

    /// Returns the raw value of a key.
    pub fn value_raw(&'a self) -> &'a [u8] {
        self.0.as_bytes()
    }

    /// Returns the value of a key as a string (lossy converted).
    pub fn value(&'a self) -> Cow<'a, str> {
        self.0.as_str()
    }
}

/// A key of a JSONB object.
#[derive(Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct JsonbString<'a>(RawBytes<'a, VarLen>);

impl<'a> JsonbString<'a> {
    pub fn new(string: impl Into<Cow<'a, [u8]>>) -> Self {
        Self(RawBytes::new(string))
    }

    /// Returns the raw string value.
    pub fn str_raw(&'a self) -> &'a [u8] {
        self.0.as_bytes()
    }

    /// Returns the string value as UTF-8 string (lossy converted).
    pub fn str(&'a self) -> Cow<'a, str> {
        self.0.as_str()
    }

    pub fn into_owned(self) -> JsonbString<'static> {
        JsonbString(self.0.into_owned())
    }

    pub fn into_raw(self) -> Cow<'a, [u8]> {
        self.0.0
    }
}

impl fmt::Debug for JsonbString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.str())
    }
}

impl<'de> MyDeserialize<'de> for JsonbString<'de> {
    const SIZE: Option<usize> = None;
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        Ok(Self(buf.parse(())?))
    }
}

impl MySerialize for JsonbString<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        self.0.serialize(buf);
    }
}

/// An iterator over entries of an object.
#[derive(Copy, Clone, PartialEq)]
pub struct ObjectIter<'a, T> {
    cur: u32,
    obj: &'a ComplexValue<'a, T, Object>,
}

impl<'a, T: StorageFormat> Iterator for ObjectIter<'a, T> {
    type Item = io::Result<(ObjectKey<'a>, Value<'a>)>;

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
    type Item = io::Result<ObjectKey<'a>>;

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

    /// Returns the number of elements.
    pub fn element_count(&self) -> u32 {
        self.element_count
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum JsonbError {
    #[error("Malformed jsonb: invalid key offset pointer.")]
    InvalidKeyOffsetPointer,
    #[error("Malformed jsonb: invalid value offset pointer.")]
    InvalidValueOffsetPointer,
    #[error("Malformed jsonb: jsonb array/object size is larger than the whole array/object.")]
    SizeOverflow,
}

impl<'a, T: StorageFormat> ComplexValue<'a, T, Object> {
    /// Returns a key with the given index.
    ///
    /// Returns `None` if `pos >= self.element_count()`.
    pub fn key_at(&'a self, pos: u32) -> io::Result<Option<ObjectKey<'a>>> {
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
                JsonbError::InvalidKeyOffsetPointer,
            ));
        }

        let key_val = &self.data[key_offset..(key_offset + key_length)];
        Ok(Some(ObjectKey::new(key_val)))
    }

    /// Returns an iterator over keys of an object.
    pub fn keys(&'a self) -> ObjectKeys<'a, T> {
        ObjectKeys { cur: 0, obj: self }
    }

    /// Returns an iterator over entries of an object.
    pub fn iter(&'a self) -> ObjectIter<'a, T> {
        ObjectIter { cur: 0, obj: self }
    }
}

impl<'a, T: StorageFormat> ComplexValue<'a, T, Array> {
    /// Returns an iterator over entries of an array.
    pub fn iter(&'a self) -> ArrayIter<'a, T> {
        ArrayIter { cur: 0, arr: self }
    }
}

impl<'a, T: StorageFormat, U: ComplexType> ComplexValue<'a, T, U> {
    /// Returns an element at the given position.
    ///
    /// * for arrays  — returns an element at the given position,
    /// * for objects — returns an element with the given key index.
    ///
    /// Returns `None` if `pos >= self.element_count()`.
    pub fn elem_at(&'a self, pos: u32) -> io::Result<Option<Value<'a>>> {
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
                JsonbError::InvalidValueOffsetPointer,
            ));
        }

        Value::deserialize_value(value_type, &mut ParseBuf(&self.data[value_offset..])).map(Some)
    }
}

impl<'a, T: StorageFormat> TryFrom<ComplexValue<'a, T, Array>> for serde_json::Value {
    type Error = JsonbToJsonError;

    fn try_from(value: ComplexValue<'a, T, Array>) -> Result<Self, Self::Error> {
        let values = value
            .iter()
            .map(|x| {
                x.map_err(Self::Error::InvalidJsonb)
                    .and_then(TryFrom::try_from)
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(serde_json::Value::Array(values))
    }
}

impl<'a, T: StorageFormat> TryFrom<ComplexValue<'a, T, Object>> for serde_json::Value {
    type Error = JsonbToJsonError;

    fn try_from(value: ComplexValue<'a, T, Object>) -> Result<Self, Self::Error> {
        let k_vs = value
            .iter()
            .map(|x| {
                x.map_err(Self::Error::InvalidJsonb)
                    .and_then(|(k, v)| Ok((from_utf8(k.value_raw())?.to_owned(), v.try_into()?)))
            })
            .collect::<Result<serde_json::Map<_, _>, _>>()?;
        Ok(serde_json::Value::Object(k_vs))
    }
}

impl<'de, T: StorageFormat, U: ComplexType> MyDeserialize<'de> for ComplexValue<'de, T, U> {
    const SIZE: Option<usize> = None;
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
                JsonbError::SizeOverflow,
            ));
        }

        let header_size = U::header_size::<T>(element_count);

        if header_size > bytes as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                JsonbError::SizeOverflow,
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
    value_type: Const<ColumnType, u8>,
    data: RawBytes<'a, VarLen>,
}

impl<'a> OpaqueValue<'a> {
    pub fn new(value_type: ColumnType, data: impl Into<Cow<'a, [u8]>>) -> Self {
        Self {
            value_type: Const::new(value_type),
            data: RawBytes::new(data),
        }
    }

    /// Returns the value type.
    pub fn value_type(&self) -> ColumnType {
        self.value_type.0
    }

    /// Returns the raw value data.
    pub fn data_raw(&'a self) -> &'a [u8] {
        self.data.as_bytes()
    }

    /// Returns the value data as a string (lossy converted).
    pub fn data(&'a self) -> Cow<'a, str> {
        self.data.as_str()
    }

    pub fn into_owned(self) -> OpaqueValue<'static> {
        OpaqueValue {
            value_type: self.value_type,
            data: self.data.into_owned(),
        }
    }

    pub fn into_data(self) -> Cow<'a, [u8]> {
        self.data.0
    }
}

/// Structured in-memory representation of a JSON value.
///
/// You can get this value using [`Value::parse`].
///
/// You can convert this into a [`serde_json::Value`] (using [`From`] impl). Opaque values will
/// be handled as follows:
///
/// *   [`ColumnType::MYSQL_TYPE_NEWDECIMAL`] — will be converted to string
/// *   [`ColumnType::MYSQL_TYPE_DATE`] — will be converted to 'YYYY-MM-DD' string
/// *   [`ColumnType::MYSQL_TYPE_TIME`] — will be converted to '[-][h]hh:mm::ss.µµµµµµ' string
/// *   [`ColumnType::MYSQL_TYPE_DATETIME`] and [`ColumnType::MYSQL_TYPE_TIMESTAMP`]
///     — will be converted to 'YYYY-MM-DD hh:mm::ss.µµµµµµ' string
/// *   other opaque values will be represented as strings in the form `base64:type<id>:<data>`
///     where:
///     -   `<id>` — [`ColumnType`] integer value
///     -   `<data>` — base64-encoded opaque data
#[derive(Debug, Clone, PartialEq)]
pub enum JsonDom {
    Container(JsonContainer),
    Scalar(JsonScalar),
}

impl From<JsonDom> for serde_json::Value {
    fn from(value: JsonDom) -> Self {
        match value {
            JsonDom::Container(json_container) => json_container.into(),
            JsonDom::Scalar(json_scalar) => json_scalar.into(),
        }
    }
}

/// [`JsonDom`] container.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonContainer {
    Array(Vec<JsonDom>),
    Object(BTreeMap<String, JsonDom>),
}

impl From<JsonContainer> for serde_json::Value {
    fn from(value: JsonContainer) -> Self {
        match value {
            JsonContainer::Array(vec) => {
                serde_json::Value::Array(Vec::from_iter(vec.into_iter().map(|x| x.into())))
            }
            JsonContainer::Object(btree_map) => serde_json::Value::Object(
                serde_json::Map::from_iter(btree_map.into_iter().map(|(k, v)| (k, v.into()))),
            ),
        }
    }
}

/// [`JsonDom`] scalar value.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonScalar {
    Boolean(bool),
    DateTime(MysqlTime),
    Null,
    Number(JsonNumber),
    Opaque(JsonOpaque),
    String(String),
}

impl From<JsonScalar> for serde_json::Value {
    fn from(value: JsonScalar) -> Self {
        match value {
            JsonScalar::Boolean(x) => serde_json::Value::Bool(x),
            JsonScalar::DateTime(mysql_time) => {
                serde_json::Value::String(format!("{:.6}", mysql_time))
            }
            JsonScalar::Null => serde_json::Value::Null,
            JsonScalar::Number(json_number) => json_number.into(),
            JsonScalar::Opaque(json_opaque) => json_opaque.into(),
            JsonScalar::String(x) => serde_json::Value::String(x),
        }
    }
}

/// [`JsonDom`] number.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonNumber {
    Decimal(Decimal),
    Double(f64),
    Int(i64),
    Uint(u64),
}

impl From<JsonNumber> for serde_json::Value {
    fn from(value: JsonNumber) -> Self {
        match value {
            JsonNumber::Decimal(decimal) => serde_json::Value::String(decimal.to_string()),
            JsonNumber::Double(x) => serde_json::Value::Number(
                Number::from_f64(x)
                    // infinities an NaN are rendered as `0`
                    .unwrap_or_else(|| Number::from(0_u64)),
            ),
            JsonNumber::Int(x) => serde_json::Value::Number(Number::from(x)),
            JsonNumber::Uint(x) => serde_json::Value::Number(Number::from(x)),
        }
    }
}

/// [`JsonDom`] opaque value.
#[derive(Debug, Clone, PartialEq)]
pub struct JsonOpaque {
    field_type: ColumnType,
    value: Vec<u8>,
}

impl fmt::Display for JsonOpaque {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "base64:type{}:{}",
            self.field_type as u8,
            BASE64_STANDARD.encode(&self.value)
        )
    }
}

impl From<JsonOpaque> for serde_json::Value {
    fn from(value: JsonOpaque) -> Self {
        serde_json::Value::String(value.to_string())
    }
}

/// Deserialized Jsonb value.
///
/// You can [`Value::parse`] it to a structured [`JsonDom`] value.
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
    String(JsonbString<'a>),
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
            JsonbType::JSONB_TYPE_SMALL_OBJECT => Ok(Value::SmallObject(buf.parse(())?)),
            JsonbType::JSONB_TYPE_LARGE_OBJECT => Ok(Value::LargeObject(buf.parse(())?)),
            JsonbType::JSONB_TYPE_SMALL_ARRAY => Ok(Value::SmallArray(buf.parse(())?)),
            JsonbType::JSONB_TYPE_LARGE_ARRAY => Ok(Value::LargeArray(buf.parse(())?)),
            _ => Value::deserialize_simple(value_type, buf),
        }
    }

    fn deserialize_simple(value_type: JsonbType, buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        match value_type {
            JsonbType::JSONB_TYPE_LITERAL => Value::deserialize_literal(buf),
            JsonbType::JSONB_TYPE_INT16 => buf.parse::<RawInt<LeI16>>(()).map(|x| Value::I16(*x)),
            JsonbType::JSONB_TYPE_UINT16 => buf.parse::<RawInt<LeU16>>(()).map(|x| Value::U16(*x)),
            JsonbType::JSONB_TYPE_INT32 => buf.parse::<RawInt<LeI32>>(()).map(|x| Value::I32(*x)),
            JsonbType::JSONB_TYPE_UINT32 => buf.parse::<RawInt<LeU32>>(()).map(|x| Value::U32(*x)),
            JsonbType::JSONB_TYPE_INT64 => buf.parse::<RawInt<LeI64>>(()).map(|x| Value::I64(*x)),
            JsonbType::JSONB_TYPE_UINT64 => buf.parse::<RawInt<LeU64>>(()).map(|x| Value::U64(*x)),
            JsonbType::JSONB_TYPE_DOUBLE => buf.parse(()).map(Value::F64),
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
        buf.parse(()).map(Value::String)
    }

    fn deserialize_opaque(buf: &mut ParseBuf<'a>) -> io::Result<Self> {
        Ok(Value::Opaque(OpaqueValue {
            value_type: buf.parse(())?,
            data: buf.parse(())?,
        }))
    }

    /// Returns true if this value is an array.
    pub fn is_array(&self) -> bool {
        matches!(self, Value::SmallArray { .. } | Value::LargeArray { .. })
    }

    /// Returns true if this value is an object.
    pub fn is_object(&self) -> bool {
        matches!(self, Value::SmallObject { .. } | Value::LargeObject { .. })
    }

    /// Returns true if this value is an int (i16, i32 or i64).
    pub fn is_int(&self) -> bool {
        matches!(self, Value::I16(_) | Value::I32(_) | Value::I64(_))
    }

    /// Returns true if this value is an int (u16, u32 or u64).
    pub fn is_uint(&self) -> bool {
        matches!(self, Value::U16(_) | Value::U32(_) | Value::U64(_))
    }

    /// Returns true if this value is f64.
    pub fn is_double(&self) -> bool {
        matches!(self, Value::F64(_))
    }

    /// Returns the number of elements in array or object.
    ///
    /// Returns `None` on none-array/non-object values.
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
    ///
    /// Returns `None` on non-opaque values.
    pub fn field_type(&self) -> Option<ColumnType> {
        match self {
            Value::Opaque(OpaqueValue { value_type, .. }) => Some(**value_type),
            _ => None,
        }
    }

    /// Parse this value to a structured representation.
    pub fn parse(self) -> io::Result<JsonDom> {
        match self {
            Value::Null => Ok(JsonDom::Scalar(JsonScalar::Null)),
            Value::Bool(value) => Ok(JsonDom::Scalar(JsonScalar::Boolean(value))),
            Value::I16(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Int(
                x as i64,
            )))),
            Value::U16(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Uint(
                x as u64,
            )))),
            Value::I32(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Int(
                x as i64,
            )))),
            Value::U32(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Uint(
                x as u64,
            )))),
            Value::I64(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Int(x)))),
            Value::U64(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Uint(x)))),
            Value::F64(x) => Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Double(x)))),
            Value::String(jsonb_string) => {
                let s = match jsonb_string.into_raw() {
                    Cow::Borrowed(x) => Cow::Borrowed(
                        std::str::from_utf8(x)
                            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
                    ),
                    Cow::Owned(x) => Cow::Owned(
                        String::from_utf8(x)
                            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
                    ),
                };
                Ok(JsonDom::Scalar(JsonScalar::String(s.into_owned())))
            }
            Value::SmallArray(complex_value) => {
                let mut elements = Vec::with_capacity(complex_value.element_count() as usize);
                for i in 0.. {
                    if let Some(value) = complex_value.elem_at(i)? {
                        let y = value.parse()?;
                        elements.push(y);
                    } else {
                        break;
                    }
                }
                Ok(JsonDom::Container(JsonContainer::Array(elements)))
            }
            Value::LargeArray(complex_value) => {
                let mut elements = Vec::with_capacity(complex_value.element_count() as usize);
                for value in complex_value.iter() {
                    elements.push(value?.parse()?);
                }
                Ok(JsonDom::Container(JsonContainer::Array(elements)))
            }
            Value::SmallObject(complex_value) => {
                let mut elements = BTreeMap::new();
                for value in complex_value.iter() {
                    let (key, value) = value?;
                    elements.insert(key.value().into_owned(), value.parse()?);
                }
                Ok(JsonDom::Container(JsonContainer::Object(elements)))
            }
            Value::LargeObject(complex_value) => {
                let mut elements = BTreeMap::new();
                for value in complex_value.iter() {
                    let (key, value) = value?;
                    elements.insert(key.value().into_owned(), value.parse()?);
                }
                Ok(JsonDom::Container(JsonContainer::Object(elements)))
            }
            Value::Opaque(opaque_value) => match opaque_value.value_type() {
                ColumnType::MYSQL_TYPE_NEWDECIMAL => {
                    let data = opaque_value.data_raw();

                    Ok(JsonDom::Scalar(JsonScalar::Number(JsonNumber::Decimal(
                        Decimal::read_packed(data, false)?,
                    ))))
                }
                ColumnType::MYSQL_TYPE_DATE => {
                    let packed_value =
                        opaque_value.data_raw().first_chunk::<8>().ok_or_else(|| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "not enough data to decode MYSQL_TYPE_DATE",
                            )
                        })?;
                    let packed_value = i64::from_le_bytes(*packed_value);
                    Ok(JsonDom::Scalar(JsonScalar::DateTime(
                        MysqlTime::from_int64_date_packed(packed_value),
                    )))
                }
                ColumnType::MYSQL_TYPE_TIME => {
                    let packed_value = dbg!(opaque_value.data_raw())
                        .first_chunk::<8>()
                        .ok_or_else(|| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "not enough data to decode MYSQL_TYPE_TIME",
                            )
                        })?;
                    let packed_value = dbg!(i64::from_le_bytes(*packed_value));
                    Ok(JsonDom::Scalar(JsonScalar::DateTime(
                        MysqlTime::from_int64_time_packed(packed_value),
                    )))
                }
                ColumnType::MYSQL_TYPE_DATETIME | ColumnType::MYSQL_TYPE_TIMESTAMP => {
                    let packed_value =
                        opaque_value.data_raw().first_chunk::<8>().ok_or_else(|| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "not enough data to decode MYSQL_TYPE_DATETIME",
                            )
                        })?;
                    let packed_value = i64::from_le_bytes(*packed_value);
                    Ok(JsonDom::Scalar(JsonScalar::DateTime(
                        MysqlTime::from_int64_datetime_packed(packed_value),
                    )))
                }

                field_type => Ok(JsonDom::Scalar(JsonScalar::Opaque(JsonOpaque {
                    field_type,
                    value: opaque_value.data.0.into_owned(),
                }))),
            },
        }
    }
}

impl<'a> TryFrom<Value<'a>> for serde_json::Value {
    type Error = JsonbToJsonError;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(serde_json::Value::Null),
            Value::Bool(x) => Ok(serde_json::Value::Bool(x)),
            Value::I16(x) => Ok(x.into()),
            Value::U16(x) => Ok(x.into()),
            Value::I32(x) => Ok(x.into()),
            Value::U32(x) => Ok(x.into()),
            Value::I64(x) => Ok(x.into()),
            Value::U64(x) => Ok(x.into()),
            Value::F64(x) => Ok(x.into()),
            Value::String(s) => Ok(from_utf8(s.str_raw())?.into()),
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
    const SIZE: Option<usize> = None;
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

/// JSONB storage format for objects and arrays. See [`ComplexType`].
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
