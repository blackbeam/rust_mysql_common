// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{convert::TryFrom, fmt, io, sync::Arc};

use bitvec::{order::Lsb0, prelude::BitVec, slice::BitSlice};

use crate::{
    constants::{ColumnFlags, ColumnType},
    io::ParseBuf,
    misc::raw::int::*,
    packets::Column,
    proto::MyDeserialize,
    value::Value,
};

use super::{
    events::{OptionalMetadataField, TableMapEvent},
    value::BinlogValue,
};

/// Bonlog rows event row value options.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
#[repr(u64)]
pub enum BinlogRowValueOptions {
    /// Store JSON updates in partial form
    PARTIAL_JSON_UPDATES = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
#[error("Unknown binlog version {}", _0)]
#[repr(transparent)]
pub struct UnknownBinlogRowValueOptions(pub u64);

impl From<UnknownBinlogRowValueOptions> for u64 {
    fn from(x: UnknownBinlogRowValueOptions) -> Self {
        x.0
    }
}

impl TryFrom<u64> for BinlogRowValueOptions {
    type Error = UnknownBinlogRowValueOptions;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::PARTIAL_JSON_UPDATES),
            x => Err(UnknownBinlogRowValueOptions(x)),
        }
    }
}

/// Representation of a binlog row.
#[derive(Clone, PartialEq)]
pub struct BinlogRow {
    values: Vec<Option<BinlogValue<'static>>>,
    columns: Arc<[Column]>,
}

impl BinlogRow {
    pub fn new(values: Vec<Option<BinlogValue<'static>>>, columns: Arc<[Column]>) -> Self {
        Self { values, columns }
    }

    /// Returns length of a row.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns true if the row has a length of 0.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Returns columns of this row.
    pub fn columns_ref(&self) -> &[Column] {
        &*self.columns
    }

    /// Returns columns of this row.
    pub fn columns(&self) -> Arc<[Column]> {
        self.columns.clone()
    }

    /// Returns reference to the value of a column with index `index` if it exists and wasn't taken
    /// by `Row::take` method.
    ///
    /// Non panicking version of `row[usize]`.
    pub fn as_ref(&self, index: usize) -> Option<&BinlogValue> {
        self.values.get(index).and_then(|x| x.as_ref())
    }

    /// Will take value of a column with index `index` if it exists and wasn't taken earlier then
    /// will converts it to `T`.
    pub fn take(&mut self, index: usize) -> Option<BinlogValue> {
        self.values.get_mut(index).and_then(|x| x.take())
    }

    /// Unwraps values of a row.
    ///
    /// # Panics
    ///
    /// Panics if any of columns was taken by `take` method.
    pub fn unwrap(self) -> Vec<BinlogValue<'static>> {
        self.values
            .into_iter()
            .map(|x| x.expect("Can't unwrap row if some of columns was taken"))
            .collect()
    }

    #[doc(hidden)]
    pub fn place(&mut self, index: usize, value: BinlogValue<'static>) {
        self.values[index] = Some(value);
    }
}

impl<'de> MyDeserialize<'de> for BinlogRow {
    const SIZE: Option<usize> = None;
    /// Content:
    ///
    /// * number of columns
    /// * column bitmap - bit is set if column is in the row
    /// * have shared image - `true` means, that this is a partial event
    ///   and this is an after image row. Therefore we need to parse a shared image
    /// * corresponding table map event
    type Ctx = (u64, &'de BitSlice<Lsb0, u8>, bool, &'de TableMapEvent<'de>);

    fn deserialize(
        (num_columns, cols, have_shared_image, table_info): Self::Ctx,
        buf: &mut ParseBuf<'de>,
    ) -> io::Result<Self> {
        let mut values: Vec<Option<BinlogValue<'static>>> = vec![];
        let mut columns = vec![];

        // read a shared image if needed (see WL#2955)
        let mut partial_cols = if have_shared_image {
            let value_options = *buf.parse::<RawInt<LenEnc>>(())?;
            if value_options & BinlogRowValueOptions::PARTIAL_JSON_UPDATES as u64 > 0 {
                let json_columns_count = table_info.json_column_count();
                let partial_columns_len = (json_columns_count + 7) / 8;
                let partial_columns: &[u8] = buf.parse(partial_columns_len)?;
                let partial_columns = BitSlice::<Lsb0, u8>::from_slice(partial_columns)
                    .expect("suspiciously large slice");
                Some(partial_columns.into_iter().take(json_columns_count))
            } else {
                None
            }
        } else {
            None
        };

        let num_bits = cols.count_ones();
        let bitmap_len = (num_bits + 7) / 8;
        let bitmap_buf: &[u8] = buf.parse(bitmap_len)?;
        let mut null_bitmap = BitVec::<Lsb0, u8>::from_slice(bitmap_buf).expect("should not fail");
        null_bitmap.truncate(num_bits);

        let mut image_idx = 0;

        let signedness = table_info.iter_optional_meta().find_map(|m| {
            m.map(|f| match f {
                OptionalMetadataField::Signedness(bit_slice) => Some(bit_slice),
                _ => None,
            })
            .unwrap_or(None)
        });

        let mut numeric_index = 0;
        for i in 0..(num_columns as usize) {
            // check if column is in columns list
            if cols.get(i).as_deref().copied().unwrap_or(false) {
                let raw_column_type = table_info.get_column_type(i);

                // TableMapEvent must define column type for the current column.
                let raw_column_type = match raw_column_type {
                    Ok(Some(ty)) => ty,
                    Ok(None) => {
                        return Err(io::Error::new(io::ErrorKind::InvalidData, "No column type"))
                    }
                    Err(_) => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "Unknown column type",
                        ))
                    }
                };

                let column_meta = table_info.get_column_metadata(i).unwrap_or(&[]);
                let column_type = match raw_column_type {
                    ColumnType::MYSQL_TYPE_STRING => {
                        let real_type = column_meta[0];
                        if real_type == ColumnType::MYSQL_TYPE_ENUM as u8
                            || real_type == ColumnType::MYSQL_TYPE_SET as u8
                        {
                            ColumnType::try_from(real_type).unwrap_or(raw_column_type)
                        } else {
                            raw_column_type
                        }
                    }
                    ColumnType::MYSQL_TYPE_DATE => ColumnType::MYSQL_TYPE_NEWDATE,
                    other => other,
                };

                let is_partial = raw_column_type == ColumnType::MYSQL_TYPE_JSON
                    && partial_cols
                        .as_mut()
                        .and_then(|bits| bits.next().as_deref().copied())
                        .unwrap_or(false);

                let is_unsigned = if column_type.is_numeric_type() {
                    let is_unsigned = signedness
                        .as_ref()
                        .and_then(|bits| bits.get(numeric_index).as_deref().copied())
                        .unwrap_or_default();
                    numeric_index += 1;
                    is_unsigned
                } else {
                    false
                };

                let mut column_flags = ColumnFlags::empty();
                if is_unsigned {
                    column_flags |= ColumnFlags::UNSIGNED_FLAG;
                }
                let column = Column::new(column_type)
                    // column name – `@<i>` where i is a column offset in a table
                    .with_name(format!("@{}", i).as_bytes())
                    .with_flags(column_flags)
                    .with_schema(table_info.database_name_raw())
                    .with_org_table(table_info.table_name_raw())
                    .with_table(table_info.table_name_raw());
                columns.push(column);

                // check if column is null
                if null_bitmap
                    .get(image_idx)
                    .as_deref()
                    .copied()
                    .unwrap_or(true)
                {
                    values.push(Some(BinlogValue::Value(Value::NULL)));
                } else {
                    let ctx = (column_type, column_meta, is_unsigned, is_partial);
                    values.push(Some(buf.parse::<BinlogValue>(ctx)?.into_owned()));
                }

                image_idx += 1;
            }
        }

        Ok(BinlogRow::new(values, columns.into_boxed_slice().into()))
    }
}

impl fmt::Debug for BinlogRow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = f.debug_struct("BinlogRow");
        for (val, column) in self.values.iter().zip(self.columns.iter()) {
            match *val {
                Some(ref val) => {
                    debug.field(column.name_str().as_ref(), val);
                }
                None => {
                    debug.field(column.name_str().as_ref(), &"<taken>");
                }
            }
        }
        debug.finish()
    }
}
