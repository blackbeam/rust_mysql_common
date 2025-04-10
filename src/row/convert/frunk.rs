// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! `FromRow` implementation for `frunk::HList`.

#![cfg(feature = "frunk")]

use std::sync::Arc;

pub use frunk::{HCons, HNil};
use frunk::{hlist::h_cons, prelude::HList};

use super::{FromRow, FromRowError};
use crate::{
    packets::Column,
    row::{Row, new_row_raw},
    value::{
        Value,
        convert::{FromValue, FromValueError},
    },
};

#[cfg_attr(docsrs, doc(cfg(feature = "frunk")))]
impl FromRow for HNil {
    fn from_row_opt(row: Row) -> Result<Self, FromRowError>
    where
        Self: Sized,
    {
        if !row.is_empty() {
            return Err(FromRowError(row));
        }
        Ok(HNil)
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "frunk")))]
impl<H, T> FromRow for HCons<H, T>
where
    H: FromValue,
    H::Intermediate: Into<Value>,
    T: FromRow,
    T: HList,
{
    fn from_row_opt(row: Row) -> Result<Self, FromRowError>
    where
        Self: Sized,
    {
        if row.len() != Self::LEN {
            return Err(FromRowError(row));
        }

        let columns = row.columns();
        let mut values = row.unwrap_raw();

        debug_assert_eq!(values.len(), Self::LEN);

        let Some(head) = values[0].take() else {
            return Err(FromRowError(new_row_raw(values, columns)));
        };

        let ir = match H::get_intermediate(head) {
            Ok(ir) => ir,
            Err(FromValueError(value)) => {
                values[0] = Some(value);
                return Err(FromRowError(new_row_raw(values, columns)));
            }
        };

        // TODO: Avoid cloning columns here by using something like `owning_ref::ArcRef`
        let columns_tail: Arc<[Column]> =
            columns.iter().skip(1).cloned().collect::<Vec<_>>().into();
        values.remove(0);

        let tail = match T::from_row_opt(new_row_raw(values, columns_tail)) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                let mut values = row.unwrap_raw();
                values.insert(0, Some(ir.into()));
                return Err(FromRowError(new_row_raw(values, columns)));
            }
        };

        Ok(h_cons(Into::<H>::into(ir), tail))
    }
}

#[test]
fn hlist_from_row() {
    use crate::{
        constants::ColumnType::{MYSQL_TYPE_LONG, MYSQL_TYPE_VARCHAR},
        packets::Column,
        row::{convert::from_row, new_row},
        value::Value::*,
    };

    let row = new_row(
        vec![
            Int(0),
            Bytes(vec![b'0']),
            Int(1),
            Bytes(vec![b'1']),
            Int(2),
            Bytes(vec![b'2']),
            Int(3),
            Bytes(vec![b'3']),
            Int(4),
            Bytes(vec![b'4']),
            Int(5),
            Bytes(vec![b'5']),
            Int(6),
            Bytes(vec![b'6']),
            Int(7),
            Bytes(vec![b'7']),
        ],
        vec![
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
            Column::new(MYSQL_TYPE_LONG),
            Column::new(MYSQL_TYPE_VARCHAR),
        ]
        .into_boxed_slice()
        .into(),
    );

    let val: frunk::HList![
        u8,
        Vec<u8>,
        u16,
        String,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8,
        u8
    ] = from_row(row);

    assert_eq!(
        val.into_tuple2(),
        (
            0,
            (
                vec![b'0'],
                (
                    1,
                    (
                        String::from("1"),
                        (2, (2, (3, (3, (4, (4, (5, (5, (6, (6, (7, 7)))))))))))
                    )
                )
            )
        )
    );
}
