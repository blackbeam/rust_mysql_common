// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::row::Row;
use crate::value::convert::{ConvIr, FromValue, FromValueError};
use std::error::Error;
use std::fmt;

/// `FromRow` conversion error.
#[derive(Debug)]
pub struct FromRowError(pub Row);

impl fmt::Display for FromRowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Couldn't convert the row `{:?}` to a desired type",
            self.0
        )
    }
}

impl Error for FromRowError {
    fn description(&self) -> &str {
        "Couldn't convert the row to a desired type"
    }
}

/// Will *panic* if could not convert `row` to `T`.
#[inline]
pub fn from_row<T: FromRow>(row: Row) -> T {
    FromRow::from_row(row)
}

/// Will return `Err(row)` if could not convert `row` to `T`
#[inline]
pub fn from_row_opt<T: FromRow>(row: Row) -> Result<T, FromRowError> {
    FromRow::from_row_opt(row)
}

/// Trait to convert `Row` into a tuple of `FromValue` implementors up to arity 12.
///
/// This trait is convenient way to convert mysql row to a tuple or rust types and relies on
/// `FromValue` trait, i.e. calling `from_row::<(T, U)>(row)` is similar to calling
/// `(T::from_value(column_1), U::from_value(column_2))`.
///
/// Note that conversion will always fail if any of columns was taken using `Row::take` method.
///
/// Conversion of individual columns of a row may fail. In this case `from_row` will panic, and
/// `from_row_opt` will roll back conversion and return original row.
///
/// Concrete types of columns in a row is usually known to a programmer so `from_value` should never
/// panic if types specified correctly. This means that column which holds `NULL` should correspond
/// to a type wrapped in `Option`, `String` is used only with column which hold correct utf8, size
/// and signedness of a numeric type should match to a value stored in a column and so on.
///
/// ```ignore
/// // Consider columns in the row is: Bytes(<some binary data>), NULL and Int(1024)
/// from_row::<(String, u8, u8>(row) // this will panic because of invalid utf8 in first column.
/// from_row::<(Vec<u8>, u8, u8)>(row) // this will panic because of a NULL in second column.
/// from_row::<(Vec<u8>, Option<u8>, u8)>(row) // this will panic because 1024 does not fit in u8.
/// from_row::<(Vec<u8>)>(row) // this will panic because number of columns != arity of a tuple.
/// from_row::<(Vec<u8>, Option<u8>, u16, Option<u8>)>(row) // same reason of panic as previous.
///
/// from_row::<(Vec<u8>, Option<u8>, u16)>(row) // this'll work and return (vec![..], None, 1024u16)
/// ```
pub trait FromRow {
    #[inline]
    fn from_row(row: Row) -> Self
    where
        Self: Sized,
    {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type T. (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(row: Row) -> Result<Self, FromRowError>
    where
        Self: Sized;
}

macro_rules! take_or_place {
    ($row:expr, $index:expr, $t:ident) => (
        match $row.take($index) {
            Some(value) => {
                match $t::get_intermediate(value) {
                    Ok(ir) => ir,
                    Err(FromValueError(value)) => {
                        $row.place($index, value);
                        return Err(FromRowError($row));
                    },
                }
            },
            None => return Err(FromRowError($row)),
        }
    );
    ($row:expr, $index:expr, $t:ident, $( [$idx:expr, $ir:expr] ),*) => (
        match $row.take($index) {
            Some(value) => {
                match $t::get_intermediate(value) {
                    Ok(ir) => ir,
                    Err(FromValueError(value)) => {
                        $($row.place($idx, $ir.rollback());)*
                        $row.place($index, value);
                        return Err(FromRowError($row));
                    },
                }
            },
            None => return Err(FromRowError($row)),
        }
    );
}

impl FromRow for Row {
    fn from_row(row: Row) -> Self {
        row
    }

    fn from_row_opt(row: Row) -> Result<Self, FromRowError>
    where
        Self: Sized,
    {
        Ok(row)
    }
}

impl<T, Ir> FromRow for T
where
    Ir: ConvIr<T>,
    T: FromValue<Intermediate = Ir>,
{
    #[inline]
    fn from_row(row: Row) -> T {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type T. (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<T, FromRowError> {
        if row.len() == 1 {
            Ok(take_or_place!(row, 0, T).commit())
        } else {
            Err(FromRowError(row))
        }
    }
}

impl<T1, Ir1> FromRow for (T1,)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
{
    #[inline]
    fn from_row(row: Row) -> (T1,) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1,). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(row: Row) -> Result<(T1,), FromRowError> {
        T1::from_row_opt(row).map(|t| (t,))
    }
}

impl<T1, Ir1, T2, Ir2> FromRow for (T1, T2)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, T2). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2), FromRowError> {
        if row.len() != 2 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        Ok((ir1.commit(), ir2.commit()))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3> FromRow for (T1, T2, T3)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, T2, T3). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3), FromRowError> {
        if row.len() != 3 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        Ok((ir1.commit(), ir2.commit(), ir3.commit()))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3, T4, Ir4> FromRow for (T1, T2, T3, T4)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T4). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3, T4), FromRowError> {
        if row.len() != 4 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        Ok((ir1.commit(), ir2.commit(), ir3.commit(), ir4.commit()))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3, T4, Ir4, T5, Ir5> FromRow for (T1, T2, T3, T4, T5)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T5). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3, T4, T5), FromRowError> {
        if row.len() != 5 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
        ))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3, T4, Ir4, T5, Ir5, T6, Ir6> FromRow for (T1, T2, T3, T4, T5, T6)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T6). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3, T4, T5, T6), FromRowError> {
        if row.len() != 6 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
        ))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3, T4, Ir4, T5, Ir5, T6, Ir6, T7, Ir7> FromRow
    for (T1, T2, T3, T4, T5, T6, T7)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
    Ir7: ConvIr<T7>,
    T7: FromValue<Intermediate = Ir7>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T7). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3, T4, T5, T6, T7), FromRowError> {
        if row.len() != 7 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        let ir7 = take_or_place!(
            row,
            6,
            T7,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6]
        );
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
            ir7.commit(),
        ))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3, T4, Ir4, T5, Ir5, T6, Ir6, T7, Ir7, T8, Ir8> FromRow
    for (T1, T2, T3, T4, T5, T6, T7, T8)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
    Ir7: ConvIr<T7>,
    T7: FromValue<Intermediate = Ir7>,
    Ir8: ConvIr<T8>,
    T8: FromValue<Intermediate = Ir8>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T8). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8), FromRowError> {
        if row.len() != 8 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        let ir7 = take_or_place!(
            row,
            6,
            T7,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6]
        );
        let ir8 = take_or_place!(
            row,
            7,
            T8,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7]
        );
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
            ir7.commit(),
            ir8.commit(),
        ))
    }
}

impl<T1, Ir1, T2, Ir2, T3, Ir3, T4, Ir4, T5, Ir5, T6, Ir6, T7, Ir7, T8, Ir8, T9, Ir9> FromRow
    for (T1, T2, T3, T4, T5, T6, T7, T8, T9)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
    Ir7: ConvIr<T7>,
    T7: FromValue<Intermediate = Ir7>,
    Ir8: ConvIr<T8>,
    T8: FromValue<Intermediate = Ir8>,
    Ir9: ConvIr<T9>,
    T9: FromValue<Intermediate = Ir9>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T9). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(mut row: Row) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9), FromRowError> {
        if row.len() != 9 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        let ir7 = take_or_place!(
            row,
            6,
            T7,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6]
        );
        let ir8 = take_or_place!(
            row,
            7,
            T8,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7]
        );
        let ir9 = take_or_place!(
            row,
            8,
            T9,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8]
        );
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
            ir7.commit(),
            ir8.commit(),
            ir9.commit(),
        ))
    }
}

impl<
        T1,
        Ir1,
        T2,
        Ir2,
        T3,
        Ir3,
        T4,
        Ir4,
        T5,
        Ir5,
        T6,
        Ir6,
        T7,
        Ir7,
        T8,
        Ir8,
        T9,
        Ir9,
        T10,
        Ir10,
    > FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
    Ir7: ConvIr<T7>,
    T7: FromValue<Intermediate = Ir7>,
    Ir8: ConvIr<T8>,
    T8: FromValue<Intermediate = Ir8>,
    Ir9: ConvIr<T9>,
    T9: FromValue<Intermediate = Ir9>,
    Ir10: ConvIr<T10>,
    T10: FromValue<Intermediate = Ir10>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T10). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), FromRowError> {
        if row.len() != 10 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        let ir7 = take_or_place!(
            row,
            6,
            T7,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6]
        );
        let ir8 = take_or_place!(
            row,
            7,
            T8,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7]
        );
        let ir9 = take_or_place!(
            row,
            8,
            T9,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8]
        );
        let ir10 = take_or_place!(
            row,
            9,
            T10,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8],
            [8, ir9]
        );
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
            ir7.commit(),
            ir8.commit(),
            ir9.commit(),
            ir10.commit(),
        ))
    }
}

impl<
        T1,
        Ir1,
        T2,
        Ir2,
        T3,
        Ir3,
        T4,
        Ir4,
        T5,
        Ir5,
        T6,
        Ir6,
        T7,
        Ir7,
        T8,
        Ir8,
        T9,
        Ir9,
        T10,
        Ir10,
        T11,
        Ir11,
    > FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
    Ir7: ConvIr<T7>,
    T7: FromValue<Intermediate = Ir7>,
    Ir8: ConvIr<T8>,
    T8: FromValue<Intermediate = Ir8>,
    Ir9: ConvIr<T9>,
    T9: FromValue<Intermediate = Ir9>,
    Ir10: ConvIr<T10>,
    T10: FromValue<Intermediate = Ir10>,
    Ir11: ConvIr<T11>,
    T11: FromValue<Intermediate = Ir11>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T11). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), FromRowError> {
        if row.len() != 11 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        let ir7 = take_or_place!(
            row,
            6,
            T7,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6]
        );
        let ir8 = take_or_place!(
            row,
            7,
            T8,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7]
        );
        let ir9 = take_or_place!(
            row,
            8,
            T9,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8]
        );
        let ir10 = take_or_place!(
            row,
            9,
            T10,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8],
            [8, ir9]
        );
        let ir11 = take_or_place!(
            row,
            10,
            T11,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8],
            [8, ir9],
            [9, ir10]
        );
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
            ir7.commit(),
            ir8.commit(),
            ir9.commit(),
            ir10.commit(),
            ir11.commit(),
        ))
    }
}

impl<
        T1,
        Ir1,
        T2,
        Ir2,
        T3,
        Ir3,
        T4,
        Ir4,
        T5,
        Ir5,
        T6,
        Ir6,
        T7,
        Ir7,
        T8,
        Ir8,
        T9,
        Ir9,
        T10,
        Ir10,
        T11,
        Ir11,
        T12,
        Ir12,
    > FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
where
    Ir1: ConvIr<T1>,
    T1: FromValue<Intermediate = Ir1>,
    Ir2: ConvIr<T2>,
    T2: FromValue<Intermediate = Ir2>,
    Ir3: ConvIr<T3>,
    T3: FromValue<Intermediate = Ir3>,
    Ir4: ConvIr<T4>,
    T4: FromValue<Intermediate = Ir4>,
    Ir5: ConvIr<T5>,
    T5: FromValue<Intermediate = Ir5>,
    Ir6: ConvIr<T6>,
    T6: FromValue<Intermediate = Ir6>,
    Ir7: ConvIr<T7>,
    T7: FromValue<Intermediate = Ir7>,
    Ir8: ConvIr<T8>,
    T8: FromValue<Intermediate = Ir8>,
    Ir9: ConvIr<T9>,
    T9: FromValue<Intermediate = Ir9>,
    Ir10: ConvIr<T10>,
    T10: FromValue<Intermediate = Ir10>,
    Ir11: ConvIr<T11>,
    T11: FromValue<Intermediate = Ir11>,
    Ir12: ConvIr<T12>,
    T12: FromValue<Intermediate = Ir12>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => panic!(
                "Couldn't convert {:?} to type (T1, .., T12). (see FromRow documentation)",
                row
            ),
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), FromRowError> {
        if row.len() != 12 {
            return Err(FromRowError(row));
        }
        let ir1 = take_or_place!(row, 0, T1);
        let ir2 = take_or_place!(row, 1, T2, [0, ir1]);
        let ir3 = take_or_place!(row, 2, T3, [0, ir1], [1, ir2]);
        let ir4 = take_or_place!(row, 3, T4, [0, ir1], [1, ir2], [2, ir3]);
        let ir5 = take_or_place!(row, 4, T5, [0, ir1], [1, ir2], [2, ir3], [3, ir4]);
        let ir6 = take_or_place!(row, 5, T6, [0, ir1], [1, ir2], [2, ir3], [3, ir4], [4, ir5]);
        let ir7 = take_or_place!(
            row,
            6,
            T7,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6]
        );
        let ir8 = take_or_place!(
            row,
            7,
            T8,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7]
        );
        let ir9 = take_or_place!(
            row,
            8,
            T9,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8]
        );
        let ir10 = take_or_place!(
            row,
            9,
            T10,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8],
            [8, ir9]
        );
        let ir11 = take_or_place!(
            row,
            10,
            T11,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8],
            [8, ir9],
            [9, ir10]
        );
        let ir12 = take_or_place!(
            row,
            11,
            T12,
            [0, ir1],
            [1, ir2],
            [2, ir3],
            [3, ir4],
            [4, ir5],
            [5, ir6],
            [6, ir7],
            [7, ir8],
            [8, ir9],
            [9, ir10],
            [10, ir11]
        );
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
            ir5.commit(),
            ir6.commit(),
            ir7.commit(),
            ir8.commit(),
            ir9.commit(),
            ir10.commit(),
            ir11.commit(),
            ir12.commit(),
        ))
    }
}
