// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use row::Row;
use std::error::Error;
use std::fmt;
use value::convert::{ConvIr, FromValue, FromValueError};

/// `FromRow` conversion error.
#[derive(Debug)]
pub struct FromRowError(pub Row);

impl fmt::Display for FromRowError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn from_row(row: Row) -> Self;
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type T. (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1,). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, T2). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, T2, T3). (see FromRow documentation)",
                    row
                )
            }
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

impl<T1, Ir1,
    T2, Ir2,
    T3, Ir3,
    T4, Ir4,> FromRow for (T1, T2, T3, T4)
    where Ir1: ConvIr<T1>, T1: FromValue<Intermediate=Ir1>,
          Ir2: ConvIr<T2>, T2: FromValue<Intermediate=Ir2>,
          Ir3: ConvIr<T3>, T3: FromValue<Intermediate=Ir3>,
          Ir4: ConvIr<T4>, T4: FromValue<Intermediate=Ir4>, {
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!("Couldn't convert {:?} to type (T1, .., T4). (see FromRow documentation)",
                       row)
            }
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
        Ok((
            ir1.commit(),
            ir2.commit(),
            ir3.commit(),
            ir4.commit(),
        ))
    }
}

impl<T1, Ir1,
    T2, Ir2,
    T3, Ir3,
    T4, Ir4,
    T5, Ir5,> FromRow for (T1, T2, T3, T4, T5)
    where Ir1: ConvIr<T1>, T1: FromValue<Intermediate=Ir1>,
          Ir2: ConvIr<T2>, T2: FromValue<Intermediate=Ir2>,
          Ir3: ConvIr<T3>, T3: FromValue<Intermediate=Ir3>,
          Ir4: ConvIr<T4>, T4: FromValue<Intermediate=Ir4>,
          Ir5: ConvIr<T5>, T5: FromValue<Intermediate=Ir5>, {
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!("Couldn't convert {:?} to type (T1, .., T5). (see FromRow documentation)",
                       row)
            }
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

impl<T1, Ir1,
    T2, Ir2,
    T3, Ir3,
    T4, Ir4,
    T5, Ir5,
    T6, Ir6,> FromRow for (T1, T2, T3, T4, T5, T6)
    where Ir1: ConvIr<T1>, T1: FromValue<Intermediate=Ir1>,
          Ir2: ConvIr<T2>, T2: FromValue<Intermediate=Ir2>,
          Ir3: ConvIr<T3>, T3: FromValue<Intermediate=Ir3>,
          Ir4: ConvIr<T4>, T4: FromValue<Intermediate=Ir4>,
          Ir5: ConvIr<T5>, T5: FromValue<Intermediate=Ir5>,
          Ir6: ConvIr<T6>, T6: FromValue<Intermediate=Ir6>, {
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!("Couldn't convert {:?} to type (T1, .., T6). (see FromRow documentation)",
                       row)
            }
        }
    }
    fn from_row_opt(mut row: Row) ->
    Result<(T1, T2, T3, T4, T5, T6), FromRowError>
    {
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T7). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T8). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T9). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T10). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T11). (see FromRow documentation)",
                    row
                )
            }
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
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T12). (see FromRow documentation)",
                    row
                )
            }
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
    T13,
    Ir13,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T13). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), FromRowError> {
        if row.len() != 13 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
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
            ir13.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T14). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), FromRowError> {
        if row.len() != 14 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
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
            ir13.commit(),
            ir14.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T15). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), FromRowError> {
        if row.len() != 15 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T16). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), FromRowError> {
        if row.len() != 16 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T17). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), FromRowError> {
        if row.len() != 17 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T18). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), FromRowError> {
        if row.len() != 18 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T19). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), FromRowError> {
        if row.len() != 19 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T20). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), FromRowError> {
        if row.len() != 20 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T21). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), FromRowError> {
        if row.len() != 21 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T22). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), FromRowError> {
        if row.len() != 22 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T23). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23), FromRowError> {
        if row.len() != 23 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T24). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24), FromRowError> {
        if row.len() != 24 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T25). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25), FromRowError> {
        if row.len() != 25 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T26). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26), FromRowError> {
        if row.len() != 26 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
    T27,
    Ir27,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
    Ir27: ConvIr<T27>,
    T27: FromValue<Intermediate = Ir27>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T27). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27), FromRowError> {
        if row.len() != 27 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
        );
        let ir27 = take_or_place!(
            row,
            26,
            T27,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [25, ir26]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
            ir27.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
    T27,
    Ir27,
    T28,
    Ir28,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
    Ir27: ConvIr<T27>,
    T27: FromValue<Intermediate = Ir27>,
    Ir28: ConvIr<T28>,
    T28: FromValue<Intermediate = Ir28>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T28). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28), FromRowError> {
        if row.len() != 28 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
        );
        let ir27 = take_or_place!(
            row,
            26,
            T27,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [25, ir26]
        );
        let ir28 = take_or_place!(
            row,
            27,
            T28,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [26, ir27]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
            ir27.commit(),
            ir28.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
    T27,
    Ir27,
    T28,
    Ir28,
    T29,
    Ir29,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
    Ir27: ConvIr<T27>,
    T27: FromValue<Intermediate = Ir27>,
    Ir28: ConvIr<T28>,
    T28: FromValue<Intermediate = Ir28>,
    Ir29: ConvIr<T29>,
    T29: FromValue<Intermediate = Ir29>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T29). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29), FromRowError> {
        if row.len() != 29 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
        );
        let ir27 = take_or_place!(
            row,
            26,
            T27,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [25, ir26]
        );
        let ir28 = take_or_place!(
            row,
            27,
            T28,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [26, ir27]
        );
        let ir29 = take_or_place!(
            row,
            28,
            T29,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [27, ir28]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
            ir27.commit(),
            ir28.commit(),
            ir29.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
    T27,
    Ir27,
    T28,
    Ir28,
    T29,
    Ir29,
    T30,
    Ir30,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
    Ir27: ConvIr<T27>,
    T27: FromValue<Intermediate = Ir27>,
    Ir28: ConvIr<T28>,
    T28: FromValue<Intermediate = Ir28>,
    Ir29: ConvIr<T29>,
    T29: FromValue<Intermediate = Ir29>,
    Ir30: ConvIr<T30>,
    T30: FromValue<Intermediate = Ir30>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T30). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30), FromRowError> {
        if row.len() != 30 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
        );
        let ir27 = take_or_place!(
            row,
            26,
            T27,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [25, ir26]
        );
        let ir28 = take_or_place!(
            row,
            27,
            T28,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [26, ir27]
        );
        let ir29 = take_or_place!(
            row,
            28,
            T29,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [27, ir28]
        );
        let ir30 = take_or_place!(
            row,
            29,
            T30,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [26, ir27],
            [28, ir29]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
            ir27.commit(),
            ir28.commit(),
            ir29.commit(),
            ir30.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
    T27,
    Ir27,
    T28,
    Ir28,
    T29,
    Ir29,
    T30,
    Ir30,
    T31,
    Ir31,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
    Ir27: ConvIr<T27>,
    T27: FromValue<Intermediate = Ir27>,
    Ir28: ConvIr<T28>,
    T28: FromValue<Intermediate = Ir28>,
    Ir29: ConvIr<T29>,
    T29: FromValue<Intermediate = Ir29>,
    Ir30: ConvIr<T30>,
    T30: FromValue<Intermediate = Ir30>,
    Ir31: ConvIr<T31>,
    T31: FromValue<Intermediate = Ir31>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T31). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31), FromRowError> {
        if row.len() != 31 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
        );
        let ir27 = take_or_place!(
            row,
            26,
            T27,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [25, ir26]
        );
        let ir28 = take_or_place!(
            row,
            27,
            T28,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [26, ir27]
        );
        let ir29 = take_or_place!(
            row,
            28,
            T29,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [27, ir28]
        );
        let ir30 = take_or_place!(
            row,
            29,
            T30,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [26, ir27],
            [28, ir29]
        );
        let ir31 = take_or_place!(
            row,
            30,
            T31,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [26, ir27],
            [27, ir28],
            [29, ir30]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
            ir27.commit(),
            ir28.commit(),
            ir29.commit(),
            ir30.commit(),
            ir31.commit(),
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
    T13,
    Ir13,
    T14,
    Ir14,
    T15,
    Ir15,
    T16,
    Ir16,
    T17,
    Ir17,
    T18,
    Ir18,
    T19,
    Ir19,
    T20,
    Ir20,
    T21,
    Ir21,
    T22,
    Ir22,
    T23,
    Ir23,
    T24,
    Ir24,
    T25,
    Ir25,
    T26,
    Ir26,
    T27,
    Ir27,
    T28,
    Ir28,
    T29,
    Ir29,
    T30,
    Ir30,
    T31,
    Ir31,
    T32,
    Ir32,
> FromRow for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31, T32)
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
    Ir13: ConvIr<T13>,
    T13: FromValue<Intermediate = Ir13>,
    Ir14: ConvIr<T14>,
    T14: FromValue<Intermediate = Ir14>,
    Ir15: ConvIr<T15>,
    T15: FromValue<Intermediate = Ir15>,
    Ir16: ConvIr<T16>,
    T16: FromValue<Intermediate = Ir16>,
    Ir17: ConvIr<T17>,
    T17: FromValue<Intermediate = Ir17>,
    Ir18: ConvIr<T18>,
    T18: FromValue<Intermediate = Ir18>,
    Ir19: ConvIr<T19>,
    T19: FromValue<Intermediate = Ir19>,
    Ir20: ConvIr<T20>,
    T20: FromValue<Intermediate = Ir20>,
    Ir21: ConvIr<T21>,
    T21: FromValue<Intermediate = Ir21>,
    Ir22: ConvIr<T22>,
    T22: FromValue<Intermediate = Ir22>,
    Ir23: ConvIr<T23>,
    T23: FromValue<Intermediate = Ir23>,
    Ir24: ConvIr<T24>,
    T24: FromValue<Intermediate = Ir24>,
    Ir25: ConvIr<T25>,
    T25: FromValue<Intermediate = Ir25>,
    Ir26: ConvIr<T26>,
    T26: FromValue<Intermediate = Ir26>,
    Ir27: ConvIr<T27>,
    T27: FromValue<Intermediate = Ir27>,
    Ir28: ConvIr<T28>,
    T28: FromValue<Intermediate = Ir28>,
    Ir29: ConvIr<T29>,
    T29: FromValue<Intermediate = Ir29>,
    Ir30: ConvIr<T30>,
    T30: FromValue<Intermediate = Ir30>,
    Ir31: ConvIr<T31>,
    T31: FromValue<Intermediate = Ir31>,
    Ir32: ConvIr<T32>,
    T32: FromValue<Intermediate = Ir32>,
{
    #[inline]
    fn from_row(row: Row) -> (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31, T32) {
        match FromRow::from_row_opt(row) {
            Ok(x) => x,
            Err(FromRowError(row)) => {
                panic!(
                    "Couldn't convert {:?} to type (T1, .., T32). (see FromRow documentation)",
                    row
                )
            }
        }
    }
    fn from_row_opt(
        mut row: Row,
    ) -> Result<(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31, T32), FromRowError> {
        if row.len() != 32 {
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
            [10, ir11]
        );
        let ir13 = take_or_place!(
            row,
            12,
            T13,
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
            [11, ir12]
        );
        let ir14 = take_or_place!(
            row,
            13,
            T14,
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
            [10, ir11],
            [12, ir13]
        );
        let ir15 = take_or_place!(
            row,
            14,
            T15,
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
            [10, ir11],
            [11, ir12],
            [13, ir14]
        );
        let ir16 = take_or_place!(
            row,
            15,
            T16,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [14, ir15]
        );
        let ir17 = take_or_place!(
            row,
            16,
            T17,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [15, ir16]
        );
        let ir18 = take_or_place!(
            row,
            17,
            T18,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [16, ir17]
        );
        let ir19 = take_or_place!(
            row,
            18,
            T19,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [17, ir18]
        );
        let ir20 = take_or_place!(
            row,
            19,
            T20,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [18, ir19]
        );
        let ir21 = take_or_place!(
            row,
            20,
            T21,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [19, ir20]
        );
        let ir22 = take_or_place!(
            row,
            21,
            T22,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [20, ir21]
        );
        let ir23 = take_or_place!(
            row,
            22,
            T23,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [21, ir22]
        );
        let ir24 = take_or_place!(
            row,
            23,
            T24,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [22, ir23]
        );
        let ir25 = take_or_place!(
            row,
            24,
            T25,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [23, ir24]
        );
        let ir26 = take_or_place!(
            row,
            25,
            T26,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [24, ir25]
        );
        let ir27 = take_or_place!(
            row,
            26,
            T27,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [25, ir26]
        );
        let ir28 = take_or_place!(
            row,
            27,
            T28,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [26, ir27]
        );
        let ir29 = take_or_place!(
            row,
            28,
            T29,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [27, ir28]
        );
        let ir30 = take_or_place!(
            row,
            29,
            T30,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [26, ir27],
            [28, ir29]
        );
        let ir31 = take_or_place!(
            row,
            30,
            T31,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [26, ir27],
            [27, ir28],
            [29, ir30]
        );
        let ir32 = take_or_place!(
            row,
            31,
            T32,
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
            [10, ir11],
            [11, ir12],
            [12, ir13],
            [13, ir14],
            [14, ir15],
            [15, ir16],
            [16, ir17],
            [17, ir18],
            [18, ir19],
            [19, ir20],
            [20, ir21],
            [21, ir22],
            [22, ir23],
            [23, ir24],
            [24, ir25],
            [25, ir26],
            [26, ir27],
            [27, ir28],
            [28, ir29],
            [30, ir31]
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
            ir13.commit(),
            ir14.commit(),
            ir15.commit(),
            ir16.commit(),
            ir17.commit(),
            ir18.commit(),
            ir19.commit(),
            ir20.commit(),
            ir21.commit(),
            ir22.commit(),
            ir23.commit(),
            ir24.commit(),
            ir25.commit(),
            ir26.commit(),
            ir27.commit(),
            ir28.commit(),
            ir29.commit(),
            ir30.commit(),
            ir31.commit(),
            ir32.commit(),
        ))
    }
}
