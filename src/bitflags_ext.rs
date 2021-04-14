// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

/// Trait for types generated using `bitflags::bitflags!` macro.
pub trait Bitflags: Copy {
    type Repr: Copy + num_traits::PrimInt;

    fn empty() -> Self;
    fn all() -> Self;
    fn bits(&self) -> Self::Repr;
    fn from_bits(bits: Self::Repr) -> Option<Self>;
    fn from_bits_truncate(bits: Self::Repr) -> Self;
    /// # Safety
    ///
    /// Safety requirements are defined by the [`bitflags!`] macro.
    unsafe fn from_bits_unchecked(bits: Self::Repr) -> Self;
    fn is_empty(&self) -> bool;
    fn is_all(&self) -> bool;
    fn intersects(&self, other: Self) -> bool;
    fn contains(&self, other: Self) -> bool;
    fn insert(&mut self, other: Self);
    fn remove(&mut self, other: Self);
    fn toggle(&mut self, other: Self);
    fn set(&mut self, other: Self, value: bool);
}

/// It's a wrapper for `bitflags::bitflags!` macro that also implements the `Bitflags` trait.
macro_rules! my_bitflags {
    ($name:ident, $(#[$em:meta])+ $err:ident, $ty:path, $($def:tt)*) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, thiserror::Error)]
        $(#[$em])+
        pub struct $err(pub $ty);

        impl TryFrom<$ty> for $name {
            type Error = $err;

            fn try_from(val: $ty) -> std::result::Result<$name, $err> {
                $name::from_bits(val).ok_or_else(|| $err(val))
            }
        }

        impl From<$name> for $ty {
            fn from(val: $name) -> $ty {
                val.bits()
            }
        }

        impl $crate::bitflags_ext::Bitflags for $name {
            type Repr = $ty;

            #[inline]
            fn empty() -> Self {
                $name::empty()
            }

            #[inline]
            fn all() -> Self {
                $name::all()
            }

            #[inline]
            fn bits(&self) -> Self::Repr {
                $name::bits(&self)
            }

            #[inline]
            fn from_bits(bits: Self::Repr) -> Option<Self> {
                $name::from_bits(bits)
            }

            #[inline]
            fn from_bits_truncate(bits: Self::Repr) -> Self {
                $name::from_bits_truncate(bits)
            }

            #[inline]
            unsafe fn from_bits_unchecked(bits: Self::Repr) -> Self {
                $name::from_bits_unchecked(bits)
            }

            #[inline]
            fn is_empty(&self) -> bool {
                $name::is_empty(self)
            }

            #[inline]
            fn is_all(&self) -> bool {
                $name::is_all(self)
            }

            #[inline]
            fn intersects(&self, other: Self) -> bool {
                $name::intersects(self, other)
            }

            #[inline]
            fn contains(&self, other: Self) -> bool {
                $name::contains(self, other)
            }

            #[inline]
            fn insert(&mut self, other: Self) {
                $name::insert(self, other)
            }
            #[inline]
            fn remove(&mut self, other: Self) {
                $name::remove(self, other)
            }
            #[inline]
            fn toggle(&mut self, other: Self) {
                $name::toggle(self, other)
            }
            #[inline]
            fn set(&mut self, other: Self, value: bool) {
                $name::set(self, other, value)
            }
        }

        impl Default for $name {
            fn default() -> $name {
                $name::empty()
            }
        }

        bitflags::bitflags! { $($def)* }
    };
}
