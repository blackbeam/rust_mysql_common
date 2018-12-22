// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::value::convert::ToValue;
use crate::value::Value;
use smallvec::SmallVec;
use std::collections::hash_map::Entry::Occupied;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::hash::BuildHasherDefault;
use twox_hash::XxHash;

/// `FromValue` conversion error.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MissingNamedParameterError(pub String);

impl fmt::Display for MissingNamedParameterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Missing named parameter `{}` for statement", self.0)
    }
}

impl Error for MissingNamedParameterError {
    fn description(&self) -> &str {
        "Missing named parameter for statement"
    }
}

/// Representations of parameters of a prepared statement.
#[derive(PartialEq, Clone, Debug)]
pub enum Params {
    Empty,
    Named(HashMap<String, Value, BuildHasherDefault<XxHash>>),
    Positional(SmallVec<[Value; 12]>),
}

impl Params {
    /// Will convert named parameters into positional assuming order passed in `named_params`
    /// attribute.
    pub fn into_positional(
        self,
        named_params: &Vec<String>,
    ) -> Result<Params, MissingNamedParameterError> {
        match self {
            Params::Named(mut map) => {
                let mut params: SmallVec<[Value; 12]> = SmallVec::new();
                'params: for (i, name) in named_params.clone().into_iter().enumerate() {
                    match map.entry(name.clone()) {
                        Occupied(entry) => {
                            let mut x = named_params.len() - 1;
                            while x > i {
                                if name == named_params[x] {
                                    params.push(entry.get().clone());
                                    continue 'params;
                                }
                                x -= 1;
                            }
                            params.push(entry.remove());
                        }
                        _ => return Err(MissingNamedParameterError(name.clone())),
                    }
                }
                Ok(Params::Positional(params))
            }
            params => Ok(params),
        }
    }
}

impl<'a, T: Into<Params> + Clone> From<&'a T> for Params {
    fn from(x: &'a T) -> Params {
        x.clone().into()
    }
}

impl<T> From<Vec<T>> for Params
where
    Value: From<T>,
{
    fn from(x: Vec<T>) -> Params {
        let mut raw_params: SmallVec<[Value; 12]> = SmallVec::new();
        for v in x.into_iter() {
            raw_params.push(Value::from(v));
        }
        if raw_params.len() == 0 {
            Params::Empty
        } else {
            Params::Positional(raw_params)
        }
    }
}

impl<N, V> From<Vec<(N, V)>> for Params
where
    String: From<N>,
    Value: From<V>,
{
    fn from(x: Vec<(N, V)>) -> Params {
        let mut map = HashMap::default();
        for (name, value) in x.into_iter() {
            let name = String::from(name);
            if map.contains_key(&name) {
                panic!("Redefinition of named parameter `{}'", name);
            } else {
                map.insert(name, Value::from(value));
            }
        }
        Params::Named(map)
    }
}

impl<'a> From<&'a [&'a dyn ToValue]> for Params {
    fn from(x: &'a [&'a dyn ToValue]) -> Params {
        let mut raw_params: SmallVec<[Value; 12]> = SmallVec::new();
        for v in x.into_iter() {
            raw_params.push(v.to_value());
        }
        if raw_params.len() == 0 {
            Params::Empty
        } else {
            Params::Positional(raw_params)
        }
    }
}

impl From<()> for Params {
    fn from(_: ()) -> Params {
        Params::Empty
    }
}

macro_rules! into_params_impl {
    ($([$A:ident,$a:ident]),*) => (
        impl<$($A: Into<Value>,)*> From<($($A,)*)> for Params {
            fn from(x: ($($A,)*)) -> Params {
                let ($($a,)*) = x;
                let mut params = SmallVec::<[Value; 12]>::new();
                $(params.push($a.into());)*
                Params::Positional(params)
            }
        }
    );
}

into_params_impl!([A, a]);
into_params_impl!([A, a], [B, b]);
into_params_impl!([A, a], [B, b], [C, c]);
into_params_impl!([A, a], [B, b], [C, c], [D, d]);
into_params_impl!([A, a], [B, b], [C, c], [D, d], [E, e]);
into_params_impl!([A, a], [B, b], [C, c], [D, d], [E, e], [F, f]);
into_params_impl!([A, a], [B, b], [C, c], [D, d], [E, e], [F, f], [G, g]);
into_params_impl!(
    [A, a],
    [B, b],
    [C, c],
    [D, d],
    [E, e],
    [F, f],
    [G, g],
    [H, h]
);
into_params_impl!(
    [A, a],
    [B, b],
    [C, c],
    [D, d],
    [E, e],
    [F, f],
    [G, g],
    [H, h],
    [I, i]
);
into_params_impl!(
    [A, a],
    [B, b],
    [C, c],
    [D, d],
    [E, e],
    [F, f],
    [G, g],
    [H, h],
    [I, i],
    [J, j]
);
into_params_impl!(
    [A, a],
    [B, b],
    [C, c],
    [D, d],
    [E, e],
    [F, f],
    [G, g],
    [H, h],
    [I, i],
    [J, j],
    [K, k]
);
into_params_impl!(
    [A, a],
    [B, b],
    [C, c],
    [D, d],
    [E, e],
    [F, f],
    [G, g],
    [H, h],
    [I, i],
    [J, j],
    [K, k],
    [L, l]
);
