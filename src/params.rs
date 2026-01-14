// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{
    collections::{
        HashMap,
        hash_map::{Entry, Entry::Occupied},
    },
    fmt,
};

use crate::value::{Value, convert::ToValue};

/// Missing named parameter for a statement
#[derive(Debug, Eq, PartialEq, Clone, thiserror::Error)]
#[error(
    "Missing named parameter `{}` for statement",
    String::from_utf8_lossy(_0)
)]
pub struct MissingNamedParameterError(pub Vec<u8>);

#[derive(Debug, PartialEq, Eq, Clone, Copy, thiserror::Error, Hash)]
pub enum ParamsConfusionError {
    #[error("Named params given where positional params are expected")]
    NamedParamsForPositionalQuery,
    #[error("Positional params given where named params are expected")]
    PositionalParamsForNamedQuery,
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ParamsError {
    #[error(transparent)]
    Missing(#[from] MissingNamedParameterError),
    #[error(transparent)]
    Confusion(#[from] ParamsConfusionError),
}

/// Representations of parameters of a prepared statement.
#[derive(Clone, PartialEq)]
pub enum Params {
    Empty,
    Named(HashMap<Vec<u8>, Value>),
    Positional(Vec<Value>),
}

impl fmt::Debug for Params {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Named(arg0) => {
                let arg0 = arg0
                    .iter()
                    .map(|(k, v)| (String::from_utf8_lossy(k), v))
                    .collect::<HashMap<_, _>>();
                f.debug_tuple("Named").field(&arg0).finish()
            }
            Self::Positional(arg0) => f.debug_tuple("Positional").field(arg0).finish(),
        }
    }
}

impl Params {
    /// Converts [`Params`] into a vector of values given the named parameters.
    ///
    /// `named_params` (if any) must follow the order they were given in the corresponding SQL
    /// statement.
    pub fn into_values(self, named_params: Option<&[Vec<u8>]>) -> Result<Vec<Value>, ParamsError> {
        match self {
            Params::Empty => match named_params {
                Some(params) => {
                    if let Some(first) = params.first() {
                        Err(MissingNamedParameterError(first.clone()).into())
                    } else {
                        Ok(vec![])
                    }
                }
                None => Ok(vec![]),
            },
            Params::Positional(values) => match named_params {
                Some(named_params) if !named_params.is_empty() => {
                    Err(ParamsConfusionError::PositionalParamsForNamedQuery.into())
                }
                _ => Ok(values),
            },
            Params::Named(map) => match named_params {
                Some(named_params) if !named_params.is_empty() => {
                    let mut values = vec![Value::NULL; named_params.len()];
                    let mut indexes = Vec::with_capacity(named_params.len());
                    for (name, value) in map {
                        let mut first = None;
                        for (i, _) in named_params.iter().enumerate().filter(|(_, x)| **x == name) {
                            indexes.push(i);
                            if first.is_none() {
                                first = Some(i);
                            } else {
                                values[i] = value.clone();
                            }
                        }
                        if let Some(first) = first {
                            values[first] = value;
                        }
                    }
                    if indexes.len() != named_params.len() {
                        indexes.sort_unstable();
                        match indexes.into_iter().enumerate().find(|x| x.0 != x.1) {
                            Some((missing, _)) => {
                                Err(MissingNamedParameterError(named_params[missing].clone())
                                    .into())
                            }
                            None => {
                                match named_params.last() {
                                    Some(last) => {
                                        Err(MissingNamedParameterError(last.clone()).into())
                                    }
                                    None => {
                                        // unreachable
                                        Ok(values)
                                    }
                                }
                            }
                        }
                    } else {
                        Ok(values)
                    }
                }
                _ => Err(ParamsConfusionError::NamedParamsForPositionalQuery.into()),
            },
        }
    }

    /// Will convert named parameters into positional assuming order passed in `named_params`
    /// attribute.
    #[deprecated = "use `into_values` instead"]
    pub fn into_positional(
        self,
        named_params: &[Vec<u8>],
    ) -> Result<Params, MissingNamedParameterError> {
        match self {
            Params::Named(mut map) => {
                let mut params: Vec<Value> = Vec::new();
                'params: for (i, name) in named_params.iter().enumerate() {
                    match map.entry(name.clone()) {
                        Occupied(entry) => {
                            let mut x = named_params.len() - 1;
                            while x > i {
                                if *name == named_params[x] {
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
        let mut raw_params: Vec<Value> = Vec::new();
        for v in x.into_iter() {
            raw_params.push(Value::from(v));
        }
        if raw_params.is_empty() {
            Params::Empty
        } else {
            Params::Positional(raw_params)
        }
    }
}

impl<N, V> From<Vec<(N, V)>> for Params
where
    Vec<u8>: From<N>,
    Value: From<V>,
{
    fn from(x: Vec<(N, V)>) -> Params {
        let mut map = HashMap::default();
        for (name, value) in x.into_iter() {
            let name: Vec<u8> = name.into();
            match map.entry(name) {
                Entry::Vacant(entry) => entry.insert(Value::from(value)),
                Entry::Occupied(entry) => {
                    panic!(
                        "Redefinition of named parameter `{}'",
                        String::from_utf8_lossy(entry.key())
                    );
                }
            };
        }
        Params::Named(map)
    }
}

impl<'a> From<&'a [&'a dyn ToValue]> for Params {
    fn from(x: &'a [&'a dyn ToValue]) -> Params {
        let mut raw_params: Vec<Value> = Vec::new();
        for v in x {
            raw_params.push(v.to_value());
        }
        if raw_params.is_empty() {
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
                Params::Positional(vec![
                    $($a.into(),)*
                ])
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
