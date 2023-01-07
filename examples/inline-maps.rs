//! IDM can't do inline map values out of the box, let's fix that!

use std::collections::BTreeMap;

use idm::{from_str, to_string, AsVec, CharExt, UnderlineSpaces};
use serde::{
    de::{self, Deserialize, DeserializeOwned},
    ser::{self, Serialize},
};

/// Wrapper that merges two word-like elements into a single word-like element
/// with a connecting colon.
///
/// ```
/// use idm::ColonPair;
///
/// assert_eq!(
///     idm::from_str::<ColonPair<String, String>>("a:b").unwrap(),
///     ColonPair("a".to_string(), "b".to_string())
/// );
/// assert_eq!(
///     idm::to_string(&ColonPair("x".to_string(), "y".to_string())).unwrap(),
///     "x:y"
/// );
/// ```
#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct ColonPair<T, U>(pub T, pub U);

impl<T, U> ColonPair<T, U> {}

impl<'de, T: DeserializeOwned, U: DeserializeOwned> Deserialize<'de>
    for ColonPair<T, U>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let ret = String::deserialize(deserializer)?;
        if let Some((head, tail)) = ret.split_once(':') {
            let head = from_str(head).map_err(de::Error::custom)?;
            let tail = from_str(tail).map_err(de::Error::custom)?;
            Ok(ColonPair(head, tail))
        } else {
            Err(de::Error::custom("No colon found in value"))
        }
    }
}

impl<T: Serialize, U: Serialize> Serialize for ColonPair<T, U> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let head = to_string(&self.0).map_err(ser::Error::custom)?;
        let tail = to_string(&self.1).map_err(ser::Error::custom)?;
        if head.chars().any(|c| c.is_idm_whitespace() || c == ':')
            || tail.chars().any(|c| c.is_idm_whitespace())
        {
            return Err(ser::Error::custom(
                "ColonPair: halves are not words or head contains a colon",
            ));
        }
        format!("{}:{}", head, tail).serialize(serializer)
    }
}

impl<T, U> From<(T, U)> for ColonPair<T, U> {
    fn from((t, u): (T, U)) -> Self {
        ColonPair(t, u)
    }
}

#[allow(clippy::from_over_into)]
impl<T, U> Into<(T, U)> for ColonPair<T, U> {
    fn into(self) -> (T, U) {
        (self.0, self.1)
    }
}

fn main() {
    const BIBLIOGRAPHY: &str = "\
title:Structure_and_Interpretation_of_Computer_Programs  isbn:0262010771
title:A_Mathematical_Theory_of_Communication             doi:10.1002/j.1538-7305.1948.tb01338.x\n";

    type InlineMap = AsVec<
        BTreeMap<String, UnderlineSpaces<String>>,
        (String, UnderlineSpaces<String>),
        ColonPair<String, UnderlineSpaces<String>>,
    >;

    let library = idm::from_str::<Vec<InlineMap>>(BIBLIOGRAPHY).unwrap();

    for work in &library {
        println!("{}", *work["title"]);
        for (key, value) in &**work {
            if key == "title" {
                continue;
            }
            println!("  :{} {}", key, **value);
        }
    }
}
