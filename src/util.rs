use std::ops::{Deref, DerefMut};

use crate::{from_str, parse::CharExt, to_string, Style};
use serde::{de, de::DeserializeOwned, ser, Deserialize, Serialize};

/// Like `guess_indent_style`, but returns `None` if input does not clearly
/// specify an indent style.
pub fn infer_indent_style(input: &str) -> Option<Style> {
    let mut at_line_start = true;
    let mut prefix = String::new();
    for c in input.chars() {
        if c == '\n' {
            at_line_start = true;
            continue;
        } else if !c.is_idm_whitespace() {
            if !prefix.is_empty() {
                // Got what we wanted.
                break;
            } else {
                at_line_start = false;
            }
        }

        if at_line_start && c.is_idm_whitespace() {
            prefix.push(c);
        }
    }

    if let Some(c) = prefix.chars().next() {
        if c == '\t' {
            Some(Style::Tabs)
        } else if c == ' ' {
            Some(Style::Spaces(prefix.len()))
        } else {
            None
        }
    } else {
        None
    }
}
/// Guess the indent style used by a given IDM text.
///
/// Can be passed to the serializer to reserialize data in the same style.
pub fn guess_indent_style(input: &str) -> Style {
    infer_indent_style(input).unwrap_or_default()
}

/// Try to convert a value of one type into a value of a different type that
/// has the same IDM serialization.
pub fn transmute<T: Serialize, U: DeserializeOwned>(e: &T) -> crate::Result<U> {
    let s = crate::to_string(e)?;
    crate::from_str(&s)
}

/// Wrapper that represents `Default` values with `"-"`, allowing them in
/// contexts that cannot represent an empty value.
///
/// ```
/// use idm::DefaultDash;
///
/// assert_eq!(
///     idm::from_str::<DefaultDash<String>>("-").unwrap(),
///     DefaultDash(String::default())
/// );
/// assert_eq!(
///     idm::from_str::<DefaultDash<String>>("xyzzy").unwrap(),
///     DefaultDash("xyzzy".to_string())
/// );
///
/// assert_eq!(
///     idm::to_string(&DefaultDash("xyzzy".to_string())).unwrap(),
///     "xyzzy"
/// );
/// assert_eq!(
///     idm::to_string(&DefaultDash(String::default())).unwrap(),
///     "-"
/// );
/// ```
#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct DefaultDash<T: Default>(pub T);

impl<'de, T: DeserializeOwned + Default> Deserialize<'de> for DefaultDash<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let ret = String::deserialize(deserializer)?;
        if ret == "-" {
            Ok(DefaultDash(Default::default()))
        } else {
            from_str(&ret).map(DefaultDash).map_err(de::Error::custom)
        }
    }
}

impl<T: Serialize + Default + PartialEq> Serialize for DefaultDash<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.0 == Default::default() {
            "-".serialize(serializer)
        } else {
            // XXX: It's up to the user to worry about non-default values of
            // the type serializing as a literal "-". Could serialize to
            // string here, error out on "-" and reserialize the string
            // otherwise, but this is simpler and probably faster.
            self.0.serialize(serializer)
        }
    }
}

impl<T: Default> Deref for DefaultDash<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Default> DerefMut for DefaultDash<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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

impl<'de, T: DeserializeOwned, U: DeserializeOwned> Deserialize<'de>
    for ColonPair<T, U>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let ret = String::deserialize(deserializer)?;
        if let Some((head, tail)) = ret.split_once(':') {
            let head = from_str(&head).map_err(de::Error::custom)?;
            let tail = from_str(&tail).map_err(de::Error::custom)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Style;

    #[test]
    fn guess_style() {
        assert_eq!(guess_indent_style(""), Style::Spaces(2));

        assert_eq!(
            guess_indent_style(
                "\
foo
bar"
            ),
            Style::Spaces(2)
        );

        assert_eq!(
            guess_indent_style(
                "\
foo
 bar
  baz"
            ),
            Style::Spaces(1)
        );

        assert_eq!(
            guess_indent_style(
                "\
foo
    bar"
            ),
            Style::Spaces(4)
        );
        assert_eq!(
            guess_indent_style(
                "\
foo
\tbar"
            ),
            Style::Tabs
        );
    }
}
