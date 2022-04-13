use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use crate::{parse::CharExt, Style};
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
/// assert_eq!(idm::from_str::<DefaultDash<String>>("-").unwrap(), DefaultDash(String::default()));
/// assert_eq!(idm::from_str::<DefaultDash<String>>("xyzzy").unwrap(), DefaultDash("xyzzy".to_string()));
///
/// assert_eq!(idm::to_string(&DefaultDash("xyzzy".to_string())).unwrap().trim(), "xyzzy");
/// assert_eq!(idm::to_string(&DefaultDash(String::default())).unwrap().trim(), "-");
/// // Cannot serialize an actual "-".
/// assert!(idm::to_string(&DefaultDash("-".to_string())).is_err());
/// ```
#[derive(Clone, Copy, Default, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct DefaultDash<T: Default>(pub T);

impl<'de, E: Display, T: FromStr<Err = E> + Default> Deserialize<'de>
    for DefaultDash<T>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let ret = String::deserialize(deserializer)?;
        if ret == "-" {
            Ok(DefaultDash(Default::default()))
        } else {
            T::from_str(&ret)
                .map(DefaultDash)
                .map_err(de::Error::custom)
        }
    }
}

impl<T: Display + Default + PartialEq> Serialize for DefaultDash<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.0 == Default::default() {
            "-".serialize(serializer)
        } else {
            let s = self.0.to_string();
            if s == "-" {
                return Err(ser::Error::custom(
                    "DefaultDash inner type serializes as dash",
                ));
            }
            s.serialize(serializer)
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
