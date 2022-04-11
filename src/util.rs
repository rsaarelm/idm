use crate::{parse::CharExt, Style};
use serde::{de::DeserializeOwned, Serialize};

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
