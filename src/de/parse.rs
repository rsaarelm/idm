//! Stateless parsing primitivies.
use std::fmt;

use crate::CharExt;

type ParseResult<'a, T> = std::result::Result<(T, &'a str), &'a str>;

/// Extract a whitespace-separated word from the start of the input. Skip all
/// whitespace after it.
pub fn word(input: &str) -> ParseResult<&str> {
    let end_pos = input
        .char_indices()
        .find_map(|(i, c)| c.is_idm_whitespace().then_some(i))
        .unwrap_or(input.len());

    let next_pos = input[end_pos..]
        .char_indices()
        .find_map(|(i, c)| (!c.is_idm_whitespace()).then_some(end_pos + i))
        .unwrap_or(input.len());

    debug_assert!(next_pos >= end_pos);

    if end_pos > 0 {
        Ok((&input[0..end_pos], &input[next_pos..]))
    } else {
        Err(input)
    }
}

/// Split a line into words
pub fn words(input: &str) -> (Vec<&str>, &str) {
    let mut ret = Vec::new();
    let mut current = input;
    while let Ok((word, rest)) = word(current) {
        current = rest;
        ret.push(word);
    }
    (ret, current)
}

pub fn line(input: &str) -> ParseResult<&str> {
    input
        .char_indices()
        .find_map(|(i, c)| {
            if c == '\n' {
                Some(Ok((&input[..i], &input[i + 1..])))
            } else {
                None
            }
        })
        .unwrap_or(Ok((input, "")))
}

/// Indentation for any line.
///
/// If the line is blank and `line_indent` would return `None`, use the
/// indent of the next line. EOF gets the default zero indent.
///
/// Moves input to position after the first well-defined indent prefix or
/// at EOF, whichever comes first.
///
/// Returns an error if it finds mixed tabs and spaces in indentation, if
/// an error is returned the input can be considered unsalvageable.
pub fn indent(input: &str) -> ParseResult<Indent> {
    let mut pos = input;

    loop {
        match line_indent(pos)? {
            (Some(prefix), rest) => return Ok((prefix, rest)),
            (None, rest) => {
                debug_assert!(rest.len() < pos.len());
                pos = rest;
            }
        }
    }
}

/// Indentation for a single line, if defined.
fn line_indent(input: &str) -> ParseResult<Option<Indent>> {
    let mut indent = Indent::default();

    for (i, c) in input.char_indices() {
        if !c.is_idm_whitespace() {
            // Content encountered, return what was read so far.
            return Ok((Some(indent), &input[i..]));
        }
        if c == '\n' {
            // Hit end of line with no content, this line has no
            // well-defined indentation.
            return Ok((None, &input[i + 1..]));
        }

        if let Some(i2) = indent.increment(c) {
            indent = i2;
        } else {
            return Err(input);
        }
    }

    // At the end of input, by convention EOF has indent level 0.
    Ok((Some(Indent::default()), ""))
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Indent {
    Undetermined,
    /// Current indentation is n spaces, subsequent indentation must be spaces.
    Spaces(usize),
    /// Current indentation is n tabs, subsequent indentation must be tabs.
    Tabs(usize),
}

impl Default for Indent {
    fn default() -> Self {
        Indent::Undetermined
    }
}

impl Indent {
    pub fn increment(self, c: char) -> Option<Indent> {
        use Indent::*;
        match self {
            Undetermined if c == ' ' => Some(Spaces(1)),
            Undetermined if c == '\t' => Some(Tabs(1)),
            Spaces(n) if c == ' ' => Some(Spaces(n + 1)),
            Tabs(n) if c == '\t' => Some(Tabs(n + 1)),
            _ => None,
        }
    }

    pub fn as_char(self) -> char {
        match self {
            Indent::Undetermined => '\0',
            Indent::Spaces(_) => ' ',
            Indent::Tabs(_) => '\t',
        }
    }

    pub fn len(self) -> usize {
        match self {
            Indent::Undetermined => 0,
            Indent::Spaces(n) | Indent::Tabs(n) => n,
        }
    }

    pub fn compatible_with(self, other: Indent) -> bool {
        !matches!(
            (self, other),
            (Indent::Tabs(_), Indent::Spaces(_))
                | (Indent::Spaces(_), Indent::Tabs(_))
        )
    }
}

impl std::ops::Add for Indent {
    type Output = Indent;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Indent::Undetermined, Indent::Undetermined) => {
                Indent::Undetermined
            }
            (Indent::Undetermined, Indent::Spaces(a)) => Indent::Spaces(a),
            (Indent::Spaces(a), Indent::Undetermined) => Indent::Spaces(a),
            (Indent::Undetermined, Indent::Tabs(a)) => Indent::Tabs(a),
            (Indent::Tabs(a), Indent::Undetermined) => Indent::Tabs(a),

            (Indent::Tabs(a), Indent::Tabs(b)) => Indent::Tabs(a + b),
            (Indent::Spaces(a), Indent::Spaces(b)) => Indent::Spaces(a + b),
            _ => panic!("Invalid indent addition"),
        }
    }
}

impl std::ops::Sub for Indent {
    type Output = Indent;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Indent::Undetermined, Indent::Undetermined) => {
                Indent::Undetermined
            }
            (Indent::Spaces(a), Indent::Undetermined) => Indent::Spaces(a),
            (Indent::Tabs(a), Indent::Undetermined) => Indent::Tabs(a),

            (Indent::Tabs(a), Indent::Tabs(b)) if b <= a => Indent::Tabs(a - b),
            (Indent::Spaces(a), Indent::Spaces(b)) if b <= a => {
                Indent::Spaces(a - b)
            }
            _ => panic!("Invalid indent subtraction"),
        }
    }
}

impl fmt::Display for Indent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Indent::*;
        let (c, n) = match self {
            Undetermined => ('\0', 0),
            Spaces(n) | Tabs(n) => (self.as_char(), *n),
        };

        for _ in 0..n {
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        de::parse::{self, Indent},
        CharExt,
    };

    #[test]
    fn test_whitespace() {
        assert!(' '.is_idm_whitespace());
        assert!('\t'.is_idm_whitespace());

        // NBSP is not whitespace to IDM.
        assert!('\u{00a0}'.is_whitespace());
        assert!(!'\u{00a0}'.is_idm_whitespace());
    }

    #[test]
    fn test_indent_parse() {
        assert_eq!(parse::indent(""), Ok((Indent::default(), "")));
        assert_eq!(parse::indent("  xyzzy"), Ok((Indent::Spaces(2), "xyzzy")));
        assert_eq!(parse::indent("\txyzzy"), Ok((Indent::Tabs(1), "xyzzy")));

        // Mismatches
        assert!(parse::indent(" \txyzzy").is_err());
        assert!(parse::indent("\t xyzzy").is_err());

        // Skip over blanks to find indent.
        assert_eq!(
            parse::indent("\n  xyzzy"),
            Ok((Indent::Spaces(2), "xyzzy"))
        );
        assert_eq!(
            parse::indent("      \n  xyzzy"),
            Ok((Indent::Spaces(2), "xyzzy"))
        );

        // No contentful lines to be found, don't get fooled by trailing
        // whitespace.
        assert_eq!(parse::indent("      \n  "), Ok((Indent::default(), "")));
    }

    #[test]
    fn test_word() {
        assert!(parse::word("").is_err());
        assert!(parse::word("   ").is_err());
        assert!(parse::word("  \n ").is_err());
        assert_eq!(parse::word("a"), Ok(("a", "")));
        assert_eq!(parse::word("a  "), Ok(("a", "")));
        assert_eq!(parse::word("a b "), Ok(("a", "b ")));
        assert_eq!(parse::word("a\nb"), Ok(("a", "b")));
    }
}
