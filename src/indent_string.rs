use crate::parse::{self, Result};

/// Representation for indentations
///
/// An IDM file can be indented with either tabs or spaces, but a file must
/// either use tabs only or spaces only.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum IndentString {
    /// The state at start, we don't know which it is.
    ///
    /// Has indent level of 0.
    Undetermined,
    /// Segments corresponding to one indentation level with the number of
    /// space characters that segment consists of.
    Spaces(Vec<usize>),
    /// Segments corresponding to one indentation level with the number of
    /// tab characters that segment consists of.
    Tabs(Vec<usize>),
}

use IndentString::*;

impl Default for IndentString {
    fn default() -> Self {
        Undetermined
    }
}

impl<'a> std::ops::Deref for IndentString {
    type Target = Vec<usize>;

    fn deref(&self) -> &Self::Target {
        static EMPTY: Vec<usize> = vec![];
        match self {
            Undetermined => &EMPTY,
            Spaces(v) => &v,
            Tabs(v) => &v,
        }
    }
}

impl IndentString {
    /// Generate a default indentation string for the given depth.
    pub fn new(depth: usize) -> IndentString {
        // Default style is two spaces per indent level.
        Spaces(vec![2; depth])
    }

    /// Generate indentation string with single tabs as indents.
    pub fn tabs(depth: usize) -> IndentString {
        Tabs(vec![1; depth])
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Undetermined => true,
            Spaces(v) => v.is_empty(),
            Tabs(v) => v.is_empty(),
        }
    }

    fn accepting(c: char) -> IndentString {
        match c {
            ' ' => Spaces(Vec::new()),
            '\t' => Tabs(Vec::new()),
            _ => panic!("Indenting::accepting: Invalid indent type"),
        }
    }

    fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        // XXX: Would be nice to impl IntoIterator, but I don't think that can
        // work as long as my iterator is defined in terms of from_fn...
        let mut iter = match self {
            Undetermined => None,
            Spaces(v) => Some(v.iter()),
            Tabs(v) => Some(v.iter()),
        };

        std::iter::from_fn(move || {
            if let Some(ref mut iter) = iter {
                iter.next().cloned()
            } else {
                None
            }
        })
    }

    /// Construct a string of given depth using the character of this indent
    /// style.
    ///
    /// Will panic if the indent string is undetermined.
    pub fn string(&self, n: usize) -> String {
        match self {
            Undetermined => {
                panic!("IndentString:string: Cannot generate Undetermined")
            }
            Spaces(_) => String::from_utf8(vec![b' '; n]).unwrap(),
            Tabs(_) => String::from_utf8(vec![b'\t'; n]).unwrap(),
        }
    }

    fn accepts(&self, c: char) -> bool {
        match self {
            Undetermined => c == ' ' || c == '\t',
            Spaces(_) => c == ' ',
            Tabs(_) => c == '\t',
        }
    }

    fn empty(&self) -> IndentString {
        match self {
            Undetermined => Undetermined,
            Spaces(_) => Spaces(vec![]),
            Tabs(_) => Tabs(vec![]),
        }
    }

    fn push(&mut self, n: usize) {
        match self {
            Undetermined => panic!("IndentString::push: Invalid string"),
            Spaces(v) => v.push(n),
            Tabs(v) => v.push(n),
        }
    }

    /// Try to match indentations on next line given self as current indent
    /// string.
    ///
    /// Either current or new input must be a prefix of the other, and new
    /// indent must not use tabs when space indentation has already been
    /// established, or vice versa.
    ///
    /// Will return an error if the line is blank (no well-defined indentation
    /// level) or if it has indentation inconsistent with the existing one. If
    /// the line is known to be non-blank, an error from `match_next`
    /// indicates inconsistent indentation and means that the entire input is
    /// unparseable.
    pub fn match_next<'a>(&self, input: &'a str) -> Result<'a, IndentString> {
        // Function used to match indentations. Will get locked to spaces or
        // tabs.
        let indent_fn;
        let mut ret;

        // Set type of indentation used or exit early.
        match input.chars().next() {
            None => return Err(input),
            Some(ws) if ws == ' ' || ws == '\t' => {
                if !self.accepts(ws) {
                    // It's whitespace but not accepted by current state, must
                    // be trying to switch between tabs and spaces mid-input.
                    return Err(input);
                }
                indent_fn = move |n, input| repeat(ws, n, input);
                ret = IndentString::accepting(ws);
            }
            Some(ws) if ws.is_whitespace() => {
                // Unknown type of whitespace, or newline, not acceptable.
                return Err(input);
            }
            Some(_) => {
                // Not whitespace, return an empty indent string.
                return Ok((self.empty(), input));
            }
        }

        let mut pos = input;
        for segment_len in self.iter() {
            // Each segment must be matched in full or indentation is
            // inconsistent.
            parse::r(&mut pos, |input| indent_fn(segment_len, input))?;
            ret.push(segment_len);

            if pos.chars().next().map_or(false, |c| !c.is_whitespace()) {
                // Next indent is shallower than previous.
                return Ok((ret, pos));
            }
        }

        // Deepen the indent level.
        //
        // indent_fn should fail if it runs into either newline (indicating a
        // blank line) or a wrong indentation character.
        if pos.chars().next().map_or(false, |c| c.is_whitespace()) {
            let new_indent = parse::r(&mut pos, |input| indent_fn(0, input))?;
            ret.push(new_indent.len());
        }

        // If we ended up at EOF, that means the line was blank.
        if pos == "" {
            return Err(input);
        }

        Ok((ret, pos))
    }
}

/// Match char 'ch' repeating 'n' times.
///
/// Special case, if n is 0, match the char for any number of times.
fn repeat(ch: char, mut n: usize, input: &str) -> Result<&str> {
    if n == 0 {
        // Special case, read as many accepted chars as there are.
        for c in input.chars().take_while(|c| c.is_whitespace()) {
            if c != ch {
                return Err(input);
            }
            n += 1;
        }
    } else {
        for c in input.chars().take(n) {
            if c != ch {
                return Err(input);
            }
        }
    }
    let pos = n * ch.len_utf8();
    Ok((&input[..pos], &input[pos..]))
}

#[cfg(test)]
mod tests {
    use super::{IndentString, IndentString::*};

    #[test]
    fn test_undetermined() {
        let prev = IndentString::default();

        assert_eq!(prev.match_next("   x"), Ok((Spaces(vec![3]), "x")));
        assert_eq!(prev.match_next("\tx"), Ok((Tabs(vec![1]), "x")));
        assert_eq!(prev.match_next("x"), Ok((Undetermined, "x")));
        assert_eq!(prev.match_next("  "), Err("  "));
        assert_eq!(prev.match_next(""), Err(""));
        assert_eq!(prev.match_next("\t bad"), Err("\t bad"));
        assert_eq!(prev.match_next("\n  a"), Err("\n  a"));
    }

    #[test]
    fn test_spaces() {
        let prev = IndentString::new(1);

        assert_eq!(prev.match_next("x"), Ok((Spaces(vec![]), "x")));
        assert_eq!(prev.match_next("  x"), Ok((Spaces(vec![2]), "x")));
        assert_eq!(prev.match_next("    x"), Ok((Spaces(vec![2, 2]), "x")));
        assert_eq!(prev.match_next("   x"), Ok((Spaces(vec![2, 1]), "x")));
        // Blank line
        assert_eq!(prev.match_next("    "), Err("    "));
        // Inconsistent with unbroken two space prefix.
        assert_eq!(prev.match_next(" x"), Err(" x"));
        // Mixed indentation.
        assert_eq!(prev.match_next("  \tx"), Err("\tx"));
        assert_eq!(prev.match_next("  \n  a"), Err("\n  a"));
    }

    #[test]
    fn test_tabs() {
        let prev = IndentString::tabs(1);

        assert_eq!(prev.match_next("x"), Ok((Tabs(vec![]), "x")));
        assert_eq!(prev.match_next("\tx"), Ok((Tabs(vec![1]), "x")));
        assert_eq!(prev.match_next("\t\tx"), Ok((Tabs(vec![1, 1]), "x")));
        assert_eq!(prev.match_next("\t\t\tx"), Ok((Tabs(vec![1, 2]), "x")));
        // Blank line
        assert_eq!(prev.match_next("    "), Err("    "));
        // Mixed indentation.
        assert_eq!(prev.match_next("\t  x"), Err("  x"));

        assert_eq!(Tabs(vec![2]).match_next("\tx"), Err("\tx"));
    }
}
