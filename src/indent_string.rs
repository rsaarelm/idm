use crate::parse::{self, Result};

/// Representation for indentations
///
/// An IDM file can be indented with either tabs or spaces, but a file must
/// either use tabs only or spaces only.
///
/// An `IndentString` derefs to a `Vec` of its indent segments, with the value
/// of a segment being the character length of that segment substring.
#[derive(Default, Clone, Eq, PartialEq, Debug)]
pub struct IndentString {
    /// Is this tab or space based indentation?
    ///
    /// Set to '\0' when indent type has not been established yet.
    indent_char: char,

    /// List of segments.
    ///
    /// An empty vector corresponds to the special logical indent level -1.
    /// Regular level 0 indent is represented by having a single 0-valued
    /// element at the start of the vector.
    segments: Vec<usize>,
}

impl<'a> std::ops::Deref for IndentString {
    type Target = Vec<usize>;

    fn deref(&self) -> &Self::Target {
        &self.segments
    }
}

impl IndentString {
    fn build(indent_char: char, segments: &[usize]) -> IndentString {
        let input = segments;
        // Constructor functions won't produce column -1 indent strings.
        // Add the initial zero to mark this.
        let mut segments = vec![0];

        for &i in input {
            if i == 0 {
                panic!("IndentString::spaces 0 segment length");
            }
            segments.push(i);
        }

        IndentString {
            indent_char,
            segments
        }
    }

    pub fn spaces(segments: &[usize]) -> IndentString {
        IndentString::build(' ', segments)
    }

    pub fn tabs(segments: &[usize]) -> IndentString {
        IndentString::build('\t', segments)
    }

    /// Return the character length (not segment count) of this indent string.
    pub fn str_len(&self) -> usize {
        self.iter().sum()
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.segments.pop()
    }

    fn set_char(mut self, c: char) -> IndentString {
        if self.indent_char != '\0' && c != self.indent_char {
            panic!("set_char: Changing established indentation");
        }
        if c != ' ' && c != '\t' {
            panic!("set_char: Invalid indent char {:?}", c);
        }
        self.indent_char = c;
        self
    }

    /// Construct a string of given character length using the character of
    /// this indent style.
    ///
    /// Will panic if the indent string is undetermined and requested depth is
    /// not zero.
    pub fn string(&self, n: usize) -> String {
        if n == 0 {
            return "".into();
        }

        if self.indent_char == '\0' {
            panic!("IndentString:string: Cannot generate Undetermined indent string");
        }

        String::from_utf8(vec![self.indent_char as u8; n]).unwrap()
    }

    fn accepts(&self, ch: char) -> bool {
        match self.indent_char {
            '\0' => ch == ' ' || ch == '\t',
            c => ch == c,
        }
    }

    fn empty(&self) -> IndentString {
        IndentString {
            segments: Vec::new(),
            indent_char: self.indent_char,
        }
    }

    /// Try to match indentations on next line given self as current indent
    /// string.
    ///
    /// Either current or new input must be a prefix of the other, and new
    /// indent must not use tabs when space indentation has already been
    /// established, or vice versa.
    ///
    /// Will skip over blank lines until it finds a non-blank one. Will return
    /// a zero indent if there are no contentful lines left.
    ///
    /// An error from `match_next` indicates inconsistent indentation and
    /// means that the entire input is unparseable.
    pub fn match_next<'a>(&self, input: &'a str) -> Result<'a, IndentString> {
        // Function used to match indentations. Will get locked to spaces or
        // tabs.
        let indent_fn;
        let mut ret = self.empty();

        // Eat blanks.
        let mut pos = input;
        while parse::r(&mut pos, parse::blank_line).is_ok() {}

        // Special case for column -1
        if self.is_empty() {
            // First input level must start with content right at column 0.
            if pos.chars().next().map_or(true, |c| c.is_whitespace()) {
                return Err(pos);
            }

            // Push the special zero element to mark being out of column -1.
            ret.segments.push(0);

            // Exit early.
            return Ok((ret, pos));
        }

        // Set type of indentation used or exit early.
        match pos.chars().next() {
            None => return Ok((self.empty(), "")),
            Some(ws) if ws == ' ' || ws == '\t' => {
                if !ret.accepts(ws) {
                    // It's whitespace but not accepted by current state, must
                    // be trying to switch between tabs and spaces mid-input.
                    return Err(pos);
                }
                indent_fn = move |n, input| repeat(ws, n, input);
            }
            Some(ws) if ws.is_whitespace() => {
                // Unknown type of whitespace, or newline, not acceptable.
                return Err(pos);
            }
            Some(_) => {
                // Not whitespace, return an empty indent string.
                return Ok((ret, pos));
            }
        }

        for &segment_len in self.iter() {
            // Each segment must be matched in full or indentation is
            // inconsistent.
            parse::r(&mut pos, |input| indent_fn(segment_len, input))?;
            ret.segments.push(segment_len);

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
            ret.segments.push(new_indent.len());
        }

        // If we ended up at EOF, that means the line was blank.
        if pos == "" {
            return Err(input);
        }

        Ok((ret, pos))
    }

    /// Like `match_next`, but indent must be equal to self.
    pub fn match_same<'a>(&self, input: &'a str) -> Result<'a, IndentString> {
        let mut pos = input;
        let next = parse::r(&mut pos, |input| self.match_next(input))?;
        if &next == self {
            Ok((next, pos))
        } else {
            Err(input)
        }
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
    use super::*;

    #[test]
    fn test_undetermined() {
        let start = IndentString::default();

        // Get to column 0, stay undetermined.
        assert!(start.match_next("x").is_ok());
        let prev = start.match_next("x").unwrap().0;
        assert_eq!(prev.indent_char, '\0');
        assert_eq!(prev.iter().cloned().collect::<Vec<usize>>(), vec![0]);

        assert_eq!(prev.match_next("   x"), Ok((IndentString::spaces(&[3]), "x")));
        /*
        assert_eq!(prev.match_next("\tx"), Ok((Tabs(vec![1]), "x")));
        assert_eq!(prev.match_next("x"), Ok((Undetermined, "x")));
        assert_eq!(prev.match_next("  "), Ok((Undetermined, "")));
        assert_eq!(prev.match_next("\n  x"), Ok((Spaces(vec![2]), "x")));
        assert_eq!(prev.match_next("\n  x\ny"), Ok((Spaces(vec![2]), "x\ny")));
        assert_eq!(prev.match_next(""), Ok((Undetermined, "")));
        assert_eq!(prev.match_next("\t bad"), Err("\t bad"));
        */
    }

    #[test]
    fn test_spaces() {
        /*
        let prev = IndentString::new(1);

        assert_eq!(prev.match_next("x"), Ok((Spaces(vec![]), "x")));
        assert_eq!(prev.match_next("  x"), Ok((Spaces(vec![2]), "x")));
        assert_eq!(prev.match_next("    x"), Ok((Spaces(vec![2, 2]), "x")));
        assert_eq!(prev.match_next("   x"), Ok((Spaces(vec![2, 1]), "x")));
        // Blank line
        assert_eq!(prev.match_next("    "), Ok((Spaces(vec![]), "")));
        // Inconsistent with unbroken two space prefix.
        assert_eq!(prev.match_next(" x"), Err(" x"));
        // Mixed indentation.
        assert_eq!(prev.match_next("  \tx"), Err("\tx"));
        assert_eq!(prev.match_next("  \n  a"), Ok((Spaces(vec![2]), "a")));
        */
    }

    #[test]
    fn test_tabs() {
        /*
        let prev = IndentString::tabs(1);

        assert_eq!(prev.match_next("x"), Ok((Tabs(vec![]), "x")));
        assert_eq!(prev.match_next("\tx"), Ok((Tabs(vec![1]), "x")));
        assert_eq!(prev.match_next("\t\tx"), Ok((Tabs(vec![1, 1]), "x")));
        assert_eq!(prev.match_next("\t\t\tx"), Ok((Tabs(vec![1, 2]), "x")));
        // Blank line
        assert_eq!(prev.match_next("    "), Ok((Tabs(vec![]), "")));
        // Mixed indentation.
        assert_eq!(prev.match_next("\t  x"), Err("  x"));

        assert_eq!(Tabs(vec![2]).match_next("\tx"), Err("\tx"));
        */
    }
}
