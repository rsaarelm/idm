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

// XXX: Where do we need this?
impl Into<Option<String>> for &IndentString {
    fn into(self) -> Option<String> {
        if self.segments.len() == 0 {
            return None;
        }
        let n = self.segments.iter().count();
        if n > 0 && self.indent_char == '\0' {
            panic!(
                "Can't turn non-empty undetermined IndentString into String"
            );
        }

        Some(String::from_utf8(vec![self.indent_char as u8; n]).unwrap())
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
            segments,
        }
    }

    /// Build space indented `IndentString` with given segments.
    pub fn spaces(segments: &[usize]) -> IndentString {
        IndentString::build(' ', segments)
    }

    /// Build tab indented `IndentString` with given segments.
    pub fn tabs(segments: &[usize]) -> IndentString {
        IndentString::build('\t', segments)
    }

    /// Build undetermined `IndentString` with segments `&[]`.
    ///
    /// An undetermined `IndentString` does not get to have a contentful
    /// segment list like `spaces` and `tabs` do, because once there is actual
    /// indentation you will know which character it uses. You still have the
    /// result of this construction, which is at column 0, and the default
    /// `IndentString` that is at column -1.
    pub fn undetermined() -> IndentString {
        IndentString {
            segments: vec![0],
            indent_char: '\0',
        }
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.segments.pop()
    }

    /// Construct a string of given character length using the character of
    /// this indent style.
    ///
    /// Will panic if the indent string is undetermined and requested depth is
    /// not zero.
    #[deprecated] // Use Into<Option<String>> instead
    pub fn string(&self, n: usize) -> String {
        if n == 0 {
            return "".into();
        }

        if self.indent_char == '\0' {
            panic!("IndentString:string: Cannot generate Undetermined indent string");
        }

        String::from_utf8(vec![self.indent_char as u8; n]).unwrap()
    }

    pub fn input_char(&self) -> Option<char> {
        match self.indent_char {
            '\0' => None,
            ' ' => Some(' '),
            '\t' => Some('\t'),
            _ => panic!("Bad input char"),
        }

    }

    fn accepts(&self, ch: char) -> bool {
        match self.indent_char {
            '\0' => ch == ' ' || ch == '\t',
            c => ch == c,
        }
    }

    fn zero_column(&self) -> IndentString {
        IndentString {
            segments: vec![0],
            indent_char: self.indent_char,
        }
    }

    fn minus_one_column(&self) -> IndentString {
        IndentString {
            segments: vec![],
            indent_char: self.indent_char,
        }
    }

    /// Clone of self, except if self has negative indent in which case the
    /// result is adjusted to column 0.
    fn clone_with_non_negative_column(&self) -> IndentString {
        let mut ret = self.clone();
        if ret.segments.is_empty() {
            ret.segments.push(0);
        }
        ret
    }

    /// Extend indent string with 1 character dummy indent.
    fn extend(&self) -> IndentString {
        let mut ret = self.clone();

        if ret.segments.is_empty() {
            ret.segments.push(0);
        } else {
            ret.segments.push(1);
        }

        ret
    }

    /// Match a slice that must be pure indent that matches with segments and
    /// indent char of this indent string into the corresponding indent
    /// string.
    pub fn merge<'a>(&self, indent: &'a str) -> Result<'a, IndentString> {
        if !indent.chars().all(|c| c == self.indent_char) {
            return Err(indent);
        }
        if self.indent_char == '\0' && !indent.is_empty() {
            return Err(indent);
        }

        let mut ret = self.zero_column();

        let mut len = 0;
        for (i, n) in self.segments.iter().enumerate() {
            // Loop through segment points
            //
            // If indent len snaps with current point, return corresponding new
            // IndentString.
            //
            // If indent len is less than current point, it didn't line up with
            // segment and failed.
        }

        todo!();
    }

    /// Try to match indentations on next line given self as current indent
    /// string.
    ///
    /// Either current or new input must be a prefix of the other, and new
    /// indent must not use tabs when space indentation has already been
    /// established, or vice versa.
    ///
    /// Will skip over blank lines until it finds a non-blank one. Will return
    /// an "out of file" negative column indent when there is no content left.
    ///
    /// An error from `fill` indicates inconsistent indentation and
    /// means that the entire input is unparseable.
    pub fn fill<'a>(&self, input: &'a str) -> Result<'a, IndentString> {
        // TODO: Rewrite in terms of parse::indent and merge. (Possibly
        // remove?)

        // Function used to match indentations. Will get locked to spaces or
        // tabs.
        let indent_fn;
        let mut ret = self.zero_column();

        // Eat blanks.
        let mut pos = input;
        while parse::r(&mut pos, parse::blank_line).is_ok() {}

        if pos == "" {
            // Out of input. Return the negative column state.
            return Ok((self.minus_one_column(), ""));
        }

        // Special case for column -1
        if self.is_empty() {
            // First input level must start with content right at column 0.
            if pos.chars().next().map_or(true, |c| c.is_whitespace()) {
                return Err(pos);
            }

            // Exit early.
            return Ok((ret, pos));
        }

        // Set type of indentation used or exit early.
        match pos.chars().next() {
            None => unreachable!(),
            Some(ws) if ws == ' ' || ws == '\t' => {
                if !ret.accepts(ws) {
                    // It's whitespace but not accepted by current state, must
                    // be trying to switch between tabs and spaces mid-input.
                    return Err(pos);
                }
                ret.indent_char = ws;
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

        for &segment_len in self.iter().skip(1) {
            // Each segment must be matched in full or indentation is
            // inconsistent. (Skip the first segment which is always the
            // zero-length marker for being past column -1.)
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

    /// Match the exact indent string to input and return remaining input
    /// after indent string.
    ///
    /// Will still succeed if there is further indentation beyond this indent
    /// string. The additional indentation will not be consumed.
    pub fn parse<'a>(&self, input: &'a str) -> Result<'a, ()> {
        let n_chars = self.segments.iter().sum();
        // Empty indent, always match.
        if n_chars == 0 {
            return Ok(((), input));
        }
        // Blank line, also always match.
        if parse::blank_line(input).is_ok() {
            return Ok(((), input));
        }

        if self.indent_char == '\0' {
            // Case that can be generated by extend where we have nonzero
            // chars but they're indeterminate type. Going to say this can't
            // match anything.
            return Err(input);
        }

        repeat(self.indent_char, n_chars, input).map(|(_, rest)| ((), rest))
    }

    /// Like `parse`, but will fail if there is further indentation past this
    /// indent string in the input.
    pub fn parse_exact<'a>(&self, input: &'a str) -> Result<'a, ()> {
        let (_, rest) = self.parse(input)?;
        if parse::blank_line(rest).is_ok() {
            return Ok(((), rest));
        }

        if rest.chars().next().map_or(false, |c| c.is_whitespace()) {
            // Further indentation detected.
            return Err(input);
        }

        Ok(((), rest))
    }

    /// Produce a deeper-indented version of the current indent string.
    ///
    /// If a new indent segment is found from a content string indented deeper
    /// than the current indent string starting from `input` and stopping at
    /// the first equal or lower indented content line, that segment is used
    /// for the extension. Otherwise a dummy extension of a single character
    /// of the current indentation type is used. The dummy extension will not
    /// match anything other than a blank line in the current input and exists
    /// only for stack machine logic purposes for the higher parsing
    /// machinery.
    pub fn extend_to<'a>(&self, input: &'a str) -> Result<'a, IndentString> {
        // Grab the actual indent for examination
        let (existing, rest) = self.fill(input)?;
        let ret = if existing.len() > self.len() {
            // A natural extension exists, use that.
            debug_assert!(existing.len() == self.len() + 1);
            existing
        } else {
            // There is no natural extension, create a dummy extension of one
            // character.
            self.extend()
        };
        Ok((ret, rest))
    }

    /// Determine indent for a multiline atom whose insides do not necessarily
    /// agree with structural indentation conventions. Return the indent
    /// string at the first line of the atom and the input starting from the
    /// contents of the first line immediately after the input string.
    ///
    /// The resulting indent string will indent up to the leftmost contentful
    /// line in the block of contiguous lines from the start of input that are
    /// indented deeper than the current indent string. If all the lines do
    /// not agree to a consistend indent string up to this depth, the function
    /// will fail. The lines do not need to agree with the indentation past
    /// the level of indentation, since it is not expected that further
    /// structural parsing will be done to the value.
    ///
    /// The method is separate from `extend` because scanning the entire
    /// outline is more expensive than stopping at the first content line and
    /// assuming you can get acceptable indentation from that.
    pub fn fill_for_indented_atom<'a>(
        &self,
        input: &'a str,
    ) -> Result<'a, IndentString> {
        let mut pos = input;

        /*
        // Probe: Go at least one character deeper than current, will keep
        // matching lines as long as we're in a block deeper than current
        // depth.
        let mut min_indent = self.extend();
        // Determine level.
        loop {
            if let Ok((
        }
        */
        todo!();
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
        if input.len() < n {
            return Err(input);
        }

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
        let prev = IndentString::undetermined();

        assert_eq!(prev.fill("   x"), Ok((IndentString::spaces(&[3]), "x")));
        assert_eq!(prev.fill("\tx"), Ok((IndentString::tabs(&[1]), "x")));
        assert_eq!(prev.fill("x"), Ok((IndentString::undetermined(), "x")));
        assert_eq!(prev.fill("  "), Ok((prev.minus_one_column(), "")));
        assert_eq!(prev.fill("\n  x"), Ok((IndentString::spaces(&[2]), "x")));
        assert_eq!(
            prev.fill("\n  x\ny"),
            Ok((IndentString::spaces(&[2]), "x\ny"))
        );
        assert_eq!(prev.fill(""), Ok((prev.minus_one_column(), "")));
        assert_eq!(prev.fill("\t bad"), Err("\t bad"));
    }

    #[test]
    fn test_spaces() {
        let prev = IndentString::spaces(&[2]);

        assert_eq!(prev.fill("x"), Ok((IndentString::spaces(&[]), "x")));
        assert_eq!(prev.fill("  x"), Ok((IndentString::spaces(&[2]), "x")));
        assert_eq!(
            prev.fill("    x"),
            Ok((IndentString::spaces(&[2, 2]), "x"))
        );
        assert_eq!(prev.fill("   x"), Ok((IndentString::spaces(&[2, 1]), "x")));
        // Blank line
        assert_eq!(prev.fill("    "), Ok((prev.minus_one_column(), "")));
        // Inconsistent with unbroken two space prefix.
        assert_eq!(prev.fill(" x"), Err(" x"));
        // Mixed indentation.
        assert_eq!(prev.fill("  \tx"), Err("\tx"));
        assert_eq!(prev.fill("  \n  a"), Ok((IndentString::spaces(&[2]), "a")));
    }

    #[test]
    fn test_tabs() {
        let prev = IndentString::tabs(&[1]);

        assert_eq!(prev.fill("x"), Ok((IndentString::tabs(&[]), "x")));
        assert_eq!(prev.fill("\tx"), Ok((IndentString::tabs(&[1]), "x")));
        assert_eq!(prev.fill("\t\tx"), Ok((IndentString::tabs(&[1, 1]), "x")));
        assert_eq!(
            prev.fill("\t\t\tx"),
            Ok((IndentString::tabs(&[1, 2]), "x"))
        );
        // Blank line
        assert_eq!(prev.fill("    "), Ok((prev.minus_one_column(), "")));
        // Mixed indentation.
        assert_eq!(prev.fill("\t  x"), Err("  x"));

        assert_eq!(IndentString::tabs(&[2]).fill("\tx"), Err("\tx"));
    }

    #[test]
    fn test_parse() {
        let prev = IndentString::spaces(&[2]);

        assert!(prev.parse("").is_err());
        assert!(prev.parse(" xy").is_err());
        assert_eq!(prev.parse("  xy"), Ok(((), "xy")));
        assert_eq!(prev.parse("    xy"), Ok(((), "  xy")));
    }
}
