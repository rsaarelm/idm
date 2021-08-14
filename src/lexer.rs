use crate::{
    err,
    error::{Error, Result},
};
use std::{borrow::Cow, fmt};

/// Low-level indented text parsing primitives.
///
/// A lexer instance generally should not be used after it has returned an
/// error from one of the methods. Use a clone of the main lexer when doing
/// recoverable probing operations and call primitives on the main lexer when
/// their failure will fail the entire parse.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Lexer<'a> {
    /// Is the indentation based on spaces or tabs.
    ///
    /// Will be `None` at the start of the input. The first indentation
    /// encountered will set `indent_char`.
    indent_char: Option<char>,

    /// The current indentation prefix, as a list of segment lenghts.
    ///
    /// Segments correspond to previous indentation levels. Dedenting is only
    /// allowed to to segment boundaries. A dedent that does not line up with
    /// the established indent segments is a syntax error.
    ///
    /// An empty vector corresponds to the special logical indent level -1.
    /// Regular level 0 indent is represented by having a single 0-valued
    /// element at the start of the list.
    indent_segments: Vec<usize>,

    /// The remaining input to be lexed.
    input: &'a str,

    /// The original input
    input_start: &'a str,
}

/// IDM element shape.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Shape {
    /// A section with a headline and body lines.
    ///
    /// Will be read as `"a\n  b\n  c"` by `Lexer::read` (all lines after
    /// the first are indented).
    Section,
    /// A block of body lines without a headline.
    ///
    /// Will be read as `"b\nc"` by `Lexer::read` (all of the block lines
    /// start at zero indentation).
    Block,
    /// A single line with no child lines.
    BodyLine,
}

// Utility public methods
// (Implemented in terms of core public methods)

impl<'a> Lexer<'a> {
    /// Classify the shape at the current lexer position.
    pub fn classify(&self) -> Option<Shape> {
        let mut probe = self.clone();
        match probe.enter_body() {
            Ok(None) => {
                if probe.exit_body().is_err() {
                    Some(Shape::Block)
                } else {
                    None
                }
            }
            Ok(Some(_)) => {
                if probe.exit_body().is_err() {
                    Some(Shape::Section)
                } else {
                    Some(Shape::BodyLine)
                }
            }
            Err(_) => None,
        }
    }

    pub fn exit_words(&mut self) -> Result<()> {
        self.enter_body()?;
        self.exit_body()?;
        Ok(())
    }
}

// Core public methods

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            indent_char: Default::default(),
            indent_segments: Default::default(),
            input,
            input_start: input,
        }
    }

    /// Succeed if only whitespace remains of input.
    pub fn end(&mut self) -> Result<()> {
        for (i, c) in self.input.char_indices() {
            if !c.is_whitespace() {
                return self.err("Lexer::end Unparsed input remains");
            }
        }
        self.input = "";
        self.indent_segments.clear();
        Ok(())
    }

    /// Enter body of current headline.
    ///
    /// Return the headline if it exists. If the lexer is at the initial
    /// indent depth -1 or has been `dedent`ed, headline does not exist and
    /// `None` will be returned.
    ///
    /// The lexer will enter a deeper indentation depth that must be exited
    /// with `dedent` or `exit_body` even if there are no body lines to the
    /// section.
    ///
    /// Will fail when there is neither a headline (at depth -1 or indented to
    /// an empty body) nor any body lines.
    ///
    /// It should be reasonably cheap to call `enter_body` on an avergae
    /// input, feel free to use this for probing input with clones of the
    /// lexer.
    pub fn enter_body(&mut self) -> Result<Option<&'a str>> {
        log::debug!("Hello world");
        // At EOF.
        // - Can't have headline or body, fail out.
        if self.input == "" {
            return self.err("Lexer::enter_body at EOF");
        }

        let (current_prefix, _) = parse::indent(self.input)?;
        let new_segments: Vec<usize> = self.match_indent(current_prefix)?;

        // Current indent is shorter than expected indent.
        // - Can't have headline or body, fail out
        if new_segments.len() < self.indent_segments.len() {
            return self.err("Lexer::enter_body no content");
        }

        self.witness_indentation_char(current_prefix)?;

        // A headline exists if content starts *exactly* at indent string.
        let (line, rest) = parse::line(self.input)?;

        // Current indent is longer than expected indent.
        // - Headline is None.
        // - Do not move input, first line of input already belongs to body.
        if new_segments.len() > self.indent_segments.len() {
            self.indent_segments = new_segments;
            return Ok(None);
        }

        debug_assert!(new_segments.len() == self.indent_segments.len());
        debug_assert!(new_segments.len() > 0);

        // Headline indent as expected, but headline is blank.
        //  - Move to next line
        //  - Cannot have children (because of how indent for blank lines is
        //    determined), set synthetic +1 indent level.
        //  - Return Some("") for headline
        if line.chars().all(|c| c.is_whitespace()) {
            self.input = rest;
            self.indent_segments.push(1);
            return Ok(Some(""));
        }

        // Headline indent as expected, headline has content
        //  - Move to next line
        self.input = rest;
        //  - Determine indent level for next line
        let (body_prefix, _) = parse::indent(self.input)?;
        let body_segments = self.match_indent(body_prefix)?;
        //  - If next line's indent level is larger than expected, set
        //    expected to that
        //  - Otherwise set expected indent level to synthetic +1 indent
        if body_segments.len() > self.indent_segments.len() {
            self.indent_segments = body_segments;
        } else {
            self.indent_segments.push(1);
        }

        //  - Parse headline content, minus indentation into result
        Ok(Some(&line[current_prefix.len()..]))
    }

    /// Pop out of current indented body even if there is more body content
    /// left.
    ///
    /// Used to parse content for the special `_contents` struct field.
    pub fn dedent(&mut self) {
        self.indent_segments
            .pop()
            .expect("Lexer::dedent already at bottom of indent stack");
    }

    /// Pop out of current indented body when there is only white space left
    /// in the body. Will fail if body still has content.
    ///
    /// Should be cheap to call, feel free to use with cloned lexers.
    pub fn exit_body(&mut self) -> Result<()> {
        let (body_prefix, _) = parse::indent(self.input)?;
        let body_segments = self.match_indent(body_prefix)?;
        if body_segments.len() < self.indent_segments.len() {
            self.dedent();
            Ok(())
        } else {
            self.err("Lexer::exit_body Unparsed input remains")
        }
    }

    /// Extract a single word from the current line, move lexer to start of
    /// next word on the same line or end of line, whichever is closest.
    ///
    /// Failure from `word` does not invalidate the lexer.
    pub fn word(&mut self) -> Result<&str> {
        if self.input == "" {
            return self.err("Lexer::word At EOF");
        }

        let (line, _) = parse::line(self.input)?;
        if line.chars().all(|c| c.is_whitespace()) {
            return self.err("Lexer::word No more words on line");
        }

        let mut start_pos = 0;
        let mut end_pos = line.len();
        let mut before_word = true;
        for (i, c) in line.char_indices() {
            if before_word {
                start_pos = i;
                if !c.is_whitespace() {
                    before_word = false;
                }
            } else {
                if c.is_whitespace() {
                    end_pos = i;
                    break;
                }
            }
        }

        let word = &self.input[start_pos..end_pos];
        self.input = &self.input[end_pos..];

        Ok(word)
    }

    /// Read element at lexer into string with the indentation up to headline
    /// removed for each line.
    ///
    /// If run when there is no headline, only the body is read and
    /// indentation is removed up to body level.
    ///
    /// Failure from `read` does not invalidate the lexer.
    ///
    /// Calls to `read` can be very expensive. Try to only call `read`
    /// when you know you want to read the thing at the current point of
    /// input.
    pub fn read(&mut self) -> Result<String> {
        let mut ret = String::new();
        self.read_into(&mut ret)?;
        if !ret.is_empty() {
            // Drop trailing newline.
            ret.pop();
        }
        Ok(ret)
    }

    /// Attach line number to error message.
    pub fn err<T>(&self, msg: impl Into<Cow<'static, str>>) -> Result<T> {
        Err(Error::new(msg).line_num(self.line_num()))
    }
}

// Private methods

impl<'a> Lexer<'a> {
    fn numerize<'b>(&'b self) -> impl FnOnce(Error) -> Error + 'b {
        move |e| e.line_num(self.line_num())
    }

    /// Return the line number where the lexer is currently at.
    pub fn line_num(&self) -> usize {
        // XXX: Sorta expensive, current assumption is that this is only used
        // for off-the-happy-path parse error reports. Rewrite to cache the
        // line number if this ever becomes a bottleneck.
        let consumed_input =
            &self.input_start[..self.input_start.len() - self.input.len()];

        1 + consumed_input.chars().filter(|&c| c == '\n').count()
    }

    /// Read an element into an existing string buffer.
    fn read_into(&mut self, buffer: &mut String) -> Result<()> {
        if self.input == "" {
            return self.err("Lexer::read_into at EOF");
        }

        // XXX: read_into does not currently validate segment dedent
        // consistency within the section.

        let (current_prefix, _) =
            parse::indent(self.input).map_err(self.numerize())?;
        self.witness_indentation_char(current_prefix)?;
        let indent_len: usize = self.indent_segments.iter().sum();
        let new_segments: Vec<usize> = self.match_indent(current_prefix)?;

        if current_prefix.len() < indent_len {
            return self.err("Lexer::read_into out of depth");
        }

        let has_headline = current_prefix.len() == indent_len
            && !self.indent_segments.is_empty();

        // Set to true at start of loop if a headline exists.
        let mut at_headline = has_headline;

        while self.input != "" {
            // Verify indentation at new level.
            let (line, rest) =
                parse::line(self.input).map_err(self.numerize())?;
            let (indent, _) =
                parse::indent(self.input).map_err(self.numerize())?;
            self.witness_indentation_char(indent)?;

            if has_headline
                && !at_headline
                && indent.len() <= current_prefix.len()
            {
                // End of section, exit.
                break;
            }
            if !has_headline && indent.len() < current_prefix.len() {
                // Out of depth, exit.
                break;
            }
            at_headline = false;

            if line.chars().all(|c| c.is_whitespace()) {
                // Blank line.
                buffer.push('\n');
            } else {
                buffer.push_str(&line[current_prefix.len()..]);
                buffer.push('\n');
            }
            self.input = rest;
        }
        Ok(())
    }

    /// Match an indentation prefix with the current indent segment state.
    ///
    /// If the indentation does not match the current input string, either
    /// by segment joints or indent char, fail.
    ///
    /// An indentation at the fake -1 column can only match indentations at
    /// column 0.
    fn match_indent(&self, prefix: &str) -> Result<Vec<usize>> {
        // Empty segment is always good.
        if prefix == "" {
            return Ok(vec![0]);
        }

        // At column -1 it's an error to start with any indent other than 0.
        if self.indent_segments.is_empty() && !prefix.is_empty() {
            return self
                .err("Lexer::match_indent First line has nonzero indent");
        }

        // Must be good to unwrap since we've already handled the empty string
        // case.
        let indent_char = prefix.chars().next().unwrap();
        debug_assert!(prefix.chars().all(|c| c == indent_char));
        if indent_char != self.indent_char.unwrap_or(indent_char) {
            return self.err("Lexer::match_indent Mixed indentation detected");
        }

        let mut ret = Vec::new();
        let mut segments_len = 0;
        for &seg in &self.indent_segments {
            segments_len += seg;
            ret.push(seg);
            if segments_len == prefix.len() {
                // Exact match to some subsequence, exit now.
                return Ok(ret);
            } else if segments_len > prefix.len() {
                // Prefix does not align with existing segments.
                return self.err("Lexer::match_indent Mismatch with established indent levels");
            }
        }

        // Add the final bit of prefix.
        debug_assert!(prefix.len() > segments_len);
        ret.push(prefix.len() - segments_len);
        Ok(ret)
    }

    /// Register indentation char from input prefix.
    ///
    /// Must match an established indentation char. If indentation char has
    /// not been set yet and `indent_prefix` is nonempty, the prefix will be
    /// the char from `indent_prefix` for the rest of the lexer's lifetime.
    fn witness_indentation_char(&mut self, indent_prefix: &str) -> Result<()> {
        if let Some(c) = indent_prefix.chars().next() {
            // If this was a public API we'd maybe need a newtype for
            // indent_prefix that takes care of these invariants.
            debug_assert!(c == ' ' || c == '\t');
            debug_assert!(indent_prefix.chars().all(|ch| ch == c));

            if c != self.indent_char.unwrap_or(c) {
                // Mismatch!
                return self
                    .err("Lexer::witness_indentation_char Mixed indentation");
            }

            self.indent_char = Some(c);
            Ok(())
        } else {
            Ok(())
        }
    }
}

impl<'a> fmt::Display for Lexer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Custom debug formatter that cuts down an over-long input string.
        const MAX_LEN: usize = 72;
        let s = format!("{:?}", self);

        if s.len() > MAX_LEN {
            let cut_pos = s
                .char_indices()
                .take(MAX_LEN - 3)
                .map(|(i, _)| i)
                .last()
                .unwrap_or(0);
            write!(f, "{}...", &s[0..cut_pos])
        } else {
            write!(f, "{}", s)
        }
    }
}

// Parsing primitives that don't rely on Lexer state.

mod parse {
    use crate::{err, error::Error};
    type Result<'a, T> = std::result::Result<(T, &'a str), Error>;

    /// Indentation for any line.
    ///
    /// If the line is blank and `line_indent` would return `None`, use the
    /// indent of the next line. EOF gets indent `""`.
    ///
    /// Moves input to position after the first well-defined indent prefix or
    /// at EOF, whichever comes first.
    pub fn indent(input: &str) -> Result<&str> {
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
    fn line_indent(input: &str) -> Result<Option<&str>> {
        let mut indent_char = None;

        for (i, c) in input.char_indices() {
            if !c.is_whitespace() {
                // Content encountered, return what was read so far.
                return Ok((Some(&input[..i]), &input[i..]));
            }
            if c == '\n' {
                // Hit end of line with no content, no well-defined indentation.
                return Ok((None, &input[i + 1..]));
            }
            match indent_char {
                None => {
                    if c == ' ' || c == '\t' {
                        indent_char = Some(c);
                    } else {
                        return err!("parse::line_indent Invalid indent char");
                    }
                }
                Some(ic) => {
                    // Mixed indentation detected.
                    if ic != c {
                        return err!("parse::line_indent Mixed indentation");
                    }
                }
            }
        }

        // At the end of input, by convention EOF has indent level 0.
        Ok((Some(""), ""))
    }

    /// Read line until newline, rest of the content is after newline.
    ///
    /// Fails if there is no input.
    pub fn line(input: &str) -> Result<&str> {
        if input == "" {
            err!("parse::line at EOF")
        } else {
            let p = input.find('\n').unwrap_or(input.len());
            Ok((&input[..p], &input[(p + 1).min(input.len())..]))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_env_log::test;

    fn t(input: &str) -> Lexer {
        Lexer::new(input)
    }

    #[test]
    fn lexer_end() {
        assert!(t("a").end().is_err());
        assert!(t("xyzzy  ").end().is_err());
        assert!(t("").end().is_ok());
        assert!(t("\n\n").end().is_ok());
    }

    #[test]
    fn lexer_enter_body() {
        assert!(t("").enter_body().is_err());
        // Lexing starts at column -1.

        let mut lexer = t("a");
        // Get out of the null layer.
        assert_eq!(lexer.enter_body(), Ok(None));
        // Enter the line.
        assert_eq!(lexer.enter_body(), Ok(Some("a")));
    }

    #[test]
    fn lexer_read() {
        assert_eq!(t("a").read(), Ok("a".into()));
        assert_eq!(t("a\n  b").read(), Ok("a\n  b".into()));
        assert_eq!(t("a\n  b\nc").read(), Ok("a\n  b\nc".into()));

        let mut lexer = t("a\n  b\nc");
        lexer.enter_body().unwrap();
        assert_eq!(lexer.read(), Ok("a\n  b".into()));
        assert_eq!(lexer.read(), Ok("c".into()));
        assert!(lexer.read().is_err());
    }

    #[test]
    fn lexer_words() {
        assert_eq!(t("a").word(), Ok("a"));
        assert_eq!(t("a b").word(), Ok("a"));

        let mut lexer = t("a b c");
        assert_eq!(lexer.word(), Ok("a"));
        assert_eq!(lexer.word(), Ok("b"));
        assert_eq!(lexer.word(), Ok("c"));
        assert!(lexer.word().is_err());

        let mut lexer = t("a\nb c");
        assert_eq!(lexer.word(), Ok("a"));
        assert!(lexer.word().is_err());

        let mut lexer = t("a\n  b c");
        assert_eq!(lexer.enter_body(), Ok(None));
        assert_eq!(lexer.enter_body(), Ok(Some("a")));
        assert_eq!(lexer.word(), Ok("b"));
        assert_eq!(lexer.word(), Ok("c"));
        assert!(lexer.word().is_err());
    }

    #[test]
    fn lexer_dedent() {
        // The standard dedent usecase, switch from body line reading mode into
        // block-reading mode in the middle of a struct when you notice you're
        // out of attribute lines.

        let mut lexer = t("\
struct
  a: 1
  b: 2
  this
  is
  contents");
        assert_eq!(lexer.enter_body(), Ok(None));
        assert_eq!(lexer.enter_body(), Ok(Some("struct")));
        assert_eq!(lexer.read(), Ok("a: 1".into()));
        assert_eq!(lexer.read(), Ok("b: 2".into()));

        lexer.dedent();

        assert_eq!(lexer.read(), Ok("this\nis\ncontents".into()));
        assert!(lexer.read().is_err());
    }
}
