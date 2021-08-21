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
#[derive(Clone, Eq, PartialEq)]
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
    indent_segments: Vec<usize>,

    /// When true, elements at current indent level are read as one monolithic
    /// block.
    in_block_mode: bool,

    /// The remaining input to be lexed.
    input: &'a str,

    /// Inline input position when reading words.
    inline_input: Option<&'a str>,

    /// The initial input, used for determining error line numbers.
    input_start: &'a str,
}

/// IDM element shape.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Shape<'a> {
    /// A section with a headline and body lines.
    ///
    /// Will be read as `"a\n  b\n  c"` by `Lexer::read` (all lines after
    /// the first are indented).
    ///
    /// The value contains the headline of the section.
    Section(&'a str),
    /// A block of body lines without a headline.
    ///
    /// Will be read as `"b\nc"` by `Lexer::read` (all of the block lines
    /// start at zero indentation).
    Block,
    /// A single line with no child lines.
    ///
    /// Contains the line contents.
    BodyLine(&'a str),
}

impl<'a> Shape<'a> {
    pub fn is_blank(&self) -> bool {
        matches!(self, Shape::BodyLine(s) if s.chars().all(|c| c.is_whitespace()))
    }

    pub fn is_standalone_comment(&self) -> bool {
        matches!(self, Shape::BodyLine(s) if s.starts_with("--"))
    }

    pub fn has_comment_head(&self) -> bool {
        matches!(self, Shape::BodyLine(s) if s.starts_with("--"))
            || matches!(self, Shape::Section(s) if s.starts_with("--"))
    }
}

// Utility public methods
// (Implemented in terms of core public methods)

impl<'a> Lexer<'a> {
    /// Classify the shape at the current lexer position.
    pub fn classify(&self) -> Option<Shape> {
        log::debug!("Lexer::classify: {:?}", self);
        let mut probe = self.clone();
        match probe.enter_body() {
            Ok(None) => {
                if probe.exit_body().is_err() {
                    log::debug!("Lexer::classify: Block");
                    Some(Shape::Block)
                } else {
                    log::debug!("Lexer::classify: Empty");
                    None
                }
            }
            Ok(Some(content)) => {
                if probe.exit_body().is_err() {
                    log::debug!("Lexer::classify: Section");
                    Some(Shape::Section(content))
                } else {
                    log::debug!("Lexer::classify: BodyLine");
                    Some(Shape::BodyLine(content))
                }
            }
            Err(_) => None,
        }
    }

    pub fn exit_words(&mut self) -> Result<()> {
        log::debug!("Lexer::exit_words: {:?}", self);

        if self.content() == "" {
            return Ok(());
        }

        if let Some(headline) = self.enter_body()? {
            if !headline.chars().all(|c| c.is_whitespace()) {
                return err!("Lexer::exit_words Unparsed input left on line");
            }
        }
        self.exit_body()?;
        Ok(())
    }

    /// Parse struct key
    pub fn key(&mut self) -> Result<String> {
        if self.inline_input.is_some() {
            return self.err("Lexer::key Key must be at start of line");
        }

        // Read word, convert from kebab-case to camel_case.
        let mut word = self.word()?.replace("-", "_");

        if !word.ends_with(':') || word == ":" {
            return self.err(format!("Lexer::key invalid key {:?}", word));
        }

        // Remove the trailing :
        word.pop();

        Ok(word)
    }

    /// Skip over the next element that would otherwise have been read with
    /// `read`.
    pub fn skip(&mut self) -> Result<()> {
        log::debug!("Lexer::skip");
        // XXX: Unoptimized, does work that gets thrown out.
        self.read()?;
        Ok(())
    }

    pub fn at_eof(&self) -> bool {
        self.content().chars().all(|c| c.is_whitespace())
    }
}

// Core public methods

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            indent_char: Default::default(),
            indent_segments: Default::default(),
            in_block_mode: true,
            input,
            inline_input: None,
            input_start: input,
        }
    }

    /// Succeed if only whitespace remains of input.
    pub fn end(&mut self) -> Result<()> {
        log::debug!("Lexer::end");
        for c in self.content().chars() {
            if !c.is_whitespace() {
                return self.err("Lexer::end Unparsed input remains");
            }
        }
        self.set_input("");
        self.indent_segments.clear();
        Ok(())
    }

    /// Enter body of current headline.
    ///
    /// Return the headline if it exists. If the lexer is in block mode,
    /// headline does not exist and `None` will be returned.
    ///
    /// The lexer will enter a deeper indentation depth that must be exited
    /// with `dedent` or `exit_body` even if there are no body lines to the
    /// section.
    ///
    /// Will fail when there is neither a headline (lexer is in block mode or
    /// indented to an empty body) nor any body lines.
    ///
    /// It should be reasonably cheap to call `enter_body` on an avergae
    /// input, feel free to use this for probing input with clones of the
    /// lexer.
    pub fn enter_body(&mut self) -> Result<Option<&'a str>> {
        log::debug!("Lexer::enter_body: {:?}", self);
        // At EOF.
        // - No headline obviously, but you can still churn indent and dedent if
        //   you want.
        if self.input == "" {
            if self.in_block_mode {
                self.in_block_mode = false;
            } else {
                self.indent_segments.push(1);
            }
            return Ok(None);
        }

        let (current_prefix, _) = parse::indent(self.input)?;
        let new_segments: Vec<usize> = self.match_indent(current_prefix)?;

        // Current indent is shorter than expected indent.
        // - Can't have headline or body, fail out
        if new_segments.len() < self.indent_segments.len() {
            return self.err("Lexer::enter_body no content");
        }

        self.witness_indentation_char(current_prefix)?;

        if self.in_block_mode {
            if new_segments.len() > self.indent_segments.len() {
                return self.err("Lexer::enter_body Unexpected indentation.");
            }
            self.in_block_mode = false;
            return Ok(None);
        }

        // A headline exists if content starts *exactly* at indent string.
        let (content, rest) = parse::line(self.content())?;

        // Current indent is longer than expected indent.
        // - Headline is None.
        // - Do not move input, first line of input already belongs to body.
        if new_segments.len() > self.indent_segments.len() {
            self.indent_segments = new_segments;
            return Ok(None);
        }

        debug_assert!(new_segments.len() == self.indent_segments.len());

        // Now we can update input to the next line.
        self.set_input(rest);

        // Headline indent as expected, but headline is blank.
        //  - Move to next line
        //  - Cannot have children (because of how indent for blank lines is
        //    determined), set synthetic +1 indent level.
        //  - Return Some("") for headline
        if content.chars().all(|c| c.is_whitespace()) {
            self.indent_segments.push(1);
            return Ok(Some(""));
        }

        // Headline indent as expected, headline has content
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
        Ok(Some(content))
    }

    /// Force the lexer into block mode.
    ///
    /// All the remaining lines at the current depth will now be parsed as a
    /// single element.
    pub fn force_block_mode(&mut self) {
        log::debug!("Lexer::force_block_mode");
        self.in_block_mode = true;
    }

    /// Pop out of current indented body.
    ///
    /// At depth 0, switch from outline mode to block mode. If already at
    /// block mode at depth 0, trying to dedent further will panic.
    pub fn dedent(&mut self) {
        if self.indent_segments.pop().is_none() {
            if !self.in_block_mode {
                self.in_block_mode = true;
            } else {
                panic!("Lexer::dedent can't dedent further");
            }
        }
        self.in_block_mode = false;
    }

    /// Pop out of current indented body when there is only white space left
    /// in the body. Will fail if body still has content.
    ///
    /// Should be cheap to call, feel free to use with cloned lexers.
    pub fn exit_body(&mut self) -> Result<()> {
        log::debug!("Lexer::exit_body: {:?}", self);
        let (body_prefix, _) = parse::indent(self.input)?;
        let body_segments = self.match_indent(body_prefix)?;
        if body_segments.len() < self.indent_segments.len() {
            self.dedent();
            Ok(())
        } else if self.indent_segments.is_empty() && self.in_block_mode {
            panic!("Lexer::exit_body: Exiting body at column 0 block mode");
        } else if self.content().chars().all(|c| c.is_whitespace()) {
            self.dedent();
            Ok(())
        } else {
            self.err("Lexer::exit_body: Unparsed input remains")
        }
    }

    /// Extract a single word from the current line, move lexer to start of
    /// next word on the same line or end of line, whichever is closest.
    ///
    /// Failure from `word` does not invalidate the lexer.
    pub fn word(&mut self) -> Result<&str> {
        log::debug!("Lexer::word");
        let input = self.content();

        if input == "" {
            return self.err("Lexer::word At EOF");
        }

        let (line, _) = parse::line(input)?;
        if line.chars().all(|c| c.is_whitespace()) {
            return self.err("Lexer::word No more words on line");
        }

        let mut end_pos = line.len();
        for (i, c) in line.char_indices() {
            if c.is_whitespace() {
                end_pos = i;
                break;
            }
        }

        let mut skip = 0;
        for (i, c) in line[end_pos..].char_indices() {
            skip = i;
            if c == '\n' || !c.is_whitespace() {
                break;
            }
        }

        let word = &input[..end_pos];
        self.inline_input = Some(&input[end_pos + skip..]);

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
        log::debug!("Lexer::read: {:?}", self);
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

    pub fn input(&self) -> &'a str {
        self.input
    }
}

// Private methods

impl<'a> Lexer<'a> {
    fn numerize(&self) -> impl FnOnce(Error) -> Error + '_ {
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
        self.match_indent(current_prefix)?;

        if current_prefix.len() < indent_len {
            return self.err("Lexer::read_into out of depth");
        }

        let has_headline =
            current_prefix.len() == indent_len && !self.in_block_mode;

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

            let content = if let Some(content) = self.inline_input {
                if content == "" {
                    ""
                } else {
                    parse::line(content).map_err(self.numerize())?.0
                }
            } else if line.chars().all(|c| c.is_whitespace()) {
                ""
            } else {
                &line[current_prefix.len()..]
            };

            if content.chars().all(|c| c.is_whitespace()) {
                // Blank line.
                buffer.push('\n');
            } else {
                buffer.push_str(content);
                buffer.push('\n');
            }
            self.set_input(rest);
        }
        Ok(())
    }

    /// Match an indentation prefix with the current indent segment state.
    ///
    /// If the indentation does not match the current input string, either
    /// by segment joints or indent char, fail.
    fn match_indent(&self, prefix: &str) -> Result<Vec<usize>> {
        // Empty segment is always good.
        if prefix == "" {
            return Ok(Vec::new());
        }

        // When in block mode, lines can't introduce an indent level.
        if self.in_block_mode
            && prefix.len() > self.indent_segments.iter().sum()
        {
            return self.err("Lexer::match_indent Unxpected indent");
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

    /// Current point where reading content should start.
    ///
    /// If words have been read, this will be at the middle of the line.
    /// Otherwise it'll be at the start of the content on the line. If the
    /// line is blank, this will be right before the newline.
    fn content(&self) -> &'a str {
        if let Some(input) = self.inline_input {
            input
        } else {
            // Not in inline input mode yet. Get started by creating an
            // inline_input pointer at the start of content on the line.
            let mut pos = self.input.len();
            for (i, c) in self.input.char_indices() {
                if !c.is_whitespace() || c == '\n' {
                    pos = i;
                    break;
                }
            }

            &self.input[pos..]
        }
    }

    /// Set input pointer and reset `inline_input`.
    ///
    /// The new input position is assumed to always be at the start of a line.
    fn set_input(&mut self, new_pos: &'a str) {
        self.input = new_pos;
        self.inline_input = None;
    }
}

impl<'a> fmt::Debug for Lexer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Custom debug formatter that cuts down an over-long input string.
        const MAX_LEN: usize = 72;

        // TODO: Visualize inline-input state and indentation as parts of
        // printed string
        write!(
            f,
            "{{ indent: {:?} x {:?}{}, ",
            self.indent_char.unwrap_or('\0'),
            self.indent_segments,
            if self.in_block_mode { '#' } else { '=' }
        )?;

        let input = self.inline_input.unwrap_or(self.input);
        if input.len() > MAX_LEN {
            let cut_pos = input
                .char_indices()
                .take(MAX_LEN - 3)
                .map(|(i, _)| i)
                .last()
                .unwrap_or(0);
            write!(f, "{:?}...", &input[..cut_pos])?;
        } else {
            write!(f, "{:?}", input)?;
        }
        write!(f, " }}")
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
        assert_eq!(t("").enter_body(), Ok(None));

        // Lexer starts in block mode
        let mut lexer = t("a");
        // Get out of block mode.
        assert_eq!(lexer.enter_body(), Ok(None));
        // Enter the line.
        assert_eq!(lexer.enter_body(), Ok(Some("a")));
    }

    #[test]
    fn lexer_exit_body() {
        let mut lexer = t("a");
        assert_eq!(lexer.enter_body(), Ok(None));
        assert!(lexer.clone().exit_body().is_err(), "Unconsumed 'a'");
        assert_eq!(lexer.enter_body(), Ok(Some("a")));
        assert!(lexer.exit_body().is_ok());
        assert!(lexer.exit_body().is_ok());
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
    fn lexer_heading_whitespace() {
        // First line of content must show up at indent 0.
        assert!(t("  a").read().is_err());
        assert!(t("  a").enter_body().is_err());

        assert!(t("\n  a").read().is_err());
        assert!(t("\n  a").enter_body().is_err());
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
    fn lexer_block_mode() {
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

        lexer.force_block_mode();

        assert_eq!(lexer.read(), Ok("this\nis\ncontents".into()));
        assert!(lexer.read().is_err());
    }

    #[test]
    fn lexer_words_and_line_read() {
        let mut lexer = t("\
--
  a b c
  d");
        assert_eq!(lexer.enter_body(), Ok(None));
        assert_eq!(lexer.enter_body(), Ok(Some("--")));
        assert_eq!(lexer.word(), Ok("a"));

        // Calling enter-body mid-words-parse
        assert_eq!(lexer.clone().enter_body(), Ok(Some("b c")));

        // Calling read mid-words-parse
        let mut lexer_2 = lexer.clone();
        assert_eq!(lexer_2.read(), Ok("b c".into()));
        assert_eq!(lexer_2.read(), Ok("d".into()));

        assert_eq!(lexer.word(), Ok("b"));
        assert!(lexer.clone().exit_words().is_err());
        assert_eq!(lexer.word(), Ok("c"));
        assert!(lexer.clone().word().is_err());
        assert!(lexer.exit_words().is_ok());

        assert_eq!(lexer.enter_body(), Ok(Some("d")));
        assert!(lexer.end().is_ok());
    }

    #[test]
    fn lexer_end_with_words() {
        // Baseline.
        assert!(t("a").end().is_err());
        assert!(t("").end().is_ok());

        // Regular consume

        let mut lexer = t("a");
        lexer.enter_body().unwrap();
        assert!(lexer.clone().end().is_err());
        lexer.enter_body().unwrap();
        assert!(lexer.end().is_ok());

        let mut lexer = t("a");
        lexer.enter_body().unwrap();
        lexer.read().unwrap();
        assert!(lexer.end().is_ok());

        // Word reading version
        let mut lexer = t("a");
        lexer.enter_body().unwrap();
        lexer.word().unwrap();
        assert!(lexer.end().is_ok());
    }

    #[test]
    fn lexer_key() {
        assert!(t("").key().is_err());
        assert!(t("a").key().is_err());
        assert!(t(":").key().is_err());

        assert_eq!(t("a:").key(), Ok("a".into()));
        assert_eq!(t("xyzzy:").key(), Ok("xyzzy".into()));
        assert_eq!(t("foo-bar:").key(), Ok("foo_bar".into()));

        let mut lexer = t("a bad:");
        assert_eq!(lexer.word(), Ok("a"));
        // Fail if key is not at start of line.
        assert!(lexer.key().is_err());
    }

    #[test]
    fn lexer_read_empty() {
        let mut lexer = t("--\n\n\t1");
        lexer.enter_body().unwrap();
        assert_eq!(lexer.enter_body(), Ok(Some("--")));
        assert_eq!(lexer.read(), Ok("".into()));
        assert_eq!(lexer.read(), Ok("1".into()));
    }
}
