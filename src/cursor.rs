use crate::{err, error::Error, error::Result, parse};
use std::borrow::Cow;

// TODO: New Result type to use with Cursor
// pub type Result<'a, T> =
//     std::result::Result<(T, Cursor<'a>), crate::error::Error>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParsingMode {
    /// The most common parsing mode, up to the next element with the same or
    /// higher indent than the current point.
    ///
    /// Flag set to true means commas should be escaped
    Block(bool),
    /// Up to the end of the current line only, even if there are more
    /// indented lines after this.
    ///
    /// Flag set to true means commas should be escaped
    Line(bool),
    /// Single whitespace-separated token. Do not move to next line.
    Word,
    /// Single whitespace-separated token, must have form "key-name:" (valid
    /// identifier, ends in colon).
    ///
    /// The colon is removed and the symbol is changed from kebab-case to
    /// camel_case when parsing.
    ///
    /// If the magic flag is set, emit '_contents' instead of parsing
    /// anything.
    Key(bool),
}

impl ParsingMode {
    pub fn is_inline(self) -> bool {
        use ParsingMode::*;
        match self {
            Word | Key(_) => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SequencePos {
    /// Currently parsed sequence is at the first element of a tuple. Special
    /// parsing rules may be in effect.
    TupleStart,
    /// Currently parsed sequence is at the last element of a tuple. Special
    /// parsing rules may be in effect.
    TupleEnd,
}

/// State-carrying deserialization cursor.
///
/// `Cursor` stores both the schema information it receives from the
/// deserializer and features of the input structure it infers from the input
/// text.
///
/// A `Cursor` is reasonably lightweight and can be cloned. This can be useful
/// if there are multiple possible parsing tracks and no easy way to
/// distinguish between them until further schema information becomes
/// available.
#[derive(Clone)]
pub struct Cursor<'a> {
    /// At the start of the next input token on current line.
    input: &'a str,

    // New stuff, these become the new cursor
    /// Current stack of indentation whitespace strings.
    current_indent: Vec<&'a str>,
    /// Expected indent level. Same format as `current_indent`
    ///
    /// The shared prefix of current and expected indent must always be
    /// identical. Expected indent level must be synced with current intent
    /// before parsing can continue.
    expected_indent: Vec<&'a str>,
    /// Line number of current input line.
    line_number: usize,
    pub mode: ParsingMode,
    pub current_depth: i32,
    pub seq_pos: Option<SequencePos>,

    // Old cursor stuff, deprecated, remove
    /// At the start of the current input line.
    line_start: &'a str,
}

////////////////////////////////
// Public Cursor API

impl<'a> Cursor<'a> {
    pub fn new(s: &'a str) -> Self {
        Cursor {
            line_start: s,
            input: s,

            current_indent: Vec::new(),
            expected_indent: Vec::new(),
            line_number: 0,
            mode: ParsingMode::Block(true),
            // Start at the head of a dummy section encompassing the whole
            // input. Since input's baseline indent is 0, our starting indent
            // for the dummy construct around it is -1.
            current_depth: -1,
            seq_pos: None,
        }
    }

    pub fn can_nest_seq(&self) -> bool {
        match self.mode {
            ParsingMode::Line(_) | ParsingMode::Word | ParsingMode::Key(_) => {
                false
            }
            _ => true
        }
    }

    pub fn start_seq(&mut self) -> Result<()> {
        if !self.can_nest_seq() {
            return err!("Nested sequence found in inline sequence");
        }

        let is_inline =
            self.has_headline_content(self.current_depth);
        let is_outline =
            self.has_body_content(self.current_depth);
        if is_inline && is_outline {
            return err!("Sequence has both headline and body");
        }

        if is_inline {
            self.mode = ParsingMode::Word;
        } else {
            self.mode = ParsingMode::Block(true);
            self.start_body()?;
        }
        Ok(())
    }

    pub fn start_tuple(&mut self, _len: usize) -> Result<()> {
        if !self.can_nest_seq() {
            return err!("Nested sequence found in inline sequence");
        }

        let is_inline =
            self.has_headline_content(self.current_depth);
        let is_outline =
            self.has_body_content(self.current_depth);

        if is_inline && !is_outline {
            self.mode = ParsingMode::Word;
        } else if !is_inline && is_outline {
            self.mode = ParsingMode::Block(true);
            self.start_body()?;
        } else {
            self.mode = ParsingMode::Line(false);
        }
        Ok(())
    }

    pub fn start_map(&mut self) -> Result<()> {
        if !self.can_nest_seq() {
            return err!("Nested sequence found in inline sequence");
        }

        self.start_body()
    }

    pub fn start_struct(&mut self, _fields: &'static [&'static str]) -> Result<()> {
        if !self.can_nest_seq() {
            return err!("Nested sequence found in inline sequence");
        }

        self.start_body()
    }

    pub fn end(&mut self) -> Result<()> {
        if self.at_end() {
            Ok(())
        } else {
            err!("Unparsed trailing input")
        }
    }
}

// Shimmed from V01 deser, may need refactoring
impl<'a> Cursor<'a> {
    pub fn has_next_token(&mut self) -> bool {
        use ParsingMode::*;
        match self.mode {
            Block(escape_commas) => {
                let has_headline = if escape_commas {
                    self.has_headline(self.current_depth)
                } else {
                    self.has_remaining_line(self.current_depth)
                };
                self.line_depth().map_or(true, |n| n >= self.current_depth)
                    && (has_headline
                        || self.has_body_content(self.current_depth))
            }
            Line(_) | Word | Key(_) => {
                self.has_headline_content(self.current_depth)
            }
        }
    }

    /// Consume the next atomic parsing element as string according to current
    /// parsing mode.
    ///
    /// This can be very expensive, it can read a whole file, so only use it
    /// when you know you need it.
    pub fn next_token(&mut self) -> Result<Cow<str>> {
        use ParsingMode::*;
        match self.mode {
            Block(escape_comma) => {
                let block =
                    self.line_or_block(self.current_depth, escape_comma)?;
                Ok(Cow::from(block))
            }
            Line(true) => {
                let line = self.line()?;
                if !line.is_empty() && line.chars().all(|c| c == ',') {
                    Ok(Cow::from(&line[1..]))
                } else {
                    Ok(Cow::from(line))
                }
            }
            Line(false) => Ok(Cow::from(self.line()?)),
            Word => Ok(Cow::from(self.word()?)),
            Key(emit_dummy_key) => {
                let key = if emit_dummy_key {
                    Ok("_contents".into())
                } else {
                    self.key()
                };
                // Keys are always a one-shot parse.
                //
                // Set block mode to not escaping commas, since the "headline"
                // will be on the already prefixed by the key line as the key
                // instead of on its own line and there
                self.mode = Block(false);
                Ok(Cow::from(key?))
            }
        }
    }

}

////////////////////////////////
// Private Cursor methods

impl<'a> Cursor<'a> {
    /// Run a parse function against current cursor state.
    ///
    /// Makes sure internal line number count is updated and line numbers
    /// propagate to error messages. Make suro to not update cursor's input
    /// position reference outside of this function.
    fn parse<T>(
        &mut self, f: impl Fn(&'a str) -> parse::Result<T>, err_msg: &str) -> Result<T> {
        match f(self.input) {
            Ok((ret, rest)) => {
                self.consume_to(rest);
                Ok(ret)
            }
            Err(rest) => {
                self.consume_to(rest);
                Err(Error(format!("{}: {}", self.line_number.max(1), err_msg)))
            }
        }
    }

    /// Consume input until it matches 'remaining_input'.
    ///
    /// This updates line numbers, you should not use other methods to update
    /// self.input.
    fn consume_to(&mut self, remaining_input: &str) {
        let consumed_len = self.input.len() - remaining_input.len();

        if consumed_len == 0 {
            return;
        }

        debug_assert_eq!(
            &self.input[consumed_len..],
            remaining_input,
            "Cursor::consume_to: Malformed remaining input slice"
        );

        // Exit the "outside of the file" special state the moment any input
        // is seen.
        if self.line_number == 0 {
            self.line_number = 1;
        }

        self.line_number += self.input[..consumed_len]
            .chars()
            .filter(|&c| c == '\n')
            .count();

        self.input = &self.input[consumed_len..];
    }
}

// Shimmed from V01 deser
impl<'a> Cursor<'a> {
    /// Move cursor to start of the current headline's body.
    pub fn start_body(&mut self) -> Result<()> {
        let depth = self.line_depth();
        if depth.map_or(false, |n| n > self.current_depth) {
            // Missing headline (but there is content, empty lines don't
            // count), we're done with just incrementing current
            // depth.
            self.current_depth += 1;
            Ok(())
        } else if depth.map_or(true, |n| n == self.current_depth) {
            // There is some headline, we need to move past it.
            // Empty headlines are counted here, since we need to skip over
            // the line.
            if self.has_headline_content(self.current_depth) {
                return err!("start_body: Unparsed headline input");
            }

            let _ = self.headline(self.current_depth);
            self.current_depth += 1;
            Ok(())
        } else {
            err!("start_body: Out of depth")
        }
    }

    pub fn end_body(&mut self) -> Result<()> {
        if self.at_end() && self.current_depth > -1 {
            // Can exit until -1 at EOF.
            self.current_depth -= 1;
            return Ok(());
        }

        let depth = self.line_depth();
        if depth.map_or(false, |n| n < self.current_depth) {
            self.current_depth -= 1;
            Ok(())
        } else if depth.map_or(true, |n| n == self.current_depth)
            && !self.has_headline_content(self.current_depth)
            && !self.has_body_content(self.current_depth)
        {
            // No current content, see if next line is out of body
            let next_depth = self.next_line_depth();
            if next_depth.map_or(false, |n| n < self.current_depth) {
                self.line()?;
                self.current_depth -= 1;
                Ok(())
            } else {
                err!("end_body: Body not empty")
            }
        } else {
            err!("end_body: Body not empty")
        }
    }

    pub fn end_line(&mut self) -> Result<()> {
        if self.has_headline_content(self.current_depth) {
            err!("end_line: Unparsed content left in line")
        } else {
            let _ = self.line();
            Ok(())
        }
    }
}

////////////////////////////////
// V01 cursor logic
// Deprecated. Get rid of this when V02 is working.

impl<'a> Cursor<'a> {
    /// Read headline at current position given a depth.
    ///
    /// Consume headline from input if found.
    /// If content is deeper than expected, missing headline is implied,
    /// return None.
    /// Fail if content is above the given depth.
    /// At expected depth, given the block separator marker ",",
    /// return None.
    /// Escapes block separator syntax.
    fn headline(&mut self, depth: i32) -> Result<Option<&str>> {
        if self.input.is_empty() {
            return err!("headline: Out of input");
        }

        if let Some(line_depth) = self.line_depth() {
            if line_depth > depth {
                return Ok(None);
            } else if line_depth < depth {
                return err!("headline: Above given depth");
            }
        } else {
            // Consume the empty line.
            self.line()?;
            return Ok(Some(""));
        }

        self.skip_indentation();

        let line = self.line()?.trim_end();
        if line == "," {
            // Empty block separator. Consume the line but return `None`.
            Ok(None)
        } else if !line.is_empty() && line.chars().all(|c| c == ',') {
            // Unescape escaped comma.
            Ok(Some(&line[1..]))
        } else {
            Ok(Some(line))
        }
    }

    fn has_headline(&self, depth: i32) -> bool {
        self.clone().headline(depth).ok().is_some()
    }

    /// Like headline, but does not escape commas.
    fn remaining_line(&mut self, depth: i32) -> Result<Option<&str>> {
        if self.input.is_empty() {
            return err!("headline: Out of input");
        }

        if let Some(line_depth) = self.line_depth() {
            if line_depth > depth {
                return Ok(None);
            } else if line_depth < depth {
                return err!("headline: Above given depth");
            }
        } else {
            // Consume the empty line.
            self.line()?;
            return Ok(Some(""));
        }

        self.skip_indentation();

        let line = self.line()?.trim_end();
        Ok(Some(line))
    }

    fn has_remaining_line(&self, depth: i32) -> bool {
        self.clone().remaining_line(depth).ok().is_some()
    }

    pub fn has_headline_content(&self, depth: i32) -> bool {
        self.clone()
            .headline(depth)
            .unwrap_or(None)
            .map_or(false, |s| !s.trim_end().is_empty())
    }

    pub fn at_empty_line(&self) -> bool {
        self.line_depth().is_none()
    }

    fn at_end(&self) -> bool {
        self.input.chars().all(|c| c.is_whitespace())
    }

    fn has_body_content(&self, depth: i32) -> bool {
        if self.input == "" {
            return false;
        }
        // Already in body depth at current line
        if self.line_depth().map_or(false, |n| n > depth) {
            return true;
        }
        let mut cursor = self.clone();
        let _ = cursor.line();
        if cursor.input == "" {
            return false;
        }
        // Next line is at body depth.
        // (But if it's empty, there's no *content*...)
        cursor.line_depth().map_or(false, |n| n > depth)
    }

    /// Content is section-shaped, ie. has both headline and body content.
    pub fn is_section(&self, depth: i32) -> bool {
        self.has_headline_content(depth) && self.has_body_content(depth)
    }

    /// Read a verbatim line at given depth.
    ///
    /// Does not escape block separator syntax. Keeps identation past the
    /// given indent level.
    pub fn verbatim_line(&mut self, depth: i32) -> Result<&'a str> {
        debug_assert!(
            self.line_start == self.input,
            "verbatim_line: Calling on partially consumed line"
        );
        assert!(depth >= 0, "Can't construct line for depth -1");

        if let Ok((line, rest)) = parse::line(self.line_start) {
            let result = if let Some(line_depth) = self.line_depth() {
                if line_depth >= depth {
                    &line[(depth as usize)..]
                } else {
                    // Looks like requested indentation is too deep, no go.
                    return err!("verbatim_line: Out of depth");
                }
            } else {
                // Empty line has whatever depth you want it to have.
                ""
            };
            self.input = rest;
            self.line_start = rest;
            Ok(result)
        } else {
            err!("verbatim_line: EOF")
        }
    }

    /// Read successive verbatim lines at at least given depth.
    ///
    /// Fails when no lines were read.
    fn block(&mut self, depth: i32) -> Result<String> {
        if self.input.is_empty() {
            return err!("Out of input");
        }

        let mut result = String::new();
        let mut first = true;
        while let Ok(line) = self.verbatim_line(depth.max(0)) {
            // Newlines before subsequent lines.
            if !first {
                result.push('\n');
            } else {
                first = false;
            }
            let line = line.trim_end();

            // Generated indentation for negative depth.
            // (But only for non-empty lines, no trailing space.)
            if !line.is_empty() {
                for _ in 0..(-depth) {
                    result.push('\t');
                }
                result.push_str(line);
            }
        }
        if first {
            err!("No block content found")
        } else {
            Ok(result)
        }
    }

    /// Reads next outline element that's either a line or a block.
    ///
    /// A line element is a single line at given depth with no body. A block
    /// element has an empty headline at the given depth and one or more body
    /// lines above the given depth. The lines of the block element will have
    /// `depth + 1` indent levels dedented from them.
    ///
    /// An element with both a headline with content and body lines will
    /// result an error.
    fn line_or_block(
        &mut self,
        depth: i32,
        escape_comma: bool,
    ) -> Result<String> {
        let has_body = self.has_body_content(depth);

        if let Some(s) = if escape_comma {
            self.headline(depth)?
        } else {
            self.remaining_line(depth)?
        } {
            if !s.is_empty() {
                if has_body {
                    return err!(
                        "line_or_block: Section-shaped element, has both parts"
                    );
                }
                return Ok(s.to_string());
            } else {
                if !has_body {
                    return Ok("".into());
                }
            }
        }

        if has_body {
            // Block separator headline will have been consumed by
            // self.headline.
            Ok(self.block(depth + 1)?)
        } else {
            err!("line_or_block: No input")
        }
    }

    /// Read next whitespace-separated word from current line.
    fn word(&mut self) -> Result<&str> {
        // Try it out in a clone so we don't change state if no words are
        // found.
        let mut cursor = self.clone();
        cursor.skip_indentation();
        let word = cursor.parse(parse::word, "Expected word")?;
        *self = cursor;
        Ok(word)
    }

    /// Read attribute key from input.
    ///
    /// Attribute keys must end in colon. They are converted from kebab-caes
    /// to camel_case. So "foo-bar:" becomes "foo_bar".
    pub fn key(&mut self) -> Result<String> {
        let mut cursor = self.clone();
        cursor.skip_indentation();
        let key = cursor.parse(parse::key, "Expected key")?;
        *self = cursor;
        Ok(key)
    }

    /// Read the rest of the current line.
    ///
    /// Does not return content already consumed by `word` or `key`. Never
    /// returns initial line indentation.
    pub fn line(&mut self) -> Result<&str> {
        let mut cursor = self.clone();
        cursor.skip_indentation();

        if let Ok((line, rest)) = parse::line(cursor.input) {
            *self = cursor;
            self.line_start = rest;
            self.consume_to(rest);
            Ok(line)
        } else {
            err!("end_line: End of input")
        }
    }

    /// Return line_depth for a line with content.
    ///
    /// Line depth does not make sense for blank lines, so those get `None`.
    fn line_depth(&self) -> Option<i32> {
        if let Ok((line, _)) = parse::line(self.line_start) {
            if line.chars().all(|c| c.is_whitespace()) {
                None
            } else {
                Some(line.chars().take_while(|c| *c == '\t').count() as i32)
            }
        } else {
            None
        }
    }

    fn next_line_depth(&self) -> Option<i32> {
        let mut cursor = self.clone();
        let _ = cursor.line();
        // Try reverting to depth of current line (if that is available), when
        // next line is blank but we're not at the end of input.
        if cursor.input.is_empty() {
            None
        } else {
            cursor.line_depth().or_else(|| self.line_depth())
        }
    }

    fn skip_indentation(&mut self) {
        while self.input.chars().next() == Some('\t') {
            self.input = &self.input[1..];
        }
    }
}
