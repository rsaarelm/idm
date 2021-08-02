use crate::{
    err, error::Error, error::Result, indent_string::IndentString, parse,
    util::Trunc,
};
use std::borrow::Cow;

// TODO: New Result type to use with Cursor
// pub type Result<'a, T> =
//     std::result::Result<(T, Cursor<'a>), crate::error::Error>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParsingMode {
    /// The most common parsing mode, up to the next element with the same or
    /// higher indent than the current point.
    Block,
    /// Up to the end of the current line only, even if there are more
    /// indented lines after this.
    Line,
    /// Single whitespace-separated token. Do not move to next line.
    Words,
    /// Single whitespace-separated token, must have form "key-name:" (valid
    /// identifier, ends in colon).
    ///
    /// The colon is removed and the symbol is changed from kebab-case to
    /// camel_case when parsing.
    Key,
    /// Like Key, but instead of parsing anything, emit the '_contents' dummy
    /// key.
    DummyKey,
}

impl ParsingMode {
    pub fn is_inline(self) -> bool {
        use ParsingMode::*;
        match self {
            Words | Key | DummyKey => true,
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Shape {
    /// Line at current input depth with no child lines
    BodyLine,
    /// Headline and child lines at current input depth.
    Section,
    /// One or more lines below current input depth.
    Block,
}

use Shape::*;

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
    current_indent: IndentString,
    /// Line number of current input line.
    line_number: i32,
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
        log::debug!("\x1b[1;32mCursor::new({:?})\x1b[0m", Trunc(s));
        Cursor {
            line_start: s,
            input: s,

            current_indent: IndentString::default(),
            line_number: 0,
            mode: ParsingMode::Block,
            // Start at the head of a dummy section encompassing the whole
            // input. Since input's baseline indent is 0, our starting indent
            // for the dummy construct around it is -1.
            current_depth: -1,
            seq_pos: None,
        }
    }

    pub fn input(&self) -> &'a str {
        self.input
    }

    /// Return true if the cursor can enter a new sequence from the current
    /// state.
    ///
    /// If it's already parsing an inline sequence, it can't.
    pub fn can_start_seq(&self) -> bool {
        match self.mode {
            ParsingMode::Line
            | ParsingMode::Words
            | ParsingMode::Key
            | ParsingMode::DummyKey => false,
            _ => true,
        }
    }

    pub fn start_seq(&mut self) -> Result<()> {
        log::debug!("Cursor::start_seq at {:?}", Trunc(self.input));
        if !self.can_start_seq() {
            log::debug!("Cursor::start_seq cannot nest");
            return err!("Nested sequence found in inline sequence");
        }

        match self.classify()? {
            BodyLine => {
                log::debug!("Cursor::start_seq starting inline");
                self.start_words()?;
            }
            Block => {
                log::debug!("Cursor::start_seq starting outline");
                self.start_block()?;
            }
            Section => {
                log::debug!("Cursor::start_seq section sequence is invalid");
                return err!(
                    "start_seq: Section shape not allowed for sequence"
                );
            }
        }

        Ok(())
    }

    pub fn start_tuple(&mut self, _len: usize) -> Result<()> {
        log::debug!("Cursor::start_tuple at {:?}", Trunc(self.input));
        if !self.can_start_seq() {
            log::debug!("Cursor::start_tuple cannot nest");
            return err!("Nested sequence found in inline sequence");
        }

        match self.classify()? {
            BodyLine => {
                log::debug!("Cursor::start_tuple starting inline");
                self.start_words()?;
            }
            Block => {
                log::debug!("Cursor::start_tuple starting outline");
                self.start_block()?;
            }
            Section => {
                log::debug!("Cursor::start_tuple starting section");
                self.start_line()?;
            }
        }

        Ok(())
    }

    pub fn start_map(&mut self) -> Result<()> {
        log::debug!("Cursor::start_map at {:?}", Trunc(self.input));
        if !self.can_start_seq() {
            return err!("Nested sequence found in inline sequence");
        }

        self.start_block()
    }

    pub fn start_struct(
        &mut self,
        _fields: &'static [&'static str],
    ) -> Result<()> {
        log::debug!("Cursor::start_struct at {:?}", Trunc(self.input));
        if !self.can_start_seq() {
            return err!("Nested sequence found in inline sequence");
        }

        self.start_block()
    }

    pub fn end(&mut self) -> Result<()> {
        log::debug!("Cursor::end at {:?}", Trunc(self.input));
        if self.at_end() {
            Ok(())
        } else {
            err!("Cursor::end: Unparsed trailing input {:?}", Trunc(self.input))
        }
    }

    /// Move in position to parse the next sequence token.
    pub fn seq_advance(&mut self) -> Result<()> {
        let old_input = self.input;

        // XXX: Need to do the clone to appease mutable borrow of self.parse.
        let ret = self.parse(
            parse::non_content(&self.current_indent.clone()),
            "seq_advance: Error parsing non-content",
        );

        if old_input != self.input {
            log::debug!(
                "Cursor::seq_advance skipped {:?} to {:?}",
                Trunc(&old_input[..(old_input.len() - self.input.len())]),
                Trunc(self.input)
            );
        }

        ret
    }

    /// Consume the next atomic parsing element as string according to current
    /// parsing mode.
    ///
    /// This can be very expensive, it can read a whole file, so only use it
    /// when you know you need it.
    pub fn next_token(&mut self) -> Result<Cow<str>> {
        use ParsingMode::*;
        log::debug!("Cursor::next_token at {:?}", Trunc(self.input));

        if self.input == "" {
            return err!("next_token: No input left");
        }

        if self.at_negative_column() {
            log::debug!(
                "Cursor::next_token run while at column -1, consuming entire content"
            );
            let ret = Cow::from(self.input.trim_end());
            self.consume_to("");
            return Ok(ret);
        }

        let indent_string = self.current_indent.clone();

        match self.mode {
            Block => {
                let block = self.parse(
                    parse::outline_item(&indent_string),
                    "next_token: Could not parse outline item",
                )?;
                log::debug!(
                    "Cursor::next_token parsed block {:?}",
                    Trunc(&block)
                );
                Ok(Cow::from(block))
            }
            Line => {
                let line =
                    Cow::from(self.parse(parse::line, "Failed to read line")?);
                log::debug!("Cursor::next_token parsed line {:?}", line);
                Ok(line)
            }
            Words => {
                let word =
                    Cow::from(self.parse(parse::word, "Failed to read word")?);
                log::debug!("Cursor::next_token parsed word {:?}", word);
                Ok(word)
            }
            Key => {
                let key = self.parse(parse::key, "Failed to read key")?;
                // Keys are always a one-shot parse.
                self.mode = Block;
                log::debug!("Cursor::next_token parsed key: {:?}", key);
                Ok(Cow::from(key))
            }

            DummyKey => {
                self.mode = Block;
                log::debug!("Cursor::next_token emitted dummy key");
                Ok(Cow::from("_contents"))
            }
        }
    }

    /// Enter an indented body block.
    pub fn start_block(&mut self) -> Result<()> {
        log::debug!("Cursor::start_block at {:?}", Trunc(self.input));

        let (mut new_indent, _) = self
            .current_indent
            .match_next(self.input)
            .map_err(self.err("start_block: Bad indentation"))?;

        if !(new_indent.len() > self.current_indent.len())
        {
            // Create a dummy depth where we'll pop out from immediately.
            // This represents an empty outline sequence.
            new_indent = self.current_indent.dummy_next_depth();
        }

        self.start_file();
        self.current_indent = new_indent;
        self.mode = ParsingMode::Block;
        Ok(())
    }

    pub fn start_line(&mut self) -> Result<()> {
        log::debug!("Cursor::start_line at {:?}", Trunc(self.input));
        if !parse::blank_line(self.input).is_ok() {
            let indent = self.current_indent.clone();
            self.parse(
                |input| indent.match_same(input),
                "start_line: Invalid indent",
            )?;
        }

        self.mode = ParsingMode::Line;
        Ok(())
    }

    pub fn start_words(&mut self) -> Result<()> {
        log::debug!("Cursor::start_words at {:?}", Trunc(self.input));
        let indent = self.current_indent.clone();
        self.parse(
            |input| indent.match_same(input),
            "start_words: Invalid indent",
        )?;
        self.mode = ParsingMode::Words;
        Ok(())
    }

    /// Leave the block entered with `start_block`.
    pub fn end_block(&mut self) -> Result<()> {
        log::debug!("Cursor::end_block at {:?}", Trunc(self.input));
        if self.at_end() {
            // Can exit until back at "-1 indent" at end.
            if !self.current_indent.is_empty() {
                self.current_indent.pop();
            } else {
                return err!("end_block: Exit not matched by indentation");
            }
            return Ok(());
        }

        let (depth, _) = self
            .current_indent
            .match_next(self.input)
            .map_err(self.err("end_block: Bad indent"))?;

        if depth.len() < self.current_indent.len() {
            self.current_indent.pop();
            Ok(())
        } else {
            err!("end_block: Block not empty")
        }
    }

    /// Call when entering the dummy '_contents' region of a struct.
    ///
    /// The actual IDM content is just data at the level of the struct fields,
    /// so this pops the indent level in preparation for parsing the contents
    /// part as a block.
    pub fn prepare_for_contents(&mut self) -> Result<()> {
        log::debug!("Cursor::prepare_for_contents at {:?}", Trunc(self.input));
        self.current_indent.pop();
        Ok(())
    }

    pub fn end_line(&mut self) -> Result<()> {
        if self.input == "" {
            return Ok(());
        }

        log::debug!("Cursor::end_line at {:?}", Trunc(&self.input));
        let line = self.parse(parse::line, "end_line: Failed to read line")?;
        if !line.chars().all(|c| c.is_whitespace()) {
            err!("end_line: Unparsed content left in line")
        } else {
            self.mode = ParsingMode::Block;
            Ok(())
        }
    }

    pub fn has_next_token(&mut self) -> bool {
        // XXX: Can be suboptimal if there are big multiline atoms, but at
        // least it's dead simple.
        return self.clone().next_token().is_ok();
    }

    /// Read attribute key from input.
    ///
    /// Attribute keys must end in colon. They are converted from kebab-caes
    /// to camel_case. So "foo-bar:" becomes "foo_bar".
    pub fn key(&mut self) -> Result<String> {
        self.parse(parse::key, "Expected key")
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
        &mut self,
        f: impl Fn(&'a str) -> parse::Result<T>,
        err_msg: &str,
    ) -> Result<T> {
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

    /// Standalone error formatter.
    fn err<'b>(&'b self, msg: &'b str) -> impl Fn(&str) -> Error + 'b {
        move |remaining_input| {
            // If this was tripped, it indicates a bug in the parser functions
            // where a parser returned an error string that isn't a suffix of
            // the input string. It should not happen no matter what kind of
            // malformed input is fed to the parser.
            debug_assert_eq!(
                &self.input[self.input.len() - remaining_input.len()..],
                remaining_input,
                "Cursor::err: Malformed remaining input slice"
            );

            // Find out how much ahead of the current line number the error
            // site is. (Force line number to be at least 1 to account for the
            // "before start of input" special state.)
            let line_number = self.line_number.max(1)
                + self.input[..self.input.len() - remaining_input.len()]
                    .chars()
                    .filter(|&c| c == '\n')
                    .count() as i32;

            Error(format!("{}: {}", line_number, msg))
        }
    }

    fn at_negative_column(&self) -> bool {
        self.current_indent.is_empty()
    }

    /// Return true if parsing the file hasn't started yet and special parsing
    /// rules may be in effect.
    fn before_file_start(&self) -> bool {
        self.line_number == 0
    }

    /// Mark the file parse as started, `before_file_start` rules will no
    /// longer apply after `start_file` has been called.
    ///
    /// Does nothing after the first time it has been called.
    fn start_file(&mut self) {
        if self.line_number == 0 {
            log::debug!("Cursor::start_file Entering file proper");
            self.line_number = 1;
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
            .count() as i32;

        self.input = &self.input[consumed_len..];
    }

    fn classify(&self) -> Result<Shape> {
        if self.at_negative_column() {
            return Ok(Block);
        }

        let (line, rest) =
            parse::indented_line(&self.current_indent, self.input)
                .map_err(self.err("classify: Couldn't parse indented_line"))?;
        if line.chars().next().unwrap_or(' ').is_whitespace() {
            // XXX: We go here if line is empty, should something else happen?
            Ok(Block)
        } else {
            let (new_indent, _) = self
                .current_indent
                .match_next(rest)
                .map_err(self.err("classify: Bad indent"))?;

            if new_indent.len() > self.current_indent.len() {
                Ok(Section)
            } else {
                Ok(BodyLine)
            }
        }
    }
}

// Shimmed from V01 deser
impl<'a> Cursor<'a> {}

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
    /// At expected depth, given the block separator marker "--",
    /// return None.
    /// Escapes block separator syntax.
    #[deprecated]
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
        if line == "--" {
            // Empty block separator. Consume the line but return `None`.
            Ok(None)
        } else {
            Ok(Some(line))
        }
    }

    #[deprecated]
    pub fn has_headline_content(&self, depth: i32) -> bool {
        self.clone()
            .headline(depth)
            .unwrap_or(None)
            .map_or(false, |s| !s.trim_end().is_empty())
    }

    #[deprecated]
    pub fn at_empty_line(&self) -> bool {
        self.line_depth().is_none()
    }

    fn at_end(&self) -> bool {
        self.input.chars().all(|c| c.is_whitespace())
    }

    #[deprecated]
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
    #[deprecated]
    pub fn is_section(&self, depth: i32) -> bool {
        self.has_headline_content(depth) && self.has_body_content(depth)
    }

    /// Read a verbatim line at given depth.
    ///
    /// Does not escape block separator syntax. Keeps identation past the
    /// given indent level.
    #[deprecated]
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

    /// Read the rest of the current line.
    ///
    /// Does not return content already consumed by `word` or `key`. Never
    /// returns initial line indentation.
    #[deprecated]
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
    #[deprecated]
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

    #[deprecated]
    fn skip_indentation(&mut self) {
        while self.input.chars().next() == Some('\t') {
            self.input = &self.input[1..];
        }
    }
}
