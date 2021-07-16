use crate::{err, error::Result, parser};

// TODO: New Result type to use with Cursor
// pub type Result<'a, T> =
//     std::result::Result<(T, Cursor<'a>), crate::error::Error>;

/// Low-level text processing for deserializer.
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
    line: usize,

    // Old cursor stuff, deprecated, remove
    /// At the start of the current input line.
    line_start: &'a str,
}

impl<'a> Cursor<'a> {
    pub fn new(s: &'a str) -> Self {
        Cursor {
            line_start: s,
            input: s,

            current_indent: Vec::new(),
            expected_indent: Vec::new(),
            line: 0,
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
    pub fn headline(&mut self, depth: i32) -> Result<Option<&str>> {
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

    pub fn has_headline(&self, depth: i32) -> bool {
        self.clone().headline(depth).ok().is_some()
    }

    /// Like headline, but does not escape commas.
    pub fn remaining_line(&mut self, depth: i32) -> Result<Option<&str>> {
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

    pub fn has_remaining_line(&self, depth: i32) -> bool {
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

    pub fn at_end(&self) -> bool {
        self.input.chars().all(|c| c.is_whitespace())
    }

    pub fn has_body_content(&self, depth: i32) -> bool {
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

        if let Ok((line, rest)) = parser::line(self.line_start) {
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
    pub fn block(&mut self, depth: i32) -> Result<String> {
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
    pub fn line_or_block(
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
    pub fn word(&mut self) -> Result<&str> {
        // Try it out in a clone so we don't change state if no words are
        // found.
        let mut cursor = self.clone();
        cursor.skip_indentation();
        let word = parser::read(&mut cursor.input, parser::word)?;
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
        let word = parser::read(&mut cursor.input, parser::key)?;
        *self = cursor;
        Ok(word)
    }

    /// Read the rest of the current line.
    ///
    /// Does not return content already consumed by `word` or `key`. Never
    /// returns initial line indentation.
    pub fn line(&mut self) -> Result<&str> {
        let mut cursor = self.clone();
        cursor.skip_indentation();

        if let Ok((line, rest)) = parser::line(cursor.input) {
            *self = cursor;
            self.line_start = rest;
            self.input = rest;
            Ok(line)
        } else {
            err!("exit_line: End of input")
        }
    }

    /// Return line_depth for a line with content.
    ///
    /// Line depth does not make sense for blank lines, so those get `None`.
    pub fn line_depth(&self) -> Option<i32> {
        if let Ok((line, _)) = parser::line(self.line_start) {
            if line.chars().all(|c| c.is_whitespace()) {
                None
            } else {
                Some(line.chars().take_while(|c| *c == '\t').count() as i32)
            }
        } else {
            None
        }
    }

    pub fn next_line_depth(&self) -> Option<i32> {
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
