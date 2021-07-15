use crate::{err, error::Error};

pub type Result<T> = std::result::Result<(T, Cursor), Error>;

/// Expected indentation level aware parsing position.
#[derive(Clone, Debug)]
struct Cursor<'a> {
    input: &'a str,
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
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> std::result::Result<Cursor<'a>, Error> {
        if input.chars().next().map_or(false, |c| c.is_whitespace()) {
            return err!("Line 1: Unexpected indentation");
        }
        Ok(Cursor {
            input,
            current_indent: Vec::new(),
            expected_indent: Vec::new(),
            // Special state, not inside the content yet.
            line: 0,
        })
    }

    fn to_next_line(&self) -> Result<()> {
        if self.line > 0 {
            // consume remaining current line up to newline
            let mut next_line = input;
            for (i, c) in self.input.char_indices() {
                if c == '\n' {
                    next_line = &self.input[i + 1..];
                    break;
                }
            }

            if next_line == input {
                // There were no newlines
                self.input = "";
            } else {
                self.input = next_line;
            }
        }
        // TODO: Scan new indentation
        // TODO: Fail if new indentation does not share prefix with expected
        // one
        todo!()
    }

    fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error(format!("Line {}: {}", self.line, msg)))
    }
}

/// Parse a line at current depth that has child lines.
///
/// Return a cursor at the start of the indented child body.
pub fn headline<'a>(input: &'a Cursor) -> Result<&'a str> {
    todo!()
}

/// Parse a line at current depth that does not have child lines.
///
/// Return a cursor at the start of the next body line or at end of the
/// indented block if this was the last body line.
pub fn body_line<'a>(input: &'a Cursor) -> Result<&'a str> {
    todo!()
}

/// Parse next whitespace-separated word from current line
///
/// Return cursor at the start of the word after the parsed one, or at the end
/// of the line if this was the last word.
pub fn word<'a>(input: &'a Cursor) -> Result<&'a str> {
    todo!()
}

/// Parse the remaining content on current depth.
///
/// The indentation prefix of current depth is removed from the output.
pub fn body<'a>(input: &'a Cursor) -> Result<Cow<str>> {
    todo!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_cursor() {}
}
