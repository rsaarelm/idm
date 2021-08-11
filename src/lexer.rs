pub type Result<'a, T> = std::result::Result<T, &'a str>;

/// Low-level indented text parsing primitives.
///
/// A lexer instance should not be used after it has returned an error from
/// any of the methods. Use a clone of the main lexer when doing recoverable
/// probing operations and call primitives on the main lexer when their
/// failure will fail the entire parse.
#[derive(Clone, Eq, PartialEq)]
pub struct Lexer<'a> {
    input: &'a str,
    indent_char: Option<char>,
    indent_segments: Vec<usize>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Result<Lexer> {
        // TODO: Fail if first line does not have zero indent
        Ok(Lexer {
            input,
            indent_char: Default::default(),
            indent_segments: Default::default(),
        })
    }

    /// Succeed if only whitespace remains of input.
    pub fn end(&mut self) -> Result<()> {
        todo!();
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
    pub fn start_body(&mut self) -> Result<Option<&str>> {
        todo!();
    }

    /// Pop out of current indented body even if there is more body content
    /// left.
    ///
    /// Used to parse content for the special `_contents` struct field.
    pub fn dedent(&mut self) -> Result<()> {
        todo!();
    }

    /// Pop out of current indented body when there is only white space left
    /// in the body. Will fail if body still has content.
    pub fn end_body(&mut self) -> Result<()> {
        todo!();
    }

    /// Extract a single word from the current line, move lexer to start of
    /// next word on the same line or end of line, whichever is closest.
    pub fn word(&mut self) -> Result<&str> {
        todo!();
    }

    /// Read section at lexer into string with the indentation up to headline
    /// removed for each line.
    ///
    /// If run when there is no headline, only the body is read and
    /// indentation is removed up to body level.
    pub fn section(&mut self) -> Result<String> {
        todo!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        assert!(false);
    }
}
