use crate::{err, lexer::Lexer, lexer::Shape, Error, Result};
use std::{str::FromStr, borrow::Cow};

#[derive(Clone)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub mode: ParsingMode,
    pub seq_pos: Option<SequencePos>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParsingMode {
    /// The most common parsing mode, up to the next element with the same or
    /// higher indent than the current point.
    Block,
    /// Up to the end of the current line only, even if there are more
    /// indented lines after this.
    Line,
    /// Single whitespace-separated token. Do not move to next line.
    Word,
    /// Single whitespace-separated token, must have form "key-name:" (valid
    /// identifier, ends in colon).
    ///
    /// The colon is removed and the symbol is changed from kebab-case to
    /// camel_case when parsing.
    Key,
    /// Like `Key`, but parses nothing and emits the `_contents` special key.
    DummyKey,
}

impl ParsingMode {
    pub fn is_block(self) -> bool {
        use ParsingMode::*;
        match self {
            Block => true,
            _ => false,
        }
    }

    pub fn is_inline(self) -> bool {
        use ParsingMode::*;
        match self {
            Word | Key | DummyKey => true,
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

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        let lexer = Lexer::new(input);
        Parser {
            lexer,
            mode: ParsingMode::Block,
            seq_pos: None,
        }
    }

    pub fn parse_next<T: FromStr>(&mut self) -> Result<T> {
        self.next_token()?
            .parse()
            .map_err(|_| Error::new("Token parse failed"))
    }

    pub fn has_next_token(&mut self) -> bool {
        // XXX: Unoptimized, does extra work that gets thrown out.
        self.clone().next_token().is_ok()
    }

    /// Consume the next atomic parsing element as string according to current
    /// parsing mode.
    ///
    /// This can be very expensive, it can read a whole file, so only use it
    /// when you know you need it.
    pub fn next_token(&mut self) -> Result<Cow<str>> {
        log::debug!("next_token: {:?}", self.lexer);
        use ParsingMode::*;

        if self.lexer.at_eof() {
            return err!("next_token: At EOF");
        }

        match self.mode {
            Block => Ok(Cow::from(self.lexer.read()?)),
            Line => {
                if let Some(head) = self.lexer.enter_body()? {
                    self.mode = Block;
                    Ok(Cow::from(head))
                } else {
                    return err!("next_token: No line");
                }
            }
            Word => Ok(Cow::from(self.lexer.word()?)),
            Key => Ok(Cow::from(self.lexer.key()?)),
            DummyKey => {
                self.mode = Block;
                self.lexer.dedent();
                Ok(Cow::from("_contents"))
            }
        }
    }

    pub fn end(&mut self) -> Result<()> {
        self.lexer.end()
    }

    pub fn verify_seq_startable(&self) -> Result<()> {
        match self.mode {
            ParsingMode::Line
            | ParsingMode::Word
            | ParsingMode::Key
            | ParsingMode::DummyKey => {
                err!("Nested sequence found in inline sequence")
            }
            _ => Ok(()),
        }
    }

    pub fn input(&self) -> &'a str {
        self.lexer.input()
    }
}
