use crate::{
    err,
    error::{Error, Result},
    lexer::Lexer,
};
use std::borrow::Cow;

/// IDM parsing state machine.
pub struct Parser<'a> {
    pub mode: ParsingMode,
    pub seq_pos: Option<SequencePos>,
    lexer: Lexer<'a>,
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SequencePos {
    /// Currently parsed sequence is at the first element of a tuple. Special
    /// parsing rules may be in effect.
    TupleStart,
    /// Currently parsed sequence is at the last element of a tuple. Special
    /// parsing rules may be in effect.
    TupleEnd,
}

// Public methods

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Parser<'a> {
        let ret = Parser {
            mode: ParsingMode::Block,
            seq_pos: None,
            lexer: Lexer::new(input),
        };
        log::debug!("\x1b[1;32mParser::new: {}\x1b[0m", ret.lexer);
        ret
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

    /// Consume the next atomic parsing element as string according to current
    /// parsing mode.
    ///
    /// This can be very expensive, it can read a whole file, so only use it
    /// when you know you need it.
    pub fn next_token(&mut self) -> Result<Cow<str>> {
        use ParsingMode::*;
        log::debug!("Parser::next_token at {}", self.lexer);

        match self.mode {
            Block => Ok(Cow::from(self.lexer.read()?)),
            Line => {
                if let Some(headline) = self.lexer.enter_body()? {
                    self.mode = Block;
                    Ok(Cow::from(headline))
                } else {
                    return self
                        .lexer
                        .err("Parser::next_token: No headline for Line mode");
                }
            }
            Words => match self.lexer.word() {
                Err(_) => err!("next_token failed to read word"),
                Ok(word) => Ok(Cow::from(word)),
            },
            Key => {
                todo!();
                /*
                let key = self.parse(parse::key, "Failed to read key")?;
                // Keys are always a one-shot parse.
                self.mode = Block;
                log::debug!("Cursor::next_token parsed key: {:?}", key);
                Ok(Cow::from(key))
                */
            }

            DummyKey => {
                self.mode = Block;
                log::debug!("Cursor::next_token emitted dummy key");
                Ok(Cow::from("_contents"))
            }
        }
    }
}

// Private methods

impl<'a> Parser<'a> {
    // TODO
}
