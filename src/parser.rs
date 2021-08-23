use crate::{
    err,
    lexer::{Lexer, Shape},
    Error, Result,
};
use std::{borrow::Cow, str::FromStr};

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
    /// The rest of a line, then fall back to previous level.
    Line,
    /// The rest of a line, then enter the body.
    Headline,
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
                    // And back down again, body must be empty.
                    self.lexer.exit_body()?;
                    self.mode = Block;
                    Ok(Cow::from(head))
                } else {
                    return err!("next_token: No line");
                }
            }
            Headline => {
                if let Some(head) = self.lexer.enter_body()? {
                    // Stay inside body, continue to body lines.
                    self.lexer.force_block_mode();
                    self.mode = Block;
                    Ok(Cow::from(head))
                } else {
                    return err!("next_token: No headline");
                }
            }
            Word => Ok(Cow::from(self.lexer.word()?)),
            Key => {
                let ret = Cow::from(self.lexer.key()?);
                // Smart next mode.
                match self.lexer.classify() {
                    None => {
                        return err!("next_token: Key without value");
                    }
                    Some(Shape::Section(headline)) => {
                        if !headline.chars().all(|c| c.is_whitespace()) {
                            // Must be either inline or block.
                            return err!(
                                "next_token: Section-shaped value for key"
                            );
                        }
                        log::debug!(
                            "next_token: Preparing for outline value for key"
                        );
                        self.lexer.enter_body()?;
                        self.lexer.force_block_mode();
                        log::debug!("next_token: Entered value body {:?}", self.lexer);
                        self.mode = Block;
                    }
                    Some(Shape::BodyLine(_)) => {
                        log::debug!(
                            "next_token: Preparing for inline value for key"
                        );
                        self.mode = Line;
                    }
                    Some(Shape::Block) => {
                        return err!("next_token: Block shape after key");
                    }
                }
                Ok(ret)
            }
            DummyKey => {
                self.mode = Block;
                self.lexer.force_block_mode();
                Ok(Cow::from("_contents"))
            }
        }
    }

    pub fn end(&mut self) -> Result<()> {
        self.lexer.end()
    }

    pub fn verify_seq_startable(&self) -> Result<()> {
        match self.mode {
            ParsingMode::Word | ParsingMode::Key | ParsingMode::DummyKey => {
                err!("Nested sequence found in inline sequence")
            }
            _ => Ok(()),
        }
    }

    pub fn input(&self) -> &'a str {
        self.lexer.input()
    }
}
