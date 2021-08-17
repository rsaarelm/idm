use crate::{err, lexer::Lexer, lexer::Shape, Error, Result};
use paste::paste;
use serde::de;
use std::{borrow::Cow, collections::HashSet, str::FromStr};

pub fn from_str<'a, T>(input: &'a str) -> Result<T>
where
    T: de::Deserialize<'a>,
{
    let mut deserializer = Deserializer::new(input);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.end()?;
    Ok(t)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum ParsingMode {
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
    fn is_block(self) -> bool {
        use ParsingMode::*;
        match self {
            Block => true,
            _ => false,
        }
    }

    fn is_inline(self) -> bool {
        use ParsingMode::*;
        match self {
            Word | Key | DummyKey => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum SequencePos {
    /// Currently parsed sequence is at the first element of a tuple. Special
    /// parsing rules may be in effect.
    TupleStart,
    /// Currently parsed sequence is at the last element of a tuple. Special
    /// parsing rules may be in effect.
    TupleEnd,
}

#[derive(Clone)]
pub struct Deserializer<'de> {
    lexer: Lexer<'de>,
    mode: ParsingMode,
    seq_pos: Option<SequencePos>,
    checkpoint: Checkpoint<'de>,
}

impl<'de> Deserializer<'de> {
    pub fn new(input: &'de str) -> Deserializer<'de> {
        let lexer = Lexer::new(input);
        log::debug!("\x1b[1;32mDeserializing {:?}\x1b[0m", lexer);
        Deserializer {
            lexer,
            mode: ParsingMode::Block,
            seq_pos: None,
            checkpoint: Default::default(),
        }
    }

    fn parse_next<T: FromStr>(&mut self) -> Result<T> {
        self.next_token()?
            .parse()
            .map_err(|_| Error::new("Token parse failed"))
    }

    fn has_next_token(&mut self) -> bool {
        // XXX: Unoptimized, does extra work that gets thrown out.
        self.clone().next_token().is_ok()
    }

    /// Consume the next atomic parsing element as string according to current
    /// parsing mode.
    ///
    /// This can be very expensive, it can read a whole file, so only use it
    /// when you know you need it.
    fn next_token(&mut self) -> Result<Cow<str>> {
        log::debug!("next_token: {:?}", self.lexer);
        use ParsingMode::*;
        match self.mode {
            Block => Ok(Cow::from(self.lexer.read()?)),
            Line => {
                if let Some(head) = self.lexer.enter_body()? {
                    self.mode = Block;
                    Ok(Cow::from(head))
                } else {
                    return err!("next_token No line");
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

    fn verify_seq_startable(&self) -> Result<()> {
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
}

////////////////////////////////
// Deserializer impl

// Convenience macro, the primitive types bit gets very boilerplatey.
macro_rules! parse_primitives {
    (
        $($typename:ident),+
    ) => {
        $(paste! {
            fn [<deserialize_ $typename>]<V>(self, visitor: V) -> Result<V::Value>
            where
                V: de::Visitor<'de>,
            {
                visitor.[<visit_ $typename>](self.parse_next()?)
            }
        })+
    }
}

impl<'a, 'de> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // Non-self-describing format, so we can't really do much with this.
        self.deserialize_str(visitor)
    }

    parse_primitives!(bool, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64);

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let token = self.next_token()?;
        if token.chars().count() == 1 {
            visitor.visit_char(token.chars().next().unwrap())
        } else {
            err!("Token {:?} was not a single-character string", token)
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_str(&self.next_token()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!();
        /*
        if self.seq_pos == Some(SequencePos::TupleStart) {
            // The hairy magic bit: If we're at the start of a tuple, force
            // line mode here. An empty headline will be read as the tuple
            // value instead of a signal to enter block mode.
            self.mode = ParsingMode::Line(true);
            self.delayed_enter_body_requested = false;
        } else if self.mode.is_inline() {
            // We can't parse a None value in inline mode because there's no
            // notation for a missing inline entry.
            return err!("deserialize_option: Not allowed in inline sequences");
        }

        if self.has_next_token() || self.cursor.at_empty_line() {
            visitor.visit_some(self)
        } else {
            // XXX: Ugly guarantee that empty-marker gets consumed.
            if let ParsingMode::Line(_) = self.mode {
                if self.cursor.clone().verbatim_line(self.current_depth)
                    == Ok(",")
                {
                    let _ = self.cursor.line();
                }
            }
            visitor.visit_none()
        }
        */
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // ¯\_(ツ)_/¯
        // NB. Vec<()> will get you an infinite loop.
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        use Shape::*;
        self.verify_seq_startable()?;
        log::debug!("deserialize_seq: {:?}", self.lexer);

        // NB: Merging is only supported for variable-length seqs, not tuples.
        // It would mess up tuple/fixed-width-array matrices otherwise.
        // It's sort of an iffy feature overall, but is needed to make
        // deserializing into the canonical Outline datatype work.
        if self.seq_pos == Some(SequencePos::TupleEnd) {
            // Skip entering body when doing merged sequence.
            self.seq_pos = None;
            return visitor.visit_seq(Sequence::new(self).merged());
        }

        match self.lexer.classify() {
            None => return err!("deserialize_seq: EOF"),
            Some(Section(headline)) => {
                if !headline.starts_with("--") {
                    return err!("deserialize_seq: Both headline and body");
                } else {
                    log::debug!("deserialize_seq: Block with comment headline");
                    self.mode = ParsingMode::Block;
                    self.lexer.enter_body()?;
                }
            }
            Some(Block) => {
                log::debug!("deserialize_seq: Block");
                self.mode = ParsingMode::Block;
                self.lexer.enter_body()?;
            }
            Some(BodyLine(_)) => {
                log::debug!("deserialize_seq: Inline");
                self.mode = ParsingMode::Word;
            }
        }
        visitor.visit_seq(Sequence::new(self))
    }

    fn deserialize_tuple<V>(
        mut self,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        use Shape::*;
        self.verify_seq_startable()?;
        log::debug!("deserialize_tuple: {:?}", self.lexer);

        match self.lexer.classify() {
            None => return err!("deserialize_tuple: EOF"),
            Some(Section(headline)) => {
                if !headline.starts_with("--") {
                    log::debug!("deserialize_tuple: Section");
                    self.mode = ParsingMode::Line;
                } else {
                    log::debug!("deserialize_tuple: Block with comment headline");
                    self.mode = ParsingMode::Block;
                    self.lexer.enter_body()?;
                }
            }
            Some(Block) => {
                log::debug!("deserialize_tuple: Block");
                self.mode = ParsingMode::Block;
                self.lexer.enter_body()?;
            }
            Some(BodyLine(_)) => {
                log::debug!("deserialize_tuple: Inline");
                self.mode = ParsingMode::Word;
            }
        }
        visitor.visit_seq(Sequence::new(self).tuple_length(len))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!();
        /*
        match self.mode {
            ParsingMode::Line(_) | ParsingMode::Word | ParsingMode::Key(_) => {
                return err!("deserialize_struct: Can't nest in inline seq");
            }
            _ => {}
        }

        self.enter_body()?;
        visitor.visit_map(Sequence::new(self))
        */
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!();
        /*
        match self.mode {
            ParsingMode::Line(_) | ParsingMode::Word | ParsingMode::Key(_) => {
                return err!("deserialize_struct: Can't nest in inline seq");
            }
            _ => {}
        }

        self.enter_body()?;
        visitor.visit_map(Sequence::new(self).as_struct(fields))
        */
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

struct Sequence<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    // If deserializing tuple, length of the tuple.
    tuple_length: Option<usize>,
    // Index of current element.
    idx: usize,
    // True if sequence is merged into an outer sequence, do not call exit
    // body on such sequence.
    is_merged: bool,
    // True if deserializing a struct. Struct keys are read using read_key.
    is_struct: bool,
    struct_fields: HashSet<&'static str>,
    // True if deserializing value into _contents dummy field
    contents_mode: bool,
}

impl<'a, 'de> Sequence<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Sequence<'a, 'de> {
        Sequence {
            de,
            tuple_length: None,
            idx: 0,
            is_merged: false,
            is_struct: false,
            struct_fields: Default::default(),
            contents_mode: false,
        }
    }

    fn tuple_length(mut self, len: usize) -> Sequence<'a, 'de> {
        self.tuple_length = Some(len);
        self
    }

    fn merged(mut self) -> Sequence<'a, 'de> {
        self.is_merged = true;
        self
    }

    fn as_struct(
        mut self,
        fields: &'static [&'static str],
    ) -> Sequence<'a, 'de> {
        self.is_struct = true;
        self.struct_fields = fields.into_iter().map(|&x| x).collect();
        self
    }

    fn exit(&mut self) -> Result<()> {
        log::debug!("exit: {:?}", self.de.lexer);
        if self.de.mode.is_inline() {
            self.de.lexer.exit_words()?;
        } else {
            if !self.is_merged {
                self.de.lexer.exit_body()?;
            }
        }
        self.de.mode = ParsingMode::Block;
        self.de.seq_pos = None;
        Ok(())
    }
}

impl<'a, 'de> de::SeqAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        // Set marker for first or last position of tuple, special parsing
        // rules are in effect in these.
        if self.tuple_length.is_some() && self.idx == 0 {
            self.de.seq_pos = Some(SequencePos::TupleStart);
        } else if self.tuple_length == Some(self.idx + 1) {
            self.de.seq_pos = Some(SequencePos::TupleEnd);
        } else {
            self.de.seq_pos = None;
        }

        if self.tuple_length.is_none() && !self.de.has_next_token() {
            log::debug!("next_element_seed: Out of sequence elements");
            self.exit()?;
            return Ok(None);
        }

        let ret = seed.deserialize(&mut *self.de).map(Some);
        log::debug!("next_element_seed: Deserialized element {}", self.idx);
        self.idx += 1;

        if let Some(len) = self.tuple_length {
            // At tuple end, we need to exit without extra probing.
            // Serde knows the tuple is ended and won't be calling
            // next_element_seed again.
            if self.idx == len {
                self.exit()?;
            }
        }

        ret

        /*
            // If the first element of tuple is Option type, it may look like
            // there's no next token. So always try to deserialize at tuple
            // start.

            // Enter body needs to be deferred up to token-reading for tuples
            // so that Option head value can cancel the deferral.
            if self.tuple_length.is_some()
                && self.idx == 0
                && self.de.mode.is_block()
            {
                self.de.delayed_enter_body_requested = true;
            }
            let ret = seed.deserialize(&mut *self.de).map(Some);
            // Just to be safe.
            self.de.delayed_enter_body_requested = false;

            // Only ever parse the first item in Line mode, dive in and start
            // doing block from then on.
            if let ParsingMode::Line(_) = self.de.mode {
                self.de.mode = ParsingMode::Block(true);

                if self.de.cursor.at_empty_line() {
                    // Enter body would consume an empty line as headline.
                    // Make it the contents of the next step instead.
                    self.de.current_depth += 1;
                } else if self
                    .de
                    .cursor
                    .clone()
                    .verbatim_line(self.de.current_depth)
                    == Ok(",")
                {
                    // Enter body would also consume the empty headline
                    // marker, so another early exit condition here.
                    // XXX: This is another bit of ugly special cases...
                    self.de.current_depth += 1;
                } else if let Err(_) = self.de.enter_body() {
                    // We were forced here because of being in a tuple, but
                    // there's no actual body left, the tuple element will be
                    // empty. Skip trying to enter the body and just increment
                    // the depth.
                    //
                    // XXX: This relies in enter_body not performing mutations
                    // if it fails.
                    self.de.current_depth += 1;
                }
            }

            self.idx += 1;

            if let Some(len) = self.tuple_length {
                // At tuple end, we need to exit without extra probing.
                // Serde knows the tuple is ended and won't be calling
                // next_element_seed again.
                if self.idx == len {
                    if self.de.mode.is_inline() {
                        self.de.exit_line()?;
                    } else {
                        if !self.is_merged {
                            self.de.exit_body()?;
                        }
                    }
                    self.de.mode = ParsingMode::Block(true);
                    self.de.seq_pos = None;
                }
            }

            ret
        */
    }
}

impl<'a, 'de> de::MapAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        todo!();
        /*
        if self.contents_mode
            || !self.de.cursor.has_headline_content(self.de.current_depth)
        {
            return Ok(None);
        }

        if self.is_struct {
            self.de.mode = ParsingMode::Key(false);
            if self.de.cursor.clone().key().is_err() {
                // Must have _contents field present for the next bit to work.
                //
                // (Or have an empty fields list, in case we assume anything
                // goes.)
                if !self.struct_fields.is_empty()
                    && !self.struct_fields.contains("_contents")
                {
                    return err!("next_key_seed: No _contents field to catch additional contents");
                }

                // There's still content, but no valid key. Do the magic bit
                // and conjure up a '_contents' key for the remaining content.
                self.de.mode = ParsingMode::Key(true);
                // Also mess with depth so it looks like this is a whole new
                // section with an empty headline, the whole remaining body
                // needs to end up in the _contents part.
                self.de.current_depth -= 1;
                self.contents_mode = true;
            }

            // Make sure we're only seeing struct keys we actually expect to
            // see.
            //
            // Special case, if the expected list is empty, assume we're doing
            // some special deserialization that takes freeform keys and just
            // process everything.
            if let Ok(key) = self.de.cursor.clone().key() {
                if !self.struct_fields.is_empty()
                    && !self.struct_fields.contains(key.as_str())
                {
                    return err!("next_key_seed: Unmatched struct key {}", key);
                }
            }
        } else {
            if self.de.cursor.is_section(self.de.current_depth) {
                // Has both headline and body.
                // Assume full headline is key, body is content.
                self.de.mode = ParsingMode::Line(false);
            } else {
                // Has no body. Assume first headline word is key, rest of
                // headline is value.
                self.de.mode = ParsingMode::Word;
            }
        }

        let ret = seed.deserialize(&mut *self.de).map(Some);
        // If we're in word mode, drop out of it, first headline item was
        // parsed as key, but the entire rest of the line needs to be value.
        if self.de.mode == ParsingMode::Word {
            self.de.mode = ParsingMode::Block(false);
        }
        ret
        */
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        todo!();
        /*
        let need_exit = if let ParsingMode::Line(_) = self.de.mode {
            self.de.mode = ParsingMode::Block(false);
            true
        } else {
            false
        };
        let ret = seed.deserialize(&mut *self.de);
        if need_exit {
            self.de.exit_body()?;
        }
        ret
        */
    }
}

/// State checkpointing support structure
///
/// For handling the hacky bit where parsing may need to back down when
/// encountering a canonical outline value.
#[derive(Clone, Default)]
struct Checkpoint<'a> {
    /// Saved state and new input position at which checkpoint was saved.
    span: Option<(Lexer<'a>, &'a str)>,
}

impl<'a> Checkpoint<'a> {
    pub fn save(&mut self, old_state: Lexer<'a>, new_pos: &'a str) {
        // Are we encoding an actual skip
        let did_skip = new_pos != old_state.input();
        let seen_position =
            self.span.as_ref().map_or(false, |(_, pos)| *pos == new_pos);

        // If a skip happened, the position should be something we haven't
        // encountered.
        debug_assert!(!did_skip || !seen_position);

        if seen_position {
            // We already stored a jump to this position, do not munge
            // checkpoint with a no-length jump
            log::debug!("Checkpoint::save: At known position, doing nothing");
            return;
        } else if did_skip {
            log::debug!("Checkpoint::save: Saving state");
            self.span = Some((old_state, new_pos));
        } else {
            // Entered a new position, but there's no skip to back over,
            // turn checkpoint into no-op.
            log::debug!("Checkpoint::save: No skip, clearing checkpoint");
            self.span = None;
        }
    }

    pub fn restore(&mut self, state: &mut Lexer<'a>) {
        if let Some((old_state, _)) = std::mem::replace(&mut self.span, None) {
            log::debug!("Checkpoint::restore: Restoring earlier state");
            *state = old_state;
        }
    }
}
