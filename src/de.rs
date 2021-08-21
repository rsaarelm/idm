use crate::{
    err,
    lexer::Shape,
    parser::{Parser, ParsingMode, SequencePos},
    Error, Result,
};
use paste::paste;
use serde::de;
use std::collections::HashSet;

pub fn from_str<'a, T>(input: &'a str) -> Result<T>
where
    T: de::Deserialize<'a>,
{
    let mut deserializer = Deserializer::new(input);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.parser.end()?;
    Ok(t)
}

#[derive(Clone)]
pub struct Deserializer<'de> {
    parser: Parser<'de>,
    checkpoint: Checkpoint<'de>,
}

impl<'de> Deserializer<'de> {
    pub fn new(input: &'de str) -> Deserializer<'de> {
        let parser = Parser::new(input);
        log::debug!("\x1b[1;32mDeserializing {:?}\x1b[0m", parser.lexer);
        Deserializer {
            parser,
            checkpoint: Default::default(),
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
                visitor.[<visit_ $typename>](self.parser.parse_next()?)
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
        let token = self.parser.next_token()?;
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
        visitor.visit_str(&self.parser.next_token()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // Byte buffer is the marker for outline mode.
        log::debug!("deserialize_bytes called");

        if self.parser.seq_pos == Some(SequencePos::TupleStart) {
            log::debug!(
                "deserialize_bytes at tuple start, switching to line mode"
            );
            // Go back to parser state before earlier sequence parsing
            // assumptions were made.
            self.checkpoint.restore(&mut self.parser);
            // Force section mode.
            self.parser.mode = ParsingMode::Headline;
        }

        visitor.visit_bytes(self.parser.next_token()?.as_bytes())
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // Parsing None isn't supported. Options can still be encountered in
        // struct fields, but it's assumed the field will not be serialized if
        // it's None.
        visitor.visit_some(self)
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
        self.parser.verify_seq_startable()?;
        log::debug!("deserialize_seq: {:?}", self.parser.lexer);

        // NB: Merging is only supported for variable-length seqs, not tuples.
        // It would mess up tuple/fixed-width-array matrices otherwise.
        // It's sort of an iffy feature overall, but is needed to make
        // deserializing into the canonical Outline datatype work.
        if self.parser.seq_pos == Some(SequencePos::TupleEnd) {
            // Skip entering body when doing merged sequence.
            self.parser.seq_pos = None;
            return visitor.visit_seq(Sequence::new(self).merged());
        }

        match self.parser.lexer.classify() {
            None => {
                log::debug!("deserialize_seq: EOF, treating as block");
                self.parser.mode = ParsingMode::Block;
                self.parser.lexer.enter_body()?;
            }
            Some(Section(headline)) => {
                if !headline.starts_with("--") {
                    return err!("deserialize_seq: Both headline and body");
                } else {
                    log::debug!("deserialize_seq: Block with comment headline");
                    self.parser.mode = ParsingMode::Block;
                    self.parser.lexer.enter_body()?;
                }
            }
            Some(Block) => {
                log::debug!("deserialize_seq: Block");
                self.parser.mode = ParsingMode::Block;
                self.parser.lexer.enter_body()?;
            }
            Some(BodyLine(_)) => {
                log::debug!("deserialize_seq: Inline");
                self.parser.mode = ParsingMode::Word;
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
        self.parser.verify_seq_startable()?;
        log::debug!("deserialize_tuple: {:?}", self.parser.lexer);

        match self.parser.lexer.classify() {
            None => return err!("deserialize_tuple: EOF"),
            Some(Section(headline)) => {
                if !headline.starts_with("--") {
                    log::debug!("deserialize_tuple: Section");
                    self.parser.mode = ParsingMode::Headline;
                } else {
                    log::debug!(
                        "deserialize_tuple: Block with comment headline"
                    );
                    self.parser.mode = ParsingMode::Block;
                    self.parser.lexer.enter_body()?;
                }
            }
            Some(Block) => {
                log::debug!("deserialize_tuple: Block");
                self.parser.mode = ParsingMode::Block;
                self.parser.lexer.enter_body()?;
            }
            Some(BodyLine(_)) => {
                log::debug!("deserialize_tuple: Inline");
                self.parser.mode = ParsingMode::Word;
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
        self.parser.verify_seq_startable()?;
        log::debug!("deserialize_map: {:?}", self.parser.lexer);

        self.parser.lexer.enter_body()?;
        visitor.visit_map(Sequence::new(self))
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
        self.parser.verify_seq_startable()?;
        log::debug!("deserialize_struct: {:?}", self.parser.lexer);

        self.parser.lexer.enter_body()?;
        visitor.visit_map(Sequence::new(self).as_struct(fields))
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
        self.struct_fields = fields.iter().copied().collect();
        self
    }

    fn exit(&mut self) -> Result<()> {
        log::debug!("exit: {:?}", self.de.parser.lexer);
        if self.de.parser.mode.is_inline() {
            self.de.parser.lexer.exit_words()?;
        } else if !self.is_merged {
            self.de.parser.lexer.exit_body()?;
        }
        self.de.parser.mode = ParsingMode::Block;
        self.de.parser.seq_pos = None;
        Ok(())
    }
}

impl<'a, 'de> de::SeqAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        log::debug!("next_element_seed: Start element {}", self.idx);
        // Set marker for first or last position of tuple, special parsing
        // rules are in effect in these.
        if self.tuple_length.is_some() && self.idx == 0 {
            self.de.parser.seq_pos = Some(SequencePos::TupleStart);
        } else if self.tuple_length == Some(self.idx + 1) {
            self.de.parser.seq_pos = Some(SequencePos::TupleEnd);
        } else {
            self.de.parser.seq_pos = None;
        }

        // Snapshot before eating content.
        let old_parser = self.de.parser.clone();

        // Consume comments and blanks in outline mode.
        if !self.de.parser.mode.is_inline() {
            while let Some(cls) = self.de.parser.lexer.classify() {
                if cls.is_blank() || cls.is_standalone_comment() {
                    self.de.parser.lexer.skip()?;
                } else if cls.has_comment_head() {
                    // Consume headline, pop right back.
                    self.de.parser.lexer.enter_body()?;
                    self.de.parser.lexer.dedent();
                    break;
                } else {
                    break;
                }
            }
        }

        if self.tuple_length.is_none() && !self.de.parser.has_next_token() {
            log::debug!("next_element_seed: Out of sequence elements");
            self.exit()?;
            return Ok(None);
        }

        self.de.checkpoint.save(old_parser, self.de.parser.input());

        let ret = seed.deserialize(&mut *self.de).map(Some);
        log::debug!(
            "next_element_seed: Deserialized element {}{}",
            self.idx,
            if ret.is_err() { " (FAILED)" } else { "" }
        );
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
    }
}

impl<'a, 'de> de::MapAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        log::debug!("next_key_seed start");
        if self.contents_mode {
            log::debug!(
                "next_key_seed: Already entered contents mode, no more keys"
            );
            // Do not call exit in contents mode.
            // Similar to merged mode. Deserializing contents object should
            // have handled the stack.
            return Ok(None);
        }

        // Is the value an outline as in
        //     x:
        //       1
        //       2
        //
        // and not
        //     x: 1 2
        let is_outline_value = match self.de.parser.lexer.classify() {
            None => {
                log::debug!("next_key_seed: Out of input, exiting");
                self.exit()?;
                return Ok(None);
            }
            Some(Shape::Block) => {
                return err!("next_key_seed: Invalid shape");
            }
            Some(Shape::Section(_)) => {
                log::debug!("next_key_seed: Section key-value");
                true
            }
            Some(Shape::BodyLine(_)) => {
                log::debug!("next_key_seed: Inline key-value");
                false
            }
        };

        if self.is_struct {
            log::debug!("next_key_seed: (struct) {:?}", self.de.parser.lexer);
            self.de.parser.mode = ParsingMode::Key;

            if self.de.parser.clone().next_token().is_err() {
                // Must have _contents field present for the next bit to work.
                //
                // (Or have an empty fields list, in case we assume anything
                // goes.)
                if !self.struct_fields.is_empty()
                    && !self.struct_fields.contains("_contents")
                {
                    return err!("next_key_seed: No _contents field to catch additional contents");
                }

                log::debug!(
                    "next_key_seed: Out of keys, entering contents mode"
                );

                // There's still content, but no valid key. Do the magic bit
                // and conjure up a '_contents' key for the remaining content.
                self.de.parser.mode = ParsingMode::DummyKey;
                self.contents_mode = true;
            }

            // Don't let unrecognized keys through.
            if let Ok(key) = self.de.parser.clone().next_token() {
                if !self.struct_fields.is_empty()
                    && !self.struct_fields.contains(&*key)
                {
                    return err!(
                        "next_key_seed: Unmatched struct key {:?}",
                        key
                    );
                }
            }
        } else {
            log::debug!("next_key_seed: (map) {:?}", self.de.parser.lexer);

            // Keys are read as regular values for a map
            if is_outline_value {
                // Lines for outline content
                self.de.parser.mode = ParsingMode::Headline;
            } else {
                // The first word for inline content.
                self.de.parser.mode = ParsingMode::Word;
            }
        }

        let ret = seed.deserialize(&mut *self.de).map(Some);

        if !self.is_struct && is_outline_value {
            self.de.parser.lexer.dedent();
        }

        ret
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        // Maps with inline entries read key as word, but the whole remaining
        // line as value.
        if self.de.parser.mode == ParsingMode::Word {
            self.de.parser.mode = ParsingMode::Line;
        }
        log::debug!("next_value_seed: {:?}", self.de.parser.lexer);
        seed.deserialize(&mut *self.de)
    }
}

/// State checkpointing support structure
///
/// For handling the hacky bit where parsing may need to back down when
/// encountering a canonical outline value.
#[derive(Clone, Default)]
struct Checkpoint<'a> {
    /// Saved state and new input position at which checkpoint was saved.
    span: Option<(Parser<'a>, &'a str)>,
}

impl<'a> Checkpoint<'a> {
    /// Save a parser state as the snapshot.
    ///
    /// A common use pattern is to clone the parser state, run some further
    /// parsing, then call `Checkpoint::save` with the old state and the new
    /// input position.
    ///
    /// This whole thing is sort of terrible and serves a kludgy bit in the
    /// parsing logic.
    pub fn save(&mut self, old_state: Parser<'a>, new_pos: &'a str) {
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

    pub fn restore(&mut self, state: &mut Parser<'a>) {
        if let Some((old_state, _)) = std::mem::replace(&mut self.span, None) {
            log::debug!("Checkpoint::restore: Restoring earlier state");
            *state = old_state;
        }
    }
}
