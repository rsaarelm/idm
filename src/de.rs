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
        // Byte buffer is the marker for raw mode used in outlines.
        if self.parser.seq_pos == Some(SequencePos::TupleStart) {
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
        // Any time an Option value is encountered, it's assumed to be
        // Some(value). Parsing None isn't supported. Options can still be
        // encountered in struct fields, but it's assumed the field will not
        // be serialized if it's None.
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // IDM doesn't do empty values well, gonna just not support unit
        // values for now. Maybe figure out something if this is actually
        // needed somewhere.
        unimplemented!()
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

        // Part of section-like tuple, treat as part of tuple.
        if self.parser.seq_pos == Some(SequencePos::SecondOfPair) {
            self.parser.seq_pos = None;
            return visitor.visit_seq(Sequence::new(self).do_not_exit());
        }

        match self.parser.lexer.classify() {
            None => {
                self.parser.mode = ParsingMode::Block;
                self.parser.lexer.enter_body()?;
            }
            Some(Section(headline)) => {
                if !headline.starts_with("--") {
                    return err!("deserialize_seq: Both headline and body");
                } else {
                    self.parser.mode = ParsingMode::Block;
                    self.parser.lexer.enter_body()?;
                }
            }
            Some(Block) => {
                self.parser.mode = ParsingMode::Block;
                self.parser.lexer.enter_body()?;
            }
            Some(BodyLine(_)) => {
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

        match self.parser.lexer.classify() {
            None => {
                // XXX: We need to okay a tuple that looks empty at this point
                // in case the values are parsed in raw mode, in which case
                // the parser might be rewound to an earlier position. If this
                // isn't the case, parsing will fail later at
                // next_element_seed.
                self.parser.mode = ParsingMode::Block;
            }
            Some(Section(headline)) => {
                if !headline.starts_with("--") {
                    self.parser.mode = ParsingMode::Headline;
                } else {
                    self.parser.mode = ParsingMode::Block;
                    self.parser.lexer.enter_body()?;
                }
            }
            Some(Block) => {
                self.parser.mode = ParsingMode::Block;
                self.parser.lexer.enter_body()?;
            }
            Some(BodyLine(_)) => {
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

        // Part of section-like tuple, treat as part of tuple.
        if self.parser.seq_pos == Some(SequencePos::SecondOfPair) {
            self.parser.seq_pos = None;

            // XXX: Why do I need to do this for a no-exit block?
            self.parser.lexer.enter_body()?;

            return visitor.visit_map(Sequence::new(self).do_not_exit());
        }

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

        // Part of section-like tuple, treat as part of tuple.
        if self.parser.seq_pos == Some(SequencePos::SecondOfPair) {
            self.parser.seq_pos = None;

            // XXX: Why do I need to do this for a no-exit block?
            self.parser.lexer.enter_body()?;

            return visitor.visit_map(
                Sequence::new(self).as_struct(fields).do_not_exit(),
            );
        }

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
    call_exit: bool,
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
            call_exit: true,
            is_struct: false,
            struct_fields: Default::default(),
            contents_mode: false,
        }
    }

    fn tuple_length(mut self, len: usize) -> Sequence<'a, 'de> {
        self.tuple_length = Some(len);
        self
    }

    fn do_not_exit(mut self) -> Sequence<'a, 'de> {
        self.call_exit = false;
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
        if self.de.parser.mode.is_inline() {
            self.de.parser.lexer.exit_words()?;
        } else if self.call_exit {
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
        // Flag for not doing a separate tuple exit call if we switched to
        // line mode at the end.
        let mut exit_tuple = true;

        // Set marker for first or last position of tuple, special parsing
        // rules are in effect in these.
        if self.tuple_length.is_some() && self.idx == 0 {
            self.de.parser.seq_pos = Some(SequencePos::TupleStart);
        } else if self.tuple_length == Some(self.idx + 1) {
            if self.idx == 1 {
                self.de.parser.seq_pos = Some(SequencePos::SecondOfPair);
            } else {
                self.de.parser.seq_pos = None;
            }
            // Switch to line mode for last element of tuple, allow multiple
            // words.
            if self.de.parser.mode.is_inline() {
                self.de.parser.mode = ParsingMode::Line;
                exit_tuple = false;
            }
        } else {
            self.de.parser.seq_pos = None;
        }

        // Snapshot before eating content.
        let old_parser = self.de.parser.clone();

        // When true, try to parse even when there doesn't seem to be more
        // elements, skipped content might go into a Raw value.
        let mut content_remains = false;

        // Consume comments and blanks in outline mode.
        if !self.de.parser.mode.is_inline() {
            while let Some(cls) = self.de.parser.lexer.classify() {
                if cls.is_blank() || cls.is_standalone_comment() {
                    self.de.parser.lexer.skip()?;
                    content_remains = true;
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

        let no_next_token = !self.de.parser.has_next_token();

        if self.tuple_length.is_none() && no_next_token && !content_remains {
            // Unambiguously out of input, exit.
            self.exit()?;
            return Ok(None);
        }

        self.de.checkpoint.save(old_parser, self.de.parser.input());

        if self.tuple_length.is_none() && no_next_token {
            // It looks like we're out of input, but there might be something
            // left for raw mode. Let's check with a throwaway clone.
            let mut cloned = self.de.clone();
            match seed.deserialize(&mut cloned) {
                Ok(ret) => {
                    // Yep! Update local state.
                    *self.de = cloned;
                    self.idx += 1;
                    return Ok(Some(ret));
                }
                Err(_) => {
                    // Nope, really out.
                    self.exit()?;
                    return Ok(None);
                }
            }
        }

        let ret = seed.deserialize(&mut *self.de).map(Some);
        self.idx += 1;

        if let Some(len) = self.tuple_length {
            // At tuple end, we need to exit without extra probing.
            // Serde knows the tuple is ended and won't be calling
            // next_element_seed again.
            if self.idx == len && exit_tuple {
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
        if self.contents_mode {
            // Do not call exit in contents mode.
            // Similar to merged mode. Deserializing contents object should
            // have handled the stack.
            return Ok(None);
        }

        // Eat comments
        while self
            .de
            .parser
            .lexer
            .classify()
            .map_or(false, |c| c.is_standalone_comment())
        {
            self.de.parser.lexer.skip()?;
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
                self.exit()?;
                return Ok(None);
            }
            Some(Shape::Block) => {
                return err!("next_key_seed: Invalid shape");
            }
            Some(Shape::Section(_)) => true,
            Some(Shape::BodyLine(_)) => false,
        };

        if self.is_struct {
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

        if !self.contents_mode && is_outline_value {
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
        } else if did_skip {
            self.span = Some((old_state, new_pos));
        } else {
            // Entered a new position, but there's no skip to back over,
            // turn checkpoint into no-op.
            self.span = None;
        }
    }

    pub fn restore(&mut self, state: &mut Parser<'a>) {
        if let Some((old_state, _)) = std::mem::replace(&mut self.span, None) {
            *state = old_state;
        }
    }
}
