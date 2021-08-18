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
            None => return err!("deserialize_seq: EOF"),
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
                    self.parser.mode = ParsingMode::Line;
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
        log::debug!("exit: {:?}", self.de.parser.lexer);
        if self.de.parser.mode.is_inline() {
            self.de.parser.lexer.exit_words()?;
        } else {
            if !self.is_merged {
                self.de.parser.lexer.exit_body()?;
            }
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

        // Consume comments and blanks in outline mode.
        if !self.de.parser.mode.is_inline() {
            loop {
                if let Some(cls) = self.de.parser.lexer.classify() {
                    if cls.is_blank() {
                        self.de.parser.lexer.skip()?;
                    } else if cls.is_standalone_comment() {
                        self.de.parser.lexer.skip()?;
                    } else if cls.has_comment_head() {
                        // Consume headline, set things in block mode.
                        self.de.parser.lexer.enter_body()?;
                        self.de.parser.lexer.dedent();
                        break;
                    } else {
                        break;
                    }
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
    span: Option<(Parser<'a>, &'a str)>,
}

impl<'a> Checkpoint<'a> {
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

    pub fn restore(&mut self, state: &mut Parser<'a>) {
        if let Some((old_state, _)) = std::mem::replace(&mut self.span, None) {
            log::debug!("Checkpoint::restore: Restoring earlier state");
            *state = old_state;
        }
    }
}
