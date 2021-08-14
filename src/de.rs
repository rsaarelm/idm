use crate::parser::{Parser, ParsingMode, SequencePos};
use crate::{err, Error, Result};
use paste::paste;
use serde::de;
use std::collections::HashSet;
use std::str::FromStr;

pub fn from_str<'a, T>(input: &'a str) -> Result<T>
where
    T: de::Deserialize<'a>,
{
    let mut deserializer = Deserializer::new(input);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.end()?;
    Ok(t)
}

#[derive(Clone)]
pub struct Deserializer<'de> {
    parser: Parser<'de>,
    // Fallback position if we need to retry speculative parsing.
    checkpoint: Checkpoint<'de>,
}

impl<'de> Deserializer<'de> {
    pub fn new(input: &'de str) -> Deserializer<'de> {
        Deserializer {
            checkpoint: Default::default(),
            parser: Parser::new(input),
        }
    }

    fn parse_next<T: FromStr>(&mut self) -> Result<T> {
        self.parser
            .next_token()?
            .parse()
            .map_err(|_| Error::new("Token parse failed"))
    }

    /// Ok when there is no more non-whitespace input left
    pub fn end(&mut self) -> Result<()> {
        self.parser.end()
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
        if self.parser.seq_pos == Some(SequencePos::TupleStart) {
            log::debug!("deserialize_option: Encountered option at tuple start, rewinding");
            // The hairy magic bit: If we're at the start of a tuple, force
            // line mode here. An empty headline will be read as the tuple
            // value instead of a signal to enter block mode.
            //
            // If the checkpoint logic looks sort of hairy and weird that's
            // because it is.

            // Roll back from entering a body
            self.checkpoint.restore(&mut self.parser);

            self.parser.start_line()?;
        } else if self.parser.mode.is_inline() {
            // We can't parse a None value in inline mode because there's no
            // notation for a missing inline entry.
            return err!("deserialize_option: Not allowed in inline sequences");
        }

        if self
            .parser
            .clone()
            .next_token()
            .map_or(false, |tok| tok == "--")
        {
            log::debug!("deserialize_option: is None");
            // Consume it for real.
            let _ = self.parser.next_token();
            visitor.visit_none()
        } else {
            log::debug!("deserialize_option: is Some");
            visitor.visit_some(self)
        }
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
        // NB: Merging is only supported for variable-length seqs, not tuples.
        // It would mess up tuple/fixed-width-array matrices otherwise.
        // It's sort of an iffy feature overall, but is needed to make
        // deserializing into the canonical Outline datatype work.
        if self.parser.can_start_seq()
            && self.parser.seq_pos == Some(SequencePos::TupleEnd)
        {
            log::debug!("deserialize_seq: Merging sequence to tuple");
            // Skip entering body when doing merged sequence.
            self.parser.seq_pos = None;
            return visitor.visit_seq(Sequence::new(self).merged());
        }

        self.parser.start_seq()?;
        visitor.visit_seq(Sequence::new(self))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.parser.start_tuple(len)?;
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
        self.parser.start_map()?;
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
        self.parser.start_struct(fields)?;
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
        self.struct_fields = fields.into_iter().map(|&x| x).collect();
        self
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
            log::debug!("next_element_seed at start of tuple");
            self.de.parser.seq_pos = Some(SequencePos::TupleStart);
        } else if self.tuple_length == Some(self.idx + 1) {
            log::debug!("next_element_seed at end of tuple");
            self.de.parser.seq_pos = Some(SequencePos::TupleEnd);
        } else {
            self.de.parser.seq_pos = None;
        }

        // Tuples are parsed up to their known number of elements.
        // Other sequences are parsed until tokens run out.
        let elements_remain = match self.tuple_length {
            Some(x) => x > self.idx,
            None => self.de.parser.has_next_token(),
        };

        if elements_remain {
            log::debug!("next_element_seed: advancing seq");
            // Checkpoint before consuming noncontent.
            let old_cursor = self.de.parser.clone();

            // Walk over noncontent
            self.de.parser.seq_advance()?;

            self.de.checkpoint.save(old_cursor, self.de.parser.input());

            // If the first element of tuple is Option type, it may look like
            // there's no next token. So always try to deserialize at tuple
            // start.

            log::debug!(
                "next_element_seed: deserializing element {}",
                self.idx
            );
            let ret = seed.deserialize(&mut *self.de).map(Some);

            // Only ever parse the first item in Line mode, dive in and start
            // doing block from then on.
            if let ParsingMode::Line = self.de.parser.mode {
                log::debug!("next_element_seed: leaving Line mode");
                self.de.parser.start_block()?;
            }

            self.idx += 1;

            if let Some(len) = self.tuple_length {
                log::debug!("next_element_seed: At tuple end, exiting");
                // At tuple end, we need to exit without extra probing.
                // Serde knows the tuple is ended and won't be calling
                // next_element_seed again.
                if self.idx == len {
                    if self.de.parser.mode.is_inline() {
                        self.de.parser.end_line()?;
                    } else {
                        if !self.is_merged {
                            self.de.parser.end_block()?;
                        }
                    }
                    self.de.parser.seq_pos = None;
                }
            }

            ret
        } else {
            log::debug!("next_element_seed: No more elements found, exiting");
            // Exit when not finding more elements.
            if self.de.parser.mode.is_inline() {
                self.de.parser.end_line()?;
            } else {
                if !self.is_merged {
                    self.de.parser.end_block()?;
                }
            }
            self.de.parser.seq_pos = None;
            Ok(None)
        }
    }
}

impl<'a, 'de> de::MapAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.contents_mode || !self.de.parser.has_next_token() {
            log::debug!("next_key_seed: Out of items");
            return Ok(None);
        }

        if self.is_struct {
            self.de.parser.mode = ParsingMode::Key;
            if self.de.parser.clone().key().is_err() {
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
                // Also mess with depth so it looks like this is a whole new
                // section with an empty headline, the whole remaining body
                // needs to end up in the _contents part.
                self.de.parser.prepare_for_contents()?;
                self.contents_mode = true;
            }

            // Make sure we're only seeing struct keys we actually expect to
            // see.
            //
            // Special case, if the expected list is empty, assume we're doing
            // some special deserialization that takes freeform keys and just
            // process everything.
            if let Ok(key) = self.de.parser.clone().key() {
                if !self.struct_fields.is_empty()
                    && !self.struct_fields.contains(key.as_str())
                {
                    return err!("next_key_seed: Unmatched struct key {}", key);
                }
            }
        } else {
            if self.de.parser.is_section(self.de.parser.current_depth) {
                // Has both headline and body.
                // Assume full headline is key, body is content.
                self.de.parser.start_line()?;
            } else {
                // Has no body. Assume first headline word is key, rest of
                // headline is value.
                self.de.parser.start_words()?;
            }
        }

        let ret = seed.deserialize(&mut *self.de).map(Some);
        // If we're in word mode, drop out of it, first headline item was
        // parsed as key, but the entire rest of the line needs to be value.
        if self.de.parser.mode == ParsingMode::Words {
            self.de.parser.mode = ParsingMode::Block;
        }
        ret
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        let need_exit = if let ParsingMode::Line = self.de.parser.mode {
            self.de.parser.mode = ParsingMode::Block;
            true
        } else {
            false
        };
        let ret = seed.deserialize(&mut *self.de);
        if need_exit {
            self.de.parser.end_block()?;
        }
        ret
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
            log::debug!(
                "Checkpoint::restore: Restoring earlier state"
            );
            *state = old_state;
        }
    }
}
