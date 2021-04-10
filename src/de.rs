use crate::cursor::Cursor;
use crate::{err, Error, Result};
use paste::paste;
use serde::de;
use std::borrow::Cow;
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
    ///
    /// If the magic flag is set, emit '_contents' instead of parsing
    /// anything.
    Key(bool),
}

impl ParsingMode {
    fn is_inline(self) -> bool {
        use ParsingMode::*;
        match self {
            Word | Key(_) => true,
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
    cursor: Cursor<'de>,
    current_depth: i32,
    mode: ParsingMode,
    seq_pos: Option<SequencePos>,
    // Hacky hack hack to support tuples with Option first field
    delayed_enter_body_requested: bool,
}

impl<'de> Deserializer<'de> {
    pub fn new(input: &'de str) -> Deserializer<'de> {
        Deserializer {
            cursor: input.into(),
            // Start at the head of a dummy section encompassing the whole
            // input. Since input's baseline indent is 0, our starting indent
            // for the dummy construct around it is -1.
            current_depth: -1,
            mode: ParsingMode::Block,
            seq_pos: None,
            delayed_enter_body_requested: false,
        }
    }

    fn parse_next<T: FromStr>(&mut self) -> Result<T> {
        self.next_token()?
            .parse()
            .map_err(|_| Error(format!("Token parse failed")))
    }

    fn has_next_token(&mut self) -> bool {
        use ParsingMode::*;
        match self.mode {
            Block => {
                self.cursor
                    .line_depth()
                    .map_or(true, |n| n >= self.current_depth)
                    && (self.cursor.has_headline(self.current_depth)
                        || self.cursor.has_body_content(self.current_depth))
            }
            Line | Word | Key(_) => {
                self.cursor.has_headline_content(self.current_depth)
            }
        }
    }

    /// Consume the next atomic parsing element as string according to current
    /// parsing mode.
    ///
    /// This can be very expensive, it can read a whole file, so only use it
    /// when you know you need it.
    fn next_token(&mut self) -> Result<Cow<str>> {
        if self.delayed_enter_body_requested {
            self.enter_body()?;
            self.delayed_enter_body_requested = false;
        }

        use ParsingMode::*;
        match self.mode {
            Block => {
                let block = self.cursor.line_or_block(self.current_depth)?;
                Ok(Cow::from(block))
            }
            Line => Ok(Cow::from(self.cursor.line()?)),
            Word => Ok(Cow::from(self.cursor.word()?)),
            Key(emit_dummy_key) => {
                let key = if emit_dummy_key {
                    Ok("_contents".into())
                } else {
                    self.cursor.key()
                };
                // Keys are always a one-shot parse.
                self.mode = Block;
                Ok(Cow::from(key?))
            }
        }
    }

    /// Move cursor to start of the current headline's body.
    fn enter_body(&mut self) -> Result<()> {
        let depth = self.cursor.line_depth();
        if depth.map_or(false, |n| n > self.current_depth) {
            // Missing headline (but there is content, empty lines don't
            // count), we're done with just incrementing current
            // depth.
            self.current_depth += 1;
            Ok(())
        } else if depth.map_or(true, |n| n == self.current_depth) {
            // There is some headline, we need to move past it.
            // Empty headlines are counted here, since we need to skip over
            // the line.
            if self.cursor.has_headline_content(self.current_depth) {
                return err!("enter_body: Unparsed headline input");
            }

            let _ = self.cursor.headline(self.current_depth);
            self.current_depth += 1;
            Ok(())
        } else {
            err!("enter_body: Out of depth")
        }
    }

    fn exit_body(&mut self) -> Result<()> {
        if self.cursor.at_end() && self.current_depth > -1 {
            // Can exit until -1 at EOF.
            self.current_depth -= 1;
            return Ok(());
        }

        let depth = self.cursor.line_depth();
        if depth.map_or(false, |n| n < self.current_depth) {
            self.current_depth -= 1;
            Ok(())
        } else if depth.map_or(true, |n| n == self.current_depth)
            && !self.cursor.has_headline_content(self.current_depth)
            && !self.cursor.has_body_content(self.current_depth)
        {
            // No current content, see if next line is out of body
            let next_depth = self.cursor.next_line_depth();
            if next_depth.map_or(false, |n| n < self.current_depth) {
                self.cursor.line()?;
                self.current_depth -= 1;
                Ok(())
            } else {
                err!("exit_body: Body not empty")
            }
        } else {
            err!("exit_body: Body not empty")
        }
    }

    fn exit_line(&mut self) -> Result<()> {
        if self.cursor.has_headline_content(self.current_depth) {
            err!("exit_line: Unparsed content left in line")
        } else {
            let _ = self.cursor.line();
            Ok(())
        }
    }

    /// Ok when there is no more non-whitespace input left
    pub fn end(&mut self) -> Result<()> {
        if self.cursor.at_end() {
            Ok(())
        } else {
            err!("Unparsed trailing input")
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
        if self.seq_pos == Some(SequencePos::TupleStart) {
            // The hairy magic bit: If we're at the start of a tuple, force
            // line mode here. An empty headline will be read as the tuple
            // value instead of a signal to enter block mode.
            self.mode = ParsingMode::Line;
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
            if self.mode == ParsingMode::Line
                && self.cursor.clone().verbatim_line(self.current_depth)
                    == Ok(",")
            {
                let _ = self.cursor.line();
            }
            visitor.visit_none()
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
        if self.delayed_enter_body_requested {
            self.enter_body()?;
            self.delayed_enter_body_requested = false;
        }

        match self.mode {
            ParsingMode::Line | ParsingMode::Word | ParsingMode::Key(_) => {
                return err!("Nested sequence found in inline sequence");
            }
            _ => {}
        }

        // NB: Merging is only supported for variable-length seqs, not tuples.
        // It would mess up tuple/fixed-width-array matrices otherwise.
        // It's sort of an iffy feature overall, but is needed to make
        // deserializing into the canonical Outline datatype work.
        if self.seq_pos == Some(SequencePos::TupleEnd) {
            // Skip entering body when doing merged sequence.
            self.seq_pos = None;
            return visitor.visit_seq(Sequence::new(self).merged());
        }

        let is_inline = self.cursor.has_headline_content(self.current_depth);
        let is_outline = self.cursor.has_body_content(self.current_depth);
        if is_inline && is_outline {
            return err!("Sequence has both headline and body");
        }

        if is_inline {
            self.mode = ParsingMode::Word;
        } else {
            self.mode = ParsingMode::Block;
            self.enter_body()?;
        }

        visitor.visit_seq(Sequence::new(self))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if self.delayed_enter_body_requested {
            self.enter_body()?;
            self.delayed_enter_body_requested = false;
        }

        match self.mode {
            ParsingMode::Line | ParsingMode::Word | ParsingMode::Key(_) => {
                return err!("Nested sequence in inline sequence");
            }
            _ => {}
        }

        let is_inline = self.cursor.has_headline_content(self.current_depth);
        let is_outline = self.cursor.has_body_content(self.current_depth);

        if is_inline && !is_outline {
            self.mode = ParsingMode::Word;
        } else if !is_inline && is_outline {
            self.mode = ParsingMode::Block;
        // Can't enter body yet, because if the first field turns out to
        // be Option, we need to backtrack into Line mode.
        //
        // The flag for deferred body entering will be flipped in
        // next_element_seed.
        } else {
            self.mode = ParsingMode::Line;
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
        match self.mode {
            ParsingMode::Line | ParsingMode::Word | ParsingMode::Key(_) => {
                return err!("deserialize_struct: Can't nest in inline seq");
            }
            _ => {}
        }

        self.enter_body()?;
        visitor.visit_map(Sequence::new(self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.mode {
            ParsingMode::Line | ParsingMode::Word | ParsingMode::Key(_) => {
                return err!("deserialize_struct: Can't nest in inline seq");
            }
            _ => {}
        }

        self.enter_body()?;
        visitor.visit_map(Sequence::new(self).struct_())
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

    fn struct_(mut self) -> Sequence<'a, 'de> {
        self.is_struct = true;
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
            self.de.seq_pos = Some(SequencePos::TupleStart);
        } else if self.tuple_length == Some(self.idx + 1) {
            self.de.seq_pos = Some(SequencePos::TupleEnd);
        } else {
            self.de.seq_pos = None;
        }

        // Tuples are parsed up to their known number of elements.
        // Other sequences are parsed until tokens run out.
        let elements_remain = match self.tuple_length {
            Some(x) => x > self.idx,
            None => self.de.has_next_token(),
        };

        if elements_remain {
            // If the first element of tuple is Option type, it may look like
            // there's no next token. So always try to deserialize at tuple
            // start.

            // Enter body needs to be deferred up to token-reading for tuples
            // so that Option head value can cancel the deferral.
            if self.tuple_length.is_some()
                && self.idx == 0
                && self.de.mode == ParsingMode::Block
            {
                self.de.delayed_enter_body_requested = true;
            }
            let ret = seed.deserialize(&mut *self.de).map(Some);
            // Just to be safe.
            self.de.delayed_enter_body_requested = false;

            // Only ever parse the first item in Line mode, dive in and start
            // doing block from then on.
            if self.de.mode == ParsingMode::Line {
                self.de.mode = ParsingMode::Block;

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
                    self.de.mode = ParsingMode::Block;
                    self.de.seq_pos = None;
                }
            }

            ret
        } else {
            // Exit when not finding more elements.
            if self.de.mode.is_inline() {
                self.de.exit_line()?;
            } else {
                if !self.is_merged {
                    self.de.exit_body()?;
                }
            }
            self.de.mode = ParsingMode::Block;
            self.de.seq_pos = None;
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
        if self.contents_mode
            || !self.de.cursor.has_headline_content(self.de.current_depth)
        {
            return Ok(None);
        }

        if self.is_struct {
            self.de.mode = ParsingMode::Key(false);
            if self.de.cursor.clone().key().is_err() {
                // There's still content, but no valid key. Do the magic bit
                // and conjure up a '_contents' key for the remaining content.
                self.de.mode = ParsingMode::Key(true);
                // Also mess with depth so it looks like this is a whole new
                // section with an empty headline, the whole remaining body
                // needs to end up in the _contents part.
                self.de.current_depth -= 1;
                self.contents_mode = true;
            }
        } else {
            if self.de.cursor.is_section(self.de.current_depth) {
                // Has both headline and body.
                // Assume full headline is key, body is content.
                self.de.mode = ParsingMode::Line;
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
            self.de.mode = ParsingMode::Block;
        }
        ret
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        let need_exit = if self.de.mode == ParsingMode::Line {
            self.de.mode = ParsingMode::Block;
            true
        } else {
            false
        };
        let ret = seed.deserialize(&mut *self.de);
        if need_exit {
            self.de.exit_body()?;
        }
        ret
    }
}
