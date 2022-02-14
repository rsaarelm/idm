use std::{cell::RefCell, rc::Rc, str::FromStr};

use crate::{fragment::Fragment, parse, Error, Result};
use serde::de;

pub fn from_str<'a, T>(input: &'a str) -> crate::Result<T>
where
    T: de::Deserialize<'a>,
{
    T::deserialize(Deserializer::from_str(input)?)
}

#[derive(Clone, Default, Debug)]
struct Deserializer<'de> {
    inner: Fragment<'de>,

    input: &'de str,
    raw_mode_track: Rc<RefCell<Option<Fragment<'de>>>>,
}

impl<'de> Deserializer<'de> {
    fn from_str(input: &'de str) -> Result<Self> {
        let inner = Fragment::new(input)?;
        Ok(Deserializer {
            inner,
            input,
            ..Default::default()
        })
    }

    fn spawn(&self, inner: Fragment<'de>) -> Self {
        // Retain input reference.
        let mut ret = self.clone();
        ret.inner = inner;
        ret.raw_mode_track = Default::default();
        ret
    }

    fn spawn_checkpointed(
        &self,
        inner: Fragment<'de>,
        raw_mode_track: Rc<RefCell<Option<Fragment<'de>>>>,
    ) -> Deserializer<'de> {
        let mut ret = self.spawn(inner);
        ret.raw_mode_track = raw_mode_track;
        ret
    }

    fn restore(&mut self) {
        if let Some(t) = self.raw_mode_track.borrow_mut().take() {
            self.inner = t;
        }
    }

    fn error(&self, msg: &'static str) -> Error {
        let error = Error::new(msg);
        if let Some(s) = self.inner.str_slice() {
            error.infer_line_num(self.input, s)
        } else {
            error
        }
    }

    fn err<T>(&self, msg: &'static str) -> Result<T> {
        Err(self.error(msg))
    }

    pub fn parse<T: FromStr>(&self) -> Result<T> {
        let s = self.inner.to_str();
        s.trim_end().parse().map_err(|_| {
            let mut e = Error::new("Failed to parse value");
            if let Some(s) = self.inner.str_slice() {
                e = e.infer_line_num(self.input, s);
            }
            e
        })
    }
}

impl<'de> std::ops::Deref for Deserializer<'de> {
    type Target = Fragment<'de>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'de> std::ops::DerefMut for Deserializer<'de> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'de> de::Deserializer<'de> for Deserializer<'de> {
    type Error = Error;

    // {{{1 Primitive types
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_bool(self.parse()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.parse()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.parse()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.parse()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.parse()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.parse()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.parse()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.parse()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.parse()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.parse()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.parse()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_char(self.parse()?)
    }

    // {{{1 Misc types
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_str(self.to_str().trim_end())
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // Deserializing bytes indicates raw mode.
        //
        // Roll back in case we're in the middle of a sequence and need to
        // walk back on skipping comments and blanks that need to be included
        // in raw mode.
        self.restore();
        visitor.visit_bytes(self.to_str().trim_end().as_bytes())
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
        // IDM doesn't support explicit None values, but the last element of a
        // tuple can read as None if the values stop early.
        if matches!(*self, Fragment::Empty) {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
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

    // {{{1 Structured types
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(Sequence::new(self))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
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
    // }}}
}

struct Sequence<'de> {
    fragment: Deserializer<'de>,

    tuple_length: Option<usize>,
    idx: usize,
    struct_fields: Option<&'static [&'static str]>,
    map_element: Option<Deserializer<'de>>,
}

impl<'de> Sequence<'de> {
    // {{{1
    fn new(fragment: Deserializer<'de>) -> Sequence<'de> {
        Sequence {
            fragment,
            tuple_length: None,
            idx: 0,
            struct_fields: None,
            map_element: None,
        }
    }

    fn tuple_length(mut self, len: usize) -> Self {
        self.tuple_length = Some(len);
        self
    }

    fn as_struct(mut self, fields: &'static [&'static str]) -> Self {
        self.struct_fields = Some(fields);
        self
    }

    fn contains_field(&self, field: &str) -> bool {
        self.struct_fields.map_or(false, |s| s.contains(&field))
    }
    // }}}
}

impl<'de> de::SeqAccess<'de> for Sequence<'de> {
    // {{{1
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if matches!(*self.fragment, Fragment::Empty)
            && self.tuple_length.is_none()
        {
            return Ok(None);
        }

        // Make a checkpoint so that deserializer can switch tracks if it
        // needs to parse a raw mode value. The checkpoint is stored in a
        // RefCell visible to Sequence, so we can also reroute sequence here
        // if the deserializer consumed the checkpoint.

        // Checkpoint where sequence iteration will restart from in raw mode.
        // Either the current starting fragment, or a reused fragment
        // checkpoint.
        let sequence_checkpoint;
        // RefCell that's shared between deserialize and sequence iteration.
        // Sharing is used to determine if deserializer encountered raw mode
        // and consumed its checkpoint.
        let raw_mode_track_cell;

        if let Some(existing_checkpoint) =
            self.fragment.raw_mode_track.borrow().as_ref()
        {
            // An unconsumed checkpoint exists, reuse it and the cell.
            sequence_checkpoint = existing_checkpoint.clone();
            raw_mode_track_cell = self.fragment.raw_mode_track.clone();
        } else {
            // No checkpoint, make a brand new one
            sequence_checkpoint = (*self.fragment).clone();
            raw_mode_track_cell = Rc::new(RefCell::new(None));
        }

        // Fragment to deserialize if we're not in raw mode.
        let fragment;

        // Checkpoint fed to fragment deserializer, child node of current
        // fragment. Constructed from sequence checkpoint which might have
        // been reused earlier.
        let fragment_checkpoint;

        // Next sequence fragment if we don't switch to raw mode.
        let mut next_fragment = (*self.fragment).clone();

        let is_last_tuple_element = self.tuple_length == Some(self.idx + 1);
        let is_pair = self.tuple_length == Some(2);
        let is_inline = next_fragment.is_inline();

        if is_last_tuple_element && (is_pair || is_inline) {
            // Fuse second item if tuple is inline or a pair, instead of
            // splitting into it just grab the whole item.
            fragment = (*self.fragment).clone();
            next_fragment = Fragment::Empty;

            // Fragment checkpoint is split from sequence checkpoint, not from
            // the input fragment (which might already be wrong if we need to
            // switch to raw mode track).
            fragment_checkpoint = sequence_checkpoint.clone();
        } else {
            // Make sure main fragment is a Fragment::Empty even if we hit end
            // of input. Main won't deserialize then, but if deser hits
            // checkpoint, it might still find something there.
            fragment = next_fragment
                .find_map(Fragment::comment_filter)
                .unwrap_or(Fragment::Empty);

            // Use raw mode split instead of regular split for checkpoint.
            fragment_checkpoint = sequence_checkpoint
                .clone()
                .split_raw()
                .map_or(Fragment::Empty, |(a, _)| a);
        }

        // Rewrite the cell (shared or new) with the new checkpoint.
        *raw_mode_track_cell.borrow_mut() = Some(fragment_checkpoint.clone());

        self.idx += 1;
        let ret = seed
            .deserialize(
                self.fragment
                    .spawn_checkpointed(fragment, raw_mode_track_cell.clone()),
            )
            .map(Some);

        if raw_mode_track_cell.borrow().is_none() {
            // Deserialize has consumed the checkpoint. We are now in raw mode
            // track, adjust accordingly.

            // Split using raw mode logic.
            if let Some((_, tail)) = sequence_checkpoint.split_raw() {
                self.fragment = self.fragment.spawn(tail);
            } else {
                self.fragment = self.fragment.spawn(Fragment::Empty);
            }
        } else {
            self.fragment = self.fragment.spawn(next_fragment);
        }

        ret
    }
    // }}}
}

impl<'de> de::MapAccess<'de> for Sequence<'de> {
    // {{{1
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        let fallback_for_contents = self.fragment.clone();
        self.map_element = self
            .fragment
            .find_map(Fragment::comment_filter)
            .map(|f| self.fragment.spawn(f));
        if let Some(elt) = self.map_element.as_mut() {
            let mut key = elt
                .next()
                .ok_or_else(|| self.fragment.error("No map key found"))?;
            if self.struct_fields.is_some() {
                if let Ok((mangled, _)) = parse::attribute_name(&*key.to_str())
                {
                    // Field must be expected.
                    if !self.contains_field(&mangled) {
                        // XXX: Support String messages, add field name to
                        // message.
                        return fallback_for_contents.err("Unexpected field");
                    }
                    // It's a struct, parse attribute names.
                    key.rewrite(mangled);
                } else if self.contains_field("_contents") {
                    // Out of parseable names, but struct expects contents.
                    key.rewrite("_contents");
                    // Reset map_element to be the complete remaining element
                    // (and remove this from fragment).
                    self.map_element = Some(fallback_for_contents);
                    self.fragment = self.fragment.spawn(Fragment::Empty);
                } else {
                    // There are non-attribute contents, but no _contents
                    // field to contain them, this is an error.
                    return fallback_for_contents
                        .err("Unhandled content in struct");
                }
            }

            seed.deserialize(self.fragment.spawn(key)).map(Some)
        } else {
            // TODO: Error if we're missing expected fields?
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        if let Some(elt) = self.map_element.take() {
            seed.deserialize(elt)
        } else {
            self.fragment.err("No map element for value")
        }
    }
    // }}}
}

/* vim:set foldmethod=marker: */
