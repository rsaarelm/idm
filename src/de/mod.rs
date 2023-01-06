use crate::{err, CharExt, Error, Result};
use parser::Parser;
use serde::de::{self, IntoDeserializer};

mod fragment;
pub(crate) mod parse;
mod parser;

pub fn from_str<'a, T>(input: &'a str) -> crate::Result<T>
where
    T: de::Deserialize<'a>,
{
    T::deserialize(&mut Deserializer::from_str(input)?)
}

type Deserializer<'de> = Parser<'de>;

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

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
        visitor.visit_bool(self.read()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.read()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.read()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.read()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.read()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.read()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.read()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.read()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.read()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.read()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.read()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_char(self.read()?)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_str(
            self.read_str()?
                .trim_end_matches(CharExt::is_idm_whitespace),
        )
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
        unimplemented!();
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
        // No support for missing values, assume that if serde requests it,
        // there should be a Some that can be read there.
        visitor.visit_some(self)
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

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.enter_seq()?;
        let ret = visitor.visit_seq(Sequence::new(self))?;
        self.exit()?;
        Ok(ret)
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if len == 1 {
            // This is an singleton tuple, it's the marker for the special
            // form. If this shows up at the start of a sequence, change the
            // sequence into the special pair.
            self.enter_special()?;
        } else {
            self.enter_tuple(len)?;
        }
        let ret = visitor.visit_seq(Sequence::new(self))?;
        if len > 1 {
            // Don't call exit with special form, it didn't involve a push in
            // the state machine.
            self.exit()?;
        }
        Ok(ret)
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
        // Assuming tuple struct len will always be >= 2 and a single value
        // will go to deserialize_newtype_struct instead. Otherwise we'd want
        // to avoid triggering `enter_special` from here since it's meant to
        // be used only with a plain singleton tuple.
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.enter_map()?;
        let ret = visitor.visit_map(Sequence::new(self))?;
        self.exit()?;
        Ok(ret)
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
        self.enter_struct(fields)?;
        let ret = visitor.visit_map(Sequence::new(self))?;
        self.exit()?;
        Ok(ret)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if self.is_word() {
            // Only a single word, parse unit variant.
            visitor.visit_enum(self.read_str()?.into_deserializer())
        } else {
            // Nonunit enums look similar to the key-value pairs of a map.
            self.enter_tuple(2)?;
            let ret = visitor.visit_enum(Enum::new(self))?;
            self.exit()?;
            Ok(ret)
        }
    }
}

struct Sequence<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> Sequence<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Sequence { de }
    }
}

impl<'a, 'de> de::SeqAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        // This one you can trust for termination.
        if self.de.is_really_empty() {
            return Ok(None);
        }

        // XXX Hack, can't trust regular is_empty here since if we end up
        // requesting a raw item, there might be a trailing comment that is
        // otherwise interpreted as an empty sequence.
        if self.de.is_empty() {
            // XXX: Is it okay to use the same deserializer here instead of
            // using a throwaway clone for when deserialize fails? Current
            // deserializer implementation isn't designed to be cheap to
            // clone...
            if let Ok(raw) = seed.deserialize(&mut *self.de) {
                return Ok(raw).map(Some);
            } else {
                return Ok(None);
            }
        }

        seed.deserialize(&mut *self.de).map(Some)
    }
}

impl<'a, 'de> de::MapAccess<'de> for Sequence<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.de.is_empty() {
            return Ok(None);
        }

        self.de.enter_tuple(2)?;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        let ret = seed.deserialize(&mut *self.de)?;
        self.de.exit()?;
        Ok(ret)
    }
}

struct Enum<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> Enum<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Enum { de }
    }
}

impl<'de, 'a> de::EnumAccess<'de> for Enum<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let val = seed.deserialize(&mut *self.de)?;
        Ok((val, self))
    }
}

impl<'de, 'a> de::VariantAccess<'de> for Enum<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        // If we're in VariantAccess, the parse element wasn't a standalone
        // word. This means it can't be an unit variant.
        err!("Data after unit variant")
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(&mut *self.de, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(&mut *self.de, "", fields, visitor)
    }
}
