use serde::{ser, Serialize};

use crate::{err, parse::CharExt, Error, Result};

#[derive(Clone, Default, Debug)]
pub(crate) enum Expr {
    #[default]
    /// Empty string, allowed in `SpecialPair` head.
    None,

    /// Atomic word.
    Word(String),
    /// Atomic word known to come from a numeric type.
    Number(String),
    /// Atomic string with spaces but no newlines.
    Line(String),
    /// Atomic string with at least one newline.
    Paragraph(String),

    /// Open-ended sequence.
    Seq(Vec<Expr>),

    /// Tuple with known length.
    ///
    /// This needs to be kept separate from `Seq` since only a singleton tuple
    /// can be a special pair marker.
    Tuple(Vec<Expr>),

    Map(Vec<(Expr, Expr)>),
    Struct(Vec<(&'static str, Expr)>),

    EnumVariant(&'static str, Box<Expr>),

    /// IDM outline special form.
    SpecialPair(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn word(v: impl ToString) -> Result<Self> {
        if let Expr::Word(s) = Expr::from_str(&v.to_string())? {
            Ok(Expr::Word(s))
        } else {
            err!("Expr::word: Not a word")
        }
    }

    pub fn number(v: impl ToString) -> Result<Self> {
        if let Expr::Word(s) = Expr::from_str(&v.to_string())? {
            Ok(Expr::Number(s))
        } else {
            err!("Expr::number: Not a word")
        }
    }

    pub fn from_str(s: &str) -> Result<Self> {
        if s.chars().next().map_or(false, |c| c.is_idm_whitespace()) {
            return err!("Expr::from_str: Leading whitespace");
        }

        let mut is_line = false;
        for c in s.chars() {
            // XXX: Could do stronger paragraph verification here, check that
            // indentation is consistent and figure out indentation style.
            // Going to punt that over to the Write serializer for now though.
            if c == '\n' {
                return Ok(Expr::Paragraph(s.to_string()));
            }
            if c.is_idm_whitespace() {
                is_line = true;
            }
        }

        if s.is_empty() {
            // This is permissible in special-head.
            Ok(Expr::None)
        } else if is_line {
            Ok(Expr::Line(s.to_string()))
        } else {
            Ok(Expr::Word(s.to_string()))
        }
    }

    pub fn push(&mut self, expr: Expr) -> Result<()> {
        if expr.is_empty() && !matches!(self, Expr::Tuple(_)) {
            // Empty elements are allowed in the head and body of a special
            // pair. We don't know what'll become a special pair yet, but the
            // containers must be tuple-like.
            //
            // Raise an error right here with anything that isn't a tuple. For
            // tuples, empty exprs that don't get converted into a special
            // pair will raise an error during normalization.
            return err!("Expr::push: Unexpected empty expr");
        }

        match self {
            Expr::Seq(elts) | Expr::Tuple(elts) => {
                elts.push(expr);
                Ok(())
            }
            Expr::Map(elts) => {
                elts.push((expr, Default::default()));
                Ok(())
            }
            Expr::EnumVariant(_, e) => e.push(expr),
            _ => panic!("Expr::push: Bad variant"),
        }
    }

    pub fn set_map_value(&mut self, expr: Expr) {
        match self {
            Expr::Map(elts) => {
                assert!(!elts.is_empty(), "Expr::set_map_value: No keys");
                let n = elts.len() - 1;
                elts[n].1 = expr;
            }
            Expr::EnumVariant(_, e) => e.set_map_value(expr),
            _ => panic!("Expr::set_map_value: Not a map"),
        }
    }

    pub fn push_field(&mut self, name: &'static str, value: Expr) {
        match self {
            Expr::Struct(elts) => elts.push((name, value)),
            Expr::EnumVariant(_, e) => e.push_field(name, value),
            _ => panic!("Expr::push_field: Not a struct"),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Expr::None => true,
            Expr::Seq(elts) if elts.is_empty() => true,
            Expr::Map(elts) if elts.is_empty() => true,
            _ => false,
        }
    }

    fn is_singleton(&self) -> bool {
        matches!(self, Expr::Tuple(es) if es.len() == 1)
    }

    pub fn is_mappish(&self) -> bool {
        matches!(self, Expr::Struct(_) | Expr::Map(_))
    }

    pub fn is_word(&self) -> bool {
        matches!(self, Expr::Word(_) | Expr::Number(_))
    }

    pub fn looks_like_comment(&self) -> bool {
        // NB. SpecialPair is not considered to look like a comment even if
        // its head starts with "--", that's part of the whole idea of the
        // special thing.
        match self {
            Expr::Word(a)
            | Expr::Number(a)
            | Expr::Line(a)
            | Expr::Paragraph(a) => a == "--" || a.starts_with("-- "),
            Expr::Seq(es) | Expr::Tuple(es) => {
                es.iter().next().map_or(false, |e| e.looks_like_comment())
            }
            _ => false,
        }
    }

    pub fn is_line(&self) -> bool {
        // NB. Do not recognize inline structs here. They are a special case
        // and aren't normally emitted by the serializer.
        match self {
            s if s.is_empty() => false,
            s if s.is_word() => true,
            Expr::Line(_) => true,
            Expr::Seq(es) | Expr::Tuple(es) => es.iter().all(|e| e.is_word()),
            Expr::EnumVariant(_, e) => e.is_line(),
            _ => false,
        }
    }

    pub fn is_section(&self) -> bool {
        match self {
            // SpecialPair is considered line-like even if the head is
            // Expr::None, this corresponds to a blank headline.
            Expr::SpecialPair(head, _)
                if head.is_line() || matches!(**head, Expr::None) =>
            {
                true
            }
            // Paragraphs are sections if all lines after the first are
            // indented.
            Expr::Paragraph(p)
                if p.lines().skip(1).all(|s| {
                    s.chars().next().map_or(true, |c| c.is_idm_whitespace())
                }) =>
            {
                true
            }
            // Enum variants are like standalone map items.
            Expr::EnumVariant(_, e) if !e.is_line() => true,
            _ => false,
        }
    }

    /// If the expr is a pair sequence with only the first element being
    /// singleton, turn it into a special pair. If it has a singleton in any
    /// other sequence position, or present in a sequence of other length than
    /// 2, raise an error.
    pub fn normalize(mut self) -> Result<Expr> {
        match self {
            Expr::Tuple(ref mut es)
                if es.len() == 2
                    && es[0].is_singleton()
                    && !es[1].is_singleton() =>
            {
                let body = es.pop().unwrap();
                let head = match es.pop() {
                    Some(Expr::Tuple(ref mut es)) if es.len() == 1 => {
                        es.pop().unwrap()
                    }
                    _ => panic!("Expr::normalize: Invalid singleton"),
                };
                if head.is_mappish()
                    && matches!(body, Expr::SpecialPair(ref head, _) if head.is_mappish())
                {
                    // This would lead to an unparseable result of
                    //     :elements from
                    //     :head part
                    //     :elements from
                    //     :body part
                    //     No way to tell where head ends and body begins
                    // since the head part and the body part are rendered at
                    // the same depth if head is mappish.
                    return err!("Expr::normalize: Invalid nesting of mappish-headed special pairs");
                }
                if !matches!(head, Expr::None)
                    && !head.is_line()
                    && !head.is_mappish()
                {
                    return err!(
                        "Expr::normalize: Invalid multiline special pair head"
                    );
                }
                Ok(Expr::SpecialPair(Box::new(head), Box::new(body)))
            }
            // Singletons are okay and can contain empty elements that would
            // cause an error with a sequence.
            e if e.is_singleton() => Ok(e),
            Expr::Seq(ref es) | Expr::Tuple(ref es) => {
                for e in es.iter() {
                    if e.is_singleton() {
                        return err!("Expr::normalize: Unexpected singleton");
                    } else if e.is_empty() {
                        return err!(
                            "Expr::normalize: Empty element in sequence"
                        );
                    }
                }
                Ok(self)
            }

            Expr::EnumVariant(a, e) => {
                Ok(Expr::EnumVariant(a, Box::new(e.normalize()?)))
            }
            _ => Ok(self),
        }
    }
}

pub(crate) struct Ast;

impl ser::Serializer for Ast {
    type Ok = Expr;
    type Error = Error;

    type SerializeSeq = Expr;
    type SerializeTuple = Expr;
    type SerializeTupleStruct = Expr;
    type SerializeTupleVariant = Expr;
    type SerializeMap = Expr;
    type SerializeStruct = Expr;
    type SerializeStructVariant = Expr;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        Expr::word(v)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        Expr::number(v)
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        Expr::word(v)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        Expr::from_str(v)
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok> {
        unimplemented!()
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        Ok(Expr::default())
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        Ok(Expr::default())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        Ok(Expr::default())
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        variant.serialize(self)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        Ok(Expr::EnumVariant(variant, Box::new(value.serialize(self)?)))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(Expr::Seq(Vec::new()))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Ok(Expr::Tuple(Vec::new()))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_tuple(len)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(Expr::EnumVariant(
            variant,
            Box::new(self.serialize_tuple(len)?),
        ))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(Expr::Map(Vec::new()))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct> {
        Ok(Expr::Struct(Vec::new()))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(Expr::EnumVariant(
            variant,
            Box::new(Expr::Struct(Vec::new())),
        ))
    }
}

impl ser::SerializeSeq for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::Serialize,
    {
        self.push(value.serialize(Ast)?)
    }

    fn end(self) -> Result<Self::Ok> {
        self.normalize()
    }
}

impl ser::SerializeTuple for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::Serialize,
    {
        self.push(value.serialize(Ast)?)
    }

    fn end(self) -> Result<Self::Ok> {
        self.normalize()
    }
}

impl ser::SerializeTupleStruct for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeTuple::end(self)
    }
}

impl ser::SerializeTupleVariant for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeTuple::end(self)
    }
}

impl ser::SerializeMap for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.push(key.serialize(Ast)?)
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.set_map_value(value.serialize(Ast)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.normalize()
    }
}

impl ser::SerializeStruct for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.push_field(key, value.serialize(Ast)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.normalize()
    }
}

impl ser::SerializeStructVariant for Expr {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Self::Ok> {
        self.normalize()
    }
}
