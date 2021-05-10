use crate::{err, Error, Result};
use serde::{ser, Serialize};
use std::fmt;

/// Maximum line length for inlined compound expressions.
const MAX_INLINE_SEQ_LENGTH: usize = 80;

pub fn to_string<T>(value: &T) -> Result<String>
where
    T: ser::Serialize,
{
    let mut serializer = Serializer::default();
    let expr = value.serialize(&mut serializer)?;
    Ok(format!("{}", expr))
}

/// Descriptor for elements that correspond to an atomic value.
#[derive(Eq, PartialEq, Clone, Debug)]
enum Value {
    /// Value with no whitespace.
    ///
    /// Guaranteed to be nonempty.
    Word(String),
    /// Value that contains spaces but no newlines.
    ///
    /// Guaranteed to be nonempty and to contain at least one space.
    /// Is usually a String.
    Line(String),
    /// Value that contains newlines.
    ///
    /// Guaranteed to be nonempty and to contain at least one newline.
    /// Is usually a String.
    Paragraph(String),
}

use Value::*;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word(s) => write!(f, "{}", s),
            Line(s) => write!(f, "{}", s),
            Paragraph(s) => write!(f, "{}", s),
        }
    }
}

impl Value {
    pub fn new(text: impl AsRef<str>) -> Result<Value> {
        let text = text.as_ref().trim_end().to_string();

        if text.is_empty() {
            return err!("Empty string for value");
        }

        let mut newline = false;
        let mut space = false;
        for c in text.chars() {
            if c.is_whitespace() {
                space = true;
            }
            if c == '\n' {
                newline = true;
                // We can't learn anything more, might as well call it quits.
                break;
            }
        }

        if newline {
            Ok(Value::Paragraph(text))
        } else if space {
            Ok(Value::Line(text))
        } else {
            Ok(Value::Word(text))
        }
    }

    pub fn empty_line() -> Value {
        Value::Paragraph("\n".into())
    }

    fn from<T: fmt::Display>(item: T) -> Result<Value> {
        Value::new(format!("{}", item))
    }

    fn len(&self) -> usize {
        match self {
            Word(s) => {
                debug_assert!(!s.is_empty());
                s.len()
            }
            Line(s) => {
                debug_assert!(!s.is_empty());
                s.len()
            }
            Paragraph(s) => {
                debug_assert!(!s.is_empty());
                s.len()
            }
        }
    }

    /// Print value inline.
    ///
    /// Not permitted for `Paragraph`.
    fn print_inline(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word(s) => write!(f, "{}", s),
            Line(s) => write!(f, "{}", s),
            Paragraph(_) => panic!("print_inline: Paragraphs can't be inlined"),
        }
    }

    /// Print value as part of outline at given depth.
    fn print_outline(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: i32,
    ) -> fmt::Result {
        match self {
            Word(s) => {
                indent(f, depth)?;
                writeln!(f, "{}", s)
            }
            Line(s) => {
                indent(f, depth)?;
                writeln!(f, "{}", s)
            }
            Paragraph(s) => {
                for line in s.lines() {
                    if line.trim().is_empty() {
                        // Don't indent empty lines.
                        writeln!(f)?;
                    } else {
                        indent(f, depth)?;
                        writeln!(f, "{}", line)?;
                    }
                }
                Ok(())
            }
        }
    }
}

/// Descriptor for an expression for a possibly structural value.
#[derive(Eq, PartialEq, Clone, Debug)]
enum Expr {
    /// Empty expression.
    None,
    /// Result of Some(expr)
    ///
    /// Mostly can just be unwrapped and recursed, but the special feature is
    /// that Some values can't be inline tokes, even if the inner value could
    /// be.
    Some(Box<Expr>),
    /// Non-structural value.
    Atom(Value),
    /// Map or struct entry.
    Entry { key: Box<Expr>, value: Box<Expr> },
    /// Expression sequence for a list or a tuple.
    Seq(Vec<Expr>),
    /// Outline expression with headline and body.
    Section { head: Box<Expr>, body: Vec<Expr> },
}

use Expr::{Atom, Entry, Section, Seq};

impl Default for Expr {
    fn default() -> Self {
        Expr::None
    }
}

impl Expr {
    fn from<T: fmt::Display>(item: T) -> Result<Expr> {
        let s = format!("{}", item);
        if s.trim_end().is_empty() {
            Ok(Expr::None)
        } else {
            Ok(Atom(Value::from(item)?))
        }
    }

    /// Try to append a subexpression to an expression in a natural way.
    ///
    /// Atomic expressions need to be `sectionize`d before they can be pushed
    /// to.
    fn push(&mut self, e: Expr) {
        match self {
            Seq(es) => es.push(e),
            Section { body, .. } => body.push(e),
            _ => panic!("Can't append to this expr type"),
        }
    }

    /// Push elements from a Seq expr into self.
    fn concat(&mut self, e: Expr) {
        match e {
            Seq(es) => {
                for e in es.into_iter() {
                    self.push(e);
                }
            }
            e => self.push(e),
        }
    }

    fn is_none(&self) -> bool {
        match self {
            Expr::None => true,
            _ => false,
        }
    }

    fn is_inline_token(&self) -> bool {
        match self {
            Atom(Word(_)) => true,
            _ => false,
        }
    }

    /// Expression needs multiple lines at the base indent depth.
    fn is_block(&self) -> bool {
        match self {
            // None doesn't actually make sense showing up on its own.
            // But you can put a group character with an empty body,
            // ie treat it as a non-listable item.
            Expr::None => true,
            Expr::Some(s) => s.is_block(),
            Atom(Paragraph(_)) => true,
            Seq(es) => {
                !es.iter().all(|e| e.is_inline_token())
                    || self.len() > MAX_INLINE_SEQ_LENGTH
            }
            _ => false,
        }
    }

    /// Expression can be printed as single line.
    fn is_line(&self) -> bool {
        !self.is_block() && !self.is_section()
    }

    fn is_section(&self) -> bool {
        match self {
            Section { .. } => true,
            Entry { value, .. } => {
                !value.is_line() || self.len() > MAX_INLINE_SEQ_LENGTH
            }
            _ => false,
        }
    }

    fn is_seq(&self) -> bool {
        match self {
            Seq(_) => true,
            _ => false,
        }
    }

    fn is_matrix(&self) -> bool {
        match self {
            Seq(es) => es.iter().any(|e| e.is_seq()),
            _ => false
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Atom(x) if x.len() == 0 => true,
            Seq(es) if es.is_empty() => true,
            _ => false,
        }
    }

    fn is_empty_line(&self) -> bool {
        match self {
            Atom(Paragraph(x)) if x == "\n" => true,
            _ => false,
        }
    }

    fn len(&self) -> usize {
        match self {
            Expr::None => 0,
            Expr::Some(e) => e.len(),
            Atom(v) => v.len(),
            Entry { key, value } => key.len() + 1 + value.len(),
            Seq(es) if !es.is_empty() => {
                es.iter().map(|e| e.len()).sum::<usize>() + es.len() - 1
            }
            Seq(_) => 0,
            Section { head, body } => head.len() + 1 + body.len(),
        }
    }

    fn print_inline(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Some(e) => e.print_inline(f),
            Atom(v) => v.print_inline(f),
            Entry { key, value } => {
                key.print_inline(f)?;
                write!(f, " ")?;
                value.print_inline(f)
            }
            Seq(es) if !self.is_block() => {
                for (i, e) in es.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    e.print_inline(f)?;
                }
                Ok(())
            }
            _ => panic!("print_inline: Can't inline expression {:?}", self),
        }
    }

    fn print_outline(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: i32,
        prev_depth: i32,
    ) -> fmt::Result {
        match self {
            Expr::None => {
                if prev_depth < depth {
                    Ok(())
                } else {
                    indent(f, depth)?;
                    writeln!(f, ",")
                }
            }
            Expr::Some(e) => e.print_outline(f, depth, prev_depth),
            e if e.is_empty_line() => writeln!(f),
            Atom(Word(v)) if v.chars().all(|c| c == ',') => {
                // Escape literal comma
                indent(f, depth)?;
                writeln!(f, ",{}", v)
            }
            Atom(v) => v.print_outline(f, depth),
            Entry { key, value } => {
                indent(f, depth)?;
                key.print_inline(f)?;
                if value.is_line() {
                    write!(f, " ")?;
                    value.print_inline(f)
                } else if value.is_section() {
                    writeln!(f)?;
                    value.print_outline(f, depth + 1, depth)
                } else {
                    writeln!(f)?;
                    value.print_outline(f, depth, depth)
                }
            }
            Section { head, body } => {
                head.print_outline(f, depth, prev_depth)?;
                for (i, e) in body.iter().enumerate() {
                    let prev_depth = if i == 0 { depth } else { depth + 1 };
                    e.print_outline(f, depth + 1, prev_depth)?;
                }
                Ok(())
            }
            Seq(es) => {
                for (i, e) in es.iter().enumerate() {
                    let print_separator = i > 0;
                    if e.is_block() {
                        // Outline sequence
                        if print_separator {
                            indent(f, depth)?;
                            writeln!(f, ",")?;
                        }
                        // We can give these all prev_depth = depth since past
                        // the first one we print the separator comma here.
                        e.print_outline(f, depth + 1, depth)?;
                    } else if e.is_section() {
                        // Outline sequence with headlines as natural
                        // separators.
                        //
                        // Now we do need to set prev_depth based on where in
                        // the sequence we are
                        let prev_depth = if i == 0 { depth } else { depth + 1 };
                        e.print_outline(f, depth + 1, prev_depth)?;
                    } else {
                        // Inline sequence
                        indent(f, depth + 1)?;
                        e.print_inline(f)?;
                        writeln!(f)?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print_outline(f, -1, -2)
    }
}

#[derive(Default)]
struct Serializer;

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = Expr;
    type Error = Error;

    type SerializeSeq = SeqSerializer;
    type SerializeTuple = SeqSerializer;
    type SerializeTupleStruct = SeqSerializer;
    type SerializeTupleVariant = Self;
    type SerializeMap = MapSerializer;
    type SerializeStruct = MapSerializer;
    type SerializeStructVariant = MapSerializer;

    fn serialize_bool(self, v: bool) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_i8(self, v: i8) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_i16(self, v: i16) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_i32(self, v: i32) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_i64(self, v: i64) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_u8(self, v: u8) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_u16(self, v: u16) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_u32(self, v: u32) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_u64(self, v: u64) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_f32(self, v: f32) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_f64(self, v: f64) -> Result<Expr> {
        Expr::from(v)
    }

    fn serialize_char(self, v: char) -> Result<Expr> {
        if v.is_whitespace() {
            return err!("Can't serialize whitespace chars");
        }

        Expr::from(v)
    }

    fn serialize_str(self, v: &str) -> Result<Expr> {
        if v.trim_end().is_empty() {
            Ok(Atom(Value::empty_line()))
        } else {
            Expr::from(v)
        }
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Expr> {
        unimplemented!();
    }

    fn serialize_none(self) -> Result<Expr> {
        Ok(Expr::None)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Expr>
    where
        T: ?Sized + Serialize,
    {
        Ok(Expr::Some(Box::new(value.serialize(self)?)))
    }

    fn serialize_unit(self) -> Result<Expr> {
        Ok(Expr::None)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Expr> {
        Ok(Expr::None)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Expr> {
        unimplemented!();
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Expr>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Expr>
    where
        T: ?Sized + Serialize,
    {
        unimplemented!();
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SeqSerializer::new(len.unwrap_or(0)))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        unimplemented!();
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(MapSerializer::default())
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct> {
        // TODO: Struct fields need different handling
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        unimplemented!();
    }
}

#[derive(Default)]
struct SeqSerializer {
    idx: usize,
    len: usize,
    /// Will contain the first item if this is an indent-style tuple.
    acc: Expr,
}

impl SeqSerializer {
    pub fn new(len: usize) -> SeqSerializer {
        SeqSerializer {
            len,
            ..Default::default()
        }
    }

    fn serialize_tuple_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let elt = value.serialize(&mut Serializer::default())?;

        // Got some early exits coming ahead, so let's just increment right
        // now and write the logic below aginst the + 1 value.
        self.idx += 1;

        if self.idx == 1 {
            // Let's see how we kick things off.
            if elt.is_inline_token() {
                // We might be able to inline the whole thing, let's go with
                // seq.
                self.acc = Seq(vec![elt]);
            } else if elt.is_seq() {
                // Also the first thing is already a seq, let's go with this
                // again.
                self.acc = Seq(vec![elt]);
            } else {
                // Otherwise start in section shape
                self.acc = Section {
                    head: Box::new(elt),
                    body: Vec::new(),
                };
            }
            return Ok(());
        }

        // Special trick: If the last tuple item is a seq, we concatenate it
        // to the current tuple instead of adding it as its own object.
        //
        // (Don't do this for tuples that are seq-like rather than sections)
        if self.idx > 1 && self.idx == self.len && !self.acc.is_matrix() {
            match elt {
                Seq(es) => {
                    for e in es {
                        self.acc.push(e);
                    }
                    return Ok(());
                }
                _ => {}
            }
        }

        self.acc.push(elt);
        Ok(())
    }
}

impl<'a> ser::SerializeSeq for SeqSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if self.idx == 0 {
            // This is seq, not tuple. Immediately start doing a seq block,
            // don't make it into Section.
            self.acc = Seq(Vec::new());
        }
        self.idx += 1;

        self.acc.push(value.serialize(&mut Serializer::default())?);
        Ok(())
    }

    fn end(self) -> Result<Expr> {
        Ok(self.acc)
    }
}

impl<'a> ser::SerializeTuple for SeqSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_tuple_element(value)
    }

    fn end(self) -> Result<Expr> {
        Ok(self.acc)
    }
}

impl<'a> ser::SerializeTupleStruct for SeqSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_tuple_element(value)
    }

    fn end(self) -> Result<Expr> {
        Ok(self.acc)
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        unimplemented!();
    }

    fn end(self) -> Result<Expr> {
        unimplemented!();
    }
}

#[derive(Default)]
struct MapSerializer {
    values: Vec<(Expr, Expr)>,
    /// Stuff inside a struct's special '_contents' field.
    contents: Expr,
}

impl<'a> ser::SerializeMap for MapSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let key = key.serialize(&mut Serializer::default())?;
        if key.is_line() {
            // Put a dummy value, fill it in in serialize_value.
            self.values.push((key, Expr::None));
            Ok(())
        } else {
            err!("serialize_key: Non-line key {:?}", key)
        }
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        // Should have been started by serialize_key before we get here.
        debug_assert!(!self.values.is_empty());

        let value = value.serialize(&mut Serializer::default())?;
        let idx = self.values.len() - 1;
        if value.is_empty() {
            // Empty value, do not save.
            self.values.pop();
        } else {
            // Replace dummy value with actual one.
            let (key, _) = self.values[idx].clone();
            self.values[idx] = (key, value);
        }

        Ok(())
    }

    fn end(self) -> Result<Expr> {
        let mut ret = Seq(Vec::new());
        for (key, value) in self.values.into_iter() {
            ret.push(Entry {
                key: Box::new(key),
                value: Box::new(value),
            });
        }
        Ok(ret)
    }
}

impl<'a> ser::SerializeStruct for MapSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let value = value.serialize(&mut Serializer::default())?;
        if value.is_empty() {
            return Ok(());
        }

        if !value.is_empty() {
            if key == "_contents" {
                // Magic contents key!
                self.contents = value;
            } else {
                // Just a regular key, kebabize it.
                let kebab_key = key.replace("_", "-");
                self.values.push((
                    Expr::from(format!("{}:", kebab_key)).unwrap(),
                    value,
                ));
            }
        }
        Ok(())
    }

    fn end(self) -> Result<Expr> {
        let mut ret = Seq(Vec::new());
        for (key, value) in self.values.into_iter() {
            ret.push(Entry {
                key: Box::new(key),
                value: Box::new(value),
            });
        }
        // Put contents after all the entries if there was one
        if !self.contents.is_none() {
            ret.concat(self.contents);
        }
        Ok(ret)
    }
}

impl<'a> ser::SerializeStructVariant for MapSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Expr> {
        ser::SerializeStruct::end(self)
    }
}

fn indent(f: &mut fmt::Formatter<'_>, n: i32) -> fmt::Result {
    for _ in 0..n {
        write!(f, "\t")?;
    }
    Ok(())
}
