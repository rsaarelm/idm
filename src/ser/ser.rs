use std::io::{self, Write};

use serde::{ser, Serialize};

use crate::{
    err,
    ser::ast::{Ast, Expr},
    CharExt, Error, Result,
};

/// Serialize a value using the default indentation style.
pub fn to_string<T: ser::Serialize>(value: &T) -> Result<String> {
    to_string_styled(Default::default(), value)
}

/// Serialize a value using the given indentation style.
pub fn to_string_styled<T: ser::Serialize>(
    style: Indentation,
    value: &T,
) -> Result<String> {
    let mut buf = Vec::new();
    value.serialize(&mut Serializer::new(&mut buf).with_indentation(style))?;
    Ok(String::from_utf8(buf)?)
}

/// Serialize a value using indentation style inferred from an existing
/// IDM sample.
pub fn to_string_styled_like<T: ser::Serialize>(
    sample: &str,
    value: &T,
) -> Result<String> {
    to_string_styled(Indentation::infer(sample).unwrap_or_default(), value)
}

pub struct Serializer<W: Write> {
    indent: Indentation,
    w: W,
}

impl<W: Write> Serializer<W> {
    pub fn new(writer: W) -> Serializer<W> {
        Serializer {
            indent: Default::default(),
            w: writer,
        }
    }

    pub fn with_indentation(mut self, indent: Indentation) -> Serializer<W> {
        self.indent = indent;
        self
    }

    /// Toplevel write method, decide if the full expression is inline or
    /// outline.
    ///
    /// The trailing newline is significant, if a fragment can be fully
    /// inlined, it should be printed without a trailing newline.
    fn write(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            // Don't do comment escape if it's just a single atom.
            Expr::Word(_) | Expr::Number(_) | Expr::Line(_) => {
                self.write_inline(expr)
            }
            // Do switch to outline with sequences if the first item looks
            // commenty.
            Expr::Seq(_) | Expr::Tuple(_)
                if expr.is_line() && !expr.looks_like_comment() =>
            {
                self.write_inline(expr)
            }
            Expr::EnumVariant(_, a) if a.is_line() => self.write_inline(expr),
            _ => self.write_outline(0, false, expr),
        }
    }

    /// Write an inline expression, no indentation, no newline at end.
    fn write_inline(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Word(a) | Expr::Number(a) | Expr::Line(a) => {
                write!(&mut self.w, "{a}")?;
                Ok(())
            }
            Expr::Seq(es) | Expr::Tuple(es) if expr.is_line() => {
                for (i, e) in es.iter().enumerate() {
                    self.write_inline(e)?;
                    if i + 1 < es.len() {
                        write!(&mut self.w, " ")?;
                    }
                }
                Ok(())
            }
            Expr::EnumVariant(e, a) if a.is_line() => {
                write!(&mut self.w, "{e} ")?;
                self.write_inline(a)
            }
            _ => err!("Serializer::write_inline: Invalid expr"),
        }
    }

    /// Write an outline expression, indent and end in newline.
    fn write_outline(
        &mut self,
        depth: usize,
        adorn: bool,
        expr: &Expr,
    ) -> Result<()> {
        match expr {
            Expr::None => Ok(()),
            Expr::Word(a) | Expr::Number(a) | Expr::Line(a) => {
                self.indent.write(&mut self.w, depth)?;
                writeln!(&mut self.w, "{a}")?;
                Ok(())
            }
            Expr::Paragraph(a) => {
                let a = self.reformat_string(a.to_string())?;
                for line in a.lines() {
                    self.indent.write(&mut self.w, depth)?;
                    writeln!(&mut self.w, "{line}")?;
                }
                Ok(())
            }
            Expr::Seq(_) | Expr::Tuple(_)
                if expr.is_line() && !expr.looks_like_comment() =>
            {
                self.indent.write(&mut self.w, depth)?;
                self.write_inline(expr)?;
                writeln!(&mut self.w)?;
                Ok(())
            }
            Expr::Seq(es) | Expr::Tuple(es) => {
                for e in es {
                    let commenty = e.looks_like_comment();
                    if e.is_line() {
                        if commenty {
                            self.indent.write(&mut self.w, depth)?;
                            writeln!(&mut self.w, "--")?;
                            self.write_outline(depth + 1, false, e)?;
                        } else {
                            self.write_outline(depth, false, e)?;
                        }
                    } else if e.is_section() && !commenty {
                        self.write_outline(depth, false, e)?;
                    } else {
                        // Sequencing blocks, add extra indentation and a
                        // comment as a separator.
                        self.indent.write(&mut self.w, depth)?;
                        writeln!(&mut self.w, "--")?;
                        self.write_outline(depth + 1, false, e)?;
                    }
                }
                Ok(())
            }
            Expr::Map(es) => {
                for (k, v) in es {
                    // An Option value set to None, do not emit.
                    if v.is_empty() {
                        continue;
                    }

                    self.indent.write(&mut self.w, depth)?;
                    if adorn {
                        write!(&mut self.w, ":")?;
                    }
                    self.write_inline(k)?;
                    if k.is_word() && v.is_line() {
                        write!(&mut self.w, " ")?;
                        self.write_inline(v)?;
                        writeln!(&mut self.w)?;
                    } else {
                        writeln!(&mut self.w)?;
                        self.write_outline(depth + 1, false, v)?;
                    }
                }
                Ok(())
            }
            // TODO: Emit inline structs if configured to do so
            Expr::Struct(es) => {
                for (k, v) in es {
                    // An Option value set to None, do not emit.
                    if v.is_empty() {
                        continue;
                    }

                    self.indent.write(&mut self.w, depth)?;
                    if adorn {
                        write!(&mut self.w, ":")?;
                    }
                    write!(&mut self.w, "{k}")?;
                    if v.is_line() {
                        write!(&mut self.w, " ")?;
                        self.write_inline(v)?;
                        writeln!(&mut self.w)?;
                    } else {
                        writeln!(&mut self.w)?;
                        self.write_outline(depth + 1, false, v)?;
                    }
                }
                Ok(())
            }
            Expr::EnumVariant(e, a) => {
                self.indent.write(&mut self.w, depth)?;
                write!(&mut self.w, "{e}")?;
                if a.is_line() {
                    write!(&mut self.w, " ")?;
                    self.write_inline(a)?;
                    writeln!(&mut self.w)?;
                } else {
                    writeln!(&mut self.w)?;
                    self.write_outline(depth + 1, false, a)?;
                }
                Ok(())
            }
            Expr::SpecialPair(head, body) => {
                if head.is_mappish() {
                    self.write_outline(depth, true, head)?;
                    self.write_outline(depth, false, body)?
                } else if head.is_empty() || head.is_line() {
                    if head.is_empty() {
                        writeln!(&mut self.w)?;
                    } else {
                        self.indent.write(&mut self.w, depth)?;
                        self.write_inline(head)?;
                        writeln!(&mut self.w)?;
                    }
                    self.write_outline(depth + 1, false, body)?;
                } else {
                    return err!("Serializer::write: Bad special pair");
                }
                Ok(())
            }
        }
    }

    /// Reformat a multiline string value if necessary so that it will be a valid
    /// IDM fragment in the indentation style of the output.
    ///
    /// Returns an error if the string is not a valid IDM fragment to begin with.
    fn reformat_string(&self, input: String) -> Result<String> {
        // Leading whitespace can't be handled, fail fast.
        if let Some(c) = input.chars().next() {
            if c.is_idm_whitespace() {
                return err!("Leading whitespace in string value");
            }
        }

        let input_style = if let Some(input_style) = Indentation::infer(&input)
        {
            input_style
        } else {
            // If the indent can't be inferred, no input lines are indented,
            // string can be used as is.
            return Ok(input);
        };

        // The input has some indentation, so it might not be valid IDM. Let's
        // check that right now. If it deserializes into Outline, it should be
        // good.
        let outline: crate::outline::Outline = crate::de::from_str(&input)?;

        if input_style.is_compatible_with(&self.indent) {
            // It's valid input and already uses the indentation type we want,
            // return as is and forget about our outline value.
            Ok(input)
        } else {
            // Otherwise fix the indentation by reserializing the outline.

            // NB. A simple outline type should only have single-line line values
            // that will return None form infer_indent_style, so this does not
            // lead into infinite recursion as these are again passed through
            // reformat_string when serializing.
            let reser = to_string_styled(self.indent, &outline)?;
            Ok(reser)
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Indentation {
    Tabs,
    Spaces(usize),
}

impl Default for Indentation {
    fn default() -> Self {
        Indentation::Spaces(2)
    }
}

impl Indentation {
    pub fn infer(input: &str) -> Option<Self> {
        let mut at_line_start = true;
        let mut prefix = String::new();
        for c in input.chars() {
            if c == '\n' {
                at_line_start = true;
                continue;
            } else if !c.is_idm_whitespace() {
                if !prefix.is_empty() {
                    // Got what we wanted.
                    break;
                } else {
                    at_line_start = false;
                }
            }

            if at_line_start && c.is_idm_whitespace() {
                prefix.push(c);
            }
        }

        if let Some(c) = prefix.chars().next() {
            if c == '\t' {
                Some(Indentation::Tabs)
            } else if c == ' ' {
                Some(Indentation::Spaces(prefix.len()))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn is_compatible_with(&self, other: &Indentation) -> bool {
        use Indentation::*;
        matches!((self, other), (Tabs, Tabs) | (Spaces(_), Spaces(_)))
    }
}

impl Indentation {
    pub fn write(&self, w: &mut impl Write, n: usize) -> io::Result<()> {
        for _ in 0..n {
            match self {
                Indentation::Tabs => write!(w, "\t")?,
                Indentation::Spaces(m) => {
                    for _ in 0..*m {
                        write!(w, " ")?;
                    }
                }
            }
        }
        Ok(())
    }
}

// The Write serializer is just a wrapper around AST serializer. We need
// complete AST expressions before the correct write format can be determined.

impl<'a, W: Write> ser::Serializer for &'a mut Serializer<W> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Sequence<'a, W>;
    type SerializeTuple = Sequence<'a, W>;
    type SerializeTupleStruct = Sequence<'a, W>;
    type SerializeTupleVariant = Sequence<'a, W>;
    type SerializeMap = Sequence<'a, W>;
    type SerializeStruct = Sequence<'a, W>;
    type SerializeStructVariant = Sequence<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        self.write(&Ast.serialize_bool(v)?)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        self.write(&Ast.serialize_i8(v)?)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        self.write(&Ast.serialize_i16(v)?)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        self.write(&Ast.serialize_i32(v)?)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        self.write(&Ast.serialize_i64(v)?)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        self.write(&Ast.serialize_u8(v)?)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        self.write(&Ast.serialize_u16(v)?)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        self.write(&Ast.serialize_u32(v)?)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        self.write(&Ast.serialize_u64(v)?)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        self.write(&Ast.serialize_f32(v)?)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        self.write(&Ast.serialize_f64(v)?)
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        self.write(&Ast.serialize_char(v)?)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        self.write(&Ast.serialize_str(v)?)
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok> {
        self.write(&Ast.serialize_bytes(v)?)
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        self.write(&Ast.serialize_none()?)
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        self.write(&Ast.serialize_some(value)?)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        self.write(&Ast.serialize_unit()?)
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok> {
        self.write(&Ast.serialize_unit_struct(name)?)
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        self.write(&Ast.serialize_unit_variant(name, variant_index, variant)?)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        self.write(&Ast.serialize_newtype_struct(name, value)?)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        self.write(&Ast.serialize_newtype_variant(
            name,
            variant_index,
            variant,
            value,
        )?)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(Sequence::new(self, Ast.serialize_seq(len)?))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        Ok(Sequence::new(self, Ast.serialize_tuple(len)?))
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Ok(Sequence::new(self, Ast.serialize_tuple_struct(name, len)?))
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(Sequence::new(
            self,
            Ast.serialize_tuple_variant(name, variant_index, variant, len)?,
        ))
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(Sequence::new(self, Ast.serialize_map(len)?))
    }

    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct> {
        Ok(Sequence::new(self, Ast.serialize_struct(name, len)?))
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(Sequence::new(
            self,
            Ast.serialize_struct_variant(name, variant_index, variant, len)?,
        ))
    }
}

pub struct Sequence<'a, W: Write> {
    ser: &'a mut Serializer<W>,
    expr: Expr,
}

impl<'a, W: Write> Sequence<'a, W> {
    fn new(ser: &'a mut Serializer<W>, expr: Expr) -> Self {
        Sequence { ser, expr }
    }

    fn write(self) -> Result<()> {
        self.ser.write(&self.expr)
    }
}

impl<'a, W: Write> ser::SerializeSeq for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::Serialize,
    {
        self.expr.serialize_element(value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

impl<'a, W: Write> ser::SerializeTuple for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::Serialize,
    {
        self.expr.serialize_element(value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

impl<'a, W: Write> ser::SerializeTupleStruct for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.expr.serialize_field(value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

impl<'a, W: Write> ser::SerializeTupleVariant for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.expr.serialize_field(value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

impl<'a, W: Write> ser::SerializeMap for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.expr.serialize_key(key)
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.expr.serialize_value(value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

impl<'a, W: Write> ser::SerializeStruct for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.expr.serialize_field(key, value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

impl<'a, W: Write> ser::SerializeStructVariant for Sequence<'a, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.expr.serialize_field(key, value)
    }

    fn end(mut self) -> Result<Self::Ok> {
        self.expr = self.expr.end()?;
        self.write()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guess_style() {
        assert_eq!(Indentation::infer(""), None);

        assert_eq!(
            Indentation::infer(
                "\
foo
bar"
            ),
            None
        );

        assert_eq!(
            Indentation::infer(
                "\
foo
 bar
  baz"
            ),
            Some(Indentation::Spaces(1))
        );

        assert_eq!(
            Indentation::infer(
                "\
foo
    bar"
            ),
            Some(Indentation::Spaces(4))
        );
        assert_eq!(
            Indentation::infer(
                "\
foo
\tbar"
            ),
            Some(Indentation::Tabs)
        );
    }
}
