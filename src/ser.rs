use crate::{err, guess_indent_style, Error, Result};
use serde::{ser, Serialize};
use std::fmt;

/// Serialize a value using the default indentation style.
pub fn to_string<T: ser::Serialize>(value: &T) -> Result<String> {
    to_string_styled(Default::default(), value)
}

/// Serialize a value using the given indentation style.
pub fn to_string_styled<T: ser::Serialize>(
    style: Style,
    value: &T,
) -> Result<String> {
    let mut serializer = Serializer::default();
    let expr = value.serialize(&mut serializer)?;
    Ok(format!("{}", Formatted(style, expr)))
}

/// Serialize a value using indentation style inferred from an existing
/// IDM sample.
pub fn to_string_styled_like<T: ser::Serialize>(
    sample: &str,
    value: &T,
) -> Result<String> {
    to_string_styled(guess_indent_style(sample), value)
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
            Word(s) | Line(s) | Paragraph(s) => write!(f, "{}", s),
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

    fn as_str(&self) -> &str {
        match self {
            Word(s) => s,
            Line(s) => s,
            Paragraph(s) => s,
        }
    }

    fn is_line_or_section(&self) -> bool {
        if let Paragraph(s) = self {
            // Non-empty line must start with non-whitespace.
            assert!(s
                .lines()
                .next()
                .unwrap()
                .chars()
                .next()
                .map_or(true, |c| !c.is_whitespace()));

            // TODO: String literal formatter in construction that actually
            // validates this stuff and errors out early if you try to feed
            // invalid strings.
            for line in s.lines().skip(1) {
                if !line.chars().next().map_or(true, |c| c.is_whitespace()) {
                    // A line after the first one was not indented so this
                    // value can not pass as a section.
                    return false;
                }
            }

            true
        } else {
            true
        }
    }

    fn is_blank_line(&self) -> bool {
        matches!(self, Paragraph(s) if s == "\n")
    }
}

/// Descriptor for an expression for a possibly structural value.
#[derive(Eq, PartialEq, Clone, Debug)]
enum Expr {
    /// Empty expression.
    None,
    /// Expression in raw mode (printing outlines). Cannot be inlined.
    Raw(Box<Expr>),
    /// Non-structural value.
    Atom(Value),
    /// Expression sequence for a variable element count sequence.
    Seq(Vec<Expr>),
    /// Expression sequence for a fixed element count tuple.
    ///
    /// Also used for key/value pairs. Can be formatted as a section, unlike
    /// `Seq`.
    Tuple(Vec<Expr>),
}

use Expr::{Atom, Raw, Seq, Tuple};

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
            Seq(es) | Tuple(es) => es.push(e),
            _ => panic!("Can't append to this expr type"),
        }
    }

    /// Push elements from a Seq expr into self.
    fn concat(&mut self, e: Expr) {
        match e {
            Seq(es) | Tuple(es) => {
                for e in es.into_iter() {
                    self.push(e);
                }
            }
            e => self.push(e),
        }
    }

    fn is_none(&self) -> bool {
        matches!(self, Expr::None)
    }

    fn is_inline_token(&self) -> bool {
        match self {
            Atom(Word(_)) => true,
            _ => false,
        }
    }

    /// Expression looks like a comment line and needs to be escaped in a
    /// sequence.
    fn looks_like_comment(&self) -> bool {
        // NB. Raw exprs are *not* checked here, whenever you're parsing a raw
        // item, it's expected from the type schema that it might be
        // comment-like, and it should not be separately escaped by things
        // that look for comment-looking things.
        match self {
            Atom(v) => v.as_str().starts_with("--"),
            Tuple(es) | Seq(es) => {
                es.first().map_or(false, |x| x.looks_like_comment())
            }
            _ => false,
        }
    }

    /// Like `can_be_inlined`, but comment-looking things are fine.
    fn can_be_tail_lined(&self) -> bool {
        match self {
            Expr::None => false,
            // Don't check for comment-likeness for Raws, just punt to
            // tail-lined again for the inside.
            Raw(e) if e.is_blank_line() => true,
            Raw(e) => e.can_be_tail_lined(),
            Atom(Paragraph(_)) => false,
            Atom(_) => true,
            // Seqs are simple, everything must be homogeneous
            Seq(es) => es.iter().all(|e| e.is_inline_token()),
            // Tuples are more complex, last item just needs to be inlinable.
            Tuple(es) => {
                for (i, e) in es.iter().enumerate() {
                    if i == es.len() - 1 {
                        // Last item of a tuple, can be an line item instead
                        // of a word item.
                        if !e.can_be_tail_lined() {
                            return false;
                        }
                    } else {
                        if !e.is_inline_token() {
                            return false;
                        }
                    }
                }
                true
            }
        }
    }

    /// Expression can be printed in inline form.
    ///
    /// (Does not check for maximum length, just the logical inlinability.)
    fn can_be_inlined(&self) -> bool {
        !self.looks_like_comment() && self.can_be_tail_lined()
    }

    /// Expr is either a line or a section with that touches the left margin
    /// with only a single point at the start of the expr.
    fn is_line_or_section(&self) -> bool {
        // NB. This *does not apply* for seqs that have a single element but
        // are not inlineable, even though they look line-like when printed
        // out. The parser seeing a single line when it expects a sequence
        // cannot tell it's looking at a single line element instead of
        // multiple inlined word elements.
        match self {
            x if x.looks_like_comment() => false,
            // Pairs are formatted as sections if the head can be inlined.
            Tuple(es) => {
                es.len() == 2
                    && es.iter().next().map_or(false, |e| e.can_be_inlined())
            }

            // String literals can also be section like if they're literally
            // section-shaped, ie. every line after first is indented.
            Atom(v) => v.is_line_or_section(),
            Raw(e) => e.is_line_or_section(),
            // Out of the complex expr types, only tuples can be section-like.
            _ => false,
        }
    }

    fn is_empty(&self) -> bool {
        matches!(self, Expr::None)
            || matches!(self, Tuple(es) | Seq(es) if es.is_empty())
    }

    fn is_blank_line(&self) -> bool {
        matches!(self, Atom(Paragraph(x)) if x == "\n")
            || matches!(self, Raw(e) if e.is_blank_line())
    }
}

struct Formatted(Style, Expr);

impl fmt::Display for Formatted {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Formatted(ref style, ref expr) = self;
        style.expr_outline(f, 0, expr)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Style {
    Tabs,
    Spaces(usize),
}

impl Default for Style {
    fn default() -> Self {
        Style::Spaces(2)
    }
}

impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Style::Tabs => write!(f, "\t"),
            Style::Spaces(n) => {
                debug_assert!(n > 0);
                for _ in 0..n {
                    write!(f, " ")?;
                }
                Ok(())
            }
        }
    }
}

impl Style {
    fn indent(&self, fmt: &'_ mut fmt::Formatter<'_>, n: i32) -> fmt::Result {
        for _ in 0..n {
            write!(fmt, "{}", self)?;
        }
        Ok(())
    }

    fn value_inline(
        &self,
        f: &mut fmt::Formatter<'_>,
        v: &Value,
    ) -> fmt::Result {
        match v {
            Word(s) => write!(f, "{}", s),
            Line(s) => write!(f, "{}", s),
            v if v.is_blank_line() => Ok(()),
            Paragraph(_) => panic!("value_inline: Paragraphs can't be inlined"),
        }
    }

    /// Print value as part of outline at given depth.
    fn value_outline(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: i32,
        v: &Value,
    ) -> fmt::Result {
        match v {
            Word(s) => {
                self.indent(f, depth)?;
                writeln!(f, "{}", s)
            }
            Line(s) => {
                self.indent(f, depth)?;
                writeln!(f, "{}", s)
            }
            Paragraph(s) => {
                for line in s.lines() {
                    if line.trim().is_empty() {
                        // Don't indent empty lines.
                        writeln!(f)?;
                    } else {
                        self.indent(f, depth)?;
                        writeln!(f, "{}", line)?;
                    }
                }
                Ok(())
            }
        }
    }

    /// Print an expression in inline form right where the cursor is now.
    fn expr_inline(
        &self,
        f: &mut fmt::Formatter<'_>,
        expr: &Expr,
    ) -> fmt::Result {
        match expr {
            Raw(e) => self.expr_inline(f, e),
            Atom(v) => self.value_inline(f, v),
            Seq(es) | Tuple(es) if expr.can_be_inlined() => {
                for (i, e) in es.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    self.expr_inline(f, e)?;
                }
                Ok(())
            }
            _ => panic!("expr_inline: Can't inline expression {:?}", expr),
        }
    }

    /// Print an expression in outline form indented to the given depth.
    ///
    /// Does not do any escaping for block-shaped or comment-looking exprs.
    /// Use `expr_pointed` if you need that.
    fn expr_outline(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: i32,
        expr: &Expr,
    ) -> fmt::Result {
        match expr {
            Expr::None => Ok(()),
            e if e.is_blank_line() => writeln!(f),
            Raw(e) => self.expr_outline(f, depth, e),
            Atom(v) => self.value_outline(f, depth, v),
            Seq(es) | Tuple(es) => {
                if expr.is_line_or_section() {
                    match es.as_slice() {
                        [head, body] => {
                            if head.is_blank_line() {
                                writeln!(f)?;
                            } else {
                                self.indent(f, depth)?;
                                self.expr_inline(f, head)?;
                                writeln!(f)?;
                            }

                            self.expr_outline(f, depth + 1, body)?;

                            Ok(())
                        }
                        _ => {
                            panic!("expr_outline: Section tuple is not a pair")
                        }
                    }
                } else {
                    for e in es {
                        self.sequence_expr(f, depth, e)?;
                    }
                    Ok(())
                }
            }
        }
    }

    /// Print an expression at given depth, ensuring that it shows up as a
    /// valid sequence element with a single margin point.
    ///
    /// Block-like elements that don't have an unique margin point at the
    /// start will get a synthetic `--` added to the top and the rest of the
    /// value indented. Exprs that start with `--` and would be mistaken as
    /// comments are escaped in similar manner, by adding the extra `--` above
    /// them.
    ///
    /// Section or line shaped expressions that don't look like comments will
    /// be printed as is. Expressions will be printed in inline form if
    /// possible.
    fn sequence_expr(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: i32,
        expr: &Expr,
    ) -> fmt::Result {
        if expr.is_blank_line() {
            writeln!(f)?;
            return Ok(());
        }

        if expr.can_be_inlined() {
            // Doesn't look like a comment, doesn't have Raw as part of
            // sequence, doesn't have non-inlineable sequence nesting.
            self.indent(f, depth)?;
            self.expr_inline(f, expr)?;
            writeln!(f)?;
            return Ok(());
        }

        if expr.is_line_or_section() {
            // Section-looking things can be printed without escaping the
            // depth. Again, the predicate isn't true if the expr looks like a
            // comment.
            self.expr_outline(f, depth, expr)?;
            return Ok(());
        }

        if matches!(expr, Expr::None) {
            return Ok(());
        }

        // If we fell through here, it's not an inlineable expr nor a section.
        // Either it looks like a comment and needs to be escaped, or it's a
        // multi-line block. Treatment is the same in both cases, insert a
        // separator comment line and print the expr normally under it.

        self.indent(f, depth)?;
        writeln!(f, "--")?;
        self.expr_outline(f, depth + 1, expr)?;
        Ok(())
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

    fn serialize_bytes(self, v: &[u8]) -> Result<Expr> {
        let s = std::str::from_utf8(v)
            .map_err(|_| Error::new("serialize_bytes: Invalid UTF-8"))?;
        if s.find('\n').is_some() {
            return err!("serialize_bytes: Cannot have multiline raw values.");
        }
        Ok(Raw(Box::new(s.serialize(self)?)))
    }

    fn serialize_none(self) -> Result<Expr> {
        return err!("Cannot serialize None values");
    }

    fn serialize_some<T>(self, value: &T) -> Result<Expr>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
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
    /// Will contain the first item if this is an indent-style tuple.
    acc: Expr,
}

impl SeqSerializer {
    pub fn new(_len: usize) -> SeqSerializer {
        Default::default()
    }

    fn serialize_tuple_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if self.idx == 0 {
            self.acc = Tuple(Vec::new());
        }
        self.idx += 1;

        self.acc.push(value.serialize(&mut Serializer::default())?);
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
        if key.can_be_inlined() {
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
            ret.push(Tuple(vec![key, value]));
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
            ret.push(Tuple(vec![key, value]));
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
