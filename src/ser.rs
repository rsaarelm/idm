use crate::{
    err, from_str, guess_indent_style, infer_indent_style, parse::CharExt,
    Error, Result,
};
use serde::{
    ser::{self, SerializeMap},
    Serialize,
};
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
    let mut serializer = Serializer { style };
    let expr = value.serialize(&mut serializer)?;
    Ok(Formatted(style, expr).to_string())
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
    pub fn new(style: Style, text: impl AsRef<str>) -> Result<Value> {
        let text = text
            .as_ref()
            .trim_end_matches(CharExt::is_idm_whitespace)
            .to_string();

        if text.is_empty() {
            return err!("Empty string for value");
        }

        let mut newline = false;
        let mut space = false;
        for c in text.chars() {
            if c.is_idm_whitespace() {
                space = true;
            }
            if c == '\n' {
                newline = true;
                // We can't learn anything more, might as well call it quits.
                break;
            }
        }

        if newline {
            Ok(Value::Paragraph(reformat_string(style, text)?))
        } else if space {
            Ok(Value::Line(text))
        } else {
            Ok(Value::Word(text))
        }
    }

    fn from(style: Style, item: impl fmt::Display) -> Result<Value> {
        Value::new(style, item.to_string())
    }

    pub fn empty_line() -> Value {
        Value::Paragraph("\n".into())
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
                .map_or(true, |c| !c.is_idm_whitespace()));

            // TODO: String literal formatter in construction that actually
            // validates this stuff and errors out early if you try to feed
            // invalid strings.
            for line in s.lines().skip(1) {
                if !line.chars().next().map_or(true, |c| c.is_idm_whitespace())
                {
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
        matches!(self, Paragraph(s) if s.trim_matches(CharExt::is_idm_whitespace) == "")
    }
}

// XXX Shouldn't Expr::MapElement and Expr::Pair be (Expr, Expr) instead of Vec<Expr>?

/// Descriptor for an expression for a possibly structural value.
#[derive(Eq, PartialEq, Clone, Debug)]
enum Expr {
    /// Empty expression.
    ///
    /// Can be used to mark items in structural values that will be omitted
    /// from the whole, like `None` values for struct fields which will lead
    /// to the whole field omitted from the serialized struct.
    None,
    /// Non-structural value.
    Atom(Value),
    /// Expression sequence for a variable element count sequence.
    Seq(Vec<Expr>),
    /// Key-value pairs in maps. Formatted section-like when vertical.
    MapElement(Vec<Expr>),
    /// Expressions with special formatting.
    Pair(Vec<Expr>),
}

use Expr::{Atom, MapElement, Pair, Seq};

#[derive(PartialEq, Debug)]
enum Special {
    None,
    /// Adorn with colons.
    Adorn,
    /// Element after adorn, print without comment and indent.
    Flatten,
}

impl Default for Expr {
    fn default() -> Self {
        Expr::None
    }
}

impl Expr {
    fn from(style: Style, item: impl fmt::Display) -> Result<Expr> {
        let s = format!("{}", item);
        if s.trim_end_matches(CharExt::is_idm_whitespace).is_empty() {
            Ok(Expr::None)
        } else {
            Ok(Atom(Value::from(style, item)?))
        }
    }

    /// Try to append a subexpression to an expression in a natural way.
    ///
    /// Atomic expressions need to be `sectionize`d before they can be pushed
    /// to.
    fn push(&mut self, e: Expr) {
        match self {
            Seq(es) | MapElement(es) | Pair(es) => es.push(e),
            _ => panic!("Can't append to this expr type"),
        }
    }

    fn is_inline_token(&self) -> bool {
        matches!(self, Atom(Word(_)))
    }

    fn is_sequence(&self) -> bool {
        matches!(self, Seq(_) | MapElement(_) | Pair(_))
    }

    /// Expression looks like a comment line and needs to be escaped in a
    /// sequence.
    fn looks_like_comment(&self) -> bool {
        match self {
            Atom(v) => v.as_str().starts_with("-- ") || v.as_str() == "--",
            MapElement(es) | Seq(es) => {
                es.first().map_or(false, |x| x.looks_like_comment())
            }
            _ => false,
        }
    }

    /// Like `can_be_inlined`, but comment-looking things are fine.
    fn can_be_tail_lined(&self) -> bool {
        match self {
            Expr::None => false,
            Atom(Paragraph(_)) => false,
            Atom(_) => true,
            // Seqs are simple, everything must be homogeneous
            Seq(es) => es.iter().all(|e| e.is_inline_token()),
            // Tuples are more complex, last item just needs to be inlinable.
            MapElement(es) => {
                for (i, e) in es.iter().enumerate() {
                    if i == es.len() - 1 {
                        // Last item of a tuple, can be an line item instead
                        // of a word item.
                        if !e.can_be_tail_lined() {
                            return false;
                        }
                    } else if !e.is_inline_token() {
                        return false;
                    }
                }
                true
            }
            Pair(_) => false,
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
            MapElement(es) => {
                es.len() == 2
                    && es.iter().next().map_or(false, |e| e.can_be_inlined())
            }
            // Use can_be_tail_lined for pairs, it doesn't check for
            // comment-likeness. Pair line heads are parsed in raw mode so
            // they can look like comments.
            Pair(es) => es
                .iter()
                .next()
                .map_or(false, |e| e.can_be_tail_lined() || e.is_blank_line()),

            // String literals can also be section like if they're literally
            // section-shaped, ie. every line after first is indented.
            Atom(v) => v.is_line_or_section(),
            // Out of the complex expr types, only tuples can be section-like.
            _ => false,
        }
    }

    fn is_empty(&self) -> bool {
        matches!(self, Expr::None)
            || matches!(self, MapElement(es) | Seq(es) | Pair(es) if es.iter().all(|e| e.is_empty()))
    }

    fn is_blank_line(&self) -> bool {
        matches!(self, Atom(Paragraph(x)) if x == "\n")
    }
}

struct Formatted(Style, Expr);

impl fmt::Display for Formatted {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Formatted(ref style, ref expr) = self;
        if expr.can_be_inlined() {
            style.expr_inline(f, expr)
        } else {
            style.expr_outline(f, 0, false, expr)
        }
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
                    if line.trim_matches(CharExt::is_idm_whitespace).is_empty()
                    {
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
            Atom(v) => self.value_inline(f, v),
            Seq(es) | MapElement(es) if expr.can_be_inlined() => {
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
    fn expr_outline(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: i32,
        is_adorned: bool,
        expr: &Expr,
    ) -> fmt::Result {
        match expr {
            Expr::None => Ok(()),
            e if e.is_blank_line() => writeln!(f),
            Atom(v) => self.value_outline(f, depth, v),
            Pair(_) if expr.is_empty() => Ok(()),
            Seq(es) | MapElement(es) | Pair(es) => {
                let empty_pair_head = matches!(expr, Pair(_))
                    && es.iter().next().map_or(false, |e| e.is_empty());
                if expr.is_line_or_section() && !empty_pair_head {
                    match es.as_slice() {
                        [head, body] => {
                            if !head.is_blank_line() {
                                self.indent(f, depth)?;
                                if is_adorned {
                                    write!(f, ":")?;
                                }
                                self.expr_inline(f, head)?;
                            }
                            writeln!(f)?;

                            self.expr_outline(f, depth + 1, false, body)?;

                            Ok(())
                        }
                        _ => {
                            panic!("expr_outline: Section tuple is not a pair")
                        }
                    }
                } else {
                    let mut was_adorned = false;
                    for (i, e) in es.iter().enumerate() {
                        let special;

                        if is_adorned {
                            // Sticky adornment.
                            special = Special::Adorn;
                        } else if matches!(expr, MapElement(_) | Pair(_))
                            && e.is_sequence()
                            && es.len() == 2
                            && i == 0
                        {
                            was_adorned = true;
                            special = Special::Adorn;
                            if e.is_empty() {
                                // Avoid emitting an extra blank line.
                                continue;
                            }
                        } else if i == 1 && was_adorned {
                            special = Special::Flatten;
                        } else {
                            special = Special::None;
                        }

                        self.sequence_expr(f, depth, special, e)?;
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
        special: Special,
        expr: &Expr,
    ) -> fmt::Result {
        if expr.is_blank_line() {
            writeln!(f)?;
            return Ok(());
        }

        if expr.can_be_inlined() {
            // Doesn't look like a comment, doesn't have non-inlineable
            // sequence nesting.
            self.indent(f, depth)?;
            if special == Special::Adorn {
                write!(f, ":")?;
            }
            self.expr_inline(f, expr)?;
            writeln!(f)?;
            return Ok(());
        }

        if expr.is_line_or_section() {
            // Section-looking things can be printed without escaping the
            // depth. Again, the predicate isn't true if the expr looks like a
            // comment.
            self.expr_outline(f, depth, special == Special::Adorn, expr)?;
            return Ok(());
        }

        if matches!(expr, Expr::None) {
            return Ok(());
        }

        // If we fell through here, it's not an inlineable expr nor a section.
        // Either it looks like a comment and needs to be escaped, or it's a
        // multi-line block. Treatment is the same in both cases, insert a
        // separator comment line and print the expr normally under it.

        if special == Special::Adorn {
            self.expr_outline(f, depth, true, expr)?;
        } else if special == Special::Flatten {
            self.expr_outline(f, depth, false, expr)?;
        } else {
            self.indent(f, depth)?;
            writeln!(f, "--")?;
            self.expr_outline(f, depth + 1, false, expr)?;
        }
        Ok(())
    }

    fn compatible_with(&self, other: &Style) -> bool {
        use Style::*;
        matches!((self, other), (Tabs, Tabs) | (Spaces(_), Spaces(_)))
    }
}

struct Serializer {
    style: Style,
}

impl Serializer {
    pub fn new(style: Style) -> Self {
        Serializer { style }
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = Expr;
    type Error = Error;

    type SerializeSeq = SeqSerializer;
    type SerializeTuple = SeqSerializer;
    type SerializeTupleStruct = SeqSerializer;
    type SerializeTupleVariant = SeqSerializer;
    type SerializeMap = MapSerializer;
    type SerializeStruct = MapSerializer;
    type SerializeStructVariant = MapSerializer;

    fn serialize_bool(self, v: bool) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_i8(self, v: i8) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_i16(self, v: i16) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_i32(self, v: i32) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_i64(self, v: i64) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_u8(self, v: u8) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_u16(self, v: u16) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_u32(self, v: u32) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_u64(self, v: u64) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_f32(self, v: f32) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_f64(self, v: f64) -> Result<Expr> {
        Expr::from(self.style, v)
    }

    fn serialize_char(self, v: char) -> Result<Expr> {
        // Going to disallow any whitespace, not just IDM ASCII ones here for
        // clarity's sake.
        if v.is_whitespace() {
            return err!("Can't serialize whitespace chars");
        }

        Expr::from(self.style, v)
    }

    fn serialize_str(self, v: &str) -> Result<Expr> {
        if v.trim_end_matches(CharExt::is_idm_whitespace).is_empty() {
            Ok(Atom(Value::empty_line()))
        } else {
            Expr::from(self.style, v)
        }
    }

    fn serialize_bytes(self, _: &[u8]) -> Result<Expr> {
        unimplemented!();
    }

    fn serialize_none(self) -> Result<Expr> {
        Ok(Expr::None)
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
        variant: &'static str,
    ) -> Result<Expr> {
        Expr::from(self.style, variant)
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
        variant: &'static str,
        value: &T,
    ) -> Result<Expr>
    where
        T: ?Sized + Serialize,
    {
        Ok(MapElement(vec![
            variant.serialize(&mut *self)?,
            value.serialize(&mut *self)?,
        ]))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SeqSerializer::new(self.style, len.unwrap_or(0)))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        if len != 2 {
            unimplemented!("Non-pair (len = {}) tuples are not supported", len);
        }
        Ok(SeqSerializer::new(self.style, len).is_pair())
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Ok(SeqSerializer::new(self.style, len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(SeqSerializer::new(self.style, len).enum_variant(variant))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(MapSerializer::new(self.style))
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
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(MapSerializer::new(self.style).enum_variant(variant))
    }
}

struct SeqSerializer {
    idx: usize,
    is_pair: bool,
    /// Will contain the first item if this is an indent-style tuple.
    acc: Expr,
    /// Name of the enum variant used by enum implementations.
    enum_variant: Expr,
    style: Style,
}

impl SeqSerializer {
    pub fn new(style: Style, _len: usize) -> Self {
        SeqSerializer {
            idx: 0,
            is_pair: false,
            acc: Default::default(),
            enum_variant: Default::default(),
            style,
        }
    }

    pub fn is_pair(mut self) -> Self {
        self.is_pair = true;
        self
    }

    pub fn enum_variant(mut self, enum_variant: &str) -> Self {
        self.enum_variant = Expr::from(self.style, enum_variant)
            .expect("Invalid enum variant name");
        self
    }

    fn serialize_tuple_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if self.idx == 0 {
            if self.is_pair {
                self.acc = Pair(Vec::new());
            } else {
                self.acc = MapElement(Vec::new());
            }
        }
        self.idx += 1;

        self.acc
            .push(value.serialize(&mut Serializer::new(self.style))?);
        Ok(())
    }
}

impl ser::SerializeSeq for SeqSerializer {
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

        self.acc
            .push(value.serialize(&mut Serializer::new(self.style))?);
        Ok(())
    }

    fn end(self) -> Result<Expr> {
        Ok(self.acc)
    }
}

impl ser::SerializeTuple for SeqSerializer {
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

impl ser::SerializeTupleStruct for SeqSerializer {
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

impl ser::SerializeTupleVariant for SeqSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_tuple_element(value)
    }

    fn end(self) -> Result<Expr> {
        Ok(MapElement(vec![self.enum_variant, self.acc]))
    }
}

struct MapSerializer {
    values: Vec<(Expr, Expr)>,
    enum_variant: Expr,
    style: Style,
}

impl MapSerializer {
    pub fn new(style: Style) -> Self {
        MapSerializer {
            values: Default::default(),
            enum_variant: Default::default(),
            style,
        }
    }

    pub fn enum_variant(mut self, enum_variant: &str) -> Self {
        self.enum_variant = Expr::from(self.style, enum_variant)
            .expect("Invalid enum variant name");
        self
    }
}

impl ser::SerializeMap for MapSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let key = key.serialize(&mut Serializer::new(self.style))?;
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

        let value = value.serialize(&mut Serializer::new(self.style))?;
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
            ret.push(MapElement(vec![key, value]));
        }
        Ok(ret)
    }
}

impl ser::SerializeStruct for MapSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_key(key)?;
        self.serialize_value(value)
    }

    fn end(self) -> Result<Expr> {
        let mut ret = Seq(Vec::new());
        for (key, value) in self.values.into_iter() {
            ret.push(MapElement(vec![key, value]));
        }
        Ok(ret)
    }
}

impl ser::SerializeStructVariant for MapSerializer {
    type Ok = Expr;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Expr> {
        Ok(MapElement(vec![
            self.enum_variant.clone(),
            ser::SerializeStruct::end(self)?,
        ]))
    }
}

/// Reformat a multiline string value if necessary so that it will be a valid
/// IDM fragment in the indentation style of the output.
///
/// Returns an error if the string is not a valid IDM fragment to begin with.
fn reformat_string(output_style: Style, input: String) -> Result<String> {
    // Leading whitespace can't be handled, fail fast.
    if let Some(c) = input.chars().next() {
        if c.is_idm_whitespace() {
            return err!("Leading whitespace in string value");
        }
    }

    let input_style = if let Some(input_style) = infer_indent_style(&input) {
        input_style
    } else {
        // If the indent can't be inferred, no input lines are indented,
        // string can be used as is.
        return Ok(input);
    };

    // The input has some indentation, so it might not be valid IDM. Let's
    // check that right now. If it deserializes into Outline, it should be
    // good.
    let outline: crate::outline::Outline = from_str(&input)?;

    if input_style.compatible_with(&output_style) {
        // It's valid input and already uses the indentation type we want,
        // return as is and forget about our outline value.
        Ok(input)
    } else {
        // Otherwise fix the indentation by reserializing the outline.

        // NB. A simple outline type should only have single-line line values
        // that will return None form infer_indent_style, so this does not
        // lead into infinite recursion as these are again passed through
        // reformat_string when serializing.
        let reser = to_string_styled(output_style, &outline)?;
        Ok(reser)
    }
}
