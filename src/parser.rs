//! Main parsing complexity lives in this module.

use std::{borrow::Cow, fmt, str::FromStr};

use crate::{
    err,
    fragment::{Fragment, Outline},
    parse, Error, Result,
};

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    /// For figuring out error numbers.
    input: &'a str,
    stack: Vec<State<'a>>,
}

impl<'a> Parser<'a> {
    pub fn from_str(input: &'a str) -> Result<Self> {
        Ok(Parser {
            input,
            stack: vec![State::Document(Fragment::from_str(input)?)],
        })
    }

    pub fn read<T: FromStr>(&mut self) -> Result<T> {
        let s = self.read_str()?;
        // TODO Attach line number to error
        T::from_str(&*s).map_err(|_| Error::new("Failed to parse value"))
    }

    /// Return the next concrete string token from current sequence or `None`
    /// if at the end of sequence.
    ///
    /// If called immediately after `enter_pair`, will parse the next input
    /// line in raw mode.
    pub fn read_str(&mut self) -> Result<Cow<str>> {
        let top = self.stack.len() - 1;
        self.stack[top].next()
    }

    /// Enter a sequence of values.
    ///
    /// A line will be parsed as a horizontal sequence of words and a block
    /// will be parsed as a vertical sequence of block items.
    ///
    /// Will fail if already parsing a horizontal sequence. Will enter
    pub fn enter_seq(&mut self) -> Result<()> {
        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_seq()?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Enter a map of key-value data.
    ///
    /// Maps can only have a vertical structure. After entering a map, `next`
    /// will interleave keys and values for as long as the map has content.
    ///
    /// Will fail if already parsing a horizontal sequence. If
    pub fn enter_map(&mut self) -> Result<()> {
        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_map()?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Enter a struct.
    ///
    /// Structs are treated very similarly to maps. Unlike maps, structs have
    /// a horizontal form where the field names are not written out. A
    /// horizontal struct will have field names fed into `next` from the field
    /// name list provided as an argument.
    ///
    /// If the struct is entered immediately after `enter_pair`, structs are
    /// treated entirely like maps and horizontal structs are not recognized.
    pub fn enter_struct(
        &mut self,
        fields: &'static [&'static str],
    ) -> Result<()> {
        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_struct(fields)?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Enter a structural pair.
    ///
    /// This is a special method that enables irregular features in IDM. It
    /// must be followed by either `next`, `enter_map` or `enter_struct` to
    /// read the first pair element.
    ///
    /// Calling `next` will read a line in raw input mode (treating comments
    /// and blank lines as input). The second element will be the indented
    /// section body under the line. If the line has no section body, the next
    /// element will be an empty block.
    ///
    /// Entering a map or struct after `enter_pair` will start parsing a block
    /// and will treat the first block item as the map or struct and the rest
    /// of the block as the second element of the pair.
    ///
    /// If `enter_map` or `enter_struct` was called for the first pair
    /// element, `enter_map` or `enter_struct` must not be called for the
    /// second element since the fused block syntax would make the separation
    /// of the second map from the first undetectable.
    pub fn enter_pair(&mut self) -> Result<()> {
        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_pair()?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Exit the current entered scope.
    ///
    /// The scope must not have any unparsed input (that would be returned by
    /// `next`, if comments are being skipped, remaining comments do not
    /// count) remamining. If there is, an error will be raised.
    ///
    /// Calling exit without a preceding enter call will panic.
    pub fn exit(&mut self) -> Result<()> {
        let top = self.stack.len() - 1;
        if !self.stack[top].is_empty() {
            return err!("Unparsed input remains");
        }
        self.stack.pop();
        Ok(())
    }

    /// Return true if the current scope has no more items.
    pub fn is_empty(&self) -> bool {
        self.stack[self.stack.len() - 1].is_empty()
    }

    /// Return true if the current scope has no more items, not even commented
    /// out items.
    pub fn is_really_empty(&self) -> bool {
        self.stack[self.stack.len() - 1].is_really_empty()
    }
}

/// Parsing states that carry context fragments and are stacked in the parser
/// stack.
///
/// The implementation can be thought of as a two-dimensional table. The rows
/// are the transition methods in `State` like `enter_seq` and `enter_map`.
/// The columns are the behaviors of each method given the current value of
/// `State` for the given transition method.
#[derive(Clone, Debug)]
enum State<'a> {
    /// Complete document
    Document(Fragment<'a>),

    /// An outline being split into individual items.
    Sequence(Outline<'a>),

    /// A line being split into individual words.
    Words(Vec<&'a str>),
    /// Value fields of an inline struct.
    InlineStruct {
        words: Vec<&'a str>,
        fields: Vec<&'static str>,
    },

    /// Map iteration returning a key next.
    MapKey(Outline<'a>),
    /// Map iteration returning a value next.
    ///
    /// If the top of the outline is a line item, the value is the line, if
    /// it's a section item, the value is the section body.
    MapValue(Outline<'a>),

    /// First element of a pair.
    ///
    /// Waiting for next operation to decide how to proceed.
    PairFirst(Fragment<'a>),
    /// Second element of a pair.
    PairSecond(Outline<'a>),
}

impl Default for State<'_> {
    fn default() -> Self {
        // This is the standard representation for an empty fragment.
        State::Document(Default::default())
    }
}

impl<'a> State<'a> {
    fn next(&mut self) -> Result<Cow<str>> {
        let mut ret = err!("Out of input");

        take_mut::take(self, |s| match s {
            State::Document(f) => {
                if !f.is_empty() {
                    ret = Ok(Cow::from(f.to_string()))
                }
                Default::default()
            }
            State::Sequence(mut o) => {
                if let Some(next) = o.pop_nonblank() {
                    ret = Ok(Cow::from(next.to_string()));
                }
                State::Sequence(o)
            }
            State::Words(mut words) => {
                if let Some(word) = words.pop() {
                    ret = Ok(Cow::from(word));
                }
                State::Words(words)
            }
            State::InlineStruct {
                mut words,
                mut fields,
            } => {
                // Alternate between struct field names (acquired from serde,
                // not present in input) and field values.
                if words.is_empty() {
                    return State::Document(Default::default());
                } else if fields.len() == words.len() {
                    // When vecs are balanced, hand out a field name first.
                    ret = Ok(Cow::from(fields.pop().unwrap()));
                } else {
                    // If out of balance, assume a field name was handed out
                    // the last time, give the corresponding value.
                    debug_assert!(fields.len() == words.len() - 1);
                    ret = Ok(Cow::from(words.pop().unwrap()));
                }
                State::InlineStruct { words, fields }
            }
            State::MapKey(mut o) => {
                match o.pop_nonblank() {
                    Some(Fragment::Item(mut i)) if i.is_line() => {
                        if let Ok((word, rest)) = parse::word(i.head) {
                            // It's a line, first word is key.
                            ret = Ok(Cow::from(word));
                            // Push the rest of the line in, that's value.
                            i.head = rest;
                            o.push(i);
                            // Switch to value
                            State::MapValue(o)
                        } else {
                            ret = err!("Invalid map entry");
                            Default::default()
                        }
                    }
                    Some(Fragment::Item(i)) => {
                        // Section, headline is key, body is value.
                        ret = Ok(Cow::from(i.head));
                        // Push the item back in, MapValue needs to know to
                        // ditch headline when it sees a section.
                        o.push(i);
                        State::MapValue(o)
                    }
                    _ => {
                        ret = err!("Invalid map entry");
                        Default::default()
                    }
                }
            }
            State::MapValue(mut o) => {
                // Regular pop, since MapKey should have already done the
                // nonblank filtering before we get here.
                match o.pop() {
                    Some(i) if i.is_line() => {
                        // If it's an item, the line should already have been
                        // snipped down to value.
                        ret = Ok(Cow::from(i.head));
                        // Return to map key for rest of stack.
                        State::MapKey(o)
                    }
                    Some(i) => {
                        // It's a section, body is the value. Ignore head.
                        ret = Ok(Cow::from(i.body.to_string()));
                        State::MapKey(o)
                    }
                    _ => {
                        ret = err!("Invalid map entry");
                        Default::default()
                    }
                }
            }
            State::PairFirst(Fragment::Item(i)) => {
                ret = Ok(Cow::from(i.head));
                State::PairSecond(i.body)
            }
            State::PairFirst(Fragment::Outline(mut o)) => {
                match o.pop() {
                    Some(i) => {
                        ret = Ok(Cow::from(i.to_string()));
                        // The remaining content.
                        State::PairSecond(o)
                    }
                    None => {
                        // XXX: Is it okay to do a blank here, or should we error
                        // out if the content is actually being directly read
                        // instead of gone into a seq/map that will then
                        // terminate with no items?
                        ret = Ok(Cow::from(""));
                        State::PairSecond(o)
                    }
                }
            }
            State::PairSecond(o) => {
                ret = Ok(Cow::from(o.to_string()));
                Default::default()
            }
        });

        ret
    }

    fn enter_seq(&mut self) -> Result<State<'a>> {
        let mut ret = err!("Out of input");

        take_mut::take(self, |s| match s {
            State::Document(f) if f.is_empty() => {
                ret = Ok(State::Sequence(Default::default()));
                Default::default()
            }
            State::Document(Fragment::Item(i)) if i.is_line() => {
                let (mut words, _) = parse::words(i.head);
                words.reverse();
                ret = Ok(State::Words(words));
                Default::default()
            }
            State::Document(Fragment::Outline(o)) => {
                ret = Ok(State::Sequence(o));
                Default::default()
            }
            State::Document(_) => {
                ret = err!("Invalid sequence shape");
                s
            }
            State::Sequence(mut o) => {
                match o.pop_nonblank() {
                    Some(Fragment::Outline(a)) => {
                        // Nested block.
                        ret = Ok(State::Sequence(a));
                    }
                    Some(Fragment::Item(i)) if i.is_line() => {
                        // Horizontal inline sequence.
                        let (mut words, _) = parse::words(i.head);
                        words.reverse();
                        ret = Ok(State::Words(words));
                    }
                    None => {}
                    _ => {
                        ret = err!("Invalid sequence shape");
                    }
                }
                State::Sequence(o)
            }
            State::MapKey(_) => {
                // Seq keys might be eventually supported?
                ret = err!("Not supported");
                s
            }
            State::MapValue(mut o) => {
                match o.pop() {
                    Some(i) if i.is_line() => {
                        // Horizontal sequence from headline.
                        let (mut words, _) = parse::words(i.head);
                        words.reverse();
                        ret = Ok(State::Words(words));
                        // Return to map key for rest of stack.
                        State::MapKey(o)
                    }
                    Some(i) => {
                        // Vertical sequence from body of section (head was
                        // key)
                        ret = Ok(State::Sequence(i.body));
                        State::MapKey(o)
                    }
                    _ => {
                        ret = err!("Invalid map entry");
                        Default::default()
                    }
                }
            }
            State::PairSecond(o) => {
                ret = Ok(State::Sequence(o));
                Default::default()
            }

            State::Words(_)
            | State::InlineStruct { .. }
            | State::PairFirst(_) => {
                ret = err!("enter_seq: Invalid type");
                s
            }
        });

        ret
    }

    fn enter_map(&mut self) -> Result<State<'a>> {
        // Only allow outlines, generate empty one in inline-y context.
        let mut ret = err!("Invalid map");

        take_mut::take(self, |s| match s {
            State::Document(f) if f.is_empty() => {
                ret = Ok(State::MapKey(Default::default()));
                Default::default()
            }
            State::Document(Fragment::Outline(mut o)) => {
                // Sugar hack, if the outline consists entirely of a
                // single colon-indented block, drop down into that.
                // This will be done a lot in enter_map and enter_struct.
                o.try_unfold_only_child_outline();
                ret = Ok(State::MapKey(o));
                Default::default()
            }
            State::Document(_) => {
                // Don't know the context, so just fail out here instead of
                // cramming an empty map before the horizontal fragment.
                s
            }

            State::Sequence(mut o) => {
                let prev_o = o.clone();

                match o.pop_nonblank() {
                    Some(Fragment::Outline(mut a)) => {
                        a.try_unfold_only_child_outline();

                        // Nested block, that's a map.
                        ret = Ok(State::MapKey(a));
                        State::Sequence(o)
                    }
                    Some(Fragment::Item(_)) => {
                        // Inline item, inject an empty map, don't consume the
                        // item.
                        ret = Ok(State::MapKey(Default::default()));
                        State::Sequence(prev_o)
                    }
                    None => {
                        // We can fit in an empty map.
                        ret = Ok(State::MapKey(Default::default()));
                        State::Sequence(o)
                    }
                }
            }
            State::PairFirst(Fragment::Outline(mut o)) => {
                // XXX: Copy-pasted from Sequence branch.
                let prev_o = o.clone();

                match o.pop_nonblank() {
                    Some(Fragment::Outline(a)) => {
                        // Nested block, that's a map.
                        ret = Ok(State::MapKey(a));
                        State::PairSecond(o)
                    }
                    Some(Fragment::Item(_)) => {
                        // Inline item, inject an empty map, don't consume the
                        // item.
                        ret = Ok(State::MapKey(Default::default()));
                        State::PairSecond(prev_o)
                    }
                    None => {
                        // We can fit in an empty map.
                        ret = Ok(State::MapKey(Default::default()));
                        State::PairSecond(o)
                    }
                }
            }

            State::PairFirst(Fragment::Item(i)) => {
                // Synthesize an outline.
                let outline = Outline(vec![i]);
                if let Some(Fragment::Outline(a)) =
                    outline.clone().pop_nonblank()
                {
                    // The single item in it looks like a map, go read it as
                    // one.
                    ret = Ok(State::MapKey(a));
                    State::PairSecond(Default::default())
                } else {
                    // Nothing map-like in sight, pass an empty map and read
                    // the content as the tail of the pair.
                    ret = Ok(State::MapKey(Default::default()));
                    State::PairSecond(outline)
                }
            }

            State::MapKey(_) => {
                ret = err!("Not supported");
                s
            }
            State::MapValue(mut o) => {
                match o.pop() {
                    Some(i) if !i.is_line() => {
                        let mut val = i.body.clone();

                        val.try_unfold_only_child_outline();

                        // Vertical sequence from body of section (head was
                        // key)
                        ret = Ok(State::MapKey(val));
                        State::MapKey(o)
                    }
                    None => {
                        // Empty value, empty map.
                        ret = Ok(State::MapKey(Default::default()));
                        State::MapKey(o)
                    }
                    _ => {
                        // Inline entry, cannot be map.
                        ret = err!("Invalid map entry");
                        Default::default()
                    }
                }
            }
            State::PairSecond(mut o) => {
                o.try_unfold_only_child_outline();
                ret = Ok(State::MapKey(o));
                Default::default()
            }

            State::Words(_) | State::InlineStruct { .. } => s,
        });

        ret
    }

    fn enter_struct(
        &mut self,
        fields: &'static [&'static str],
    ) -> Result<State<'a>> {
        // XXX: This is *almost* the same as enter_map, very copy-pastey, but
        // it has the difference that inline structs are a thing, but aren't
        // allowed in pair head position.
        let mut ret = err!("Invalid struct");

        take_mut::take(self, |s| match s {
            State::Document(f) if f.is_empty() => {
                ret = Ok(State::MapKey(Default::default()));
                Default::default()
            }
            State::Document(Fragment::Outline(mut o)) => {
                o.try_unfold_only_child_outline();

                ret = Ok(State::MapKey(o));
                Default::default()
            }
            State::Document(Fragment::Item(i)) => {
                let (mut words, _) = parse::words(i.head);
                words.reverse();
                let fields: Vec<&'static str> =
                    fields.iter().rev().cloned().collect();
                if words.len() == fields.len() {
                    ret = Ok(State::InlineStruct { words, fields });
                }
                Default::default()
            }

            State::Sequence(mut o) => {
                match o.pop_nonblank() {
                    Some(Fragment::Outline(mut a)) => {
                        a.try_unfold_only_child_outline();

                        // Nested block, like a map.
                        ret = Ok(State::MapKey(a));
                        State::Sequence(o)
                    }
                    Some(Fragment::Item(i)) => {
                        let (mut words, _) = parse::words(i.head);
                        words.reverse();
                        let fields: Vec<&'static str> =
                            fields.iter().rev().cloned().collect();
                        if words.len() == fields.len() {
                            ret = Ok(State::InlineStruct { words, fields });
                        }
                        State::Sequence(o)
                    }
                    None => {
                        // Not supporting completely empty structs in
                        // sequences.
                        Default::default()
                    }
                }
            }
            State::PairFirst(Fragment::Outline(mut o)) => {
                let prev_o = o.clone();

                // Behave exactly like a map in pair head position, do not
                // consider inline structs.
                match o.pop_nonblank() {
                    Some(Fragment::Outline(a)) => {
                        // Nested block, that's a map.
                        ret = Ok(State::MapKey(a));
                        State::PairSecond(o)
                    }
                    Some(Fragment::Item(_)) => {
                        // Inline item, inject an empty map, don't consume the
                        // item.
                        ret = Ok(State::MapKey(Default::default()));
                        State::PairSecond(prev_o)
                    }
                    None => {
                        // Empty struct
                        ret = Ok(State::MapKey(Default::default()));
                        State::PairSecond(o)
                    }
                }
            }

            State::MapKey(_) => {
                // TODO: We definitely want to support inline structs for
                // section-head map keys.
                ret = err!("Not supported");
                s
            }
            State::MapValue(mut o) => {
                match o.pop() {
                    Some(i) if !i.is_line() => {
                        let mut val = i.body.clone();

                        val.try_unfold_only_child_outline();

                        // Vertical sequence from body of section (head was
                        // key)
                        ret = Ok(State::MapKey(val));
                        State::MapKey(o)
                    }
                    Some(i) => {
                        // Inline struct.
                        let (mut words, _) = parse::words(i.head);
                        words.reverse();
                        let fields: Vec<&'static str> =
                            fields.iter().rev().cloned().collect();
                        if words.len() == fields.len() {
                            ret = Ok(State::InlineStruct { words, fields });
                        }
                        State::MapKey(o)
                    }
                    None => {
                        // Empty value, empty map.
                        ret = Ok(State::MapKey(Default::default()));
                        State::MapKey(o)
                    }
                }
            }
            State::PairSecond(mut o) => {
                o.try_unfold_only_child_outline();
                ret = Ok(State::MapKey(o));
                Default::default()
            }

            State::Words(_)
            | State::InlineStruct { .. }
            | State::PairFirst(Fragment::Item(_)) => s,
        });

        ret
    }

    fn enter_pair(&mut self) -> Result<State<'a>> {
        let mut ret = err!("Invalid pair");
        take_mut::take(self, |s| match s {
            State::Document(Fragment::Item(i)) => {
                ret = Ok(State::PairFirst(i.into()));
                State::Document(Default::default())
            }
            State::Document(Fragment::Outline(mut o)) => {
                let content = if o.len() == 1 {
                    Fragment::Item(o.pop().unwrap())
                } else {
                    Fragment::Outline(o)
                };
                ret = Ok(State::PairFirst(content));
                State::Document(Default::default())
            }
            State::Sequence(mut o) => {
                // Sequence -> pair, prepare for raw mode, no blank filtering.
                if let Some(i) = o.pop() {
                    ret = Ok(State::PairFirst(i.into()));
                }
                State::Sequence(o)
            }
            State::MapValue(mut o) => {
                // Must be an outline-style value.
                match o.pop() {
                    Some(i) if !i.is_line() => {
                        // Vertical sequence from body of section (head was
                        // key)
                        ret = Ok(State::PairFirst(i.body.into()));
                        State::MapKey(o)
                    }
                    _ => Default::default(),
                }
            }
            State::PairSecond(o) => {
                ret = Ok(State::PairFirst(o.into()));
                State::Document(Default::default())
            }
            // Others are no-go.
            State::Words(_)
            | State::InlineStruct { .. }
            | State::MapKey(_)
            | State::PairFirst(_) => s,
        });

        ret
    }

    fn is_empty(&self) -> bool {
        match self {
            State::Document(f) => f.is_empty(),
            State::Sequence(o) => o.is_empty_or_blank(),
            State::Words(w) => w.is_empty(),
            State::InlineStruct { words, .. } => words.is_empty(),
            State::MapKey(o) => o.is_empty_or_blank(),
            // Only accessible after MapKey, must have a value if MapKey
            // passed
            State::MapValue(_) => false,
            // Pair halves are considered nonempty if pair was entered
            // succesfully.
            State::PairFirst(_) => false,
            State::PairSecond(_) => false,
        }
    }

    fn is_really_empty(&self) -> bool {
        match self {
            State::Document(f) => f.is_empty(),
            State::Sequence(o) => o.is_empty(),
            State::Words(w) => w.is_empty(),
            State::InlineStruct { words, .. } => words.is_empty(),
            State::MapKey(o) => o.is_empty(),
            // Only accessible after MapKey, must have a value if MapKey
            // passed
            State::MapValue(_) => false,
            // Pair halves are considered nonempty if pair was entered
            // succesfully.
            State::PairFirst(_) => false,
            State::PairSecond(_) => false,
        }
    }
}

impl fmt::Display for State<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            State::Document(Fragment::Outline(o)) => {
                write!(f, "Document(\n{})", o)
            }
            State::Document(Fragment::Item(i)) => write!(f, "Document({})", i),
            State::Sequence(o) => write!(f, "Sequence(\n{})", o),
            State::Words(words) => {
                write!(f, "Words(")?;
                for w in words.iter().rev() {
                    write!(f, " {}", w)?;
                }
                write!(f, ")")
            }
            State::InlineStruct { words, fields } => {
                writeln!(f, "InlineStruct(")?;
                write!(f, " ")?;
                for a in fields.iter().rev() {
                    write!(f, " {}", a)?;
                }
                writeln!(f)?;
                for w in words.iter().rev() {
                    write!(f, " {}", w)?;
                }
                write!(f, ")")
            }
            State::MapKey(o) => write!(f, "MapKey(\n{})", o),
            State::MapValue(o) => write!(f, "MapValue(\n{})", o),
            State::PairFirst(Fragment::Outline(o)) => {
                write!(f, "PairFirst(\n{})", o)
            }
            State::PairFirst(Fragment::Item(i)) => {
                write!(f, "PairFirst({})", i)
            }
            State::PairSecond(o) => write!(f, "PairSecond(\n{})", o),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper trait for automating enter/exit calls with scopes.
    // Unit tests become very noisy without this.
    trait ParserExt {
        fn seq(&mut self, p: impl FnOnce(&mut Self));
        fn pair(&mut self, p: impl FnOnce(&mut Self));
        fn map(&mut self, p: impl FnOnce(&mut Self));
        fn strct(
            &mut self,
            fields: &'static [&'static str],
            p: impl FnOnce(&mut Self),
        );

        // Syntax sugar for reading next value.
        fn n(&mut self) -> Cow<str>;
    }

    impl ParserExt for Parser<'_> {
        fn seq(&mut self, p: impl FnOnce(&mut Self)) {
            self.enter_seq().unwrap();
            p(self);
            self.exit().unwrap();
        }

        fn pair(&mut self, p: impl FnOnce(&mut Self)) {
            self.enter_pair().unwrap();
            p(self);
            self.exit().unwrap();
        }

        fn map(&mut self, p: impl FnOnce(&mut Self)) {
            self.enter_map().unwrap();
            p(self);
            self.exit().unwrap();
        }

        fn strct(
            &mut self,
            fields: &'static [&'static str],
            p: impl FnOnce(&mut Self),
        ) {
            self.enter_struct(fields).unwrap();
            p(self);
            self.exit().unwrap();
        }

        fn n(&mut self) -> Cow<str> {
            self.read_str().unwrap()
        }
    }

    #[test]
    fn construction() {
        assert!(Parser::from_str("").is_ok());
        assert!(Parser::from_str("a").is_ok());
        assert!(Parser::from_str(
            "\
a"
        )
        .is_ok());
        assert!(Parser::from_str(
            "\
a
  b
  c"
        )
        .is_ok());

        // Mixed indentation, not ok.
        assert!(Parser::from_str(
            "\
a
  b
\tc"
        )
        .is_err());

        // Bad dedentation, not ok.
        assert!(Parser::from_str(
            "\
a
  b
 c"
        )
        .is_err());
    }

    #[test]
    fn seq_1() {
        Parser::from_str(
            "\
A
B
C",
        )
        .unwrap()
        .seq(|p| {
            assert_eq!(p.n(), "A");
            assert_eq!(p.n(), "B");
            assert_eq!(p.n(), "C");
        });
    }

    #[test]
    fn fragment_seq() {
        // Note lack of newline in input.
        Parser::from_str("A B C").unwrap().seq(|p| {
            assert_eq!(p.n(), "A");
            assert_eq!(p.n(), "B");
            assert_eq!(p.n(), "C");
        });
    }

    #[test]
    fn seq_2() {
        Parser::from_str(
            "\
A B
C D",
        )
        .unwrap()
        .seq(|p| {
            p.seq(|p| {
                assert_eq!(p.n(), "A");
                assert_eq!(p.n(), "B");
            });
            p.seq(|p| {
                assert_eq!(p.n(), "C");
                assert_eq!(p.n(), "D");
            });
        });
    }

    #[test]
    fn outline() {
        // Simulate outline traversal, alternating sequences and raw element
        // pairs.
        Parser::from_str(
            "\
A
  B
  -- C
D",
        )
        .unwrap()
        .seq(|p| {
            p.pair(|p| {
                assert_eq!(p.n(), "A");
                p.seq(|p| {
                    p.pair(|p| {
                        assert_eq!(p.n(), "B");
                        p.seq(|_| {});
                    });
                    // Check that comment-looking things get read in, pair
                    // heads have raw mode.
                    p.pair(|p| {
                        assert_eq!(p.n(), "-- C");
                        p.seq(|_| {});
                    });
                });
            });
            p.pair(|p| {
                assert_eq!(p.n(), "D");
                p.seq(|_| {});
            });
        });
    }

    #[test]
    fn outline_attributes() {
        Parser::from_str(
            "\
Title
  :a 1
  :b 2
  Content
Title 2
  Stuff",
        )
        .unwrap()
        .seq(|p| {
            p.pair(|p| {
                assert_eq!(p.n(), "Title");
                p.pair(|p| {
                    p.map(|p| {
                        assert_eq!(p.n(), "a");
                        assert_eq!(p.n(), "1");
                        assert_eq!(p.n(), "b");
                        assert_eq!(p.n(), "2");
                    });
                    p.seq(|p| {
                        assert_eq!(p.n(), "Content");
                    });
                });
            });
            p.pair(|p| {
                assert_eq!(p.n(), "Title 2");
                p.pair(|p| {
                    p.map(|_| {
                        // Allow an empty map, don't consume content.
                    });
                    p.seq(|p| {
                        assert_eq!(p.n(), "Stuff");
                    });
                });
            });
        });
    }
}
