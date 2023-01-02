//! Main parsing complexity lives in this module.

use std::{borrow::Cow, fmt, str::FromStr};

use crate::{
    de::{
        fragment::{Fragment, Item, Outline},
        parse,
    },
    err, Error, Result,
};

#[derive(Debug)]
pub struct Parser<'a> {
    stack: Vec<State<'a>>,
    /// Flag for the weird special stuff part.
    ///
    /// When entering a two-element sequence, no immediate stack ops are done,
    /// instead, at_pair_start is set to true. If the very next element is a
    /// one-element sequence (singleton tuple), this marks the start of the
    /// special mode and different stack operations will be performed. Any
    /// other element will cause normal sequence processing to commence.
    at_pair_start: bool,
}

impl<'a> Parser<'a> {
    pub fn from_str(input: &'a str) -> Result<Self> {
        Ok(Parser {
            stack: vec![State::Document(Fragment::from_str(input)?)],
            at_pair_start: false,
        })
    }

    pub fn read<T: FromStr>(&mut self) -> Result<T> {
        let s = self.read_str()?;

        // Do an extra trim here, values might have NBSP padding that passes
        // through main IDM parsing but does not apply to primitives.
        T::from_str(s.trim()).map_err(|_| Error::new("Failed to parse value"))
        // TODO Attach line number to error
    }

    /// Return the next concrete string token from current sequence or `None`
    /// if at the end of sequence.
    ///
    /// If called immediately after `enter_special`, will parse the next input
    /// line in raw mode.
    pub fn read_str(&mut self) -> Result<Cow<str>> {
        self.normal_mode()?;

        let top = self.stack.len() - 1;
        self.stack[top].next()
    }

    /// Enter a sequence of values.
    ///
    /// A line will be parsed as a horizontal sequence of words and a block
    /// will be parsed as a vertical sequence of block items.
    ///
    /// Will fail if already parsing a horizontal sequence.
    pub fn enter_seq(&mut self) -> Result<()> {
        self.normal_mode()?;

        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_seq(SeqConfig::Seq)?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Enter a tuple with a known length.
    ///
    /// Tuples can have special parsing for first and last items and can parse
    /// a section-shaped value.
    pub fn enter_tuple(&mut self, n: usize) -> Result<()> {
        self.normal_mode()?;

        if n == 2 {
            // Fake pair logic for inline struct traversal.
            let top = self.stack.len() - 1;
            if let State::InlineStruct {
                ref mut fake_seq, ..
            } = self.stack[top]
            {
                if !*fake_seq {
                    *fake_seq = true;
                    return Ok(());
                } else {
                    return err!("Invalid nesting");
                }
            }

            // Pairs can switch into special mode. When a pair tuple is
            // entered, instead of doing anything immediately, just toggle the
            // at_pair_start flag.
            self.at_pair_start = true;
            Ok(())
        } else {
            self.really_enter_tuple(n)
        }
    }

    /// Enter a sequence if in pair-start state. Otherwise do nothing.
    pub fn normal_mode(&mut self) -> Result<()> {
        if self.at_pair_start {
            self.at_pair_start = false;
            self.really_enter_tuple(2)
        } else {
            Ok(())
        }
    }

    fn really_enter_tuple(&mut self, n: usize) -> Result<()> {
        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_seq(SeqConfig::Tuple(n))?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Enter a map of key-value data.
    ///
    /// Maps can only have a vertical structure. After entering a map, `next`
    /// will interleave keys and values for as long as the map has content.
    ///
    /// Will fail if already parsing a horizontal sequence.
    pub fn enter_map(&mut self) -> Result<()> {
        self.normal_mode()?;

        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_seq(Map)?;
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
    /// If the struct is entered immediately after `enter_special`, structs
    /// are treated entirely like maps and horizontal structs are not
    /// recognized.
    pub fn enter_struct(
        &mut self,
        fields: &'static [&'static str],
    ) -> Result<()> {
        self.normal_mode()?;

        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_seq(Struct(fields))?;
        self.stack.push(new_top);
        Ok(())
    }

    /// Enter the special state.
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
    /// Entering a map or struct after `enter_special` will start parsing a
    /// block and will treat the first block item as the map or struct and the
    /// rest of the block as the second element of the pair.
    ///
    /// If `enter_map` or `enter_struct` was called for the first pair
    /// element, `enter_map` or `enter_struct` must not be called for the
    /// second element since the fused block syntax would make the separation
    /// of the second map from the first undetectable.
    pub fn enter_special(&mut self) -> Result<()> {
        if self.at_pair_start {
            self.at_pair_start = false;
        } else {
            return err!("Special mode marker not at start of pair");
        }

        let top = self.stack.len() - 1;
        let new_top = self.stack[top].enter_special()?;
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
        self.normal_mode()?;

        let top = self.stack.len() - 1;

        // Fake pair logic for inline struct traversal.
        if let State::InlineStruct {
            ref mut fake_seq, ..
        } = self.stack[top]
        {
            if *fake_seq {
                *fake_seq = false;
                return Ok(());
            }
        }

        if !self.stack[top].is_empty() {
            return err!("Unparsed input remains");
        }
        self.stack.pop();
        Ok(())
    }

    /// Return true if the current scope has no more items.
    pub fn is_empty(&self) -> bool {
        // Can't tell what the state is before taking another deser step if
        // `at_pair_start` is set.
        !self.at_pair_start && self.stack[self.stack.len() - 1].is_empty()
    }

    /// Return true if the current scope has no more items, not even commented
    /// out items.
    pub fn is_really_empty(&self) -> bool {
        // Can't tell what the state is before taking another deser step if
        // `at_pair_start` is set.
        !self.at_pair_start
            && self.stack[self.stack.len() - 1].is_really_empty()
    }

    /// Return if the current state is exactly one word
    pub fn is_word(&self) -> bool {
        // XXX: Calling next on the top item, whatever it is, can be very
        // expensive. There is probably a smarter way to do this.
        let mut head = self.stack[self.stack.len() - 1].clone();
        let text = head.next().unwrap_or_else(|_| Cow::from(""));
        let text = text.trim();
        !text.is_empty() && !text.chars().any(|c| c.is_whitespace())
    }
}

#[derive(Copy, Clone, Debug)]
enum SeqConfig {
    /// Variable-length Vec or similar.
    Seq,
    /// Fixed-length tuple
    Tuple(usize),
    /// Homogeneous map.
    Map,
    /// Struct with a fixed set of fields.
    Struct(&'static [&'static str]),
}

use SeqConfig::*;

impl SeqConfig {
    fn allows_inline(&self) -> bool {
        !matches!(self, Map)
    }

    /// Turn an outline that's all one colon-indented block into a regular
    /// block if there's no syntactic ambiguity for parsing. Used to maintain
    /// the pretense that colons are map field syntax instead of an
    /// indentation mechanism.
    fn try_unfold(&self, outline: &mut Outline) {
        if !matches!(self, Seq) {
            outline.try_unfold_only_child_outline();
        }
    }

    fn inline_state<'a>(&self, item: Item<'a>) -> Option<State<'a>> {
        match self {
            Seq if item.is_line() => Some(State::SectionSeq {
                i: 0,
                n: None,
                item,
            }),
            Struct(fields) if item.is_line() => {
                let mut words = parse::words(item.head).0;
                words.reverse();
                let mut fields = fields.to_vec();
                fields.reverse();
                Some(State::InlineStruct {
                    words,
                    fields,
                    fake_seq: false,
                })
            }
            // Only tuples can be built from section-shaped (non-line) items.
            Tuple(n) => Some(State::SectionSeq {
                i: 0,
                n: Some(*n),
                item,
            }),
            _ => None,
        }
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

    /// Vertical list of items.
    VerticalSeq(Outline<'a>),

    /// A line being split into individual words.
    SectionSeq {
        /// Current element index.
        i: usize,
        /// Total element count (not available for inline vecs).
        n: Option<usize>,
        /// Remaining item.
        item: Item<'a>,
    },

    /// Value fields of an inline struct.
    InlineStruct {
        words: Vec<&'a str>,
        fields: Vec<&'static str>,
        // Set to true to spoof entering and false to spoof exiting a pair
        // when reading an inline struct.
        fake_seq: bool,
    },

    /// First element of the special form.
    ///
    /// Waiting for next operation to decide how to proceed.
    SpecialFirst(Fragment<'a>),

    /// Second element of the special form.
    SpecialSecond(Outline<'a>),
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
            State::VerticalSeq(mut o) => {
                if let Some(next) = o.pop_nonblank() {
                    ret = Ok(Cow::from(next.to_string()));
                }
                State::VerticalSeq(o)
            }
            State::SectionSeq { i, n, mut item } => {
                // Special logic for tuples.
                if let Some(len) = n {
                    // Known length
                    if i == 0 && len == 2 && item.is_section() {
                        // Grab whole headline from section as first item of a
                        // pair when the item is section-shaped.
                        ret = Ok(Cow::from(item.head));
                        // Blank out the headline, we're left with the body
                        // for the second (and last) part.
                        item.head = "";
                        return State::SectionSeq { i: i + 1, n, item };
                    } else if i == len - 1 {
                        // Last item, apply tail-lining.
                        if item.is_line() {
                            // Tail-line the last part of a line item.
                            if !item.is_blank() {
                                ret = Ok(Cow::from(item.head));
                            }
                            return Default::default();
                        } else {
                            // Section-shaped item.
                            if !item.has_blank_line() {
                                // Mixed headline and body content in last
                                // item, must be one or the other.
                                ret =
                                    err!("Unparsed input in section headline");
                            } else {
                                // Last value is entire section body.
                                ret = Ok(Cow::from(item.body.to_string()));
                            }
                            return Default::default();
                        }
                    }
                }

                if let Some(word) = item.pop_word() {
                    ret = Ok(Cow::from(word));
                }
                State::SectionSeq { i: i + 1, n, item }
            }
            State::InlineStruct {
                mut words,
                mut fields,
                fake_seq,
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
                    debug_assert!(fields.len() + 1 == words.len());
                    ret = Ok(Cow::from(words.pop().unwrap()));
                }
                State::InlineStruct {
                    words,
                    fields,
                    fake_seq,
                }
            }
            State::SpecialFirst(Fragment::Item(i)) => {
                ret = Ok(Cow::from(i.head));
                State::SpecialSecond(i.body)
            }
            State::SpecialFirst(Fragment::Outline(mut o)) => {
                match o.pop() {
                    Some(i) => {
                        ret = Ok(Cow::from(i.to_string()));
                        // The remaining content.
                        State::SpecialSecond(o)
                    }
                    None => {
                        // XXX: Is it okay to do a blank here, or should we error
                        // out if the content is actually being directly read
                        // instead of gone into a seq/map that will then
                        // terminate with no items?
                        ret = Ok(Cow::from(""));
                        State::SpecialSecond(o)
                    }
                }
            }
            State::SpecialSecond(o) => {
                ret = Ok(Cow::from(o.to_string()));
                Default::default()
            }
        });

        ret
    }

    fn enter_seq(&mut self, config: SeqConfig) -> Result<State<'a>> {
        let mut ret = err!("Out of input");

        take_mut::take(self, |s| match s {
            State::Document(f) if f.is_empty() => {
                ret = Ok(State::VerticalSeq(Default::default()));
                Default::default()
            }
            State::Document(Fragment::Item(item)) => {
                if let Some(next) = config.inline_state(item) {
                    ret = Ok(next);
                } else {
                    ret = err!("Invalid sequence shape");
                }
                Default::default()
            }
            State::Document(Fragment::Outline(mut o)) => {
                config.try_unfold(&mut o);
                ret = Ok(State::VerticalSeq(o));
                Default::default()
            }
            State::VerticalSeq(mut o) => {
                let prev_o = o.clone();
                match o.pop_nonblank() {
                    Some(Fragment::Outline(mut a)) => {
                        // Nested block.
                        config.try_unfold(&mut a);
                        ret = Ok(State::VerticalSeq(a));
                    }
                    None | Some(Fragment::Item(_))
                        if !config.allows_inline() =>
                    {
                        ret = Ok(State::VerticalSeq(Default::default()));
                        return State::VerticalSeq(prev_o);
                    }
                    Some(Fragment::Item(item)) => {
                        if let Some(next) = config.inline_state(item) {
                            ret = Ok(next);
                        }
                    }
                    _ => {
                        ret = err!("Invalid sequence shape");
                    }
                }
                State::VerticalSeq(o)
            }
            State::SectionSeq { i, n, mut item } => {
                if let Some(len) = n {
                    if i == 0
                        && len == 2
                        && item.is_section()
                        && config.allows_inline()
                    {
                        // Whole headline at start of section-pair.
                        ret = config
                            .inline_state(item.detach_head())
                            .ok_or_else(|| Error::new("Can't inline"));
                        item.head = "";
                        return State::SectionSeq { i: i + 1, n, item };
                    } else if i == len - 1
                        && item.is_line()
                        && config.allows_inline()
                    {
                        // Trailing tail at the end of line-tuple.
                        ret = config
                            .inline_state(item)
                            .ok_or_else(|| Error::new("Can't inline"));
                        return Default::default();
                    } else if i == len - 1 && item.is_block() {
                        let mut body = item.body;
                        config.try_unfold(&mut body);
                        // Entire body at end of section-tuple.
                        ret = Ok(State::VerticalSeq(body));
                        return Default::default();
                    }
                } else {
                    ret = err!("Invalid inline nesting");
                }

                // If no special case applies, deny further nesting.
                Default::default()
            }

            State::SpecialFirst(Fragment::Item(i)) => {
                // Synthesize an outline.
                let outline = Outline(vec![i]);
                if let Some(Fragment::Outline(mut a)) =
                    outline.clone().pop_nonblank()
                {
                    // Single item looks like a vertical seq.
                    config.try_unfold(&mut a);
                    ret = Ok(State::VerticalSeq(a));
                    State::SpecialSecond(Default::default())
                } else {
                    // Nothing that looks like a vertical seq, pass an empty
                    // container and read the contents as the tail of the
                    // special form.
                    ret = Ok(State::VerticalSeq(Default::default()));
                    State::SpecialSecond(outline)
                }
            }

            State::SpecialFirst(Fragment::Outline(mut o)) => {
                let prev_o = o.clone();

                match o.pop_nonblank() {
                    Some(Fragment::Outline(a)) => {
                        // Nested block, that's a map.
                        ret = Ok(State::VerticalSeq(a));
                        State::SpecialSecond(o)
                    }
                    Some(Fragment::Item(_)) => {
                        // Inline item, inject an empty map, don't consume the
                        // item.
                        ret = Ok(State::VerticalSeq(Default::default()));
                        State::SpecialSecond(prev_o)
                    }
                    None => {
                        // Nothing here, empty map.
                        ret = Ok(State::VerticalSeq(Default::default()));
                        State::SpecialSecond(prev_o)
                    }
                }
            }
            State::SpecialSecond(mut o) => {
                config.try_unfold(&mut o);
                ret = Ok(State::VerticalSeq(o));
                Default::default()
            }

            _ => {
                ret = err!("enter_seq: Invalid type");
                s
            }
        });

        ret
    }

    fn enter_special(&mut self) -> Result<State<'a>> {
        let mut ret = err!("Invalid pair");
        take_mut::take(self, |s| match s {
            State::Document(Fragment::Item(i)) => {
                ret = Ok(State::SpecialFirst(i.into()));
                State::Document(Default::default())
            }
            State::Document(Fragment::Outline(mut o)) => {
                let content = if o.len() == 1 {
                    Fragment::Item(o.pop().unwrap())
                } else {
                    Fragment::Outline(o)
                };
                ret = Ok(State::SpecialFirst(content));
                State::Document(Default::default())
            }
            State::VerticalSeq(mut o) => {
                // Sequence -> pair, prepare for raw mode, no blank filtering.
                if let Some(i) = o.pop() {
                    ret = Ok(State::SpecialFirst(i.into()));
                }
                State::VerticalSeq(o)
            }
            State::SectionSeq { i, item, n } if item.is_block() => {
                // Map values etc.
                ret = Ok(State::SpecialFirst(item.body.into()));
                State::SectionSeq {
                    i: i + 1,
                    n,
                    item: Default::default(),
                }
            }
            State::SpecialSecond(o) => {
                ret = Ok(State::SpecialFirst(o.into()));
                State::Document(Default::default())
            }
            // Others are no-go.
            _ => s,
        });

        ret
    }

    fn is_empty(&self) -> bool {
        match self {
            State::Document(f) => f.is_empty(),
            State::VerticalSeq(o) => o.is_empty_or_blank(),
            State::SectionSeq { item, .. } => item.is_blank(),
            State::InlineStruct { words, .. } => words.is_empty(),
            // Special halves are considered nonempty if pair was entered
            // succesfully.
            State::SpecialFirst(_) => false,
            State::SpecialSecond(_) => false,
        }
    }

    fn is_really_empty(&self) -> bool {
        match self {
            State::Document(f) => f.is_empty(),
            State::VerticalSeq(o) => o.is_empty(),
            State::SectionSeq { item, .. } => item.is_blank(),
            State::InlineStruct { words, .. } => words.is_empty(),
            // Special halves are considered nonempty if pair was entered
            // succesfully.
            State::SpecialFirst(_) => false,
            State::SpecialSecond(_) => false,
        }
    }
}

impl fmt::Display for State<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            State::Document(Fragment::Outline(o)) => {
                write!(f, "Document(\n{o})")
            }
            State::Document(Fragment::Item(i)) => write!(f, "Document({i})"),
            State::VerticalSeq(o) => write!(f, "VerticalSeq(\n{o})"),
            State::SectionSeq { item, .. } => {
                write!(f, "SectionSeq({item})")
            }
            State::InlineStruct { words, fields, .. } => {
                writeln!(f, "InlineStruct(")?;
                write!(f, " ")?;
                for a in fields.iter().rev() {
                    write!(f, " {a}")?;
                }
                writeln!(f)?;
                for w in words.iter().rev() {
                    write!(f, " {w}")?;
                }
                write!(f, ")")
            }
            State::SpecialFirst(Fragment::Outline(o)) => {
                write!(f, "SpecialFirst(\n{o})")
            }
            State::SpecialFirst(Fragment::Item(i)) => {
                write!(f, "SpecialFirst({i})")
            }
            State::SpecialSecond(o) => write!(f, "SpecialSecond(\n{o})"),
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
            self.enter_tuple(2).unwrap();
            self.enter_special().unwrap();
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
                        p.seq(|p| {
                            assert_eq!(p.n(), "a");
                            assert_eq!(p.n(), "1");
                        });
                        p.seq(|p| {
                            assert_eq!(p.n(), "b");
                            assert_eq!(p.n(), "2");
                        });
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
