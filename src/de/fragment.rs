use std::fmt;

use crate::{
    de::parse::{self, Indent},
    err, CharExt, Error, Result,
};

/// Structural type for fragments of an IDM document.
#[derive(Clone, Debug)]
pub enum Fragment<'a> {
    Item(Item<'a>),
    Outline(Outline<'a>),
}

impl Default for Fragment<'_> {
    fn default() -> Self {
        Fragment::Outline(Default::default())
    }
}

impl<'a> Fragment<'a> {
    pub fn from_str(input: &'a str) -> Result<Self> {
        if input
            .chars()
            .next()
            .map_or(false, |c| c.is_idm_whitespace())
        {
            // Must not start with indentation.
            Err(Error::new("Unexpected indentation").with_line_num(1))
        } else if input.chars().all(CharExt::is_idm_whitespace) {
            // Empty input is empty.
            Ok(Fragment::Outline(Outline::default()))
        } else if !input.chars().any(|c| c == '\n') {
            // Single line input makes an item fragment rather than an
            // outline.
            Ok(Fragment::Item(Item {
                head: input,
                ..Default::default()
            }))
        } else {
            Ok(Fragment::Outline(
                Outline::parse(input, Indent::default(), input)
                    .map_err(|e| e.with_line_num(1))?
                    .0,
            ))
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Fragment::Item(i) => i.is_blank(),
            Fragment::Outline(o) => o.is_empty(),
        }
    }
}

impl<'a> From<Outline<'a>> for Fragment<'a> {
    fn from(o: Outline<'a>) -> Self {
        Fragment::Outline(o)
    }
}

impl<'a> From<Item<'a>> for Fragment<'a> {
    fn from(i: Item<'a>) -> Self {
        Fragment::Item(i)
    }
}

impl fmt::Display for Fragment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Fragment::Item(i) => i.write(Default::default(), f),
            Fragment::Outline(o) => o.write(Default::default(), f),
        }
    }
}

/// An outline is a list of consecutive items at the same indent depth.
#[derive(Clone, Default, Debug)]
pub struct Outline<'a>(pub(crate) Vec<Item<'a>>);

impl<'a> std::ops::Deref for Outline<'a> {
    type Target = Vec<Item<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> Outline<'a> {
    fn parse(
        input_start: &'a str,
        expected_indent: Indent,
        input: &'a str,
    ) -> Result<(Outline<'a>, &'a str)> {
        let mut ret: Vec<Item<'a>> = Vec::new();

        let mut current = input;
        loop {
            let (item, rest) =
                Item::parse(input_start, expected_indent, current)
                    .map_err(|e| e.infer_line_num(input_start, current))?;
            current = rest;

            ret.push(item);

            if rest.is_empty() {
                break;
            }

            let (next_indent, _) = parse::indent(current)?;
            if !next_indent.compatible_with(expected_indent) {
                return Err(Error::new("Inconsistent indentation")
                    .infer_line_num(input_start, current));
            } else if next_indent.len() > expected_indent.len() {
                // This occurs when dedent goes out of item body depth but not
                // fully into previous depth, eg.
                //
                // A
                //   B
                //  C
                return Err(Error::new("Unexpected indentation")
                    .infer_line_num(input_start, current));
            } else if next_indent.len() < expected_indent.len() {
                // No inconsistencies detected, but we've dropped out of the
                // current indentation scope, stop parsing the outline.
                break;
            }

            // Otherwise the indents are the same, meaning there are more
            // items and the loop continues.
        }

        // Reverse the vec since we'll be doing stack popping starting from
        // the first item.
        ret.reverse();
        Ok((Outline(ret), current))
    }

    fn write(&self, indent: Indent, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.iter().rev().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            item.write(indent, f)?;
        }
        Ok(())
    }

    pub fn pop(&mut self) -> Option<Item<'a>> {
        self.0.pop()
    }

    pub fn pop_nonblank(&mut self) -> Option<Fragment<'a>> {
        // Accumulate a decoloned outline.
        let mut colon_block = Vec::new();

        loop {
            if self.0.is_empty() {
                return if !colon_block.is_empty() {
                    colon_block.reverse();
                    Some(Outline(colon_block).into())
                } else {
                    None
                };
            }
            let top = self.0.len() - 1;

            if self.0[top].is_comment() || self.0[top].is_blank() {
                // Skip blanks.
                self.0.pop();
                continue;
            } else if self.0[top].is_colon_item() {
                // Coloned element, accumulate into colon block.
                //
                // Remove the colon (and any whitespace between colon and
                // content).
                self.0[top].head = self.0[top].head[1..]
                    .trim_start_matches(CharExt::is_idm_whitespace);
                // Accumulate to colon block and continue.
                colon_block.push(self.0.pop().unwrap());

                continue;
            } else if !colon_block.is_empty() {
                // There's a non-blank, non-coloned element, but some colon
                // blocks have already been collected. Return the collected
                // block.
                colon_block.reverse();
                return Some(Outline(colon_block).into());
            } else if self.0[top].head_is_comment() {
                // The next item has a body and a comment headline. In
                // blank-filtering context, this is interpreted as an escaped
                // block. Make it into a real block here.
                return Some(self.0.pop().unwrap().body.into());
            } else {
                // Otherwise just return it as is.
                return Some(self.0.pop().unwrap().into());
            }
        }
    }

    pub fn is_empty_or_blank(&self) -> bool {
        self.0.iter().all(|i| i.is_comment() || i.is_blank())
    }

    /// If this outline consists of a single outline child, replace this with
    /// the child outline.
    ///
    /// This is an awkward method used by the awkward syntax exception that
    /// allows you to use the `:attribute 123` naming style even when it's not
    /// structurally warranted to keep up the charade that the colons are
    /// actually part of attribute syntax.
    pub(crate) fn try_unfold_only_child_outline(&mut self) {
        let mut a = self.clone();

        if let Some(Fragment::Outline(child)) = a.pop_nonblank() {
            // There's an outline child...
            if a.pop_nonblank().is_none() {
                // ...and it's the only child.
                // Replace self with it.
                *self = child;
            }
        }
    }
}

impl fmt::Display for Outline<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(Default::default(), f)
    }
}

/// An item is a line of text and a (possibly empty) indented body outline
/// below it.
#[derive(Clone, Default, Debug)]
pub struct Item<'a> {
    pub head: &'a str,
    pub body_indent: Indent,
    pub body: Outline<'a>,
}

impl<'a> Item<'a> {
    fn parse(
        input_start: &'a str,
        expected_indent: Indent,
        input: &'a str,
    ) -> Result<(Item<'a>, &'a str)> {
        let (indent, _) = parse::indent(input)?;
        if indent != expected_indent {
            return err!("Unexpected indentation");
        }

        let (line, rest) = parse::line(input)?;

        // Strip indentation when putting it in headline.
        // Must always have at least one headline when parsing a block.
        let head = if line.chars().all(CharExt::is_idm_whitespace) {
            ""
        } else {
            &line[indent.len()..]
        };

        // Does the initial section have body lines?
        let (next_indent, _) = parse::indent(rest)?;
        if !next_indent.compatible_with(expected_indent) {
            return err!("Inconsistent indentation");
        }

        let (item, rest) = if next_indent.len() > expected_indent.len() {
            // There is a body, parse as outline.
            let (body, rest) =
                Outline::parse(input_start, next_indent, rest)
                    .map_err(|e| e.infer_line_num(input_start, rest))?;
            let body_indent = next_indent - expected_indent;

            (
                Item {
                    head,
                    body_indent,
                    body,
                },
                rest,
            )
        } else {
            // Just the headline and an empty body.
            (
                Item {
                    head,
                    ..Default::default()
                },
                rest,
            )
        };

        Ok((item, rest))
    }

    fn write(&self, indent: Indent, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.head.is_empty() {
            write!(f, "{indent}{}", self.head)?;
        }
        if !self.body.0.is_empty() {
            writeln!(f)?;
        }
        self.body.write(indent + self.body_indent, f)
    }

    pub fn head_is_comment(&self) -> bool {
        self.head == "--" || self.head.starts_with("-- ")
    }

    pub fn is_comment(&self) -> bool {
        self.is_line() && self.head_is_comment()
    }

    pub fn is_line(&self) -> bool {
        self.body.is_empty()
    }

    pub fn is_section(&self) -> bool {
        !self.is_line()
    }

    pub fn is_block(&self) -> bool {
        self.has_blank_line() && !self.body.is_empty()
    }

    pub fn is_blank(&self) -> bool {
        self.is_line() && self.has_blank_line()
    }

    pub fn has_blank_line(&self) -> bool {
        self.head.chars().all(CharExt::is_idm_whitespace)
    }

    pub fn is_colon_item(&self) -> bool {
        // Must be a colon followed immediately by a non-whitespace element.
        self.head.starts_with(':')
            && self
                .head
                .chars()
                .nth(1)
                .map_or(false, |c| !c.is_idm_whitespace())
    }

    pub fn pop_word(&mut self) -> Option<&'a str> {
        if let Ok((word, rest)) = parse::word(self.head) {
            self.head = rest;
            Some(word)
        } else {
            None
        }
    }

    pub fn detach_head(&self) -> Item<'a> {
        Item {
            head: self.head,
            body_indent: self.body_indent,
            body: Default::default(),
        }
    }
}

impl fmt::Display for Item<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(Default::default(), f)
    }
}
