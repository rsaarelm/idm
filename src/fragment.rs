use std::{borrow::Cow, fmt, rc::Rc};

use crate::{
    err,
    parse::{self, CharExt, Indent},
    Error, Result,
};

#[derive(Clone, Debug)]
/// Parse tree for an IDM outline text.
pub enum Fragment<'a> {
    /// No content. (Not a blank line.)
    Empty,
    /// A single (non-empty) line.
    Line(&'a str),
    DecolonLine(&'a str),
    /// A section with a (non-empty) indented body.
    ///
    /// The head is `Line` and the body is `Block`.
    ///
    /// The head may change to `Empty` during fragment traversal, but should
    /// never be empty for initially constructed sections.
    Section {
        head: Rc<Fragment<'a>>,
        body_indent: Indent,
        body: Rc<Fragment<'a>>,
    },
    /// A sequence of elements at the same indent depth.
    /// First elements must be `Section` or `Line`. Next elements must be
    /// `Block` or `Empty`.
    Block {
        first: Rc<Fragment<'a>>,
        next: Rc<Fragment<'a>>,
    },

    Phrase(&'a str),
    ConstName {
        text: String,
        // Keep a reference to actual input text so we can get a line number.
        input_pos: Option<&'a str>,
    },
}

use Fragment::*;

impl<'a> Fragment<'a> {
    pub fn new(input: &'a str) -> Result<Fragment<'a>> {
        if input
            .chars()
            .next()
            .map_or(false, |c| c.is_idm_whitespace())
        {
            Err(Error::new("Unexpected indentation").with_line_num(1))
        } else if input.chars().all(CharExt::is_idm_whitespace) {
            Ok(Empty)
        } else if !input.chars().any(|c| c == '\n') {
            Ok(Line(input))
        } else {
            Ok(Fragment::parse_block(input, Indent::default(), input)?.0)
        }
    }

    fn parse_block(
        input_start: &'a str,
        expected_indent: Indent,
        input: &'a str,
    ) -> Result<(Fragment<'a>, &'a str)> {
        let err = |msg: &'static str| {
            move |e: &str| Error::new(msg).infer_line_num(input_start, e)
        };
        let indent_err = |e| {
            Error::new("Inconsistent indentation")
                .infer_line_num(input_start, e)
        };

        let (indent, _) = parse::indent(input).map_err(indent_err)?;
        if indent != expected_indent {
            return Err(input).map_err(indent_err);
        }

        let (line, rest) = parse::line(input).map_err(err("Line error"))?;

        // Strip indentation when putting it in headline.
        // Must always have at least one headline when parsing a block.
        let line = if line.is_empty() {
            line
        } else {
            &line[indent.len()..]
        };

        let head = Line(line);

        // Does the initial section have body lines?
        let (mut next_indent, _) = parse::indent(rest).map_err(indent_err)?;
        if !next_indent.compatible_with(expected_indent) {
            return Err(rest).map_err(indent_err);
        }
        let (first, rest) = if next_indent.len() > expected_indent.len() {
            // Yep, hit a deeper level of indent.
            // Recursively parse child blocks.
            let (body, rest) =
                Fragment::parse_block(input_start, next_indent, rest)?;
            let body_indent = next_indent - expected_indent;

            // Recompute indent after reading body blocks.
            next_indent = parse::indent(rest).map_err(indent_err)?.0;

            (
                Rc::new(Section {
                    head: Rc::new(head),
                    body_indent,
                    body: Rc::new(body),
                }),
                rest,
            )
        } else {
            // Nope, do nothing.
            (Rc::new(head), rest)
        };

        let (next, rest) =
            if !rest.is_empty() && next_indent.len() == expected_indent.len() {
                // There's a sibling block, parse that.
                let (block, rest) =
                    Fragment::parse_block(input_start, next_indent, rest)?;
                (Rc::new(block), rest)
            } else {
                (Rc::new(Empty), rest)
            };

        Ok((Block { first, next }, rest))
    }

    pub fn print_section(
        &self,
        indent: Indent,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Section {
                head,
                body_indent,
                body,
            } => {
                match &**head {
                    Line(line) => {
                        if line != &"" {
                            writeln!(f, "{}{}", indent, line)?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                    DecolonLine(line) => {
                        if line != &"" {
                            let colon_pos = line
                                .find(':')
                                .expect("No colon in DecolonLine");
                            writeln!(
                                f,
                                "{}{}{}",
                                indent,
                                &line[0..colon_pos],
                                &line[colon_pos + 1..]
                            )?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                    Empty => {
                        writeln!(f)?;
                    }
                    _ => panic!("Invalid section"),
                }
                body.print_block(indent + *body_indent, f)
            }
            _ => panic!("Not a section"),
        }
    }

    /// Recursively print the contents of a fragment.
    pub fn print_block(
        &self,
        indent: Indent,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Empty => Ok(()),
            Block { first, next } => {
                match &**first {
                    s @ Section { .. } => s.print_section(indent, f)?,
                    // XXX: Lots of repetition from print_section
                    Line(line) => {
                        if line != &"" {
                            writeln!(f, "{}{}", indent, line)?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                    DecolonLine(line) => {
                        if line != &"" {
                            let colon_pos = line
                                .find(':')
                                .expect("No colon in DecolonLine");
                            writeln!(
                                f,
                                "{}{}{}",
                                indent,
                                &line[0..colon_pos],
                                &line[colon_pos + 1..]
                            )?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                    _ => panic!("Invalid block"),
                }
                next.print_block(indent, f)
            }
            _ => panic!("Not a block"),
        }
    }

    pub fn str_slice(&self) -> Option<&'a str> {
        match self {
            Empty => None,
            Line(s) | DecolonLine(s) => Some(s),
            Section { head, body, .. } => {
                head.str_slice().or_else(|| body.str_slice())
            }
            Block { first, .. } => first.str_slice(),
            Phrase(s) => Some(s),
            ConstName { input_pos, .. } => *input_pos,
        }
    }

    pub fn is_blank(&self) -> bool {
        matches!(self, Line(s) if s.chars().all(CharExt::is_idm_whitespace))
    }

    pub fn is_comment_line(&self) -> bool {
        matches!(self, Line(s) if s.starts_with("--"))
    }

    pub fn is_inline(&self) -> bool {
        !matches!(self, Block { .. } | Section { .. } | Empty)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Empty)
    }

    /// If fragment is commenty, return the comment-less version.
    ///
    /// Standalone comment lines turn to Empty, sections with comment headline
    /// turn into their child blocks.
    ///
    /// Also skips blank lines because you generally always want them gone
    /// when filtering comments
    pub fn comment_filter(self) -> Option<Self> {
        match self {
            a if a.is_comment_line() => None,
            a if a.is_blank() => None,
            Section { head, body, .. } if head.is_comment_line() => {
                Some((*body).clone())
            }
            a => Some(a.clone()),
        }
    }

    pub fn rewrite(&mut self, text: impl Into<String>) {
        let text: String = text.into();
        let input_pos = self.str_slice();
        *self = ConstName { text, input_pos };
    }

    pub fn to_str(&self) -> Cow<'a, str> {
        match self {
            Block { .. } | Section { .. } => Cow::from(self.to_string()),
            Line(l) | Phrase(l) => Cow::from(*l),
            DecolonLine(l) => {
                let colon_pos = l.find(':').expect("No colon in DecolonLine");
                Cow::from(format!(
                    "{}{}",
                    &l[0..colon_pos],
                    &l[colon_pos + 1..]
                ))
            }
            Empty => Cow::from(""),
            ConstName { text, .. } => Cow::from(text.clone()),
        }
    }

    fn _split(self, in_raw_mode: bool) -> Option<(Self, Self)> {
        match self {
            Line(s) => {
                if in_raw_mode {
                    Some((Line(s), Fragment::Empty))
                } else if let Ok((p, rest)) = parse::word(s) {
                    Some((
                        Phrase(p),
                        if rest.chars().all(CharExt::is_idm_whitespace) {
                            Empty
                        } else {
                            Line(rest)
                        },
                    ))
                } else {
                    None
                }
            }
            DecolonLine(s) => {
                if let Ok((p, rest)) = parse::word(s) {
                    let decolon = &p[0..p.len() - 1];
                    Some((
                        Phrase(decolon),
                        if rest.chars().all(CharExt::is_idm_whitespace) {
                            Empty
                        } else {
                            Line(rest)
                        },
                    ))
                } else {
                    None
                }
            }
            Section { head, body, .. } => {
                Some(((*head).clone(), (*body).clone()))
            }
            Block { first, next } => Some(((*first).clone(), (*next).clone())),
            _ => None,
        }
    }

    pub fn split(self) -> Option<(Self, Self)> {
        self._split(false)
    }

    pub fn split_raw(self) -> Option<(Self, Self)> {
        self._split(true)
    }

    pub fn decolonate(self) -> Result<Self> {
        match self {
            Empty => Ok(Empty),
            Line(s) => Ok(DecolonLine(s)),
            Section {
                head,
                body_indent,
                body,
            } => Ok(Section {
                head: (*head).clone().decolonate()?.into(),
                body_indent,
                body: body.clone(),
            }),
            _ => err!("Bad attribute fragment"),
        }
    }

    pub fn join(self, other: Self) -> Self {
        match self {
            Empty => Block {
                first: other.into(),
                next: Empty.into(),
            },
            Block { first, next } => Block {
                first: first.clone(),
                next: (*next).clone().join(other).into(),
            },
            _ => panic!("Cannot join fragments"),
        }
    }
}

impl<'a> Default for Fragment<'a> {
    fn default() -> Self {
        Fragment::Empty
    }
}

impl<'a> fmt::Display for Fragment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Empty => Ok(()),
            Line(l) | Phrase(l) => l.fmt(f),
            DecolonLine(l) => {
                let colon_pos = l.find(':').expect("No colon in DecolonLine");
                write!(f, "{}{}", &l[0..colon_pos], &l[colon_pos + 1..])
            }
            s @ Section { .. } => s.print_section(Default::default(), f),
            b @ Block { .. } => b.print_block(Default::default(), f),
            ConstName { text, .. } => write!(f, "{}", text),
        }
    }
}

impl<'a> Iterator for Fragment<'a> {
    type Item = Fragment<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((head, tail)) = self.clone().split() {
            *self = tail;
            Some(head)
        } else {
            None
        }
    }
}
