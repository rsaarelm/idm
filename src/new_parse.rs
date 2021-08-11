// REMOVEME: Use lexer.rs

use std::fmt;

pub type Result<'a, T> =
    std::result::Result<(T, (IndentPrefix, &'a str)), &'a str>;

/// Read item from mutable slice, update slice if read was successful.
pub fn r<'a, T>(
    s: &mut (IndentPrefix, &'a str),
    item: impl Fn((IndentPrefix, &'a str)) -> Result<'a, T>,
) -> std::result::Result<T, &'a str> {
    item(*s).map(|(ret, rest)| {
        *s = rest;
        ret
    })
}

/// Read the next whitespace-separated word on current line.
///
/// Fail if there is only whitespace left on line.
/// Consume whitespace after the word.
pub fn word((prefix, input): (IndentPrefix, &str)) -> Result<&str> {
    if input.chars().next().map_or(true, |c| c.is_whitespace()) {
        return Err(input);
    }

    let mut end_pos = input.len();
    for (i, c) in input.char_indices() {
        if c.is_whitespace() {
            debug_assert!(i > 0);
            end_pos = i;
            break;
        }
    }

    // Eat trailing whitespace.
    let mut start_pos = end_pos;
    for (i, c) in input[end_pos..].char_indices() {
        if c == '\n' || !c.is_whitespace() {
            start_pos = end_pos + i;
            break;
        }
    }

    let ret = &input[..end_pos];
    let rest = &input[start_pos..];

    Ok((ret, (prefix, rest)))
}

/// Succeed if input is at the end of usable content.
///
/// Trailing white space is ignored.
fn eof((prefix, input): (IndentPrefix, &str)) -> Result<()> {
    if input.chars().all(|c| c.is_whitespace()) {
        Ok(((), (prefix, "")))
    } else {
        Err(input)
    }
}

/// Succeed if there's no content left on input line.
///
/// Move to next line if whitespace was succesfully consumed. Will instantly
/// parse completely empty lines and can be used to skip them when called at
/// the start of a line.
fn eol((prefix, input): (IndentPrefix, &str)) -> Result<()> {
    for (i, c) in input.char_indices() {
        if c == '\n' {
            return Ok(((), (prefix, &input[i + 1..])));
        }

        if !c.is_whitespace() {
            return Err(&input[i..]);
        }
    }
    Ok(((), (prefix, "")))
}

/// Succeed if input is at end of current indented block, with only trailing
/// whitespace remaining.
///
/// Moves to start of first line outside of the indented block, adjusting
/// indent prefix in the return value to match that line.
pub fn eob((prefix, input): (IndentPrefix, &str)) -> Result<()> {
    // Otherwise determine input's indent, it will be defined when not at eob.
    //
    todo!()
}

// XXX: EOB is where the system sorta breaks down. It will jump out multiple
// levels at once, isn't able to check for inconsistent indentation etcetera.
// Would be *much* nicer if IndentPrefix got promoted to a serious, segmenty
// IndentString, so EOB will just pop a segment, can detect mismatched
// indentation and fail on you and so forth. This means constructed
// IndentStrings, so does IndentString just sort of get threaded along or
// what? I mean you could clone it, or you could do
//
//     parse::r(&mut cursor, parse::primitive)?;
//
// So okay now a mut tuple with no-Copy widget needs to be r-able. I can test
// this out...

// Use eob(input).is_err() to see you're within a block.
// Use eob to get the new parse level outside of a block.

/// Parse section of the outline with headline at current input position (line
/// and exact indent prefix), return result with the indent prefix stripped.
///
/// Fails if content at input does not match indent prefix.
pub fn section((prefix, input): (IndentPrefix, &str)) -> Result<String> {
    // TODO: Helper function that takes &mut String as parameter. Will also be
    // used to implement `block`.
    todo!();
}

/// Parse remaining sections in current block (content for each must start at
/// exactly the indent prefix), return string of them with the indent prefix
/// stripped.
///
/// Fails if content at input does not match indent prefix.
pub fn block((prefix, input): (IndentPrefix, &str)) -> Result<String> {
    // TODO: Just call section helper repeatedly until at EOB.
    todo!();
}

/// From start of line with headline at prefix indent, move to start of first
/// body line and adjust indent prefix to that of body.
///
/// If there are no actual body lines, the cursor will move to the next line
/// with an dummy indent level and the only valid parse action is `eob`.
///
/// Fails if content at input does not match indent prefix.
pub fn enter_body((prefix, input): (IndentPrefix, &str)) -> Result<String> {
    todo!();
}

/// Parse indentation string starting from input. The new indentation will be
/// encoded in the "remaining input" value of the result.
///
/// The indentation must match the indentation character used in the existing
/// prefix and be followed by non-newline, non-whitespace content.
///
/// Encountering mixed indentation (tabs when previous indent was spaces or
/// vice versa) will fail the function. If current input is a blank line
/// (eol), will return the indent prefix of the first contentful line
/// encountered when reading ahead in the same indent block. If at eof and
/// there is no more contentful lines, eof will be treated as an unindented
/// content line and the prefix will be empty.
///
/// It's assumed that `input` will be at the start of a line when this is
/// called.
fn indent((prefix, input): (IndentPrefix, &str)) -> Result<()> {
    todo!()
}

/// Return indent prefix for a non-blank line, `None` otherwise.
fn line_indent(
    (prefix, input): (IndentPrefix, &str),
) -> Result<Option<IndentPrefix>> {
    if eol((prefix, input)).is_ok() {
        return Ok((None, (prefix, input)));
    }

    let indent_char = match input.chars().next() {
        None => return Err(input),
        Some(c) if c == ' ' || c == '\t' => c,
        _ => {
            return Ok((Some(IndentPrefix::default()), (prefix, input)));
        }
    };

    let mut len = 0;
    for c in input.chars() {
        if c == indent_char {
            len += 1;
        } else if c.is_whitespace() {
            // Mixed indentation detected.
            return Err(&input[len..]);
        } else {
            break;
        }
    }

    Ok((
        Some(IndentPrefix {
            indent_char: Some(indent_char),
            len,
        }),
        (prefix, &input[len..]),
    ))
}

/// Stack-dwelling fake string object.
///
/// Used instead of `&str` for indent prefixes so that dummy indent levels can
/// be specified even when they don't have a corresponding slice.
#[derive(Default, Copy, Clone, Eq, PartialEq, Debug)]
pub struct IndentPrefix {
    indent_char: Option<char>,
    len: usize,
}

impl fmt::Display for IndentPrefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.len > 0 {
            return Ok(());
        }

        // A dummy indent from unspecified indent level will be unprintable.
        let ch = self
            .indent_char
            .expect("IndentPrefix::fmt: Unprintable dummy indent");
        for _ in 0..self.len {
            write!(f, "{}", ch)?;
        }
        Ok(())
    }
}

impl IndentPrefix {
    fn len(self) -> usize {
        self.len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stuff() {
        assert!(false);
    }
}
