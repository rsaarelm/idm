
pub type Result<'a, T> = std::result::Result<(T, (&'a str, &'a str)), &'a str>;

/// Read item from mutable slice, update slice if read was successful.
pub fn r<'a, T>(
    s: &mut (&'a str, &'a str),
    item: impl Fn((&'a str, &'a str)) -> Result<'a, T>
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
pub fn word<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, &'a str> {
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
fn eof<'a>((prefix, input): (&'a str, &'a str)) -> Result<()> {
    if input.chars().all(|c| c.is_whitespace()) {
        Ok(((), (prefix, "")))
    } else {
        Err(input)
    }
}

/// Succeed if there's no content left on input line.
///
/// Move to next line if whitespace was succesfully consumed.
fn eol<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, ()> {
    for (i, c) in input.char_indices() {
        if c == '\n' {
            return Ok(((), (prefix, &input[i+1..])));
        }

        if !c.is_whitespace() {
            return Err(&input[i..]);
        }
    }
    Ok(((), (prefix, "")))
}

/// Succeed if input is at end of current block of indentation.
///
/// Move to line starting next block if block was successfully ended.
pub fn eob<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, ()> {
    todo!()
}

// Use eob(input).is_err() to see you're within a block.

/// Enter the indentation level at input position that must be less than
/// current prefix.
///
/// Does not consume anything, affects the indent prefix in the remaining
/// input parameter instead.
pub fn dedent<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, ()> {
    todo!();
}

/// Parse section of the outline with headline at current input position (line
/// and exact indent prefix), return result with the indent prefix stripped.
pub fn section<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, String> {
    // TODO: Helper function that takes &mut String as parameter. Will also be
    // used to implement `block`.
    todo!();
}

/// Parse remaining sections in current block (content for each must start at
/// exactly the indent prefix), return string of them with the indent prefix
/// stripped.
pub fn block<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, String> {
    // TODO: Just call section helper repeatedly until at EOB.
    todo!();
}

/// From start of line with headline at prefix indent, move to start of first
/// body line and adjust indent prefix to that of body.
///
/// Will fail if there are no contentful body lines indented deeper than the
/// headline.
pub fn enter_body<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, String> {
    // XXX: Fixme, can't actually create a dummy prefix as &str, because it
    // doesn't exist anywhere in the content.
}

/// Parse indentation string starting from input. The new indentation will be
/// encoded in the "remaining input" value of the result.
///
/// The indentation must match the indentation character used in the existing
/// prefix and be followed by non-newline, non-whitespace content.
///
/// Encountering inconsistent indentation will fail the function. If the
/// current input line is at eob, returns prefix. If current input is a blank
/// line (eol), will return the indent prefix of the first contentful line
/// encountered when reading ahead in the same indent block.
///
/// It's assumed that `input` will be at the start of a line when this is
/// called.
fn indent<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, ()> {
    todo!()
}

/// Parse indent segment that matches indent char used in prefix.
fn indent_segment<'a>((prefix, input): (&'a str, &'a str)) -> Result<'a, &str> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stuff() {
        assert!(false);
    }
}
