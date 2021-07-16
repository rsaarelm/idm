//! Stateless parsing primitives

use crate::{err, error::Error};

type Result<'a, T> = std::result::Result<(T, &'a str), Error>;

/// Read item from mutable slice, update slice if read was successful.
pub fn read<'a, T>(
    s: &mut &'a str,
    item: impl Fn(&'a str) -> Result<T>,
) -> crate::error::Result<T> {
    item(*s).map(|(ret, rest)| {
        *s = rest;
        ret
    })
}

/// Read the next whitespace-separated word on current line.
///
/// Fail if there is only whitespace left on line.
/// Consume whitespace after the word.
pub fn word(input: &str) -> Result<&str> {
    if input.chars().next().map_or(true, |c| c.is_whitespace()) {
        return err!("word: No text content");
    }

    let mut end_pos = input.len();
    for (i, c) in input.char_indices() {
        if c.is_whitespace() {
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
    debug_assert!(!ret.is_empty(), "word: Trying to return empty word");
    let rest = &input[start_pos..];

    Ok((ret, rest))
}

/// Read line until newline, rest of the content is after newline.
///
/// Fails if there is no input.
pub fn line(input: &str) -> Result<&str> {
    if input == "" {
        err!("line: End of input")
    } else {
        let p = input.find('\n').unwrap_or(input.len());
        Ok((&input[..p], &input[(p + 1).min(input.len())..]))
    }
}

/// Matches given constant with input.
///
/// Fails if input does not match, consumes constant otherwise.
pub fn constant<'a, 'b: 'a>(
    text: &'b str,
) -> impl Fn(&'a str) -> Result<'a, ()> {
    move |input| {
        if input.len() < text.len() {
            return err!("constant: Out of input");
        }
        for (c1, c2) in text.chars().zip(input.chars()) {
            if c1 != c2 {
                return err!("constant: Input does not match");
            }
        }
        Ok(((), &input[text.len()..]))
    }
}

pub fn indentation(input: &str) -> Result<&str> {
    if input == "" {
        return err!("indentation: End of input");
    }

    for (i, c) in input.char_indices() {
        if c == '\n' || !c.is_whitespace() {
            if i == 0 {
                return err!("indentation: No indentation");
            } else {
                return Ok((&input[0..i], &input[i..]));
            }
        }
    }

    // If we fell through with nonempty input, the whole rest of input is
    // indetation.
    Ok((input, ""))
}

/// Parse indentations from beginning of the line.
///
/// They must share a maximum prefix of complete segments with the previous
/// indentation given as parameter.
pub fn indentations<'a, 'b: 'a>(
    previous: &'b Vec<&'a str>,
) -> impl Fn(&'a str) -> Result<'a, Vec<&'a str>> {
    move |input| {
        let mut pos = input;
        let mut ret: Vec<&'a str> = Vec::new();

        for segment in previous {
            // While we have the exact same indetation as before, just push the
            // segments in.
            if read(&mut pos, constant(segment)).is_ok() {
                ret.push(*segment);
                continue;
            }

            // Fallthrough, failed to match constant.

            match indentation(pos) {
                Ok(_) => {
                    // There are still segments left, input has indentation, but
                    // the indentation in input does not match the latest segment.
                    // This is not allowed.
                    return err!("indentations: Mismatched indentation");
                }
                Err(_) => {
                    // If indentation parse fails right away, this means we're out
                    // of indentation, either hit the content or at EOL. We're
                    // done here.
                    return Ok((ret, pos));
                }
            }
        }

        // Fell through, check if there's extra indentation.
        if let Ok((new_segment, rest)) = indentation(pos) {
            // Everything after the expected sequence becomes a new chunk.
            ret.push(new_segment);
            return Ok((ret, rest));
        }

        // Final case, perfect line-up with previous indetation.
        Ok((ret, pos))
    }
}

pub fn empty_comment(input: &str) -> Result<()> {
    let (line, rest) = line(input)?;

    if line.starts_with("--") && line[2..].chars().all(|c| c.is_whitespace()) {
        Ok(((), rest))
    } else {
        err!("empty_comment: Not an empty comment")
    }
}

pub fn comment(input: &str) -> Result<&str> {
    if let Ok((_, rest)) = empty_comment(input) {
        return Ok(("--", rest));
    }

    let (line, rest) = line(input)?;
    // Content after -- must be separated with a whitespace.
    if line.starts_with("--")
        && line[2..].chars().next().map_or(true, |c| c.is_whitespace())
    {
        Ok((line, rest))
    } else {
        err!("comment: Not a comment")
    }
}

pub fn blank_line(input: &str) -> Result<()> {
    let (line, rest) = line(input)?;

    if line.chars().all(|c| c.is_whitespace()) {
        Ok(((), rest))
    } else {
        err!("blank_line: Line has content")
    }
}

/// Read body indented beyond previous indentation.
///
/// Input is assumed to be at the start of the line for the first line of the
/// indented body. All body lines must be indented deeper than the previous
/// indentation. The function will look for the line with the shallowest
/// indentation deeper than the previous indentation, and use that as the
/// common new indentation prefix. All body lines must have consistent
/// indentation with this prefix, but their indentation beyond the prefix is
/// ignored. The resulting string will have the prefix indentations stripped.
pub fn indented_body<'a>(
    previous: &'a Vec<&'a str>,
    input: &'a str,
) -> Result<'a, String> {
    // Find the minimum indent.
    let (mut indent, _): (Vec<&'a str>, _) = indentations(previous)(input)?;
    if indent.len() == previous.len() {
        return err!("indented_body: No indented lines found");
    }
    let mut pos = input;
    loop {
        if pos == "" {
            break;
        }
        // We might catch inconsistent indentation with the existing indents
        // here.
        let candidate = read(&mut pos, indentations(previous))?;
        if candidate.len() <= previous.len() {
            break;
        } else {
            // Shortest additional indent -> the one we want.
            debug_assert!(candidate.len() == previous.len() + 1);
            if candidate[candidate.len() - 1].len()
                < indent[indent.len() - 1].len()
            {
                indent = candidate;
            }
        }
    }

    // Read the lines with our indent
    let mut pos = input;
    let mut ret = String::new();
    loop {
        if pos == "" {
            break;
        }
        if read(&mut pos, blank_line).is_ok() {
            ret.push('\n');
        } else {
            let line_indent = read(&mut pos, indentations(&indent))?;

            if line_indent.len() < indent.len() {
                break;
            }
            if line_indent.len() > indent.len() {
                ret.push_str(line_indent[line_indent.len() - 1]);
            }
            ret.push_str(read(&mut pos, line)?);
            ret.push('\n');
        }
    }
    debug_assert!(!ret.is_empty());
    // Get rid of trailing newline.
    ret.pop();

    // XXX: Stupid hackery instead of just returning (ret, pos) because borrow
    // checker thinks pos has been somehow tainted at this point.
    Ok((ret, &input[(input.len() - pos.len())..]))
}

/// Parse an IDM attribute key.
///
/// Key must be "name:", with whitespace after the colon. The names are
/// converted from IDM's kebab-case to Rust's camel_case.
pub fn key(input: &str) -> Result<String> {
    let mut pos = input;
    let word = read(&mut pos, word)?;

    if !word.ends_with(":") || word == ":" {
        return err!("key: Invalid syntax");
    }

    let word = &word[..(word.len() - 1)]; //  Drop the trailing ':'.
    let word = word.replace("-", "_"); //     Convert to camel_case.

    Ok((word, pos))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_words() {
        assert!(word("").is_err());

        let text = "The quick brown fox\njumps over";
        let mut cursor = &text[..];
        assert_eq!(read(&mut cursor, word), Ok("The"));
        assert_eq!(read(&mut cursor, word), Ok("quick"));
        assert_eq!(read(&mut cursor, word), Ok("brown"));
        assert_eq!(read(&mut cursor, word), Ok("fox"));
        assert!(read(&mut cursor, word).is_err(), "Must stop at EOL");
    }

    #[test]
    fn test_indentation() {
        // NB. Test inputs are for educational purpose only. Please don't mix
        // tabs and spaces in any actual IDM file's indentation.

        assert_eq!(indentations(&vec!["  ", "\t"])(""), Ok((vec![], "")));
        assert_eq!(indentations(&vec!["  ", "\t"])("abc"), Ok((vec![], "abc")));
        assert!(indentations(&vec!["  ", "\t"])(" ").is_err());
        assert_eq!(indentations(&vec!["  ", "\t"])("  "), Ok((vec!["  "], "")));
        assert_eq!(
            indentations(&vec!["  ", "\t"])("  abc"),
            Ok((vec!["  "], "abc"))
        );

        assert!(indentations(&vec!["  ", "\t"])("\t").is_err());
        assert_eq!(
            indentations(&vec!["  ", "\t"])("  \t"),
            Ok((vec!["  ", "\t"], ""))
        );
        assert_eq!(
            indentations(&vec!["  ", "\t"])("  \t\t  "),
            Ok((vec!["  ", "\t", "\t  "], ""))
        );
        assert_eq!(
            indentations(&vec!["  ", "\t"])("  \t\t  abc"),
            Ok((vec!["  ", "\t", "\t  "], "abc"))
        );
    }

    #[test]
    fn test_comment() {
        assert!(empty_comment("").is_err());
        assert!(empty_comment("a").is_err());
        assert!(empty_comment("--").is_ok());
        assert!(empty_comment("--  ").is_ok());
        assert!(empty_comment("--a").is_err());
        assert!(empty_comment("-- a").is_err());

        assert!(comment("").is_err());
        assert!(comment("a").is_err());
        assert!(comment("--").is_ok());
        assert!(comment("--a").is_err());
        assert!(comment("-- a").is_ok());

        assert_eq!(comment("-- xyzzy"), Ok(("-- xyzzy", "")));
        assert_eq!(comment("-- xyzzy\nplugh"), Ok(("-- xyzzy", "plugh")));
    }

    #[test]
    fn test_key() {
        assert!(key("").is_err());
        assert!(key("abc").is_err());
        assert!(key(":").is_err());

        assert_eq!(key("foo: bar"), Ok(("foo".into(), "bar")));
        assert_eq!(key("foo-bar: baz"), Ok(("foo_bar".into(), "baz")));
    }
}
