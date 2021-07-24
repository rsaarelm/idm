//! Stateless parsing primitives

use crate::indent_string::IndentString;

pub type Result<'a, T> = std::result::Result<(T, &'a str), &'a str>;

/// Read item from mutable slice, update slice if read was successful.
pub fn r<'a, T>(
    s: &mut &'a str,
    item: impl Fn(&'a str) -> Result<T>,
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
pub fn word(input: &str) -> Result<&str> {
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

    Ok((ret, rest))
}

/// Read line until newline, rest of the content is after newline.
///
/// Fails if there is no input.
pub fn line(input: &str) -> Result<&str> {
    if input == "" {
        Err(input)
    } else {
        let p = input.find('\n').unwrap_or(input.len());
        Ok((&input[..p], &input[(p + 1).min(input.len())..]))
    }
}

pub fn empty_comment(input: &str) -> Result<()> {
    let (line, rest) = line(input)?;

    if line.starts_with("--") && line[2..].chars().all(|c| c.is_whitespace()) {
        Ok(((), rest))
    } else {
        // Not an empty comment
        Err(input)
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
        // Not a comment
        Err(input)
    }
}

pub fn blank_line(input: &str) -> Result<()> {
    let (line, rest) = line(input)?;

    if line.chars().all(|c| c.is_whitespace()) {
        Ok(((), rest))
    } else {
        // Not a blank line
        Err(input)
    }
}

/// Consume consecutive comment and blank lines at or below current indent.
///
/// End at the beginning of the first contentful line or the first line above
/// given indent depth, whichever comes first.
pub fn non_content<'a: 'b, 'b>(
    current_indent: &'b IndentString,
) -> impl Fn(&'a str) -> Result<'a, ()> + 'b {
    move |input| {
        let mut pos = input;
        loop {
            if r(&mut pos, blank_line).is_ok() {
                continue;
            }

            let (indent, mut line) = current_indent.match_next(pos)?;

            if indent.len() != current_indent.len() {
                // If we're above expected depth, we're out of the block and
                // should exit.
                //
                // If we're *below* the depth, the line is also assumed to be
                // content, even if it looks like a comment. The standard way
                // for escaping a comment-looking line is turning a line into
                // a block:
                //
                //     --
                //       -- bar
                return Ok(((), pos));
            }

            if r(&mut line, comment).is_ok() {
                pos = line;
                continue;
            } else {
                // Not blank, not comment, assume it's content
                return Ok(((), pos));
            }
        }
    }
}

/// Parse a line only if it has more indented child lines.
///
/// On success, return the headline and the new indent string for the first
/// non-empty child line. Leave cursor at the start of the line after the
/// headline.
pub fn headline<'a: 'b, 'b>(
    current_indent: &'b IndentString,
) -> impl Fn(&'a str) -> Result<'a, (&'a str, IndentString)> + 'b {
    move |input| {
        let (ret, rest) = line(input)?;
        let mut pos = rest;
        loop {
            if pos == "" {
                return Err(input);
            }

            // Skip over blank lines until we get something that actually has
            // a defined indent depth.
            if let Ok((_, rest)) = blank_line(pos) {
                pos = rest;
                continue;
            }

            let indent = current_indent.match_next(pos)?.0;
            return if indent.len() > current_indent.len() {
                Ok(((ret, indent), rest))
            } else {
                Err(input)
            };
        }
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
///
/// As a special case, if `previous` indentation is empty, the entire input is
/// assumed to belong to the body and will just be returned as is.
pub fn indented_body<'a>(
    prev: &'a IndentString,
    input: &'a str,
) -> Result<'a, String> {
    // Special case if there's no expected initial indentation, return input
    // as is.
    //
    // XXX: To keep things less surprising, this could be modified to check if
    // all of the input lines still have a shared indent prefix and stripping
    // that prefix if there is (and erroring out if the global indentation is
    // inconsistent with tabs and spaces). Otherwise there's an unspoken
    // assumption that `indented_body` never returns values with a global
    // indentation, which is violated by the zero starting indent case.
    if prev.is_empty() {
        return Ok((input.trim_end().into(), ""));
    }

    // Find the minimum indent.
    let (mut indent, _) = prev.match_next(input)?;
    if indent.len() == prev.len() {
        // No indented lines found
        return Err(input);
    }
    let mut pos = input;
    loop {
        if pos == "" {
            break;
        }

        if r(&mut pos, blank_line).is_ok() {
            continue;
        }

        // We might catch inconsistent indentation with the existing indents
        // here.
        let candidate = r(&mut pos, |input| prev.match_next(input))?;
        if candidate.len() <= prev.len() {
            break;
        } else {
            // Shortest additional indent -> the one we want.
            debug_assert!(candidate.len() == prev.len() + 1);
            if candidate[candidate.len() - 1] < indent[indent.len() - 1] {
                indent = candidate;
            }
        }

        if r(&mut pos, line).is_err() {
            break;
        }
    }

    // Read the lines with our indent
    let mut pos = input;
    let mut ret = String::new();
    loop {
        if pos == "" {
            break;
        }
        if r(&mut pos, blank_line).is_ok() {
            ret.push('\n');
        } else {
            let mut next_pos = pos;
            let line_indent =
                r(&mut next_pos, |input| indent.match_next(input))?;

            if line_indent.len() < indent.len() {
                break;
            }
            if line_indent.len() > indent.len() {
                ret.push_str(
                    &line_indent.string(line_indent[line_indent.len() - 1]),
                );
            }
            ret.push_str(r(&mut next_pos, line)?);
            ret.push('\n');
            pos = next_pos;
        }
    }
    debug_assert!(!ret.is_empty());
    // Get rid of trailing newline.
    ret.pop();

    Ok((ret, pos))
}

/// Parse an IDM attribute key.
///
/// Key must be "name:", with whitespace after the colon. The names are
/// converted from IDM's kebab-case to Rust's camel_case.
pub fn key(input: &str) -> Result<String> {
    let mut pos = input;
    let word = r(&mut pos, word)?;

    if !word.ends_with(":") || word == ":" {
        // Invalid syntax
        return Err(input);
    }

    let word = &word[..(word.len() - 1)]; //  Drop the trailing ':'.
    let word = word.replace("-", "_"); //     Convert to camel_case.

    Ok((word, pos))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::indent_string::IndentString;

    #[test]
    fn test_words() {
        assert!(word("").is_err());

        let text = "The quick brown fox\njumps over";
        let mut cursor = &text[..];
        assert_eq!(r(&mut cursor, word), Ok("The"));
        assert_eq!(r(&mut cursor, word), Ok("quick"));
        assert_eq!(r(&mut cursor, word), Ok("brown"));
        assert_eq!(r(&mut cursor, word), Ok("fox"));
        assert!(r(&mut cursor, word).is_err(), "Must stop at EOL");
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

    #[test]
    fn test_indented_body() {
        let empty = IndentString::default();
        let space_1 = IndentString::Spaces(vec![1]);
        let space_2 = IndentString::Spaces(vec![1, 1]);
        assert_eq!(indented_body(&empty, ""), Ok(("".into(), "")));
        assert_eq!(indented_body(&empty, "a"), Ok(("a".into(), "")));
        assert_eq!(indented_body(&empty, "a\nb"), Ok(("a\nb".into(), "")));
        assert_eq!(indented_body(&empty, "  a\nb"), Ok(("  a\nb".into(), "")));
        assert_eq!(indented_body(&empty, "a\nb\n"), Ok(("a\nb".into(), "")));

        assert_eq!(indented_body(&space_1, "  a\n b"), Ok(("a".into(), " b")));
        assert_eq!(
            indented_body(&space_1, "  a\n  b\n c"),
            Ok(("a\nb".into(), " c"))
        );
        assert_eq!(
            indented_body(&space_1, "  a\n   \t \n  b\n c"),
            Ok(("a\n\nb".into(), " c"))
        );
        assert_eq!(indented_body(&space_1, "  a\n \tb\n c"), Err("\tb\n c"));
        assert_eq!(indented_body(&space_1, " a"), Err(" a"));

        assert_eq!(indented_body(&space_2, "    a\nb"), Ok(("a".into(), "b")));

        // Indented block's indent level not revealed at the first line
        assert_eq!(
            indented_body(
                &space_1,
                "   ##
  ####
  ####
   ##"
            ),
            Ok((
                " ##
####
####
 ##"
                .into(),
                ""
            ))
        );

        // On second thought, let's not bother allowing this. You gotta stick
        // with one indentation, even inside the block, even though it's
        // possible to write the parser to allow this.
        /*
                // Allow inconsistent indentation within body.
                assert_eq!(
                    indented_body(
                        &space_1,
                        "  \t##
          ####
          ####
           ##"
                    ),
                    Ok((
                        "\t##
        ####
        ####
         ##"
                        .into(),
                        ""
                    ))
                );
                */

        // Mix things up with a blank line.
        assert_eq!(
            indented_body(
                &space_1,
                "   ##

  ####
  ####
   ##"
            ),
            Ok((
                " ##

####
####
 ##"
                .into(),
                ""
            ))
        );
    }

    #[test]
    fn test_headline() {
        let empty = IndentString::default();

        assert_eq!(headline(&empty)(""), Err(""));
        assert_eq!(headline(&empty)("a"), Err("a"));
        assert_eq!(headline(&empty)("\n"), Err("\n"));
        assert_eq!(headline(&empty)("a\n"), Err("a\n"));

        assert_eq!(
            headline(&empty)(
                "\
a
b"
            ),
            Err("a\nb")
        );

        assert_eq!(
            headline(&empty)(
                "\
a

b"
            ),
            Err("a\n\nb")
        );

        assert_eq!(
            headline(&empty)(
                "\
a
  b"
            ),
            Ok((("a", IndentString::new(1)), "  b"))
        );

        assert_eq!(
            headline(&empty)(
                "\
a

  b"
            ),
            Ok((("a", IndentString::new(1)), "\n  b"))
        );
    }
}
