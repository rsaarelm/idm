//! Stateless parsing primitives
//!
//! This is the bottom level of the parsing system. Things should be pushed
//! here whenever they conveniently can, because stateless functions are
//! easier to reason about than a stateful cursor.

// NB. Parsing multi-line items that care about indentation levels is tricky.
// Inconsistent indentation (eg. indentation by tabs following indentation by
// spaces) is always an unrecoverable error, but the parse
// API idiom is using Err to signal general failure to parse a primitive,
// which might also be a recoverable situation. It's therefore possible that
// parts of the parsing process appear to skip over a line of inconsistent
// indentation without immediately propagating an error, since the error is
// treated as just failing to parse that particular parse element (ie. an
// incorrectly indented comment line is treated as "this line is not
// syntactically a comment, try something else"). Every part of the file must
// be ultimately parsed by some primitive though, so as long as there are no
// buggy primitives that accept a line with inconsistent indentation, the
// complete parse should still reliably fail at the point of bad indentation.
//
// XXX: The current approach does have the problem that the cause of error is
// not propagated up from the parse routine. The higher level can tell there
// was a parsing error with a specific primitive, but not that it was caused
// by inconsistent indentation specifically.

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

/// Return an item of an outline sequence at the given depth.
///
/// Items can be sections or body lines (childless headlines) at current
/// indent or blocks below current indent. Body line output will have indent
/// removed to the beginning of the line, sections will have global indent
/// removed to the beginning of the headline and blocks will have indent
/// removed until the leftmost line of the block has no indent.
///
/// It is assumed that before `outline_item` was called, comments and blank
/// lines at the expected depth have been skipped over. The block element is
/// assumed to start immediately below the current indent, and blank lines at
/// the start of a block are expected to belong to the content instead of
/// being between-element space.
pub fn outline_item<'a: 'b, 'b>(
    current_indent: &'b IndentString,
) -> impl Fn(&'a str) -> Result<'a, String> + 'b {
    move |input| {
        let (local_indent, _) = current_indent.fill(input)?;
        if local_indent.len() < current_indent.len() {
            // No items
            Err(input)
        } else if local_indent.len() > current_indent.len() {
            // A block
            indented_body(current_indent, input)
        } else {
            section(current_indent, input)
        }
    }
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

/// Succeed if input is at the end of usable content.
///
/// Trailing white space is ignored.
pub fn eof(input: &str) -> Result<()> {
    if input.chars().all(|c| c.is_whitespace()) {
        Ok(((), ""))
    } else {
        Err(input)
    }
}

/// Return the indentation prefix for a line.
///
/// Will fail at `eof`. If run on blank line, will skip ahead until it finds a
/// contentful line. Will fail if the indentation string mixes tabs and
/// spaces.
///
/// The remaining input returned will always be on the line where `indent` was
/// run. If the initial line has content, remaining input will be at start of
/// content. If the line is blank, no input will be consumed.
pub fn indent(input: &str) -> Result<&str> {
    // Indentation is undefined if there are no contentful lines ahead of the
    // input position.
    if eof(input).is_ok() {
        return Err(input);
    }

    let mut pos = input;

    // Consume blank lines.
    loop {
        if r(&mut pos, blank_line).is_err() {
            break;
        }
    }

    // Once the indentation starts, remember which char was used.
    // Zero byte is used before any indentation has been seen.
    let mut indent_char = '\0';

    let mut len = 0;
    for (i, c) in pos.char_indices() {
        debug_assert!(c != '\n'); // Already skipped blank lines.
        len = i;
        if c.is_whitespace() {
            if indent_char != '\0' && c != indent_char {
                // Multiple types of indent char seen.
                return Err(input);
            }
            if c != ' ' && c != '\t' {
                // Unsupported input char.
                return Err(input);
            }
            indent_char = c;
        } else {
            break;
        }
    }

    if pos != input {
        // First line was blank, consume nothing.
        Ok((&pos[..len], input))
    } else {
        // Indent was extracted from first line, consume indent.
        Ok((&pos[..len], &input[len..]))
    }
}

/// Return the indentation string for an atomic multiline value.
///
/// An atomic value does not need to conform to IDM's indentation discipline
/// below its initial indentation. It's initial line can be indented deeper
/// than the remaining lines, and it can even feature indentation with
/// characters that are inconsistent with the ones used by the surrounding IDM
/// (eg. space-indented ASCII art embedded in a tab-indented IDM file).
///
/// All accepted lines must share an IDM-compliant indentation prefix past
/// `base_indent`. This prefix must reach to the start of content for the
/// leftmost indented contentful line in the atomic value. Values will always
/// be parsed in a form where at least one of their lines is unindented, there
/// is no way to express an initial indentation shared by all lines of the
/// value.
fn block_indent<'a, 'b>(
    base_indent: &'b IndentString,
    input: &'a str,
) -> Result<'a, &'a str> {
    // No-go around EOF.
    if eof(input).is_ok() {
        return Err(input);
    }

    let mut pos = input;

    let prefix_len =
        Into::<Option<String>>::into(base_indent).map_or(0, |s| s.len());

    // Character used for IDM indentation, must be consistent.
    //
    // Does not necessarily get specified by base_indent, so it may need to be
    // determined later.
    let mut input_char = base_indent.input_char();

    // Smallest indentation to content seen.
    let mut min_indent: Option<&str> = None;

    loop {
        // Exit when out of input.
        if eof(pos).is_ok() {
            break;
        }

        // Skip blank lines while there is still input.
        if r(&mut pos, blank_line).is_ok() {
            continue;
        }

        // Exit when out of the depth for the block.
        if base_indent.parse_exact(pos).is_ok() {
            break;
        }

        let current_indent = if let Ok((run, _)) = indent_run(pos) {
            let run_char = run.chars().next().unwrap();

            // We're not within the block indent region anymore, exit.
            if run.len() <= prefix_len {
                break;
            }

            // If indentation is inconsistent, bail out.
            if let Some(c) = input_char {
                if c != run_char {
                    return Err(pos);
                }
            }

            // Remember input char the first time we see it.
            if input_char.is_none() {
                input_char = Some(run_char);
            }

            run
        } else {
            ""
        };

        // Keep track of shortest indent prefix seen.
        if let Some(indent) = min_indent {
            if current_indent.len() < indent.len() {
                min_indent = Some(current_indent);
            }
        } else {
            min_indent = Some(current_indent);
        }

        let _ = r(&mut pos, line)?;
    }

    if let Some(indent) = min_indent {
        Ok((indent, &input[indent.len()..]))
    } else {
        Err(input)
    }
}

/// Return run of repeated indent char.
///
/// A successful parse is guaranteed to return a string of one or more characters
/// that consists of either only spaces or only tabs, followed by a character
/// that is different from the repeated character in the result string.
fn indent_run(input: &str) -> Result<&str> {
    let ch = match input.chars().next() {
        Some(' ') => ' ',
        Some('\t') => '\t',
        _ => {
            return Err(input);
        }
    };

    let len = input
        .char_indices()
        .take_while(|(_, c)| *c == ch)
        .last()
        .unwrap()
        .0;
    Ok((&input[..len], &input[len..]))
}

/// Read line until newline, rest of the content is after newline.
///
/// Fails if there is no input.
pub fn line(input: &str) -> Result<&str> {
    if eof(input).is_ok() {
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

fn indented_comment<'a, 'b>(
    current_indent: &'b IndentString,
    input: &'a str,
) -> Result<'a, &'a str> {
    let mut pos = input;
    r(&mut pos, |input| current_indent.parse(input))?;
    comment(pos)
}

/// Match one or more comments at exactly given depth.
fn indented_comments<'a, 'b>(
    current_indent: &'b IndentString,
    input: &'a str,
) -> Result<'a, ()> {
    let mut pos = input;
    // Must match once.
    r(&mut pos, |input| indented_comment(current_indent, input))?;
    // Can match any number of times after the first one.
    while let Ok(_) =
        r(&mut pos, |input| indented_comment(current_indent, input))
    {}
    Ok(((), pos))
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

/// Match one or more blank lines.
pub fn blank_lines(input: &str) -> Result<()> {
    let mut pos = input;
    r(&mut pos, blank_line)?;
    while let Ok(_) = r(&mut pos, blank_line) {}
    Ok(((), pos))
}

/// Consume consecutive comment and blank lines at or below current indent.
///
/// End at the beginning of the first contentful line or the first line above
/// given indent depth, whichever comes first.
///
/// Blank lines that are followed by a contentful line below the expected
/// indent depth will not be parsed into `non_content`. Instead, they are
/// assumed to be the prefix of the indented text block.
pub fn non_content<'a: 'b, 'b>(
    current_indent: &'b IndentString,
) -> impl Fn(&'a str) -> Result<'a, ()> + 'b {
    move |input| {
        let mut pos = input;
        loop {
            // Eat one or more comment lines at exactly the expected indent
            if r(&mut pos, |input| indented_comments(current_indent, input))
                .is_ok()
            {
                continue;
            }
            // There will be either blanks or content at cursor at this point.
            if blank_line(pos).is_err() {
                // Looks like content right after comment, we can stop here.
                return Ok(((), pos));
            }

            // Remember pos before blanks, we may need to backtrack.
            let old_pos = pos;

            // Consume blanks.
            r(&mut pos, blank_lines)?;
            let (indent, _) = current_indent.fill(pos)?;

            if indent.len() < current_indent.len() {
                // Block has ended, exit
                return Ok(((), pos));
            } else if indent.len() > current_indent.len() {
                // The blanks preceded a block that was indented deeper,
                // exit but do not consume the last batch of blanks. This is
                // why we kept pos around.
                return Ok(((), old_pos));
            }
        }
    }
}

/// Read a headline at indent level and any child lines it has into one String
/// result.
pub fn section<'a, 'b>(
    prev: &'b IndentString,
    input: &'a str,
) -> Result<'a, String> {
    let mut pos = input;
    let mut ret = String::new();

    loop {
        match indented_line(prev, pos) {
            Ok((line, rest)) => {
                if !ret.is_empty() {
                    if line.chars().next().map_or(false, |c| !c.is_whitespace())
                    {
                        // We're past the first line and this looks like
                        // another headline, abort.
                        return Ok((ret, pos));
                    }

                    // Funny ordering for newlines so the final newline will be
                    // skipped.
                    ret.push('\n');
                }
                ret += line;
                pos = rest;
            }
            Err(_) => {
                // Ran out of indented lines, exit.
                return Ok((ret, pos));
            }
        }
    }
}

/// Return line at or above the given indent level.
pub fn indented_line<'a, 'b>(
    prev: &'b IndentString,
    input: &'a str,
) -> Result<'a, &'a str> {
    let mut pos = input;
    // Eat indent up to the expected level.
    r(&mut pos, |a| prev.parse(a))?;

    // Once the indent has been verified and skipped, use regular line parse.
    // Preserve indentation past the given level.
    line(pos)
}

/// Read body indented beyond previous indentation.
///
/// Input is assumed to be at the start of the line for the first line of the
/// indented body. All body lines must be indented deeper than the given
/// indentation. The function will look for the line with the shallowest
/// indentation deeper than the given indentation, and use that as the common
/// new indentation prefix. All body lines must have consistent indentation
/// with this prefix, but their indentation beyond the prefix is ignored. The
/// resulting string will have the prefix indentations stripped.
pub fn indented_body<'a, 'b>(
    prev: &'b IndentString,
    input: &'a str,
) -> Result<'a, String> {
    // Find the minimum indent.
    let (mut indent, _) = prev.fill(input)?;
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
        let candidate = r(&mut pos, |input| prev.fill(input))?;
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
            let line_indent = r(&mut next_pos, |input| indent.fill(input))?;

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
    fn test_indent() {
        // Normal cases.
        assert_eq!(indent("    xyzzy"), Ok(("    ", "xyzzy")));
        assert_eq!(indent("\txyzzy"), Ok(("\t", "xyzzy")));
        assert_eq!(indent("    xyzzy\nplugh"), Ok(("    ", "xyzzy\nplugh")));

        // Scan ahead when there are blanks.
        assert_eq!(indent("\n    xyzzy"), Ok(("    ", "\n    xyzzy")));
        assert_eq!(indent("   \n    xyzzy"), Ok(("    ", "   \n    xyzzy")));
        assert_eq!(indent("\n  \n    xyzzy"), Ok(("    ", "\n  \n    xyzzy")));
        assert_eq!(indent("\n\txyzzy"), Ok(("\t", "\n\txyzzy")));

        // No defined indent when there is only whitespace left.
        assert!(indent("").is_err());
        assert!(indent("    ").is_err());
        assert!(indent("\t").is_err());
        assert!(indent("    \n").is_err());
        assert!(indent("\t\n").is_err());
        assert!(indent("    \n   ").is_err());

        // Mixed indentation is not allowed.
        assert!(indent(" \txyzzy").is_err());
        assert!(indent("\t xyzzy").is_err());
    }

    #[test]
    fn test_indented_body() {
        let empty = IndentString::undetermined();
        let space_1 = IndentString::spaces(&[1]);
        let space_2 = IndentString::spaces(&[1, 1]);
        assert_eq!(indented_body(&empty, ""), Err(""));
        assert_eq!(indented_body(&empty, "a"), Err("a"));
        assert_eq!(indented_body(&empty, "a\n b"), Err("a\n b"));
        assert_eq!(indented_body(&empty, " a\n b"), Ok(("a\nb".into(), "")));
        assert_eq!(indented_body(&empty, "  a\n b"), Ok((" a\nb".into(), "")));

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
    fn test_section() {
        let empty = IndentString::undetermined();

        assert_eq!(
            section(&empty, "bar\n  baz"),
            Ok(("bar\n  baz".into(), ""))
        );
    }

    #[test]
    fn test_non_content() {
        let empty = IndentString::undetermined();

        assert_eq!(non_content(&empty)(""), Ok(((), "")));
        assert_eq!(non_content(&empty)("abc"), Ok(((), "abc")));
        assert_eq!(non_content(&empty)("\nabc"), Ok(((), "abc")));

        assert_eq!(
            non_content(&empty)(
                "\
--
abc"
            ),
            Ok(((), "abc"))
        );

        assert_eq!(
            non_content(&empty)(
                "\
-- Comment content
abc"
            ),
            Ok(((), "abc"))
        );

        // Must have space between -- and comment body
        assert_eq!(
            non_content(&empty)(
                "\
--notcomment
abc"
            ),
            Ok(((), "--notcomment\nabc"))
        );

        assert_eq!(
            non_content(&empty)(
                "\
-- Comment and space

abc"
            ),
            Ok(((), "abc"))
        );

        assert_eq!(
            non_content(&empty)(
                "\
-- Complex sequence
-- multiple lines


-- And more comments

-- And more space...

abc"
            ),
            Ok(((), "abc"))
        );

        assert_eq!(
            non_content(&empty)(
                "\
-- Space is different if content after it is indented

  abc"
            ),
            Ok(((), "\n  abc"))
        );

        assert_eq!(
            non_content(&empty)(
                "\
-- Stick to your depth, don't indent comments.
  --"
            ),
            Ok(((), "  --"))
        );

        // Using preset indentation level
        let space = IndentString::spaces(&[2]);

        assert_eq!(
            non_content(&space)(
                "\
-- abc"
            ),
            Ok(((), "-- abc"))
        );

        assert_eq!(
            non_content(&space)(
                "  -- abc
  xyz"
            ),
            Ok(((), "  xyz"))
        );

        // It can actually fail if indent is screwy.
        assert_eq!(
            non_content(&space)(
                "  -- abc
-- Out of depth"
            ),
            Ok(((), "-- Out of depth"))
        );

        // Case with inconsistent indentation.
        // NB: This just bails out with Ok instead of returning Err as you might
        // expect. The important part is that it does not consume the invalid
        // line, leaving it for the next parse step to tumble over.
        assert_eq!(
            non_content(&space)(
                "  -- abc
\t-- This is bad"
            ),
            Ok(((), "\t-- This is bad"))
        );
    }

    #[test]
    fn test_outline_item() {
        let empty = IndentString::undetermined();

        assert_eq!(outline_item(&empty)("abc"), Ok(("abc".into(), "")));
        assert_eq!(outline_item(&empty)("abc\ndef"), Ok(("abc".into(), "def")));
        assert_eq!(outline_item(&empty)("  abc"), Ok(("abc".into(), "")));
        assert_eq!(
            outline_item(&empty)("  abc\n  def"),
            Ok(("abc\ndef".into(), ""))
        );

        let space = IndentString::spaces(&[2]);
        assert_eq!(outline_item(&space)("abc"), Err("abc"));
        assert_eq!(outline_item(&space)("  abc"), Ok(("abc".into(), "")));
        assert_eq!(outline_item(&space)("    abc"), Ok(("abc".into(), "")));
        assert_eq!(
            outline_item(&space)("    abc\n    def"),
            Ok(("abc\ndef".into(), ""))
        );
        assert_eq!(
            outline_item(&space)("    abc\n  def"),
            Ok(("abc".into(), "  def"))
        );
        assert_eq!(
            outline_item(&space)("  abc\n  def"),
            Ok(("abc".into(), "  def"))
        );
    }
}
