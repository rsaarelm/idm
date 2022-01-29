# Implicit Data Markup

IDM is a non-self-describing data serialization format intended to be both
written and read by humans.

It uses an indentation-based outline syntax with semantically significant
indentation. It requires an external type schema to direct parsing the input,
the same input can be parsed in different ways if a different type is
expected. In the Rust version, the type schema is provided by the [Serde data
model](https://serde.rs/data-model.html).

The motivation is to provide a notation that is close to freeform handwritten
plaintext notes. Because the format is not self-describing, the syntax can be
very lightweight.

It's basically a fixie bike serialization format. Simple, arguably fun and
having it as your daily driver might cause a horrific crash sooner or later.
It is expected that the user controls both the data and the types when using
it, and can work around corner cases which it can't handle.

If you need robust serialization of any Rust data structure or just general
high reliability, you probably want a more verbose serialization language like
JSON, YAML or [RON](https://github.com/ron-rs/ron).

## Syntax

Indentation uses either spaces everywhere or physical tabs everywhere, never
mix the the two in the same file. The preferred indentation style is to use
two spaces per level of indetation.

There are three general shapes for a complex value, a
*line*:

```notrust
a b c
```

a *block* of multiple lines at the same indent depth:

```notrust
a b
c
d e f
```

and a *section* with a headline and an indented block body:

```notrust
a b
  c
  d e f
```

Dedents must match the corresponding indents. This is fine:

```notrust
a
    b
        c
    d
```

This is an error, the dedent for `d` does not match any preceding indent level:

```notrust
a
    b
        c
 d
```

There is no string quoting or character escape syntax. Inline sequences are
expected to consist entirely of whitespace-less tokens. For values that
require whitespace, indentation works as a general-purpose escape mechanism.
The value ends at the line that is indented less than the one at the start of
the value.

Since tuples have a predetermined element count, we can tell when we've
arrived at the last tuple element when parsing an inline value. This allows
tuples to have spaces in the last value.

```rust
assert_eq!(
    idm::from_str::<Vec<(String, String)>>("A B C").unwrap(),
    vec![("A".to_string(), "B C".to_string())]);
```

### Comments

Non-inline sequential IDM items, seqs, tuples and structs, can have blank
lines and comment lines that are ignored when parsing. Comment lines begin
with `--`.

```notrust
Data
  x: 1
  y: 2
  -- This is a comment

  z: 3
```

Comments also serve as syntax, they are needed to block out outline elements
in a nested list:

```notrust
Tables
  --
    1 2
    3 4
  --
    5 6
    7 8
```

Comments are not parsed in `Raw` values and outline literals.

```notrust
Text paragraphs
  -- This is a comment, dropped when parsing
    This is a paragraph of text, all is parsed verbatim
    until we drop out of indentation.
    -- So this line is part of the literal, not a comment
    And out.
  -- This is a comment again
    Next paragraph starts...
```

Parsed as `BTreeMap<String, Vec<String>>`, the middle `--` line would be
included, the rest will not. Parsed as a canonical outline, `struct
Outline(Vec<(idm::Raw<String>, Outline)>)`, all three `--` lines would be
included, since every line of content is read as a `Raw` value.

### Structs

Struct fields are in kebab-case (the Serde IDM implementation automatically
converts between Rust's default camel\_case and kebab-case) and followed by a
colon. The valid fields of a struct block must be in the initial lines, lines
from the first field that can't be parsed as a struct field onwards are
treated as additional contents.

The additional contents map to the special `_contents` struct field. If a
struct has this field specified, the contents part of the struct block are
serialized into the `_contents` field. This allows nested structures with
minimum additional levels of indentation.

Struct fields that start with an underscore cannot be used as regular data
with IDM, they are reserved for special values like `_contents` and any other
that might be added later. The reason to avoid them is that kebab-casing a
field name that starts with double underline, `__not_comment`, becomes
`--not-comment`, which ends up using the comment syntax.

An IDM outline like

```notrust
Sol
  age: 4.6e9
  mass: 1.0
  Earth
    orbit: 1.0
    mass: 1.0
  Mars
    orbit: 1.52
    mass: 0.1
Alpha Centauri
  age: 5.3e9
  mass: 1.1
  Chiron
    orbit: 1.32
    mass: 1.33
```

can be deserialized into a nested `Starmap` type:

```rust
use serde::{Serialize, Deserialize};
use std::collections::BTreeMap;

type Starmap = BTreeMap<String, Star>;

#[derive(Clone, PartialEq, Default, Debug, Serialize, Deserialize)]
struct Star {
    age: f32,
    mass: f32,
    _contents: BTreeMap<String, Planet>,
}

#[derive(Clone, PartialEq, Default, Debug, Serialize, Deserialize)]
struct Planet {
    orbit: f32,
    mass: f32,
}
```

This will produce a nesting of named maps and data.

Note the design pattern where the names of the elements are pushed out of the
value struct and into map keys. This will probably be a recurring feature in
datatypes optimized for IDM.

## Usage

IDM implements [Serde](https://serde.rs/) serialization.

Use `idm::from_str` to deserialize Serde-deserializable data. Use
`idm::to_string` to serialize Serde-serializable data.

### Example

```rust
use serde::{Deserialize, Serialize};

// A data serializable data type used as the type schema.
#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Example {
    description_line: String,
    tags: Vec<String>,
    matrix: Vec<Vec<i32>>,
}

// IDM data using this schema
const IDM_DATA: &str = "\
description-line: A single string
tags: one two three
matrix:
  1 2 3
  4 5 6
  7 8 9";

// Data deserializes into a Rust value.
// Note how schema differences make 'description-line' and 'tags' get parsed differently.
// Note how IDM 'description-line' becomes Rust 'description_line'.
assert_eq!(
    idm::from_str::<Example>(IDM_DATA).unwrap(),
    Example {
        description_line: "A single string".into(),
        tags: vec!["one".into(), "two".into(), "three".into()],
        matrix: vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]],
    }
);
```

## The canonical outline type

Any IDM file can be described by the general outline type, `struct
Outline(Vec<(idm::Raw<String>, Outline)>)`. This denotes a block of sections,
where the headlines is parsed verbatim in raw mode. Values of this type will
serialize into the corresponding IDM documents with the default Serde
derivation.

The raw mode (implemented internally using Serde's byte buffer `Bytes` type,
basically as a hack that exploits an otherwise unused data model bit) changes
how sequence parsing works. Blank lines are lines that look like comments are
now read in as content. This is desirable so that entire outline files can be
read in, manipulated via the data type, and written back out via
serialization.

IDM prefers to parse pair tuples in section mode, and will always use section
mode if the head of the pair is a raw mode type.

## Outline structure and value parsing

The structure alternates between an outline that's a sequence of sections, and
sections that are a headline and an outline body. The section is the structure
that naturally corresponds to a single value.

Given the option of having an empty headline, the three basic forms can all be
interpreted as sections. A line is a section with an empty body and a block is
the body of a section with a missing headline. Input files are a block-like
outline, so the parser may do hacky things like declaring the whole thing to
be a single section with a missing headline at line -1 and depth -1.

## Parse matrix

|                   | Inline item (word)     | Full line                      | Block                         | Section                                         |
|-------------------|------------------------|--------------------------------|-------------------------------|-------------------------------------------------|
| primitive         | read as string, parse  | read as string, parse          | read as string, parse         | n/a                                             |
| string            | read to whitespace/EOL | read to EOL                    | read to end of indented block | read 1st line and more indented subsequent ones |
| tuple             | n/a                    | words are items                | block sections are items      | headline is 1st item, body is rest              |
| seq               | n/a                    | words are items                | block sections are items      | n/a                                             |
| struct / map      | n/a                    | n/a                            | block sections are items      | n/a                                             |
| struct / map item | n/a                    | key is 1st word, value is rest | n/a                           | headline is key, body is value                  |

## Notes

* Serde's struct flattening `#[serde(flatten)]` attribute does not work well
  with IDM. It switches from struct-like parsing to map-like parsing, and
  those are syntactically different in IDM. A struct-like flatten operator is
  currently an [unresolved
  issue](https://github.com/serde-rs/serde/issues/1346) with Serde.

* When writing matrix-like tabular data, use `Vec` instead of tuples for the
  table rows, even when you know the table has a specific number of rows (eg.
  it's a matrix of known size). If there are exactly two rows and a tuple type
  is used for the rows, the section format printing for tuples will kick in
  when serializing the table and you will end up with weird-looking results.

* The `_contents` value for a struct can not be another struct with named
  fields, because these cannot be syntactically distinguished from the fields
  of the parent struct. You usually want it to be a map type.

* To set up the locally versioned githooks, do

```notrust
git config --local core.hooksPath githooks/
```

## License

IDM is dual-licensed under Apache-2.0 and MIT.
