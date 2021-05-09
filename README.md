# Implicit Data Markup

IDM is a non-self-describing data serialization format intended to be both
written and read by humans.

It uses an indentation-based outline syntax with semantically significant
physical tab indents. (Yes, they must be physical tabs. No, this can't change
without altering the format.) It requires an external type schema to direct
parsing the input, the same input can be parsed in different ways if a
different type is expected. In the Rust version, the type schema is provided
by the [Serde data model](https://serde.rs/data-model.html).

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

Indentation uses physical tab characters only, one tab per indent level. There
are three general shapes for a complex value, a *line*:

```
a b c
```

a *block* of multiple lines at the same indent depth:

```
a b
c
d e f
```

and a *section* with a headline and an indented block body:

```
a b
	c
	d e f
```

Whitespace is the only separator, there is no string quoting or character
escape syntax. Inline sequences are expected to consist entirely of
whitespace-less tokens. For values that require whitespace, indentation works
as a general-purpose escape mechanism. The value ends at the line that is
indented less than the one at the start of the value.

Struct fields are in kebab-case (the Serde IDM implementation automatically
converts between Rust's default camel\_case and kebab-case) and followed by a
colon. The valid fields of a struct block must be in the initial lines, lines
from the first field that can't be parsed as a struct field onwards are
treated as additional contents.

The additional contents map to the special `_contents` struct field. If a
struct has this field specified, the contents part of the struct block are
serialized into the `_contents` field. This allows nested structures with
minimum additional levels of indentation.

An IDM outline like

```
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
use serde_derive::{Deserialize, Serialize};

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
Outline(Vec<(Option<String>, Outline)>)`. This denotes a block of sections,
where the headlines may be missing. Values of this type will serialize into
the corresponding IDM documents with the default Serde derivation.

Missing headlines are different from headlines containing an empty string. They
can be entirely absent from the document, while empty strings must be present
as empty lines. A missing headline that would have been indented deeper than
the previous line is just omitted, and it's subsequent child line is indented
more than one level past the previous line. A missing headline when the
previous line is at the same or deeper indent depth cannot be parsed when
missing, so it is represented as a line with a single comma (',').

```
A
		Child of first missing sub-headline
		Indented two levels past 'A'
	,
		Child of second missing sub-headline
		Second missing headline must be marked with a ','
```

To have a literal single comma, write `,,`. And then `,,,` for a literal `,,`
and so on.

During data structure deserialization, when parsing mode has switched to
reading a multi-line string literal, lines with a single comma within this
string literal are read verbatim instead of escaped.

Some unprincipled exceptions are needed to make the canonical outline type
serialization work.

* Tuples (but not sequences like `Vec`) can be in section form with the first
  element being the headline, as well as line or block forms. If the first
  element is an `Option` type, the tuple *must* be in section form. This
  ensures that the `(Option<String>, Outline)` sections of the outline type
  will serialize as sections.
* If the last element of a tuple is a sequence type, it's contents will be
  merged into the tuple's sequence of elements instead of it being serialized
  as a separate value. This makes the second part of the `(Outline<String>,
  Outline)`, which expands to a `Vec` type, show as the body of the serialized
  text outline without an extra level of indentation.

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

|                   | Inline item (word)     | Full line                      | Block                         | Section                                      |
|-------------------|------------------------|--------------------------------|-------------------------------|----------------------------------------------|
| primitive         | read as string, parse  | read as string, parse          | read as string, parse         | n/a                                          |
| string            | read to whitespace/EOL | read to EOL                    | read to end of indented block | n/a                                          |
| tuple             | n/a                    | words are items                | block sections are items      | headline is 1st item, body is rest           |
| seq               | n/a                    | words are items                | block sections are items      | n/a                                          |
| struct / map      | n/a                    | n/a                            | block sections are items      | n/a                                          |
| struct / map item | n/a                    | key is 1st word, value is rest | n/a                           | headline is key, body is value               |
| option            | n/a                    | n/a                            | n/a                           | missing headline is None, present is Some \* |

\*: `None` option values are only expected to show up in the first position
of a tuple for the canonical outline type, not standalone. Most of IDM doesn't
have good facilities for expressing empty values.

## License

IDM is dual-licensed under Apache-2.0 and MIT.
