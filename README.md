# Implicit Data Markup

IDM is a non-self-describing, indentation based data serialization format that
is intended to be succinct and comfortable for editing by hand. The parser
expects to know the structure of the type it is deserializing into and may
parse the same input differently based on it. By not having the format be
self-describing, the syntax can be very lightweight.

IDM is somewhat fragile and experimental. The purpose is to provide a very
concise and minimalist syntax for typing structured data by hand. You are
expected to have control of both the data and the datatypes it serializes into
to prevent corner cases that cannot be handled. If you need a more verbose and
robust data language, use JSON, YAML or [RON](https://github.com/ron-rs/ron).

The use case for IDM is a sort of freeform handwritten personal database
embedded in a plaintext indented outline notes file. The entire file
deserializes into the canonical IDM outline type, and parts of it can be
deserialized into various structured data types.

## Type-driven serialization

The same IDM data can be parsed in several ways depending on the type it's being parsed
into. Depending on the type,

```
1 2 3
4 5 6
7 8 9
```

might be a 3x3 matrix, a list of 3 strings, a single multi-line string or an
indented outline document (that happens to not have any indented lines).

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

Struct fields are in kebab-case (the serde IDM implementation automatically
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

## The canonical outline type

Any IDM file can be described by the general outline type, `struct
Outline(Vec<(Option<String>, Outline)>)`. This denotes a block of sections,
where the headlines may be missing. Values of this type will serialize into
the corresponding IDM documents with the default serde derivation.

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

## Parse matrix

|                   | Inline item (word)     | Full line                      | Block                         | Section                                   |
|-------------------|------------------------|--------------------------------|-------------------------------|-------------------------------------------|
| primitive         | read as string, parse  | read as string, parse          | read as string, parse         | n/a                                       |
| string            | read to whitespace/EOL | read to EOL                    | read to end of indented block | n/a                                       |
| tuple             | n/a                    | words are items                | block sections are items      | headline is 1st item, body is rest        |
| seq               | n/a                    | words are items                | block sections are items      | n/a                                       |
| struct / map      | n/a                    | n/a                            | block sections are items      | n/a                                       |
| struct / map item | n/a                    | key is 1st word, value is rest | n/a                           | headline is key, body is value            |
| option            | n/a                    | n/a                            | n/a                           | missing headline is None, present is Some |

## License

IDM is dual-licensed under Apache-2.0 and MIT.
