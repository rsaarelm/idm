# Implicit Data Markup

IDM is a non-self-describing data serialization format intended to be both
written and read by humans.

It uses an indentation-based outline syntax with semantically significant
indentation. It requires an external type schema to direct parsing the input,
the same input can be parsed in different ways if a different type is
expected. In the Rust version, the type schema is provided by the [Serde data
model](https://serde.rs/data-model.html). Because the format is not
self-describing, IDM files can get by with less syntax than almost any other
human-writable data serialization language.

The motivation is to provide a notation that is close to freeform handwritten
plaintext notes. Because the format is not self-describing, the syntax can be
very lightweight. IDM can be thought of as a long-form user interface to a
program rather than just a data exchange protocol for computers.

It's basically a fixie bike serialization format. Simple, arguably fun and
having it as your daily driver might cause a horrific crash sooner or later.
It is expected that the user controls both the data and the types when using
it, and can work around corner cases which it can't handle.

If you need robust serialization of any Rust data structure or just general
high reliability, you probably want a more verbose serialization language like
JSON, YAML or [RON](https://github.com/ron-rs/ron).

## Usage

IDM implements [Serde](https://serde.rs/) serialization.

Use `idm::from_str` to deserialize Serde-deserializable data. Use
`idm::to_string` to serialize Serde-serializable data.

## IDM is a non-self-describing data format

Depending on the expected type, the same input can be parsed in several ways.
Take something like the following:

```notrust
    1 2 3
    4 5 6
    7 8 9
```

When expecting a single `String`, the whole thing gets read verbatim as a
three-line paragraph. When expecting a list `Vec<String>` sequence, it's three
lines, `["1 2 3", "4 5 6", "7 8 9"]`. When expecting a matrix `Vec<Vec<i32>>`,
it's three lists of three numbers, `[[1, 2, 3], [4, 5, 6], [7, 8, 9]]`.

A sequence can be read either vertically as an outline of items or
horizontally as a sequence of whitespace-separated words. For the horizontal
sequence, whitespace is the only recognized separator. If the representations
of the elements of a sequence contain spaces, a vertical sequence can always
be used.

## Basic syntax

An IDM *outline* document consists of one or more lines of text terminated by
newline. It must contain at least one newline, or it will be considered an
inline *fragment* document instead. All indentation in a single IDM document
must use only ASCII spaces or only ASCII tabs, these can not be mixed in any
way.

IDM treats only ASCII tabs and spaces (U+0009 and U+0020) as whitespace. In
the subsequent text, 'whitespace' will refer only to these characters and to
newlines. For example NBSP (U+00A0) characters are treated as non-whitespace.

Outlines are defined recursively to consist of *items* that consist of a line
indented to the outline's indent depth, with an optional body outline with a
deeper indentation under the line. If the item consists of only the line, it
is called a *line*, if it has a nonempty body outline, it is called a
*section*, it's top line is called a *headline* and the child outline is
called a *body*.

The indentation of body items in one document must use either only ASCII
spaces or only ASCII tabs, these can not be mixed in any way.
All items of a section body must be indented to the same depth. The following
outline has invalid syntax:

```notrust
Headline
    Item 1 at indent depth 4
  Item 2 at indent depth 2, inconsistent dedentation!
```

Blank lines are interpreted to have the indent depth of the first non-blank
line after them, or 0 if no non-blank line exists after them. This means that a
blank line can never be a section headline, since the headline must have a
shallower indent depth than the line immediately following it.

The presence of a trailing newline is syntactically significant for
single-line documents. A single line with a trailing newline is read as a
single-item outline, while a single line without a trailing newline is read as
a fragment that may be interpreted as a horizontal sequence.

Some parts of an IDM document may correspond to multi-line string values from
serialized user data, but they must still follow IDM's indentation conventions
along with the rest of the document. String values with leading whitespace,
inconsistent dedentation or indentation that mixes tabs and spaces cannot be
serialized. If the serialization is using a different indentation style than
the multi-line string (tabs instead of spaces or vice versa), the string is
rewritten to use the different indentation style. Precise indentation depths
from the original indentation may be lost when this happens, though the
logical structure of the indentation should always be preserved.

## Special syntax

Aside from significant whitespace, IDM has only two built-in syntax elements,
comments and colon blocks.

Comments are always the only element on a line, and they must either be only
`"--"` or start with `"-- "`, followed by arbitrary text after the space.
Besides the usual role of leaving notes in documents, comments also serve as
syntax separators. A sequence of vertical sequences must be separated with
dedented comment lines between the sequences:

```notrust
  1 2
  3 4
--
  4 5
  6 7
```

Colon lines start with a colon immediately followed by a non-whitespace
character. A colon block is a contiguous run of colon lines, with only
comments and blank lines allowed in between:

```notrust
:a 1
:b 2
Not part of the attribute block
```

A colon block is syntactic sugar for an extra level of indentation and a
comment line that separates the headline-less outline. The fragment above is
equivalent to

```notrust
--
  a 1
  b 2
Not part of the attribute block
```

## Special forms

Because you hopefully aren't using them for anything else, IDM repurposes
singleton tuples (`(A,),`) as a marker for special forms in IDM documents.
Special forms are needed so that IDM can read entire files, comments, blanks
and all, into standard outline structures that preserve all the file contents.

A `String` singleton in the head position of a pair (`((String,), _)`) matches
a line in *raw mode*. The headline of the current item, even if it's a comment
or a blank line, is read into the pair's head string. The body of the section
is parsed normally into the tail of the pair.

```notrust
-- This gets read into the String at pair head (even with comment syntax)
  These lines get
  Read into the body
  Of the pair type
```

A pair with a map-like type (struct or map) in the head singleton will read an
outline (not an item as the string-headed pair), where it will expect to find
the initial map-like value in an indented block, and will then read all the
remaining outline items into the tail of the pair as a single block. So a type
like `((BTreeMap<String, String>,), Vec<String>)` would expect something like

```notrust
--
  key1 value1
  key2 value2
First element in pair tail
Second element in pair tail...
```

However, this is exactly the thing colon blocks are made for. Instead of
writing the indented block explicitly, the idiomatic way to write the value is

```notrust
:key1 value1
:key2 value2
First element in pair tail
Second element in pair tail...
```

All map-like values at pair head position must be in vertical form. If the
type is a struct, it cannot be written in the horizontal inline struct form
here.

## Tuples and sequences

The two types of simple collections supported by IDM are sequences of
homogeneous item type and unknown length and tuples of known length and
heterogeneous item type. Sequences match standard forms of a vertical sequence
of lines or blocks and a horizontal sequence of words. Tuples match the same
patterns, but also some other ones. Since singleton tuples are reserved for
IDM special forms, actual tuples used must have length of at least 2.

Since the length of a tuple is known, special rules can be applied to its
final item. While all elements of a single-line horizontal sequence must be
single words, the final element of a tuple is the entire rest of the line, and
can contain whitespace. The line `key A multi-line value` can be matched into
`(String, String)` with values `("key", "A multi-line value")`. The last value
of a tuple can also be an item body, so the pair tuple could also match

```notrust
head
  Multiple lines
  of body
```

## Structs and maps

Structs and maps (map-like types) are treated very similarly. Their actual
syntax is an outline of unadorned keys followed by attributes, but they are
often placed in colon blocks which gives the illusion of a syntax where the
colon prefix is how you write map keys. To keep up the charade, any standalone
map-like value can be written as a colon block, and the parser will detect the
additional block nesting and pop out the inner value.

Parsing items of a map or a vertical struct is equivalent to parsing a
sequence of tuples of the key and value types of a map or of strings for the
struct fields and corresponding field value types for a struct.

Unlike most other IDM types, maps only have a vertical form. This is
important, since it makes it possible to parse an absence of a map in the
special form where the map is parsed at the head of a pair.

Structs do have a horizontal form. In this form, the struct value consists of
only the values of struct fields, the field names are not included. The values
are listed in the exact order they show up in struct declaration. This form is
usually used when writing tabular data. The user needs to be cautious with
this form since it does not include field names, so any change in the number
or order of the fields in a struct type will make inline values written
against a previous version of the type invalid.

IDM has very limited capabilities for representing missing values, so the
convention for `Option` values is to omit the entire item (both key and value)
from a map or a struct if the value is `None`. Structs written in horizontal
form cannot have missing values. Completely empty structs or maps can be
matched in the special pair head singleton position but not elsewhere.

## Complex structure example

Complex structures can be written using maps from object names to objects,
which transform into outlines of sections, and using pairs of structs with
such maps, which become colon blocks with followed by child elements. The
following example shows how a database of stars and their orbiting planets is
represented. The planet data is represented compactly using inline structs.

The IDM data:

```notrust
Sol
  :age 4.6e9
  :mass 1.0
  --    :orbit :mass
  Earth  1.0   1.0
  Mars   1.52  0.1
Alpha Centauri
  :age 5.3e9
  :mass 1.1
  --    :orbit :mass
  Chiron 1.32  1.33
```

The type signature and parse test:

```rust
use serde::Deserialize;
use std::collections::BTreeMap;

type StarSystem = ((Star,), BTreeMap<String, Planet>);
type Starmap = BTreeMap<String, StarSystem>;

#[derive(PartialEq, Debug, Deserialize)]
struct Star {
    age: f32,
    mass: f32,
}

#[derive(PartialEq, Debug, Deserialize)]
struct Planet {
    orbit: f32,
    mass: f32,
}

assert_eq!(
    idm::from_str::<Starmap>("\
Sol
  :age 4.6e9
  :mass 1.0
  --    :orbit :mass
  Earth  1.0   1.0
  Mars   1.52  0.1
Alpha Centauri
  :age 5.3e9
  :mass 1.1
  --    :orbit :mass
  Chiron 1.32  1.33

").unwrap(),
  BTreeMap::from([
    ("Sol".into(),
        ((Star { age: 4.6e9, mass: 1.0 },),
         BTreeMap::from([
           ("Earth".into(), Planet { orbit: 1.0, mass: 1.0 }),
           ("Mars".into(),  Planet { orbit: 1.52, mass: 0.1 })]))),
    ("Alpha Centauri".into(),
        ((Star { age: 5.3e9, mass: 1.1 },),
         BTreeMap::from([
           ("Chiron".into(),  Planet { orbit: 1.32, mass: 1.33 })])))]));
```

For an example on how to add syntax to IDM using user-defined types, see the
[inline maps](./examples/inline-maps.rs) example.

## Outline forms

The uses for IDM seen so far expect you to have an explicit,
application-specific type to serialize to. However, IDM can also serialize
generic outline types, which can parse most plaintext files.

A simple outline has a type signature like

```rust
struct Outline(Vec<((String,), Outline)>);
```

This matches the form of a generic IDM outline, with the outline items being
the `((String,), Outline)` pairs. The pair tuple with the string head makes
IDM parse each item in raw mode, so that comment and blank lines are included
in the data structure and will get echoed back when the structure gets
reserialized.

A richer structure is a data outline, which supports an arbitrary data map for
each item:

```rust
use indexmap::IndexMap;

struct DataOutline((IndexMap<String, String>,), Vec<((String,), DataOutline)>);
```

You can now read the named attributes from any item in the outline. The values
will all be strings, but an application which knows the expected type for a
specific attribute can deserialize the attribute value again using IDM into
the more appropriate type.

```rust
use indexmap::IndexMap;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct DataOutline(
    (IndexMap<String, String>,),
    Vec<((String,), DataOutline)>,
);

let outline = idm::from_str::<DataOutline>(
    "\
Example outline
  Stuff
    :tags foo bar
    This part has stuff
  Things",
)
.unwrap();

// Raw access patterns are pretty rough.
// A proper app would need some sort of selection API here,
// the explicit indexing gets very rough very fast.
assert_eq!(outline.1[0].1 .1[0].0 .0, "Stuff"); // On the right track...
let tags = &outline.1[0].1 .1[0].1 .0 .0["tags"]; // Grab tags field.
assert_eq!(tags, "foo bar");

// Cast to a more appropriate format.
let tags: Vec<String> = idm::from_str(tags).unwrap();
assert_eq!(tags, vec!["foo", "bar"]);
```

A caveat for the reserialization of data outlines is that the attribute block
is not read in raw mode. Comment lines among the attributes will therefore not
be preserved when the outline is deserialized and serialized from memory, and
therefore should be avoided when writing outlines that are meant to be
rewritten through IDM.

The outline types are intentionally not included in the IDM crate. There's
literally nothing going on with them other than the type signature, and you're
expected to copy that in your own application. You will also probably want to
implement your own application-specific methods for the type, which will be
easier if you own the type.

For another example of an outline structure with data mixed in, see the
minimal [blog engine](./examples/blog.rs) and corresponding [content
file](./examples/blog.idm) in examples.

## Accepted shapes

| Type                | Vertical value           | Horizontal value |
|---------------------|--------------------------|------------------|
| atom                | block, section           | line, word       |
| special string head | section, line-as-section | -                |
| special map head    | block                    | -                |
| map                 | block                    | -                |
| tuple / map element | block, section           | line             |
| struct              | block                    | line             |
| seq                 | block                    | line             |

Some types support both vertical (each item on their own line) and horizontal
(every item on one line) values, the rest support only vertical values.

Pairs with a singleton string tuple in the first position trigger raw mode.
Raw mode has the unique line-as-section parsing mode where it interprets a
line as a section with an empty body. Normally a line is interpreted as the
horizontal variant of a structured type.

## Notes

* Serde's `#[serde(flatten)]` attribute does not work well with IDM if used
  with structs. It switches from struct-like parsing to map-like parsing, and
  stops providing types for values. The result is that all values to the
  flattened struct are provided as strings and won't deserialize correctly. It
  can still be useful for maps that collect string values or structs that have
  string values (or deserialize via strings) for all their fields.

* A special pair where the first half is a colon-indented struct or map can't
  have the second half be another special pair with a map head. Due to how the
  second half is fused in the first, the second map cannot be syntactically
  distinguished from the first.

* Primitive types, chars, numeric types and booleans, are trimmed of Unicode
  whitespace like NBSP before being parsed. The main IDM algorithm treats NBSP
  as content instead of indentation. This allows you to do left-padded table
  rows without breaking IDM parsing by padding with NBSP, and still having
  primitive elements parse correctly from the leftmost table column. User
  types with custom parsing from a string value may need to trim the input
  string on their own.

## License

IDM is dual-licensed under Apache-2.0 and MIT.
