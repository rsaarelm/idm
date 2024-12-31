# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed
- Tail lined data with inline structs is parsed correctly.

## [0.4.2] - 2023-11-07

### Added
- Inline structs, tuple enums and struct enums support similar tail value
  inlining as do plain tuples.

### Fixed
- Standalone section-shaped pairs are parsed correctly.

## [0.4.1] - 2023-01-12

### Fixed
- Bugfix for determining whether an enum variant is unit or nonunit.
- Do not try to deserialize an inline struct if the number of items does not
  match the number of struct fields.

## [0.4.0] - 2023-01-08

### Added
- Publicly visible `Serializer` and `Deserializer` types.

### Changed
- The IDM special structures are now marked with a singleton tuple as the
  first element of a tuple or tuple struct pair. Regular non-struct tuples no
  longer have intrinsic special treatment and work like sequences.
- Tuples can match a multi-word value as their last value when line-shaped and
  the body of a section as their last value when section-shaped.
  Section-shaped pairs will match the whole headline as the first value and
  the body as the second value. Parsing map items is now equivalent to parsing
  a sequence of pair tuples.
- The indentations of multi-line string values are munged to remain consistent
  with current indentation style when serializing if necessary. String values
  that have leading whitespace or are not valid IDM outline fragments will
  fail to serialize.
- NBSP and other Unicode whitespace is removed when parsing primitive values
  (integers, floats). This allows table rows with a right-aligned leftmost
  column that are left-padded with NBSP to be valid IDM.
- The check for IDM whitespace is now a standalone function instead of a
  `char` extension trait method.
- Minor improvement in error value API ergonomics

### Removed
- `ColonPair` type removed from the utilities module and preserved as an
  example. It's too marginal and opinionated to have in the core utilities.
- Made some internal macros private that were never intended to be public to
  begin with.

## [0.3.2] - 2022-11-03
New release to fix cargo crate that had junk included.

## [0.3.1] - 2022-09-16

### Added
- Enumeration type support

## [0.3.0] - 2022-04-18

### Added
- Horizontal struct support
- Utility wrapper types that do two-way value transformations during
  serialization, for example for allowing map-like inline values.

### Changed
- Horizontal sequences can now show up at the parsing top level and are
  distinguished from vertical sequences by the input having no newlines.
- Unified structs and maps, syntactic sugar replaces separate struct syntax.
- Due to struct and map unification, Serde's struct flattening can now be used
  (though it remains of limited use as all values are read as strings when
  using it).
- Tuples are no longer allowed as a regular sequence-like serialization
  target. Only pair tuples are supported, and these are used to describe
  special structural elements in IDM.
- Removed `_contents` magic field from struct parsing, fields + contents
  entries are now represented as pairs and using the new colon prefix syntax
  to mark the struct half.
- Raw type is removed, raw parsing is now indicated by parsing a string as the
  first element of a pair tuple.
- Deserialization parser has been completely rewritten. New version is a
  stacked state machine that decomposes into easier-to-maintain individual
  state transitions instead of the previous monolithic parser.

## [0.2.0] - 2021-09-09

### Added
- Comment syntax
- Error reporting with line numbers
- Spaces are also supported for indentation

### Changed
- Missing headlines (double indentation) are no longer supported
- Raw mode is now indicated by byte buffer type instead of `Option`
- Comma separator syntax is removed, comments are now used as structural
  separators

### Fixed
- Section-like pair tuples are parsed correctly

## [0.1.0] - 2021-04-10
Initial release
