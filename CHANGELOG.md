# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## [0.3.2] - 2022-11-03
New release to fix cargo crate that had junk included.

## [0.3.1] - 2022-09-16
### Additions
- Enumeration type support

## [0.3.0] - 2022-04-18
### Additions
- Horizontal struct support
- Utility wrapper types that do two-way value transformations during
  serialization, for example for allowing map-like inline values.
### Changes
- Horizontal sequences can now show up at the parsing top level and are
  distinguished from vertical sequences by the input having no newlines.
- Unified structs and maps, syntactic sugar replaces separate struct syntax.
- Due to struct and map unification, serde's struct flattening can now be used
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
### Additions
- Comment syntax
- Error reporting with line numbers
- Spaces are also supported for indentation
### Changes
- Missing headlines (double indentation) are no longer supported
- Raw mode is now indicated by byte buffer type instead of `Option`
- Comma separator syntax is removed, comments are now used as structural
  separators
### Fixes
- Section-like pair tuples are parsed correctly

## [0.1.0] - 2021-04-10
Initial release
