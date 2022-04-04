# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Changes
- Horizontal sequences can now show up at the parsing top level when the input
  has now newlines
- New parser data structure based on an immutable parse tree object
- Unified structs and maps, syntactic sugar replaces separate struct syntax
- Due to struct and map unification, serde's struct flattening can now be used
- Removed `_contents` magic field from struct parsing, fields + contents
  entries are now represented as pairs and using the new colon prefix syntax
  to mark the struct half
### Additions
- Horizontal struct support
- Enumeration type support

## [0.2.0] - 2021-09-09
### Changes
- Do not support empty headlines
- Raw mode is now indicated by byte buffer type instead of `Option`
- Comma separator syntax is removed, comments are now used as structural
  separators
### Fixes
- Section-like pair tuples are parsed correctly
### Additions
- Comment syntax
- Error reporting with line numbers
- Spaces are also supported for indentation

## [0.1.0] - 2021-04-10
Initial release
