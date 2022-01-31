# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Changes
- Toplevel sequence type can now be deserialized from inline sequence
- Parser using an immutable data structure
### Additions
- Generic attribute collection using `_attributes` reserved field name
- Inline struct support
- Enumeration type support

## [0.2.0] - 2021-09-09
### Changes
- Support either tabs or spaces for indentation
- Do not support empty headlines
- Raw mode is now indicated by byte buffer type instead of `Option`
- Comman syntax is removed, comments are now used as structural separators
### Fixes
- Section-like pair tuples are parsed correctly
### Additions
- Comment syntax
- Error reporting with line numbers

## [0.1.0] - 2021-04-10
Initial release
