# Implicit Data Markup

IDM is a non-self-describing, indentation based data serialization format that
is intended to be succinct and comfortable for editing by hand. The parser
expects to know the structure of the type it is deserializing into and may
parse the same input differently based on it. By not having the format be
self-describing, the syntax can be very lightweight.

## Example

Deserializing

```
title: A single string
tags: one two three
matrix:
  1 2 3
  4 5 6
  7 8 9
```

into a value of type

```rust
struct Example {
    title: String,
    tags: Vec<String>,
    matrix: Vec<Vec<i32>>,
}
```

produces

```rust
Example {
    title: "A single string",
    tags: ["one", "two", "three"],
    matrix: [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]],
}
```

## License

IDM is dual-licensed under Apache-2.0 and MIT.
