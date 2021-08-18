use crate::{from_str, outline, outline::Outline, to_string};
use pretty_assertions::assert_eq;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use std::iter::FromIterator;

#[test]
fn ser_atom() {
    test("123", &123u32);
    test("2.718", &2.718f32);
    test("xyzzy", &s("xyzzy"));
    test("one two", &s("one two"));
    test("one\ntwo", &s("one\ntwo"));
    test("one two\nthree", &s("one two\nthree"));
}

#[test]
fn ser_simple_sequence() {
    test("foo\nbar\nbaz", &vec![s("foo"), s("bar"), s("baz")]);
    test("1\n2\n3", &vec![1, 2, 3]);
    test("1\n2\n3", &(1, 2, 3));
}

#[test]
fn ser_block_sequence() {
    test(
        "\
--
  foo
  bar
--
  baz",
        &vec![s("foo\nbar"), s("baz")],
    );
    test(
        "\
--
  foo
  bar
baz",
        &vec![s("foo\nbar"), s("baz")],
    );

    test(
        "\
baz
--
  foo
  bar",
        &vec![s("baz"), s("foo\nbar")],
    );

    // Comment escaping idiom
    test("foo\n--\n  -- baz", &vec![s("foo"), s("-- baz")]);
}

#[test]
fn ser_nested_sequence() {
    // All tests are repeated for seq-like Vec types and tuple-like array
    // types because those types have different code paths in the serializer.

    // Inline inner form matrix

    // Vec case (seq)
    test(
        "\
1 2
3 4",
        &vec![vec![1, 2], vec![3, 4]],
    );

    // Array case (tuple)
    test(
        "\
1 2
3 4",
        &[[1, 2], [3, 4]],
    );
}

#[test]
fn ser_sequence_with_separators() {
    // Not the default serialization form, so we specify an inexact test.

    // Mid-struct blank lines
    test_inexact(
        "\
1 2

3 4",
        &vec![vec![1, 2], vec![3, 4]],
    );

    // Array case (tuple)
    test_inexact(
        "\
1 2

3 4",
        &[[1, 2], [3, 4]],
    );

    // Outline inner form
    test_inexact(
        "\
--
\t1
\t2
--
\t3
\t4",
        &vec![vec![1, 2], vec![3, 4]],
    );

    test_inexact(
        "\
--

\t1
\t2
--
\t-- Comment
\t3
\t4",
        &[[1, 2], [3, 4]],
    );

    test_inexact(
        "\
--
  1
  2
-- Here is a multiline comment
-- Multiple comment lines with no content in between
--
-- Even if they're empty
-- Don't mean an empty sequence exists in between them
--
  3
  4",
        &vec![vec![1, 2], vec![3, 4]],
    );

    // Outline list of matrices.
    test(
        "\
--
\t1 2
\t3 4
--
\t5 6
\t7 8",
        &vec![vec![vec![1, 2], vec![3, 4]], vec![vec![5, 6], vec![7, 8]]],
    );

    test(
        "\
--
\t1 2
\t3 4
--
\t5 6
\t7 8",
        &[[[1, 2], [3, 4]], [[5, 6], [7, 8]]],
    );
}

#[test]
fn ser_section_tuple() {
    test_inexact(
        "\
1
\t2",
        &vec![(1, 2)],
    );

    test_inexact(
        "\
1
\t2
\t3",
        &vec![(1, 2, 3)],
    );

    test_inexact(
        "\
1
\t2
3
\t4",
        &vec![(1, 2), (3, 4)],
    );

    test(
        "\
Lorem
  --
    ipsum dolor sit amet
    consectetur adipiscing elit",
        &vec![(
            "Lorem".to_string(),
            "ipsum dolor sit amet\nconsectetur adipiscing elit".to_string(),
        )],
    );
}

#[test]
fn ser_tuple_tail_sequence_continuation() {
    test::<(i32, i32, Vec<i32>)>(
        "\
1
2
3
4",
        &(1, 2, vec![3, 4]),
    );
}

#[test]
fn ser_option_tuple() {
    test::<Vec<(Option<i32>, i32)>>(
        "\
1
\t2",
        &vec![(Some(1), 2)],
    );
}

#[test]
fn ser_basic_outlines() {
    test("", &Outline::default());

    test("Xyzzy", &outline!["Xyzzy"]);

    test("A\nB", &outline!["A", "B"]);
    test("A\nB\nC", &outline!["A", "B", "C"]);
    test("A\n  B", &outline![["A", "B"]]);
    test("A\n  B\nC", &outline![["A", "B"], "C"]);

    test(
        "\
Xyzzy
  Plugh
Qux
  Quux",
        &outline![["Xyzzy", "Plugh"], ["Qux", "Quux"]],
    );

    test(
        "\
Xyzzy
  Plugh
  Blorb
Qux
  Quux",
        &outline![["Xyzzy", "Plugh", "Blorb"], ["Qux", "Quux"]],
    );
}

#[test]
fn ser_outline_with_blanks() {
    test(
        "\
A
--
\tC",
        &outline!["A", ["--", "C"]],
    );

    test(
        "\
A

B",
        &outline!["A", "", "B"],
    );
    test(
        "\
A
  B

  C",
        &outline![["A", "B", "", "C"]],
    );
}

#[test]
fn ser_escape_comment() {
    // Standalone string (not sequence), no escaping
    test("--", &s("--"));

    // Use separator to make the comment paragraph-like in an outline list
    test_inexact::<Vec<String>>(
        "\
--
\t--
foo",
        &vec![s("--"), s("foo")],
    );
}

#[test]
fn ser_inline_map() {
    test(
        "\
foo 1
bar 2",
        &BTreeMap::from_iter(vec![
            ("foo".to_string(), 1),
            ("bar".to_string(), 2),
        ]),
    );
}

#[test]
fn ser_outline_map() {
    test_inexact(
        "\
foo
  1
bar
  2",
        &BTreeMap::from_iter(vec![
            ("foo".to_string(), 1),
            ("bar".to_string(), 2),
        ]),
    );

    test_inexact(
        "\
foo
  1
  2
  3
bar
  2
  3
  4",
        &BTreeMap::from_iter(vec![
            ("foo".to_string(), vec![1, 2, 3]),
            ("bar".to_string(), vec![2, 3, 4]),
        ]),
    );
}

#[test]
fn ser_simple_struct() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Simple {
        name_text: String,
        x: i32,
        y: i32,
    }

    test(
        "\
name-text: Foo bar
x: 1
y: 2",
        &Simple {
            name_text: s("Foo bar"),
            x: 1,
            y: 2,
        },
    );

    // Must fail if there's no _contents field to grab contents
    assert!(from_str::<Simple>(
        "\
name-text: Foo bar
x: 1
y: 2
chaff"
    )
    .is_err());

    // Must fail if a field can't be handled.
    assert!(from_str::<Simple>(
        "\
name_text: a
x: 1
y: 2
unexpected: stuff"
    )
    .is_err());
}

#[test]
fn ser_map_structs() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Simple {
        x: i32,
        y: i32,
    }

    test(
        "\
one
  x: 3
  y: 4
two
  x: 5
  y: 6",
        &BTreeMap::from_iter(vec![
            ("one".to_string(), Simple { x: 3, y: 4 }),
            ("two".to_string(), Simple { x: 5, y: 6 }),
        ]),
    );
}

#[test]
fn ser_vector_struct() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Vectored {
        v: Vec<i32>,
    }

    test(
        "\
v: 1 2 3",
        &Vectored { v: vec![1, 2, 3] },
    );

    test_inexact(
        "\
v:
\t1
\t2
\t3",
        &Vectored { v: vec![1, 2, 3] },
    );
}

#[test]
fn ser_struct_flatten() {
    /*
         XXX: Does not currently work, see https://github.com/serde-rs/serde/issues/1346
         FIXME if the Serde issue gets resolved.

         #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
         struct Nested {
             #[serde(flatten)]
             simple: Simple,
         }

         test(
             "\
     name-text: Foo bar
     x: 1
     y: 2",
             &Nested {
                 simple: Simple {
                     name_text: s("Foo bar"),
                     x: 1,
                     y: 2,
                 },
             },
         );
    */
}

#[test]
fn ser_struct_contents() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Contentful {
        x: i32,
        y: i32,
        _contents: Outline,
    }

    test(
        "\
x: 1
y: 2
A
\tB",
        &Contentful {
            x: 1,
            y: 2,
            _contents: outline![["A", "B"]],
        },
    );

    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Contentful2 {
        x: i32,
        y: i32,
        _contents: Vec<String>,
    }
    test(
        "\
x: 1
y: 2
A
B",
        &Contentful2 {
            x: 1,
            y: 2,
            _contents: vec![s("A"), s("B")],
        },
    );

    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Recursive {
        #[serde(default, skip_serializing_if = "is_default")]
        a: i32,
        #[serde(default)]
        _contents: BTreeMap<String, Recursive>,
    }

    test(
        "\
a: 1
x
  a: 2",
        &Recursive {
            a: 1,
            _contents: BTreeMap::from_iter(vec![(
                "x".to_string(),
                Recursive {
                    a: 2,
                    ..Default::default()
                },
            )]),
        },
    );

    test(
        "\
item
  a: 1",
        &Recursive {
            a: 0,
            _contents: BTreeMap::from_iter(vec![(
                "item".to_string(),
                Recursive {
                    a: 1,
                    ..Default::default()
                },
            )]),
        },
    );

    test(
        "\
items
  item
    a: 1",
        &Recursive {
            a: 0,
            _contents: BTreeMap::from_iter(vec![(
                "items".to_string(),
                Recursive {
                    a: 0,
                    _contents: BTreeMap::from_iter(vec![(
                        "item".to_string(),
                        Recursive {
                            a: 1,
                            ..Default::default()
                        },
                    )]),
                },
            )]),
        },
    );
}

/* FIXME
#[test]
fn ser_oneshot_section() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Data {
        x: i32,
        y: i32,
    }

    test(
        "\
Headline
  x: 1
  y: 2",
        &vec![("Headline".to_string(), Data { x: 1, y: 2 })],
    );
}
*/

#[test]
fn ser_nesting_contents() {
    const STARMAP: &str = "\
Sol
  age: 4.6e9
  mass: 1.0
  Mercury
    orbit: 0.39
    mass: 0.055
  Venus
    orbit: 0.72
    mass: 0.815
  Earth
    orbit: 1.0
    mass: 1.0
  Mars
    orbit: 1.52
    mass: 0.1
Alpha Centauri
  age: 5.3e9
  mass: 1.1
  Eurytion
    orbit: 0.47
    mass: 0.08
  Chiron
    orbit: 1.32
    mass: 1.33";

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

    #[rustfmt::skip]
    let starmap = BTreeMap::from_iter(
        vec![
            ("Sol".to_string(),
             Star { age: 4.6e9, mass: 1.0,
                    _contents: BTreeMap::from_iter(
                        vec![
                            ("Mercury".to_string(),
                             Planet { orbit: 0.39, mass: 0.055 }),
                            ("Venus".to_string(),
                             Planet { orbit: 0.72, mass: 0.815 }),
                            ("Earth".to_string(),
                             Planet { orbit: 1.0, mass: 1.0 }),
                            ("Mars".to_string(),
                             Planet { orbit: 1.52, mass: 0.1 })
                        ].into_iter(),
                    ),
                },
            ),
            ("Alpha Centauri".to_string(),
             Star { age: 5.3e9, mass: 1.1,
                    _contents: BTreeMap::from_iter(
                        vec![
                            ("Eurytion".to_string(),
                             Planet { orbit: 0.47, mass: 0.08 }),
                            ("Chiron".to_string(),
                             Planet { orbit: 1.32, mass: 1.33 }),
                        ].into_iter(),
                    ),
                },
            ),
        ].into_iter(),
    );

    test_inexact(STARMAP, &starmap);
}

#[test]
fn ser_value_length() {
    #[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Entry {
        x: String,
        y: String,
    }
    test("\
x: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
y: b",
    &Entry {
x: s("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
y: s("b")
    });

    #[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
    struct List {
        x: Vec<String>,
        y: String,
    }

    test(
        "\
x: a b c
y: a",
        &List {
            x: vec![s("a"), s("b"), s("c")],
            y: s("a"),
        },
    );

    test(
        "\
x:
\trindfleischetikettierungsüberwachungsaufgabenübertragungsgesetz
\tbetäubungsmittelverschreibungsverordnung
\trechtsschutzversicherungsgesellschaften
y: a",
        &List {
            x: vec![
s("rindfleischetikettierungsüberwachungsaufgabenübertragungsgesetz"),
s("betäubungsmittelverschreibungsverordnung"),
s("rechtsschutzversicherungsgesellschaften")],
            y: s("a"),
        },
    );

    test_inexact(
        "\
x: rindfleischetikettierungsüberwachungsaufgabenübertragungsgesetz betäubungsmittelverschreibungsverordnung rechtsschutzversicherungsgesellschaften
y: a",
        &List {
            x: vec![
s("rindfleischetikettierungsüberwachungsaufgabenübertragungsgesetz"),
s("betäubungsmittelverschreibungsverordnung"),
s("rechtsschutzversicherungsgesellschaften")],
y: s("a"),
        },
    );
}

#[test]
fn ser_comment_value() {
    #[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
    pub struct Ch {
        c: String,
    }
    test(
        "\
c: --",
        &Ch { c: "--".into() },
    );
}

////////////////////////////////
// Helper functions

/// Test that deserialization matches value and serialization matches IDM.
fn test<T>(idm: &str, val: &T)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let deser = from_str::<T>(idm).expect("IDM did not deserialize to type");
    assert_eq!(&deser, val);

    // TODO: Re-enable when serializer handles v02
    // let reser = to_string(val).expect("Value did not serialize to IDM");
    // assert_eq!(idm, reser.trim_end());
}

/// Test that deserialization matches value and value's serialization
/// deserializes to value.
///
/// Use this version for IDM that does not reserialize the exact same way it
/// is written.
fn test_inexact<T>(idm: &str, val: &T)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let deser = from_str::<T>(idm).expect("IDM did not deserialize to type");
    assert_eq!(&deser, val);

    // TODO: Re-enable when serializer handles v02
    // let reser = to_string(val).expect("Value did not serialize to IDM");
    // // Reserialization may differ from original IDM (different order
    // // of fields, removed comments etc).
    // let new_deser = from_str::<T>(&reser)
    //     .expect("Serialized IDM did not deserialize to type");
    // // It must still deserialize to same value.
    // assert_eq!(&new_deser, val);
}

// Conveninence constructor for String literals.
fn s(s: &str) -> String {
    s.to_string()
}

pub fn is_default<T: Default + Eq>(a: &T) -> bool {
    a == &Default::default()
}
