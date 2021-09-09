use crate::{
    from_str, outline,
    outline::{Outline, Raw},
    to_string, to_string_styled_like,
};
use pretty_assertions::assert_eq;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use std::iter::FromIterator;

macro_rules! test {
    ($val:expr, $canon:expr) => {
        test($canon, $val);
    };

    ($val:expr, $canon:expr, $($alt:expr),*) => {
        test($canon, $val);
        $(test_inexact($alt, $val);)*
    };
}

#[test]
fn atom() {
    test!(&123u32, "123");
    test!(&2.718f32, "2.718");
    test!(&s("xyzzy"), "xyzzy");
    test!(&s("one two"), "one two");
    test!(&s("one\ntwo"), "one\ntwo");
    test!(&s("one two\nthree"), "one two\nthree");
}

#[test]
fn simple_sequence() {
    test!(&vec![s("foo"), s("bar"), s("baz")], "foo\nbar\nbaz");
    test!(&vec![1, 2, 3], "1\n2\n3");
    test!(&(1, 2, 3), "1\n2\n3");

    test!(&vec![(Some(s("A")), 2)], "A 2");

    // Raw forces section when you could be inlined otherwise.
    test!(
        &vec![(Raw(s("A")), 2)],
        "\
A
  2"
    );
}

#[test]
fn block_sequence() {
    test!(
        &vec![s("foo\nbar"), s("baz")],
        "\
--
  foo
  bar
baz",
        "\
--
  foo
  bar
--
  baz"
    );

    test(
        "\
--
  foo
  bar
baz",
        &vec![s("foo\nbar"), s("baz")],
    );

    test!(
        &vec![s("baz"), s("foo\nbar")],
        "\
baz
--
  foo
  bar"
    );
}

#[test]
fn block_sequence_comment_escape() {
    test!(
        &vec![s("foo"), s("-- baz")],
        "\
foo
--
  -- baz"
    );
}

#[test]
fn nested_sequence() {
    // All tests are repeated for seq-like Vec types and tuple-like array
    // types because those types have different code paths in the serializer.

    // Inline inner form matrix

    // Vec case (seq)
    test!(
        &vec![vec![1, 2], vec![3, 4]],
        "\
1 2
3 4",
        "\
1 2

3 4",
        "\
--
\t1
\t2
--
\t3
\t4",
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
  4"
    );

    // NB. Tuples are pretty happy to opportunistically switch to section
    // mode, so they're not viable for the rows level.

    // Array case (tuple)
    test!(
        &vec![[1, 2], [3, 4]],
        "\
1 2
3 4",
        "\
1 2

3 4",
        "\
--
\t1
\t2
--
\t3
\t4",
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
  4"
    );
}

#[test]
fn multi_nesting() {
    // Outline list of matrices.
    test!(
        &vec![vec![vec![1, 2], vec![3, 4]], vec![vec![5, 6], vec![7, 8]]],
        "\
--
  1 2
  3 4
--
  5 6
  7 8"
    );

    test!(
        &vec![vec![[1, 2], [3, 4]], vec![[5, 6], [7, 8]]],
        "\
--
  1 2
  3 4
--
  5 6
  7 8"
    );
}

#[test]
fn section_atoms() {
    // Atoms can be section-like.
    test!(
        &vec![
            s("’Twas brillig, and the slithy toves
      Did gyre and gimble in the wabe:"),
            s("All mimsy were the borogoves,
      And the mome raths outgrabe."),
            s("“Beware the Jabberwock, my son!
      The jaws that bite, the claws that catch!"),
            s("Beware the Jubjub bird, and shun
      The frumious Bandersnatch!”")
        ],
        "\
’Twas brillig, and the slithy toves
      Did gyre and gimble in the wabe:
All mimsy were the borogoves,
      And the mome raths outgrabe.
“Beware the Jabberwock, my son!
      The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
      The frumious Bandersnatch!”",
        "\
’Twas brillig, and the slithy toves
      Did gyre and gimble in the wabe:
All mimsy were the borogoves,
      And the mome raths outgrabe.

“Beware the Jabberwock, my son!
      The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
      The frumious Bandersnatch!”"
    );
}

#[test]
fn section_tuple() {
    test!(
        &vec![(1, 2)],
        "1 2",
        "\
1
  2"
    );

    test!(
        &vec![(1, 2), (3, 4)],
        "\
1 2
3 4",
        "\
1
\t2
3
\t4"
    );

    test!(
        &vec![(
            "Lorem".to_string(),
            "ipsum dolor sit amet\nconsectetur adipiscing elit".to_string(),
        )],
        "\
Lorem
  ipsum dolor sit amet
  consectetur adipiscing elit"
    );
}

#[test]
fn tuple_tail_line_mode() {
    // Can still inline things if the last tuple item isn't a word token.

    test!(&vec![(s("Foo"), s("Bar Baz Quux"))], "Foo Bar Baz Quux");

    test!(
        &vec![(s("Foo"), s("Bar"), s("Baz Quux"))],
        "Foo Bar Baz Quux"
    );

    // Now it's not the last item that's line-like, have to go block mode.
    test!(
        &vec![(s("Foo"), s("Bar Baz"), s("Quux"))],
        "\
--
  Foo
  Bar Baz
  Quux"
    );
}

#[test]
fn basic_outlines() {
    test!(&Outline::default(), "");

    test!(&outline!["Xyzzy"], "Xyzzy");

    test!(&outline!["A", "B"], "A\nB");
    test!(&outline!["A", "B", "C"], "A\nB\nC");
    test!(&outline![["A", "B"]], "A\n  B");
    test!(&outline![["A", "B"], "C"], "A\n  B\nC");

    test!(
        &outline![["Xyzzy", "Plugh"], ["Qux", "Quux"]],
        "\
Xyzzy
  Plugh
Qux
  Quux"
    );

    test!(
        &outline![["Xyzzy", "Plugh", "Blorb"], ["Qux", "Quux"]],
        "\
Xyzzy
  Plugh
  Blorb
Qux
  Quux"
    );
}

#[test]
fn outline_with_blanks() {
    test!(
        &outline!["A", ["--", "C"]],
        "\
A
--
\tC"
    );

    test!(
        &outline!["A", "", "B"],
        "\
A

B"
    );
    test!(
        &outline![["A", "B", "", "C"]],
        "\
A
  B

  C"
    );
}

#[test]
fn escape_comment() {
    // Standalone string (not sequence), no escaping
    test!(&s("--"), "--");

    // Use separator to make the comment paragraph-like in an outline list
    test!(
        &vec![s("--"), s("foo")],
        "\
--
  --
foo"
    );
}

#[test]
fn map() {
    test!(
        &BTreeMap::from_iter(vec![
            ("bar".to_string(), 1),
            ("foo".to_string(), 2),
        ]),
        "\
bar 1
foo 2",
        "\
bar
  1
foo
  2"
    );

    test!(
        &BTreeMap::from_iter(vec![
            ("bar".to_string(), vec![1, 2, 3]),
            ("foo".to_string(), vec![2, 3, 4]),
        ]),
        "\
bar 1 2 3
foo 2 3 4",
        "\
bar
  1
  2
  3
foo
  2
  3
  4"
    );
}

#[test]
fn simple_struct() {
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
fn map_structs() {
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
fn vector_struct() {
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

    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct VectoredString {
        v: Vec<String>,
    }
    test_inexact(
        "\
v:
  a
  b
  c",
        &VectoredString {
            v: vec![s("a"), s("b"), s("c")],
        },
    );
}

#[test]
fn struct_flatten() {
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
fn struct_contents() {
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

#[test]
fn oneshot_section() {
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

#[test]
fn nesting_contents() {
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

/*

// Disabled, currently inlining opportunistically at any length.

#[test]
fn value_length() {
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
*/

#[test]
fn comment_value() {
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

    // Use to_string_styled_like to pick the indent style from the input's
    // example, serialize tabs when parsing tabs, spaces when parsing spaces.
    let reser = to_string_styled_like(idm, val)
        .expect("Value did not serialize to IDM");

    let reser = reser.trim_end();

    if idm != reser {
        println!("Deserialized \n\x1b[1;32m{}\x1b[0m", idm);
        println!("Reserialized \n\x1b[1;31m{}\x1b[0m", reser);
    }

    assert_eq!(idm, reser);

    // Also check for default style ser, two characters per indent can trip
    // some deserialization bugs that generate 1-character dummy indents where
    // they shouldn't.
    let reser = to_string(val).expect("Value did not serialize to IDM");
    let new_deser = from_str::<T>(&reser)
        .expect("Serialized IDM did not deserialize to type");
    assert_eq!(&new_deser, val);
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

    let reser = to_string(val).expect("Value did not serialize to IDM");
    // Reserialization may differ from original IDM (different order
    // of fields, removed comments etc).
    let new_deser = from_str::<T>(&reser)
        .expect("Serialized IDM did not deserialize to type");
    // It must still deserialize to same value.
    assert_eq!(&new_deser, val);
}

// Conveninence constructor for String literals.
fn s(s: &str) -> String {
    s.to_string()
}

pub fn is_default<T: Default + Eq>(a: &T) -> bool {
    a == &Default::default()
}
