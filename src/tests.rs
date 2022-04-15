use std::{collections::BTreeMap, fmt, iter::FromIterator};

use crate::{
    from_str, outline, outline::Outline, to_string, to_string_styled_like,
};
use indexmap::IndexMap;
use lazy_static::lazy_static;
use pretty_assertions::assert_eq;
use serde::{Deserialize, Serialize};

macro_rules! test {
    ($val:expr, $canon:expr) => {
        test($val, $canon);
    };

    ($val:expr, $canon:expr, $($alt:expr),*) => {
        test($val, $canon);
        $(test_inexact($val, $alt);)*
    };

    ($val:expr, _, $($alt:expr),*) => {
        $(test_inexact($val, $alt);)*
    };
}

fn fails<T: serde::de::DeserializeOwned + fmt::Debug>(idm: &str) {
    if let Ok(val) = from_str::<T>(idm) {
        panic!("Bad input parsed into '{:?}'", val);
    }
}

// FIXME: Re-enable error line numbers.
/*
fn fails_at<T: serde::de::DeserializeOwned + fmt::Debug>(
    line_num: usize,
    idm: &str,
) {
    match from_str::<T>(idm) {
        Err(e) => assert_eq!(e.line_num(), Some(line_num)),
        Ok(val) => panic!("Bad input parsed into '{:?}'", val),
    }
}
*/

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
fn primitives() {
    test!(&-128i8, "-128");
    test!(&255u8, "255");
    test!(&-30000i16, "-30000");
    test!(&60000u16, "60000");
    test!(&-2000000000i32, "-2000000000");
    test!(&4000000000u32, "4000000000");
    test!(&-9000000000000000000i64, "-9000000000000000000");
    test!(&10000000000000000000u64, "10000000000000000000");
    test!(&-12.3e4f32, "-123000", "-12.3e4");
    test!(&-12.3e4f64, "-123000", "-12.3e4");
    test!(&'@', "@");
    test!(&true, "true");
    test!(&false, "false");
}

#[test]
fn inconsistent_indentation() {
    // FIXME: Re-enable line numbers in error reports.
    fails /*_at*/::<Outline>(
        //3,
        "\
foo
  bar
\tbaz",
    );

    fails /*_at*/::<Outline>(
        //1,
        "\
foo
  bar
  \tbaz",
    );

    // TODO: Detect mixed indentation even when you dip into zero indentation
    // in between. Not critical, since everything will work consistently even
    // without this, but it's still something I feel like should be
    // disallowed.
    // fails_at::<Outline>(4, "foo\n  bar\nqux\n\tquux");
}

#[test]
fn simple_sequence() {
    // Test inline fragments.
    test!(
        &vec![s("foo"), s("bar"), s("baz")],
        "foo bar baz",
        "foo\nbar\nbaz"
    );
    test!(&vec![1, 2, 3], "1 2 3", "1\n2\n3");
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

    test!(
        &vec![s("foo\nbar"), s("baz")],
        "\
--
  foo
  bar
baz",
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
  1
  2
--
  3
  4",
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
        "\
1
  2"
    );

    test!(
        &vec![(1, 2), (3, 4)],
        "\
1
\t2
3
\t4"
    );

    test!(
        &vec![(
            s("Lorem"),
            s("ipsum dolor sit amet\nconsectetur adipiscing elit"),
        )],
        "\
Lorem
  ipsum dolor sit amet
  consectetur adipiscing elit"
    );
}

#[test]
fn basic_outlines() {
    test!(&Outline::default(), "");

    test!(&outline!["Xyzzy"], "Xyzzy\n");

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
fn outline_tail_comments() {
    test!(
        &outline![["A", "B", "-- C"]],
        "\
A
  B
  -- C"
    );

    test!(
        &outline![["A", "B", "-- C", "", "-- D"]],
        "\
A
  B
  -- C

  -- D"
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
        &BTreeMap::from_iter(vec![(s("bar"), 1), (s("foo"), 2),]),
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
            (s("bar"), vec![1, 2, 3]),
            (s("foo"), vec![2, 3, 4]),
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

#[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct Simple {
    name_text: String,
    x: i32,
    y: i32,
}

#[test]
fn simple_struct_parse() {
    test!(
        &Simple {
            name_text: s("Foo bar"),
            x: 1,
            y: 2,
        },
        "\
name-text Foo bar
x 1
y 2",
        "\
-- Comment at start.
name-text Foo bar
x 1
y 2",
        "\
-- Comments

-- and blank lines
name-text Foo bar
x 1
y 2",
        "\
name-text Foo bar
-- Comment in the middle
x 1
y 2",
        "\
name-text Foo bar
x 1
y 2
-- Comment at end"
    );
}

#[test]
fn simple_struct_errors() {
    // Fails on extra junk at end
    fails::<Simple>(
        "\
name-text Foo bar
x 1
y 2
chaff",
    );

    // Must fail if a field can't be handled.
    fails::<Simple>(
        "\
name-text a
x 1
y 2
unexpected stuff",
    );

    // Key isn't a word.
    fails::<Simple>(
        "\
name-text a
  b
x 1
y 2",
    );
}

#[test]
fn colon_struct() {
    test!(
        &(
            Simple {
                name_text: s("A"),
                x: 1,
                y: 2
            },
            s("Content")
        ),
        _,
        "\
:name-text A
:x 1
:y 2
Content"
    );

    // No content
    test!(
        &(
            Simple {
                name_text: s("A"),
                x: 1,
                y: 2
            },
            String::new()
        ),
        _,
        "\
:name-text A
:x 1
:y 2"
    );
}

#[test]
fn struct_block_value() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    struct Simple {
        name_text: String,
        x: i32,
    }

    test!(
        &Simple {
            name_text: s("Foo\nbar"),
            x: 1,
        },
        "\
name-text
  Foo
  bar
x 1"
    );
}

#[test]
fn map_structs() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Simple {
        x: i32,
        y: i32,
    }

    test!(
        &BTreeMap::from_iter(vec![
            (s("one"), Simple { x: 3, y: 4 }),
            (s("two"), Simple { x: 5, y: 6 }),
        ]),
        "\
one
  x 3
  y 4
two
  x 5
  y 6",
    );
}

#[test]
fn vector_struct() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Vectored {
        v: Vec<i32>,
    }

    test!(
        &Vectored { v: vec![1, 2, 3] },
        "v 1 2 3\n",
        "\
v
  1
  2
  3"
    );

    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct VectoredString {
        v: Vec<String>,
    }

    test!(
        &VectoredString {
            v: vec![s("a"), s("b"), s("c")],
        },
        "v a b c\n",
        "\
v
  a
  b
  c"
    );
}

#[test]
fn indented_vector_struct() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Vectored {
        v: Vec<i32>,
        x: Option<i32>,
    }

    test!(
        &vec![
            Vectored {
                v: vec![1, 2],
                x: Some(3)
            },
            Vectored {
                v: vec![4, 5],
                x: Some(6)
            },
        ],
        "\
--
  v 1 2
  x 3
--
  v 4 5
  x 6"
    );
}

#[test]
fn options_struct() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Options {
        a: Option<i32>,
        b: Option<i32>,
        c: Option<i32>,
    }

    test!(
        &vec![
            Options {
                a: Some(1),
                ..Default::default()
            },
            Options {
                b: Some(2),
                ..Default::default()
            },
            Options {
                c: Some(3),
                ..Default::default()
            },
        ],
        "\
--
  a 1
--
  b 2
--
  c 3"
    );
}

#[test]
fn oneshot_section() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Data {
        x: i32,
        y: i32,
    }

    test!(
        &(s("Headline"), Data { x: 1, y: 2 }),
        "\
Headline
  x 1
  y 2",
    );
}

#[test]
fn oneshot_section_struct_vec() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Data2 {
        a: String,
        b: Vec<String>,
        c: Option<String>,
    }

    test!(
        &vec![(
            s("Some words"),
            Data2 {
                a: s("a"),
                b: vec![s("b1"), s("b2")],
                c: Some(s("c"))
            }
        )],
        "\
Some words
  a a
  b b1 b2
  c c",
    );

    test!(
        &vec![
            (
                s("Some words"),
                Data2 {
                    a: s("a"),
                    b: vec![s("b1"), s("b2")],
                    c: Some(s("c"))
                }
            ),
            (
                s("Second run"),
                Data2 {
                    a: s("a"),
                    b: vec![s("b1"), s("b2")],
                    c: Some(s("c"))
                }
            )
        ],
        "\
Some words
  a a
  b b1 b2
  c c
Second run
  a a
  b b1 b2
  c c",
    );
}

#[test]
fn oneshot_section_map_vec() {
    test!(
        &vec![(
            s("Some words"),
            BTreeMap::from_iter(
                vec![(s("a"), vec![1, 2]), (s("b"), vec![3, 4])].into_iter()
            )
        )],
        "\
Some words
  a 1 2
  b 3 4"
    );

    test!(
        &vec![
            (
                s("Some words"),
                BTreeMap::from_iter(
                    vec![(s("a"), vec![1, 2]), (s("b"), vec![3, 4])]
                        .into_iter()
                )
            ),
            (
                s("Second run"),
                BTreeMap::from_iter(
                    vec![(s("a"), vec![1, 2]), (s("b"), vec![3, 4])]
                        .into_iter()
                )
            )
        ],
        "\
Some words
  a 1 2
  b 3 4
Second run
  a 1 2
  b 3 4"
    );
}

#[derive(Clone, PartialEq, Default, Debug, Serialize, Deserialize)]
struct Star {
    age: f32,
    mass: f32,
}

#[derive(Clone, PartialEq, Default, Debug, Serialize, Deserialize)]
struct Planet {
    orbit: f32,
    mass: f32,
}

lazy_static! {
    #[rustfmt::skip]
    static ref STARMAP: IndexMap<String, (Star, IndexMap<String, Planet>)> = IndexMap::from_iter(
        vec![
            (s("Sol"),
            (Star { age: 4.6e9, mass: 1.0},
             IndexMap::from_iter(vec![
                 (s("Mercury"), Planet { orbit: 0.39, mass: 0.055 }),
                 (s("Venus"),   Planet { orbit: 0.72, mass: 0.815 }),
                 (s("Earth"),   Planet { orbit: 1.0,  mass: 1.0 }),
                 (s("Mars"),    Planet { orbit: 1.52, mass: 0.1 })
            ].into_iter()))),
            (s("Alpha Centauri"),
            (Star { age: 5.3e9, mass: 1.1},
             IndexMap::from_iter(vec![
                 (s("Eurytion"), Planet { orbit: 0.47, mass: 0.08 }),
                 (s("Chiron"),   Planet { orbit: 1.32, mass: 1.33 }),
            ].into_iter()))),
        ].into_iter());
}

#[test]
fn nesting_contents() {
    test!(
        &*STARMAP,
        "\
Sol
  :age 4600000000
  :mass 1
  Mercury
    orbit 0.39
    mass 0.055
  Venus
    orbit 0.72
    mass 0.815
  Earth
    orbit 1
    mass 1
  Mars
    orbit 1.52
    mass 0.1
Alpha Centauri
  :age 5300000000
  :mass 1.1
  Eurytion
    orbit 0.47
    mass 0.08
  Chiron
    orbit 1.32
    mass 1.33",
        "\
Sol
  :age 4600000000
  :mass 1
  Mercury
    :orbit 0.39
    :mass 0.055
  Venus
    :orbit 0.72
    :mass 0.815
  Earth
    :orbit 1
    :mass 1
  Mars
    :orbit 1.52
    :mass 0.1
Alpha Centauri
  :age 5300000000
  :mass 1.1
  Eurytion
    :orbit 0.47
    :mass 0.08
  Chiron
    :orbit 1.32
    :mass 1.33"
    );
}

#[test]
fn nesting_contents_2() {
    // Section-like adorned field.
    test!(
        &(
            Simple {
                name_text: s("foo\nbar"),
                x: 1,
                y: 2
            },
            s("Content")
        ),
        "\
:name-text
  foo
  bar
:x 1
:y 2
Content"
    );
}

#[test]
fn inline_structs() {
    test!(
        &*STARMAP,
        _,
        "\
Sol
  :age 4.6e9
  :mass 1.0
  --       :orbit :mass
  Mercury  0.39  0.055
  Venus    0.72  0.815
  Earth    1.0   1.0
  Mars     1.52  0.1
Alpha Centauri
  :age 5.3e9
  :mass 1.1
  --        :orbit :mass
  Eurytion  0.47  0.08
  Chiron    1.32  1.33"
    );
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
fn test_generic_attributes() {
    #[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Partial {
        a: u32,
        #[serde(flatten)]
        attributes: BTreeMap<String, String>,
    }

    test!(
        &Partial {
            a: 1,
            attributes: BTreeMap::from_iter(vec![(s("b"), s("2"))].into_iter())
        },
        "\
a 1
b 2"
    );

    test!(
        &Partial {
            a: 1,
            attributes: BTreeMap::from_iter(
                vec![
                    (s("b"), s("2")),
                    // Note that _attributes fields do not get converted to
                    // snake_case from kebab-case.
                    (s("foo-bar"), s("6")),
                    (s("xyzzy"), s("5"))
                ]
                .into_iter()
            )
        },
        "\
a 1
b 2
foo-bar 6
xyzzy 5"
    );
}

#[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
struct DataOutline(Vec<(String, (IndexMap<String, String>, DataOutline))>);

#[test]
fn test_data_outline() {
    test!(&DataOutline::default(), "");

    test!(&DataOutline(vec![(s("A"), Default::default())]), "A\n");

    test!(
        &DataOutline(vec![(
            s("Title"),
            (
                vec![(s("attr"), s("123"))].into_iter().collect(),
                DataOutline(vec![(s("Subpage"), Default::default())])
            )
        )]),
        "\
Title
  :attr 123
  Subpage",
        "\
Title
  -- 'Normal form' of head map, using a comment to mark it
    attr 123
  Subpage"
    );
}

#[test]
fn test_serialize_data_outline() {
    assert_eq!(to_string(&DataOutline::default()).unwrap(), "");

    assert_eq!(
        to_string(&DataOutline(vec![(s("A"), Default::default())])).unwrap(),
        "A\n"
    );

    assert_eq!(
        to_string(&DataOutline(vec![(
            s("A"),
            (
                vec![(s("x"), s("1"))].into_iter().collect(),
                Default::default(),
            )
        )]))
        .unwrap(),
        "\
A
  :x 1
"
    );

    assert_eq!(
        to_string(&DataOutline(vec![(
            s("A"),
            (
                Default::default(),
                DataOutline(vec![(s("B"), Default::default())])
            )
        )]))
        .unwrap(),
        "\
A
  B
"
    );

    assert_eq!(
        to_string(&DataOutline(vec![(
            s("A"),
            (
                vec![(s("x"), s("1"))].into_iter().collect(),
                DataOutline(vec![(s("B"), Default::default())])
            )
        )]))
        .unwrap(),
        "\
A
  :x 1
  B
"
    );
}

////////////////////////////////
// Helper functions

/// Test that deserialization matches value and serialization matches IDM.
fn test<T>(val: &T, idm: &str)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let is_inline = !idm.chars().any(|c| c == '\n');

    let deser = from_str::<T>(idm).expect("IDM did not deserialize to type");
    assert_eq!(&deser, val);

    // Use to_string_styled_like to pick the indent style from the input's
    // example, serialize tabs when parsing tabs, spaces when parsing spaces.
    let reser = to_string_styled_like(idm, val)
        .expect("Value did not serialize to IDM");

    let reser = reser.trim_end();
    let idm = idm.trim_end();

    if idm != reser {
        println!("Deserialized \n\x1b[1;32m{}\x1b[0m", idm);
        println!("Reserialized \n\x1b[1;31m{}\x1b[0m", reser);
    }

    assert_eq!(idm, reser);

    // Also check for default style ser, two characters per indent can trip
    // some deserialization bugs that generate 1-character dummy indents where
    // they shouldn't.
    let mut reser = to_string(val).expect("Value did not serialize to IDM");
    if is_inline {
        reser = reser.trim_end().to_string();
    }

    let new_deser = from_str::<T>(&reser)
        .expect("Serialized IDM did not deserialize to type");
    assert_eq!(&new_deser, val);
}

/// Test that deserialization matches value and value's serialization
/// deserializes to value.
///
/// Use this version for IDM that does not reserialize the exact same way it
/// is written.
fn test_inexact<T>(val: &T, idm: &str)
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
