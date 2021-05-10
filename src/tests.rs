use crate::{from_str, outline, to_string};
use pretty_assertions::assert_eq;
use serde_derive::{Deserialize, Serialize};
use std::fmt;

type Outline = outline::Outline<Option<String>>;

#[derive(Copy, Clone, Default, Debug)]
struct TestCase {
    allow_inexact_reserialization: bool,
}

impl TestCase {
    fn inexact() -> TestCase {
        TestCase {
            allow_inexact_reserialization: true,
        }
    }

    fn test<T>(self, idm: &str, val: &T)
    where
        T: PartialEq
            + fmt::Debug
            + serde::Serialize
            + serde::de::DeserializeOwned,
    {
        let deser =
            from_str::<T>(idm).expect("IDM did not deserialize to type");
        assert_eq!(&deser, val);

        let reser = to_string(val).expect("Value did not serialize to IDM");

        if self.allow_inexact_reserialization {
            // Reserialization may differ from original IDM (different order
            // of fields, removed comments etc).
            let new_deser = from_str::<T>(&reser)
                .expect("Serialized IDM did not deserialize to type");
            // It must still deserialize to same value.
            assert_eq!(&new_deser, val);
        } else {
            // Reserialization must be the same as the original IDM.
            // In this case it should be safe to assume reserialization also
            // deserializes the same way.
            assert_eq!(idm, reser.trim_end());
        }
    }
}

// Convenience wrapper for fluent test.
fn test<T>(idm: &str, val: &T)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    TestCase::default().test(idm, val);
}

fn test_inexact<T>(idm: &str, val: &T)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    TestCase::inexact().test(idm, val);
}

// Conveninence constructor for String literals.
fn s(s: &str) -> String {
    s.to_string()
}

#[test]
fn test_atom() {
    test("123", &123u32);
    test("2.718", &2.718f32);
    test("xyzzy", &s("xyzzy"));
    test("one two", &s("one two"));
    test("one\ntwo", &s("one\ntwo"));
    test("one two\nthree", &s("one two\nthree"));
}

#[test]
fn test_simple_sequence() {
    test::<Vec<i32>>("", &vec![]);

    test("foo\nbar\nbaz", &vec![s("foo"), s("bar"), s("baz")]);
    test("1\n2\n3", &vec![1, 2, 3]);
    test("1\n2\n3", &(1, 2, 3));
}

#[test]
fn test_nested_sequence() {
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

    // Outline inner form
    // Not the default serialization form, so we specify an inexact test.

    test_inexact(
        "\
\t1
\t2
,
\t3
\t4",
        &vec![vec![1, 2], vec![3, 4]],
    );

    test_inexact(
        "\
\t1
\t2
,
\t3
\t4",
        &[[1, 2], [3, 4]],
    );

    // Outline list of matrices.
    test(
        "\
\t1 2
\t3 4
,
\t5 6
\t7 8",
        &vec![vec![vec![1, 2], vec![3, 4]], vec![vec![5, 6], vec![7, 8]]],
    );

    test(
        "\
\t1 2
\t3 4
,
\t5 6
\t7 8",
        &[[[1, 2], [3, 4]], [[5, 6], [7, 8]]],
    );
}

#[test]
fn test_section_tuple() {
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
}

#[test]
fn test_tuple_tail_sequence_continuation() {
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
fn test_option_tuple() {
    test::<Vec<(Option<i32>, i32)>>(
        "\
1
\t2",
        &vec![(Some(1), 2)],
    );

    test_inexact::<Vec<(Option<i32>, i32)>>(
        "\
,
\t2",
        &vec![(None, 2)],
    );

    test::<Vec<(Option<i32>, i32)>>(
        "\

\t2",
        &vec![(None, 2)],
    );
}

#[test]
fn test_canonical_outline() {
    test("", &Outline::default());

    test("Xyzzy", &outline!["Xyzzy"]);

    test("A\n\nB", &outline![["A", ""], "B"]);

    test("A\n\tB\n\n\tC", &outline![["A", ["B", ""], "C"]]);

    test("A\n  B", &outline!["A", "  B"]);

    test(
        "\
Xyzzy
\tPlugh",
        &outline![["Xyzzy", "Plugh"]],
    );

    test(
        "\
Xyzzy
Plugh",
        &outline!["Xyzzy", "Plugh"],
    );

    test(
        "\
\tPlugh",
        &outline![[, "Plugh"]],
    );

    test_inexact(
        "\
,
\tPlugh",
        &outline![[, "Plugh"]],
    );

    test(
        "\
Xyzzy
\tPlugh
Qux
\tQuux",
        &outline![["Xyzzy", "Plugh"], ["Qux", "Quux"]],
    );

    test(
        "\
Xyzzy
\tPlugh
\tBlorb
Qux
\tQuux",
        &outline![["Xyzzy", "Plugh", "Blorb"], ["Qux", "Quux"]],
    );

    test(
        "\
Xyzzy
\tPlugh
,
\tQuux",
        &outline![["Xyzzy", "Plugh"], [, "Quux"]],
    );

    test(
        "\
A
,
\tC",
        &outline!["A", [, "C"]],
    );
}

#[test]
fn test_comma_escape() {
    test_inexact::<Vec<String>>("\t,,\n,\n\tfoo", &vec![s(","), s("foo")]);

    test(
        "\
A
,,
B",
        &outline!["A", ",", "B"],
    );
}

#[test]
fn test_struct() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Simple {
        name_text: String,
        x: i32,
        y: i32,
    }

    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Vectored {
        v: Vec<i32>,
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
\t3
chaff",
        &Vectored { v: vec![1, 2, 3] },
    );
}

#[test]
fn test_struct_contents() {
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
}

#[test]
fn test_nesting_contents() {
    use std::collections::BTreeMap;
    use std::iter::FromIterator;

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
fn test_value_length() {
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
	rindfleischetikettierungsüberwachungsaufgabenübertragungsgesetz
	betäubungsmittelverschreibungsverordnung
	rechtsschutzversicherungsgesellschaften
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
