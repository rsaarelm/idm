use crate::{from_str, outline, to_string};
use pretty_assertions::assert_eq;
use serde_derive::{Deserialize, Serialize};

type Outline = crate::Outline<Option<String>>;

#[test]
fn test_atom() {
    assert_eq!(from_str::<u32>("123").unwrap(), 123);
    assert_eq!(from_str::<f32>("2.718").unwrap(), 2.718);
    assert_eq!(from_str::<String>("xyzzy").unwrap(), "xyzzy");
    assert_eq!(from_str::<String>("one\ntwo").unwrap(), "one\ntwo");
}

#[test]
fn test_simple_sequence() {
    assert_eq!(from_str::<Vec<i32>>("").unwrap(), vec![]);

    assert_eq!(
        from_str::<Vec<String>>("foo\nbar\nbaz").unwrap(),
        vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]
    );

    assert_eq!(from_str::<Vec<i32>>("1\n2\n3").unwrap(), vec![1, 2, 3]);
}

#[test]
fn test_nested_sequence() {
    // Inline inner form matrix
    assert_eq!(
        from_str::<Vec<Vec<i32>>>(
            "\
1 2
3 4"
        )
        .unwrap(),
        vec![vec![1, 2], vec![3, 4]]
    );

    assert_eq!(
        from_str::<[[i32; 2]; 2]>(
            "\
1 2
3 4"
        )
        .unwrap(),
        [[1, 2], [3, 4]]
    );

    // Outline inner form
    assert_eq!(
        from_str::<Vec<Vec<i32>>>(
            "\
,
\t1
\t2
,
\t3
\t4"
        )
        .unwrap(),
        vec![vec![1, 2], vec![3, 4]]
    );

    // Outline list of matrices.
    assert_eq!(
        from_str::<Vec<Vec<Vec<i32>>>>(
            "\
,
\t1 2
\t3 4
,
\t5 6
\t7 8"
        )
        .unwrap(),
        vec![vec![vec![1, 2], vec![3, 4]], vec![vec![5, 6], vec![7, 8]]]
    );
}

#[test]
fn test_simple_tuple() {
    assert_eq!(from_str::<(i32, i32)>("1\n2").unwrap(), (1, 2));
    assert_eq!(
        from_str::<Vec<(i32, i32)>>("1 2\n3 4").unwrap(),
        vec![(1, 2), (3, 4)]
    );
}

#[test]
fn test_section_tuple() {
    assert_eq!(from_str::<Vec<(i32, i32)>>("1\n\t2").unwrap(), vec![(1, 2)]);
    assert_eq!(
        from_str::<Vec<(i32, i32, i32)>>("1\n\t2\n\t3").unwrap(),
        vec![(1, 2, 3)]
    );
    assert_eq!(
        from_str::<Vec<(i32, i32)>>("1\n\t2\n3\n\t4").unwrap(),
        vec![(1, 2), (3, 4)]
    );
}

#[test]
fn test_tuple_tail_sequence_continuation() {
    assert_eq!(
        from_str::<(i32, i32, Vec<i32>)>("1\n2\n3\n4").unwrap(),
        (1, 2, vec![3, 4])
    );
}

#[test]
fn test_option_tuple() {
    assert_eq!(
        from_str::<Vec<(Option<i32>, i32)>>("1\n\t2").unwrap(),
        vec![(Some(1), 2)]
    );
    assert_eq!(
        from_str::<Vec<(Option<i32>, i32)>>(",\n\t2").unwrap(),
        vec![(None, 2)]
    );
    assert_eq!(
        from_str::<Vec<(Option<i32>, i32)>>("\t2").unwrap(),
        vec![(None, 2)]
    );
}

#[test]
fn test_canonical_outline() {
    assert_eq!(from_str::<Outline>("").unwrap(), Outline::default());

    assert_eq!(from_str::<Outline>("Xyzzy").unwrap(), outline!["Xyzzy"]);

    assert_eq!(
        from_str::<Outline>(
            "\
Xyzzy
\tPlugh"
        )
        .unwrap(),
        outline![["Xyzzy", "Plugh"]]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
Xyzzy
Plugh"
        )
        .unwrap(),
        outline!["Xyzzy", "Plugh"]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
\tPlugh"
        )
        .unwrap(),
        outline![[, "Plugh"]]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
,
\tPlugh"
        )
        .unwrap(),
        outline![[, "Plugh"]]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
Xyzzy
\tPlugh
Qux
\tQuux"
        )
        .unwrap(),
        outline![["Xyzzy", "Plugh"], ["Qux", "Quux"]]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
Xyzzy
\tPlugh
\tBlorb
Qux
\tQuux"
        )
        .unwrap(),
        outline![["Xyzzy", "Plugh", "Blorb"], ["Qux", "Quux"]]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
Xyzzy
\tPlugh
,
\tQuux"
        )
        .unwrap(),
        outline![["Xyzzy", "Plugh"], [, "Quux"]]
    );

    assert_eq!(
        from_str::<Outline>(
            "\
A
,
\tC",
        )
        .unwrap(),
        outline!["A", [, "C"]]
    );
}

#[test]
fn test_blank_line() {
    type Outline = crate::Outline<Option<String>>;

    assert_eq!(
        from_str::<Outline>("a\n\tb\n\n\tc").unwrap(),
        outline![["a", ["b", ""], "c"]]
    );
}

#[test]
fn test_comma_escape() {
    assert_eq!(
        from_str::<Vec<String>>(",,\n,\n\tfoo").unwrap(),
        vec![",", "foo"]
    );
}

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

#[test]
fn test_struct() {
    assert_eq!(
        from_str::<Simple>(
            "\
name-text: Foo bar
x: 1
y: 2"
        )
        .unwrap(),
        Simple {
            name_text: "Foo bar".into(),
            x: 1,
            y: 2
        }
    );

    assert_eq!(
        from_str::<Vectored>(
            "\
v: 1 2 3"
        )
        .unwrap(),
        Vectored { v: vec![1, 2, 3] }
    );

    assert_eq!(
        from_str::<Vectored>(
            "\
v:
\t1
\t2
\t3
blarp"
        )
        .unwrap(),
        Vectored { v: vec![1, 2, 3] }
    );

    assert_eq!(
        to_string(&Simple {
            name_text: "Foo bar".into(),
            x: 1,
            y: 2
        })
        .unwrap()
        .trim_end(),
        "\
name-text: Foo bar
x: 1
y: 2"
    );

    assert_eq!(
        to_string(&Vectored { v: vec![1, 2, 3] })
            .unwrap()
            .trim_end(),
        "v: 1 2 3"
    );
}

#[test]
fn test_struct_contents() {
    #[derive(Clone, Eq, PartialEq, Default, Debug, Serialize, Deserialize)]
    struct Contentful {
        x: i32,
        y: i32,
        _contents: String,
    }

    assert_eq!(
        from_str::<Contentful>(
            "\
x: 1
y: 2
Hello, world!
xyzzy"
        )
        .unwrap(),
        Contentful {
            x: 1,
            y: 2,
            _contents: "Hello, world!\nxyzzy".into()
        }
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

    let parsed_starmap = from_str::<BTreeMap<String, Star>>(STARMAP).unwrap();

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

    assert_eq!(parsed_starmap, starmap);

    // Roundtrip doesn't keep the formatting quite intact...
    assert_eq!(
        to_string(&starmap).unwrap().trim(),
        "\
Alpha Centauri
	age: 5300000000
	mass: 1.1
	Chiron
		orbit: 1.32
		mass: 1.33
	Eurytion
		orbit: 0.47
		mass: 0.08
Sol
	age: 4600000000
	mass: 1
	Earth
		orbit: 1
		mass: 1
	Mars
		orbit: 1.52
		mass: 0.1
	Mercury
		orbit: 0.39
		mass: 0.055
	Venus
		orbit: 0.72
		mass: 0.815"
    );
}

#[test]
fn test_serialize_atom() {
    assert_eq!(to_string(&1u32).unwrap().trim(), "1");
    assert_eq!(to_string(&"foo").unwrap().trim(), "foo");
    assert_eq!(to_string(&"foo bar").unwrap().trim(), "foo bar");
    assert_eq!(to_string(&"foo bar\nbaz").unwrap().trim(), "foo bar\nbaz");
}

#[test]
fn test_serialize_seq() {
    // Toplevel seq can't be inlined because it always has a missing dummy
    // headline at depth -1 that forces the rest into outline mode.

    // Tuple and seq serializers are different. Test this with both arrays
    // (treated as tuples) and vecs (treated as seqs).
    assert_eq!(
        to_string(&["foo", "bar", "baz"]).unwrap().trim_end(),
        "foo\nbar\nbaz"
    );

    assert_eq!(
        to_string(&vec!["foo", "bar", "baz"]).unwrap().trim_end(),
        "foo\nbar\nbaz"
    );

    assert_eq!(
        to_string(&[&[1u32, 2], &[3, 4]]).unwrap().trim_end(),
        "1 2\n3 4"
    );

    assert_eq!(
        to_string(&vec![&vec![1u32, 2], &vec![3, 4]])
            .unwrap()
            .trim_end(),
        "1 2\n3 4"
    );

    assert_eq!(
        to_string(&[&[&[1u32, 2], &[3, 4]], &[&[5, 6], &[7, 8]]])
            .unwrap()
            .trim_end(),
        "\t1 2\n\t3 4\n,\n\t5 6\n\t7 8"
    );
}

#[test]
fn test_serialize_outline() {
    assert_eq!(to_string(&outline![]).unwrap().trim_end(), "");

    assert_eq!(to_string(&outline!["A"]).unwrap().trim_end(), "A");

    assert_eq!(to_string(&outline!["A", "B"]).unwrap().trim_end(), "A\nB");

    assert_eq!(
        to_string(&outline![["A", "B"]]).unwrap().trim_end(),
        "A\n\tB"
    );

    assert_eq!(to_string(&outline![[, "B"]]).unwrap().trim_end(), "\tB");

    assert_eq!(
        to_string(&outline!["A", "", "B"]).unwrap().trim_end(),
        "A\n\nB"
    );

    assert_eq!(
        to_string(&outline!["A", "  B"]).unwrap().trim_end(),
        "A\n  B"
    );

    assert_eq!(
        to_string(&outline!["A", [, "B"]]).unwrap().trim_end(),
        "A\n,\n\tB"
    );

    // Escaping literal comma.
    assert_eq!(
        to_string(&outline!["A", ",", "B"]).unwrap().trim_end(),
        "A\n,,\nB"
    );

    assert_eq!(
        to_string(&outline![["A", "B", [, "D"]]])
            .unwrap()
            .trim_end(),
        "A\n\tB\n\t,\n\t\tD"
    );
}
