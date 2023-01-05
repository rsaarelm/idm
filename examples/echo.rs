use std::io::Read;

use idm;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

// Deserialize input from stdin into data outline and serialize the outline to
// stdout. Use this to diagnose any problems with parsing handwritten outlines
// files into a data outline structure.

#[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
struct DataOutline((IndexMap<String, String>,), Vec<((String,), DataOutline)>);

fn main() {
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf).unwrap();

    let outline = idm::from_str::<DataOutline>(&buf).unwrap();

    let reser = idm::to_string_styled_like(&buf, &outline).unwrap();
    print!("{}", reser);
}
