use std::io::{self, Read};

use idm::Raw;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
struct Outline {
    #[serde(default)]
    _attributes: Vec<(String, String)>,
    #[serde(default)]
    _contents: Vec<(Raw<String>, Outline)>,
}

fn main() {
    let mut buffer = String::new();
    io::stdin()
        .read_to_string(&mut buffer)
        .expect("Reading from stdin failed");

    let outline = match idm::from_str::<Outline>(&buffer) {
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(2);
        }
        Ok(val) => val,
    };

    let reser = idm::to_string_styled_like(&buffer, &outline)
        .expect("Reserialization failed");

    print!("{}", reser);

    if buffer != reser {
        std::process::exit(1);
    }
}
