use std::io::Read;

use idm;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};

// Convert v02 attributes: to v03 :attributes.
// One-off tool, probably won't be maintained.

#[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
struct Outline(Vec<(String, Outline)>);

impl Outline {
    fn migrate(&mut self) {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"^(?P<a>[a-z][a-z0-9-]+):(?P<b>$|\s)").unwrap();
        }

        let mut in_header = true;
        for (head, body) in self.0.iter_mut() {
            if RE.is_match(&head) && in_header {
                *head = RE.replace(&head, ":$a$b").to_string();
            } else {
                in_header = false;
            }
            body.migrate();
        }
    }
}

fn main() {
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf).unwrap();

    let mut outline = idm::from_str::<Outline>(&buf).unwrap();
    outline.migrate();
    print!("{}", idm::to_string_styled_like(&buf, &outline).unwrap());
}
