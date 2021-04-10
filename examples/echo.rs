use idm::Outline;
use std::io::{self, prelude::*};

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();

    let otl = idm::from_str::<Outline<Option<String>>>(&buf).unwrap();

    let roundtrip =
        idm::to_string(&otl).expect("Roundtrip serialization failed");

    if buf == roundtrip {
        print!("{:?}", otl);
        println!("\x1b[1;32mRoundtrip serialization identical\x1b[0m");
    } else {
        println!("{}", roundtrip);
        println!("\x1b[1;31mRoundtrip error\x1b[0m");
    }
}
