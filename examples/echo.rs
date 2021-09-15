use std::io::{self, Read};

// Read outline from stdin, verify that it's parsed and it reserializes
// identical to original, then print out the Outline data structure.
//
// NB. Reserialization can fail to match with valid files if they use space
// based indentation with odd indent lengths.

fn main() {
    let mut buffer = String::new();
    io::stdin()
        .read_to_string(&mut buffer)
        .expect("Reading from stdin failed");

    let idm = match idm::from_str::<idm::Outline>(&buffer) {
        Err(e) => {
            println!("{}", e);
            return;
        }
        Ok(val) => val,
    };

    let reser = idm::to_string_styled_like(&buffer, &idm)
        .expect("Reserialization failed");
    assert_eq!(buffer, reser);

    println!("{:?}", idm);
}
