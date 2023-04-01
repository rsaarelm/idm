use std::io::Read;

fn main() {
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf).unwrap();

    let json: serde_json::Value =
        serde_json::from_str(&buf).expect("Failed to parse JSON");

    print!(
        "{}",
        idm::to_string(&json).expect("Failed to serialize to IDM")
    );
}
