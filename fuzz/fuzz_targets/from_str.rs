#![no_main]
use std::collections::BTreeMap;
use serde::{Serialize, Deserialize};

use libfuzzer_sys::fuzz_target;

#[derive(Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Outline(Vec<(String, Outline)>);

#[derive(Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DataOutline((BTreeMap<String, String>, Vec<(String, DataOutline)>));

fuzz_target!(|data: &str| {
    let _ = idm::from_str::<String>(data);
    let _ = idm::from_str::<Vec<String>>(data);
    let _ = idm::from_str::<Vec<Vec<String>>>(data);
    let _ = idm::from_str::<f32>(data);
    let _ = idm::from_str::<Vec<f32>>(data);
    let _ = idm::from_str::<Vec<Vec<f32>>>(data);
    let _ = idm::from_str::<Outline>(data);
    let _ = idm::from_str::<DataOutline>(data);
});
