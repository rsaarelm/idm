pub mod de;
pub use de::{from_reader, from_str, Deserializer};

mod error;
pub use error::{Error, Result};

mod outline;

pub mod ser;
pub use ser::{to_string, to_string_styled, to_string_styled_like, Serializer};

mod util;
pub use util::{
    transmute, AsVec, CharExt, ColonPair, DefaultDash, UnderlineSpaces,
};

#[cfg(doctest)]
#[doc = include_str!("../README.md")]
pub struct ReadmeDoctests;

#[cfg(test)]
mod tests;
