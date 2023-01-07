pub mod de;
pub use de::{from_reader, from_str, Deserializer};

pub use de::parse::is_whitespace;

mod error;
pub(crate) use error::err;
pub use error::{Error, Result};

mod outline;
#[allow(unused_imports)]
pub(crate) use outline::outline;

pub mod ser;
pub use ser::{to_string, to_string_styled, to_string_styled_like, Serializer};

mod util;
pub use util::{transmute, AsVec, DefaultDash, UnderlineSpaces};

#[cfg(doctest)]
#[doc = include_str!("../README.md")]
pub struct ReadmeDoctests;

#[cfg(test)]
mod tests;
