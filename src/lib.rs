mod de;
pub use de::from_str;

mod error;
pub use error::{Error, Result};

mod fragment;

#[cfg(test)]
mod outline;

mod parse;

mod parser;

mod ser;
pub use ser::{to_string, to_string_styled, to_string_styled_like, Style};

mod util;
pub use util::{
    guess_indent_style, infer_indent_style, transmute, DefaultDash,
};

/*
 // FIXME: Rewrite README
#[cfg(doctest)]
#[doc = include_str!("../README.md")]
pub struct ReadmeDoctests;
*/

#[cfg(test)]
mod tests;
