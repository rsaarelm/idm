mod de;
pub use de::{from_str, Deserializer};

mod error;
pub use error::{Error, Result};

mod lexer;
mod outline;
mod parser;

mod ser;
pub use ser::{to_string, to_string_styled, to_string_styled_like, Style};

#[cfg(test)]
mod tests;

mod util;
pub use util::guess_indent_style;
