use std::error;
use std::fmt;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Eq, PartialEq, Debug, Default)]
pub struct Error(pub String);

impl serde::de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error(format!("{}", msg))
    }
}

impl serde::ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error(format!("{}", msg))
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[macro_export(local_inner_macros)]
macro_rules! err {
    () => {
        Err($crate::error::Error("".to_string()))
    };

    ($fmt:expr) => {
        Err($crate::error::Error(std::format!($fmt)))
    };

    ($fmt:expr, $($arg:expr),*) => {
        Err($crate::error::Error(std::format!($fmt, $($arg),*)))
    };
}
