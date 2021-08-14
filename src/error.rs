use std::{error, fmt, borrow::Cow};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Eq, PartialEq, Debug, Default)]
pub struct Error {
    file_name: Option<String>,
    line_num: Option<usize>,
    msg: Cow<'static, str>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut need_colon = false;
        if let Some(file_name) = self.file_name.as_ref() {
            need_colon = true;
            write!(f, "{}", file_name)?;
        }
        if let Some(line_num) = self.line_num {
            if need_colon {
                write!(f, ", ")?;
            } else {
                need_colon = true;
            }
            write!(f, "line {}", line_num)?;
        }
        if need_colon {
            write!(f, ": ")?;
        }

        write!(f, "{}", self.msg)
    }
}

impl serde::de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::new(format!("{}", msg))
    }
}

impl serde::ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::new(format!("{}", msg))
    }
}

impl error::Error for Error {}

impl Error {
    pub fn new(msg: impl Into<Cow<'static, str>>) -> Error {
        Error {
            msg: msg.into(),
            line_num: None,
            file_name: None,
        }
    }

    pub fn line_num(mut self, line_num: usize) -> Error {
        self.line_num = Some(line_num);
        self
    }

    pub fn file_name(mut self, file_name: String) -> Error {
        self.file_name = Some(file_name);
        self
    }
}

#[macro_export(local_inner_macros)]
macro_rules! err {
    () => {
        Err($crate::error::Error(""))
    };

    ($fmt:expr) => {
        Err($crate::error::Error::new($fmt))
    };

    ($fmt:expr, $($arg:expr),*) => {
        Err($crate::error::Error::new(std::format!($fmt, $($arg),*)))
    };
}
