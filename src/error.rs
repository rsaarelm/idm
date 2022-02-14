use std::{borrow::Cow, error, fmt};

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

    pub fn line_num(&self) -> Option<usize> {
        self.line_num
    }

    pub fn file_name(&self) -> Option<&str> {
        self.file_name.as_ref().map(|s| s.as_str())
    }

    pub fn with_line_num(mut self, line_num: usize) -> Error {
        self.line_num = Some(line_num);
        self
    }

    pub fn with_file_name(mut self, file_name: String) -> Error {
        self.file_name = Some(file_name);
        self
    }

    pub(crate) fn infer_line_num(
        mut self,
        input: &str,
        error_slice: &str,
    ) -> Error {
        // hax
        if let Some(input_so_far) = input.get(
            0..unsafe {
                error_slice.as_ptr().offset_from(input.as_ptr()) as usize
            },
        ) {
            self.line_num =
                Some(1 + input_so_far.chars().filter(|&c| c == '\n').count())
        }

        self
    }
}

impl<'a> From<&'a str> for Error {
    fn from(s: &'a str) -> Self {
        Error::new(s.to_string())
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
