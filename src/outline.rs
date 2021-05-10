use serde_derive::{Deserialize, Serialize};
use std::fmt;

pub type Section<T> = (T, Outline<T>);

/// Canonical Outline datatype.
#[derive(Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Outline<T>(pub Vec<Section<T>>);

impl<T> std::ops::Deref for Outline<T> {
    type Target = Vec<Section<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> std::iter::FromIterator<(T, Outline<T>)> for Outline<T> {
    fn from_iter<U: IntoIterator<Item = Section<T>>>(iter: U) -> Self {
        Outline(iter.into_iter().collect())
    }
}

impl<T: fmt::Debug> fmt::Debug for Outline<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn print<T: fmt::Debug>(
            f: &mut fmt::Formatter,
            depth: usize,
            otl: &Outline<T>,
        ) -> fmt::Result {
            for (title, body) in &otl.0 {
                for _ in 0..depth {
                    write!(f, "  ")?;
                }
                writeln!(f, "{:?}", title)?;
                print(f, depth + 1, &body)?;
            }

            Ok(())
        }

        if self.is_empty() {
            writeln!(f, "ø")
        } else {
            print(f, 0, self)
        }
    }
}

#[macro_export(local_inner_macros)]
macro_rules! outline_elt {
    ([$arg:expr, $($child:tt),+]) => {
        (Some($arg.into()), outline![$($child),+])
    };
    ([, $($child:tt),+]) => {
        (None, outline![$($child),+])
    };
    ($arg:expr) => {
        (Some($arg.into()), $crate::outline::Outline::default())
    };
}

/// Construct Outline literals.
#[macro_export]
macro_rules! outline {
    [$($arg:tt),*] => {
        {
            use std::iter::FromIterator;
            let ret: $crate::outline::Outline<Option<String>> =
                $crate::outline::Outline::from_iter(vec![
                    $($crate::outline_elt!($arg)),*
                ].into_iter());
            ret
        }
    }
}
