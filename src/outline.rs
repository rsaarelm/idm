use serde::{Deserialize, Serialize};
use std::{fmt, ops::Deref};

#[derive(Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Outline(Vec<((String,), Outline)>);

impl Deref for Outline {
    type Target = Vec<((String,), Outline)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::iter::FromIterator<((String,), Outline)> for Outline {
    fn from_iter<U: IntoIterator<Item = ((String,), Outline)>>(
        iter: U,
    ) -> Self {
        Outline(iter.into_iter().collect())
    }
}

impl fmt::Debug for Outline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn print(
            f: &mut fmt::Formatter,
            depth: usize,
            otl: &Outline,
        ) -> fmt::Result {
            for ((title,), body) in &otl.0 {
                for _ in 0..depth {
                    write!(f, "  ")?;
                }
                writeln!(f, "{:?}", title)?;
                print(f, depth + 1, body)?;
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
        (($arg.into(),), outline![$($child),+])
    };
    ($arg:expr) => {
        (($arg.into(),), $crate::outline::Outline::default())
    };
}

/// Construct Outline literals.
#[macro_export]
macro_rules! outline {
    [$($arg:tt),*] => {
        {
            use std::iter::FromIterator;
            let ret: $crate::outline::Outline =
                $crate::outline::Outline::from_iter(vec![
                    $($crate::outline_elt!($arg)),*
                ].into_iter());
            ret
        }
    }
}
