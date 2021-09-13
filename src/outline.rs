use serde::{Deserialize, Serialize};
use serde_bytes::{ByteBuf, Bytes};
use std::{fmt, ops::Deref, ops::DerefMut};

/// Wrapper type for section headlines that will force an IDM structure
/// including it to be parsed in raw mode.
#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Raw<T>(pub T);

impl<T: AsRef<[u8]>> serde::Serialize for Raw<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Convert headline to bytes to trigger special outline
        // serialization.
        Bytes::new(self.0.as_ref()).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Raw<String> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // Deserialize byte buffer to trigger outline mode, convert to String.
        let ret: ByteBuf = serde::Deserialize::deserialize(deserializer)?;
        let ret = String::from_utf8(ret.into_vec())
            .map_err(serde::de::Error::custom)?;
        Ok(Raw(ret))
    }
}

impl<T> Deref for Raw<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Raw<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> AsRef<T> for Raw<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl AsRef<str> for Raw<String> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<T: fmt::Debug> fmt::Debug for Raw<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::fmt(self, f)
    }
}

impl<T: fmt::Display> fmt::Display for Raw<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::fmt(self, f)
    }
}

#[derive(Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Section(pub Raw<String>, pub Outline);

/// Canonical Outline datatype.
#[derive(Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Outline(pub Vec<Section>);

impl Deref for Outline {
    type Target = Vec<Section>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::iter::FromIterator<(String, Outline)> for Outline {
    fn from_iter<U: IntoIterator<Item = (String, Outline)>>(iter: U) -> Self {
        Outline(
            iter.into_iter()
                .map(|(head, body)| Section(Raw(head), body))
                .collect(),
        )
    }
}

impl fmt::Debug for Outline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn print(
            f: &mut fmt::Formatter,
            depth: usize,
            otl: &Outline,
        ) -> fmt::Result {
            for Section(title, body) in &otl.0 {
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
        ($arg.into(), outline![$($child),+])
    };
    ($arg:expr) => {
        ($arg.into(), $crate::outline::Outline::default())
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
