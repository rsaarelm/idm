use serde::{Deserialize, Serialize};
use serde_bytes::{ByteBuf, Bytes};
use std::{fmt, ops::Deref};

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Section(pub String, pub Outline);

impl serde::Serialize for Section {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Convert headline to bytes to trigger special outline
        // serialization.
        (Bytes::new(self.0.as_bytes()), &self.1).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Section {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // Deserialize byte buffer to trigger outline mode, convert to String.
        let (headline, body): (ByteBuf, Outline) =
            serde::Deserialize::deserialize(deserializer)?;
        let headline = String::from_utf8(headline.into_vec())
            .map_err(serde::de::Error::custom)?;
        Ok(Section(headline, body))
    }
}

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
                .map(|(head, body)| Section(head, body))
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
