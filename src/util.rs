use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::{from_str, to_string};
use serde::{de, de::DeserializeOwned, ser, Deserialize, Serialize};

/// Try to convert a value of one type into a value of a different type that
/// has the same IDM serialization.
pub fn transmute<T: Serialize, U: DeserializeOwned>(e: &T) -> crate::Result<U> {
    let s = crate::to_string(e)?;
    crate::from_str(&s)
}

/// Wrapper that represents `Default` values with `"-"`, allowing them in
/// contexts that cannot represent an empty value.
///
/// ```
/// use idm::DefaultDash;
///
/// assert_eq!(
///     idm::from_str::<DefaultDash<String>>("-").unwrap(),
///     DefaultDash(String::default())
/// );
/// assert_eq!(
///     idm::from_str::<DefaultDash<String>>("xyzzy").unwrap(),
///     DefaultDash("xyzzy".to_string())
/// );
///
/// assert_eq!(
///     idm::to_string(&DefaultDash("xyzzy".to_string())).unwrap(),
///     "xyzzy"
/// );
/// assert_eq!(
///     idm::to_string(&DefaultDash(String::default())).unwrap(),
///     "-"
/// );
/// ```
#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct DefaultDash<T: Default>(pub T);

impl<'de, T: DeserializeOwned + Default> Deserialize<'de> for DefaultDash<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let ret = String::deserialize(deserializer)?;
        if ret == "-" {
            Ok(DefaultDash(Default::default()))
        } else {
            from_str(&ret).map(DefaultDash).map_err(de::Error::custom)
        }
    }
}

impl<T: Serialize + Default + PartialEq> Serialize for DefaultDash<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.0 == Default::default() {
            "-".serialize(serializer)
        } else {
            // XXX: It's up to the user to worry about non-default values of
            // the type serializing as a literal "-". Could serialize to
            // string here, error out on "-" and reserialize the string
            // otherwise, but this is simpler and probably faster.
            self.0.serialize(serializer)
        }
    }
}

impl<T: Default> Deref for DefaultDash<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Default> DerefMut for DefaultDash<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Wrapper type that replaces spaces with underscores when serializing.
///
/// Use this to make multi-word strings look like single words so they can be
/// used as items in a horizontal IDM sequence. Do not use with an inner type
/// that has actual underscores in its string representation, these will be
/// turned into spaces during the roundtrip. Do not use with strings that have
/// newlines in them.
#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct UnderlineSpaces<T>(pub T);

impl<'de, T: DeserializeOwned> Deserialize<'de> for UnderlineSpaces<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let ret = String::deserialize(deserializer)?.replace('_', " ");
        Ok(UnderlineSpaces(from_str(&ret).map_err(de::Error::custom)?))
    }
}

impl<T: Serialize> Serialize for UnderlineSpaces<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        to_string(&self.0)
            .map_err(ser::Error::custom)?
            .replace(' ', "_")
            .serialize(serializer)
    }
}

impl<T: Default> Deref for UnderlineSpaces<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Default> DerefMut for UnderlineSpaces<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A wrapper that causes the wrapped container to be serialized as a `Vec`
/// collected from its iterated values.
///
/// The item type of the container needs to be given as the second type
/// parameter.
#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct AsVec<T, U, SU>(pub T, pub PhantomData<U>, pub PhantomData<SU>);

impl<T, U, SU> AsVec<T, U, SU> {
    pub fn new(t: T) -> Self {
        AsVec(t, PhantomData, PhantomData)
    }
}

impl<'de, T: FromIterator<U>, U, SU: Into<U> + DeserializeOwned>
    Deserialize<'de> for AsVec<T, U, SU>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(AsVec(
            Vec::<SU>::deserialize(deserializer)?
                .into_iter()
                .map(Into::into)
                .collect(),
            PhantomData,
            PhantomData,
        ))
    }
}

impl<T: Clone + IntoIterator<Item = U>, U, SU: Serialize + From<U>> Serialize
    for AsVec<T, U, SU>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0
            .clone()
            .into_iter()
            .map(From::from)
            .collect::<Vec<SU>>()
            .serialize(serializer)
    }
}

impl<T, U, SU> Deref for AsVec<T, U, SU> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, U, SU> DerefMut for AsVec<T, U, SU> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
