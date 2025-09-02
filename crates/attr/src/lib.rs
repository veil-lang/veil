#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Attributes/metadata crate (placeholder) with a stable `AttributeBag`.
//!
//! Goals:
//! - Deterministic ordering and iteration for reproducible builds
//! - Typed accessors for common scalar types
//! - Minimal, dependency-light API suitable for transport between passes
//! - Optional (feature-gated) serde support for caching and snapshots
//!
//! Notes:
//! - We use `BTreeMap` to guarantee deterministic ordering.
//! - A stable fingerprint is provided via a simple FNV-1a over a canonical
//!   textual representation (stable across platforms).

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt;

/// A single attribute value.
///
/// Variants are intentionally compact and self-contained to minimize coupling.
/// Floats are represented as f64; for canonicalization we use `to_bits()`
/// to ensure stability across formatting differences.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, PartialEq)]
pub enum AttributeValue {
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Str(String),
    List(Vec<AttributeValue>),
    Map(BTreeMap<String, AttributeValue>),
}

impl fmt::Debug for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AttributeValue::Bool(v) => write!(f, "Bool({v})"),
            AttributeValue::Int(v) => write!(f, "Int({v})"),
            AttributeValue::UInt(v) => write!(f, "UInt({v})"),
            AttributeValue::Float(v) => write!(f, "Float({v})"),
            AttributeValue::Str(s) => write!(f, "Str({s:?})"),
            AttributeValue::List(xs) => f.debug_list().entries(xs).finish(),
            AttributeValue::Map(m) => f.debug_map().entries(m.iter()).finish(),
        }
    }
}

impl From<bool> for AttributeValue {
    fn from(v: bool) -> Self {
        AttributeValue::Bool(v)
    }
}
impl From<i64> for AttributeValue {
    fn from(v: i64) -> Self {
        AttributeValue::Int(v)
    }
}
impl From<i32> for AttributeValue {
    fn from(v: i32) -> Self {
        AttributeValue::Int(v as i64)
    }
}
impl From<u64> for AttributeValue {
    fn from(v: u64) -> Self {
        AttributeValue::UInt(v)
    }
}
impl From<u32> for AttributeValue {
    fn from(v: u32) -> Self {
        AttributeValue::UInt(v as u64)
    }
}
impl From<f64> for AttributeValue {
    fn from(v: f64) -> Self {
        AttributeValue::Float(v)
    }
}
impl From<f32> for AttributeValue {
    fn from(v: f32) -> Self {
        AttributeValue::Float(v as f64)
    }
}
impl From<String> for AttributeValue {
    fn from(v: String) -> Self {
        AttributeValue::Str(v)
    }
}
impl From<&str> for AttributeValue {
    fn from(v: &str) -> Self {
        AttributeValue::Str(v.to_string())
    }
}
impl From<Vec<AttributeValue>> for AttributeValue {
    fn from(v: Vec<AttributeValue>) -> Self {
        AttributeValue::List(v)
    }
}
impl From<BTreeMap<String, AttributeValue>> for AttributeValue {
    fn from(v: BTreeMap<String, AttributeValue>) -> Self {
        AttributeValue::Map(v)
    }
}

/// Bag of attributes with deterministic ordering.
///
/// - Keys are unique strings.
/// - Values are `AttributeValue`.
/// - Iteration order is sorted by key (via BTreeMap).
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Default, PartialEq)]
pub struct AttributeBag {
    inner: BTreeMap<String, AttributeValue>,
}

impl fmt::Debug for AttributeBag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.inner.iter()).finish()
    }
}

impl AttributeBag {
    /// Create an empty attribute bag.
    pub fn new() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }

    /// Number of entries.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// True if empty.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Insert or replace an attribute by key.
    pub fn insert<K, V>(&mut self, key: K, value: V) -> Option<AttributeValue>
    where
        K: Into<String>,
        V: Into<AttributeValue>,
    {
        self.inner.insert(key.into(), value.into())
    }

    /// Builder-style set that returns self for chaining.
    pub fn set<K, V>(mut self, key: K, value: V) -> Self
    where
        K: Into<String>,
        V: Into<AttributeValue>,
    {
        self.insert(key, value);
        self
    }

    /// Remove an attribute by key.
    pub fn remove(&mut self, key: &str) -> Option<AttributeValue> {
        self.inner.remove(key)
    }

    /// Returns true if key exists.
    pub fn contains(&self, key: &str) -> bool {
        self.inner.contains_key(key)
    }

    /// Get a reference to the raw attribute value.
    pub fn get(&self, key: &str) -> Option<&AttributeValue> {
        self.inner.get(key)
    }

    /// Get a string value by key.
    pub fn get_str(&self, key: &str) -> Option<&str> {
        match self.inner.get(key) {
            Some(AttributeValue::Str(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Get a bool value by key.
    pub fn get_bool(&self, key: &str) -> Option<bool> {
        match self.inner.get(key) {
            Some(AttributeValue::Bool(v)) => Some(*v),
            _ => None,
        }
    }

    /// Get an i64 value by key.
    pub fn get_i64(&self, key: &str) -> Option<i64> {
        match self.inner.get(key) {
            Some(AttributeValue::Int(v)) => Some(*v),
            _ => None,
        }
    }

    /// Get a u64 value by key.
    pub fn get_u64(&self, key: &str) -> Option<u64> {
        match self.inner.get(key) {
            Some(AttributeValue::UInt(v)) => Some(*v),
            _ => None,
        }
    }

    /// Get an f64 value by key.
    pub fn get_f64(&self, key: &str) -> Option<f64> {
        match self.inner.get(key) {
            Some(AttributeValue::Float(v)) => Some(*v),
            _ => None,
        }
    }

    /// Iterate key-value pairs in deterministic order.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &AttributeValue)> {
        self.inner.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Merge another bag into this one.
    ///
    /// When `overwrite` is true, values in `other` replace existing values.
    /// Otherwise, existing keys are preserved.
    pub fn merge(&mut self, other: &AttributeBag, overwrite: bool) {
        for (k, v) in other.inner.iter() {
            if overwrite || !self.inner.contains_key(k) {
                self.inner.insert(k.clone(), v.clone());
            }
        }
    }

    /// Return a stable canonical string for the bag contents.
    ///
    /// This canonical form is deterministic across runs and platforms
    /// and can be used for external hashing or snapshotting.
    pub fn to_canonical_string(&self) -> String {
        let mut s = String::new();
        Self::write_canonical_map(&mut s, &self.inner);
        s
    }

    /// Compute a stable FNV-1a 64-bit fingerprint of the canonical string.
    ///
    /// This is a deterministic, non-cryptographic hash tailored for cache keys.
    /// If you need cryptographic properties, hash `to_canonical_string()` with a
    /// cryptographic hash in a higher-level crate.
    pub fn fingerprint(&self) -> u64 {
        fn fnv1a64(bytes: &[u8]) -> u64 {
            // FNV-1a 64-bit
            let mut hash: u64 = 0xcbf29ce484222325;
            for &b in bytes {
                hash ^= u64::from(b);
                hash = hash.wrapping_mul(0x100000001b3);
            }
            hash
        }
        fnv1a64(self.to_canonical_string().as_bytes())
    }

    /// Hex-encoded fingerprint helper.
    pub fn fingerprint_hex(&self) -> String {
        format!("{:016x}", self.fingerprint())
    }

    // ---- Canonicalization helpers ----

    fn write_canonical_val(out: &mut String, v: &AttributeValue) {
        match v {
            AttributeValue::Bool(b) => {
                out.push_str(if *b { "b:1" } else { "b:0" });
            }
            AttributeValue::Int(i) => {
                out.push_str("i:");
                out.push_str(&i.to_string());
            }
            AttributeValue::UInt(u) => {
                out.push_str("u:");
                out.push_str(&u.to_string());
            }
            AttributeValue::Float(f) => {
                // Use bit pattern for stability, including +0.0 vs -0.0 and NaNs.
                out.push_str("f:0x");
                out.push_str(&format!("{:016x}", f.to_bits()));
            }
            AttributeValue::Str(s) => {
                out.push_str("s:");
                Self::write_escaped(out, s);
            }
            AttributeValue::List(xs) => {
                out.push('[');
                let mut first = true;
                for x in xs {
                    if !first {
                        out.push(',');
                    }
                    first = false;
                    Self::write_canonical_val(out, x);
                }
                out.push(']');
            }
            AttributeValue::Map(m) => {
                Self::write_canonical_map(out, m);
            }
        }
    }

    fn write_canonical_map(out: &mut String, m: &BTreeMap<String, AttributeValue>) {
        out.push('{');
        let mut first = true;
        for (k, v) in m.iter() {
            if !first {
                out.push(',');
            }
            first = false;
            Self::write_escaped(out, k);
            out.push('=');
            Self::write_canonical_val(out, v);
        }
        out.push('}');
    }

    fn write_escaped(out: &mut String, s: &str) {
        for ch in s.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '{' => out.push_str("\\{"),
                '}' => out.push_str("\\}"),
                '[' => out.push_str("\\["),
                ']' => out.push_str("\\]"),
                '=' => out.push_str("\\="),
                ',' => out.push_str("\\,"),
                ':' => out.push_str("\\:"),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                c => out.push(c),
            }
        }
    }
}

// --------- Convenience builders ---------

impl AttributeBag {
    /// Build a bag from a slice of key/value pairs.
    pub fn from_pairs<'a, I, K, V>(pairs: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<Cow<'a, str>>,
        V: Into<AttributeValue>,
    {
        let mut m = BTreeMap::new();
        for (k, v) in pairs {
            m.insert(k.into().into_owned(), v.into());
        }
        Self { inner: m }
    }
}

/// Helper macro to build an `AttributeBag` concisely.
///
/// Example:
/// let bag = attr_bag! {
///   "opt" => true,
///   "level" => 2i64,
///   "name" => "foo",
///   "features" => ["a".into(), "b".into()],
/// };
#[macro_export]
macro_rules! attr_bag {
    () => { $crate::AttributeBag::new() };
    ( $( $k:expr => $v:expr ),+ $(,)? ) => {{
        let mut __bag = $crate::AttributeBag::new();
        $(
            __bag.insert($k.to_string(), $v);
        )+
        __bag
    }};
}

// --------- Typed getters on AttributeValue ---------

impl AttributeValue {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            AttributeValue::Bool(v) => Some(*v),
            _ => None,
        }
    }
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            AttributeValue::Int(v) => Some(*v),
            _ => None,
        }
    }
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            AttributeValue::UInt(v) => Some(*v),
            _ => None,
        }
    }
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            AttributeValue::Float(v) => Some(*v),
            _ => None,
        }
    }
    pub fn as_str(&self) -> Option<&str> {
        match self {
            AttributeValue::Str(s) => Some(s.as_str()),
            _ => None,
        }
    }
    pub fn as_list(&self) -> Option<&[AttributeValue]> {
        match self {
            AttributeValue::List(xs) => Some(xs.as_slice()),
            _ => None,
        }
    }
    pub fn as_map(&self) -> Option<&BTreeMap<String, AttributeValue>> {
        match self {
            AttributeValue::Map(m) => Some(m),
            _ => None,
        }
    }
}

// --------- Tests ---------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_and_fingerprint_stable() {
        let mut a = AttributeBag::new()
            .set("bool", true)
            .set("i", 42i64)
            .set("u", 7u64)
            .set("f", 3.5f64)
            .set("s", "hello");

        let mut m = BTreeMap::new();
        m.insert("k".to_string(), AttributeValue::from("v"));
        a.insert("m", AttributeValue::from(m));

        let c1 = a.to_canonical_string();
        let fp1 = a.fingerprint();
        let c2 = a.to_canonical_string();
        let fp2 = a.fingerprint();
        assert_eq!(c1, c2);
        assert_eq!(fp1, fp2);
    }

    #[test]
    fn typed_getters_work() {
        let bag = AttributeBag::new()
            .set("b", true)
            .set("i", 1i64)
            .set("u", 2u64)
            .set("f", 1.25f64)
            .set("s", "x");

        assert_eq!(bag.get_bool("b"), Some(true));
        assert_eq!(bag.get_i64("i"), Some(1));
        assert_eq!(bag.get_u64("u"), Some(2));
        assert_eq!(bag.get_f64("f"), Some(1.25));
        assert_eq!(bag.get_str("s"), Some("x"));
        assert!(bag.get_str("nope").is_none());
    }

    #[test]
    fn merge_behaves_as_expected() {
        let mut a = AttributeBag::new().set("x", 1i64).set("y", "a");
        let b = AttributeBag::new().set("x", 2i64).set("z", "b");

        let mut c = a.clone();
        c.merge(&b, false); // preserve existing
        assert_eq!(c.get_i64("x"), Some(1));
        assert_eq!(c.get_str("z"), Some("b"));

        a.merge(&b, true); // overwrite
        assert_eq!(a.get_i64("x"), Some(2));
        assert_eq!(a.get_str("y"), Some("a"));
        assert_eq!(a.get_str("z"), Some("b"));
    }
}
