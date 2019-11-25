//! Parsing via regular expressions using format syntax
//!
//! Derive will require attribute reformation to specify format string,
//! which will be treated as format string -> regular expression string
//!
//! Types implementing `Reformation` by default:
//!
//! + signed integers: `i8` `i16` `i32` `i64` `i128` `isize`
//! + unsigned integers: `u8` `u16` `u32` `u64` `u128` `usize`
//! + floats: `f32` `f64`
//! + `String`, &str
//! + `char`
//!
//! ## Structs
//!
//! ```
//! use reformation::Reformation;
//!
//! #[derive(Reformation, Debug)]
//! #[reformation(r"{year}-{month}-{day} {hour}:{minute}")]
//! struct Date{
//!     year: u16,
//!     month: u8,
//!     day: u8,
//!     hour: u8,
//!     minute: u8,
//! }
//!
//! fn main(){
//!     let date = Date::parse("2018-12-22 20:23").unwrap();
//!
//!     assert_eq!(date.year, 2018);
//!     assert_eq!(date.month, 12);
//!     assert_eq!(date.day, 22);
//!     assert_eq!(date.hour, 20);
//!     assert_eq!(date.minute, 23);
//! }
//! ```
//!
//! ## Tuple Structs
//!
//! ```
//! use reformation::Reformation;
//!
//! #[derive(Reformation)]
//! #[reformation(r"{} -> {}")]
//! struct Predicate(Empty, char);
//!
//! #[derive(Reformation, Debug, PartialEq)]
//! #[reformation(r"Empty")]
//! struct Empty;
//!
//! fn main(){
//!     let p = Predicate::parse("Empty -> X").unwrap();
//!     assert_eq!(p.0, Empty);
//!     assert_eq!(p.1, 'X');
//! }
//! ```
//!
//! ## Enums
//! ```
//! use reformation::Reformation;
//!
//! #[derive(Reformation, Eq, PartialEq, Debug)]
//! enum Ant{
//!     #[reformation(r"Queen\({}\)")]
//!     Queen(String),
//!     #[reformation(r"Worker\({}\)")]
//!     Worker(i32),
//!     #[reformation(r"Warrior")]
//!     Warrior
//! }
//!
//! fn main(){
//!     let queen = Ant::parse("Queen(We are swarm)").unwrap();
//!     assert_eq!(queen, Ant::Queen("We are swarm".to_string()));
//!
//!     let worker = Ant::parse("Worker(900000)").unwrap();
//!     assert_eq!(worker, Ant::Worker(900000));
//!
//!     let warrior = Ant::parse("Warrior").unwrap();
//!     assert_eq!(warrior, Ant::Warrior);
//! }
//! ```
//!
//! ## In place parsing
//! ```
//! use reformation::Reformation;
//!
//! #[derive(Reformation, Eq, PartialEq, Debug)]
//! #[reformation("{a} {b}")]
//! struct InPlace<'a, 'b>{
//!     #[reformation("[a-z]*")]
//!     a: &'a str,
//!     #[reformation("[a-z]*")]
//!     b: &'b str,
//! }
//!
//! fn main(){
//!     // Then parsed from &'x str value will have type
//!     // InPlace<'x, 'x>
//!     println!("{}", InPlace::regex_str());
//!     let inplace = InPlace::parse("aval bval").unwrap();
//!     assert_eq!(inplace, InPlace{a: "aval", b: "bval"})
//! }
//! ```
//!
//! ## Modes
//!
//! Order, in which modes are specified does not matter.
//!
//! ### no_regex
//!
//! Makes format string behave as regular string (in contrast with being regular expression),
//! by escaping all special regex characters.
//!
//! ```
//! use reformation::Reformation;
//!
//! #[derive(Reformation, Debug)]
//! #[reformation("Vec{{{x}, {y}}}", no_regex=true)]
//! struct Vec{
//!     x: i32,
//!     y: i32,
//! }
//!
//! fn main(){
//!     let v= Vec::parse("Vec{-1, 1}").unwrap();
//!     assert_eq!(v.x, -1);
//!     assert_eq!(v.y, 1);
//! }
//! ```
//!
//! ### slack
//!
//! Allow arbitrary number of spaces after separators: ',', ';', ':'. For separator to be recognized
//! as slack, it must be followed by at least one space in format string.
//!
//! ```
//! use reformation::Reformation;
//!
//! #[derive(Reformation, Debug)]
//! #[reformation(r"Vec\{{{x}, {y}\}}", slack=true)]
//! struct Vec{
//!     x: i32,
//!     y: i32,
//! }
//!
//! fn main(){
//!     let v = Vec::parse("Vec{-1,1}").unwrap();
//!     assert_eq!(v.x, -1);
//!     assert_eq!(v.y, 1);
//!
//!     let r = Vec::parse("Vec{15,   2}").unwrap();
//!     assert_eq!(r.x, 15);
//!     assert_eq!(r.y, 2);
//! }
//! ```

#![cfg_attr(feature="containers", feature(const_generics))]

#[macro_use]
extern crate derive_more;

pub use once_cell::sync::OnceCell;
pub use reformation_derive::*;
pub use regex::{CaptureLocations, Error as RegexError, Regex};
use std::collections::HashMap;
use std::sync::RwLock;

#[cfg(feature="containers")]
pub mod containers;

/// Declares how object can be parsed from `&'a str`
/// with possibility of in place parsing
pub trait ParseOverride<'t>: Sized {
    fn parse_override(input: &'t str) -> Result<Self, Error>;
}

pub trait Reformation<'t>: Sized {
    /// regular expression for matching this struct
    fn regex_str() -> &'static str;

    /// number of used capture groups.
    // Can be calculated from regex_str, but
    // setting explicit value by hand avoids
    // any extra cost and can be inlined in nested structs, but
    // more error prone.
    fn captures_count() -> usize;

    /// create instance of function from captures with given offset
    fn from_captures<'a>(c: &Captures<'a, 't>, offset: usize) -> Result<Self, Error>;

    /// parse struct from str
    fn parse(input: &'t str) -> Result<Self, Error>;
}

impl<'t, T: Reformation<'t>> ParseOverride<'t> for T {
    fn parse_override(input: &'t str) -> Result<Self, Error> {
        <Self as Reformation>::parse(input)
    }
}

macro_rules! group_impl_parse_primitive{
    ($re: expr, $($name: ty),*) => {
        $(group_impl_parse_primitive!{@single $re, $name})*
    };

    (@single $re: expr, $name: ty) => {
        impl<'t> Reformation<'t> for $name{
            #[inline]
            fn regex_str() -> &'static str{
                $re
            }

            #[inline]
            fn captures_count() -> usize{
                1
            }

            #[inline]
            fn from_captures<'a>(c: &Captures<'a, 't>, offset: usize) -> Result<Self, Error>{
                let res = c.get(offset)
                    .ok_or_else(|| Error::DoesNotContainGroup)?
                    .parse::<$name>()
                    .map_err(|e| Error::Other(e.to_string()))?;
                Ok(res)
            }

            #[inline]
            fn parse(input: &'t str) -> Result<Self, Error>{
                let res = input.parse::<$name>().map_err(|e| Error::Other(e.to_string()))?;
                Ok(res)
            }
        }
    };
}

pub struct GenericStaticStr<T: 'static> {
    map: RwLock<HashMap<fn() -> String, &'static T>>,
}

impl<T: 'static> GenericStaticStr<T> {
    pub fn new() -> Self {
        Self {
            map: RwLock::new(HashMap::new()),
        }
    }

    pub fn call_once<F: FnOnce(&str) -> T>(
        &'static self,
        key: fn() -> String,
        map: F,
    ) -> &'static T {
        {
            let read = self.map.read().unwrap();
            if let Some(v) = read.get(&key) {
                return v;
            }
        }
        {
            let mut write = self.map.write().unwrap();
            // double check that value still was not inserted to avoid
            // memory leaks.
            // Box::leak ing one value per key is completely ok since it is
            // meant for this value to live as long as program, and then it
            // would be freed. But if two processes will try to initialize
            // same key both might be sure in absence of value and leak it twice.
            if let Some(v) = write.get(&key) {
                return v;
            }
            let value = Box::new(map(&key()));
            let value = Box::leak(value);
            write.insert(key, value);
            value
        }
    }
}

#[derive(Copy, Clone)]
/// Wrapper to get captures of regular expression
#[derive(Debug)]
pub struct Captures<'a, 't> {
    captures: &'a CaptureLocations,
    input: &'t str,
}

impl<'a, 't> Captures<'a, 't> {
    #[inline]
    pub fn new(captures: &'a CaptureLocations, input: &'t str) -> Self {
        Self { captures, input }
    }

    #[inline]
    /// Get string corresponding to `id` capture group
    pub fn get(&self, id: usize) -> Option<&'t str> {
        self.captures.get(id).map(|(a, b)| &self.input[a..b])
    }
}

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Error {
    NoRegexMatch(NoRegexMatch),
    DoesNotContainGroup,
    #[display(fmt = "{:?}", "_0")]
    Other(String),
}

#[derive(Debug, Display, Eq, PartialEq)]
#[display(
    fmt = "No regex match: regex {:?} does not match  string {:?}",
    format,
    request
)]
pub struct NoRegexMatch {
    pub format: &'static str,
    pub request: String,
}
group_impl_parse_primitive! {r"(\d+)", u8, u16, u32, u64, u128, usize}
group_impl_parse_primitive! {r"([\+-]?\d+)", i8, i16, i32, i64, i128, isize}
group_impl_parse_primitive! {r"((?:[\+-]?\d+(?:.\d*)?|.\d+)(?:[eE][\+-]?\d+)?)", f32, f64}
group_impl_parse_primitive! {r"(.*)", String}
group_impl_parse_primitive! {r"(.)", char}

impl<'t, T: Reformation<'t>> Reformation<'t> for Option<T> {
    #[inline]
    fn regex_str() -> &'static str {
        fn generate_string<'a, T: Reformation<'a>>() -> String {
            T::regex_str().to_string() + "?"
        }
        static RE: OnceCell<GenericStaticStr<String>> = OnceCell::new();
        let re = RE.get_or_init(GenericStaticStr::new);
        re.call_once(generate_string::<T>, |x: &str| x.to_string())
            .as_str()
    }

    #[inline]
    fn captures_count() -> usize {
        T::captures_count()
    }

    #[inline]
    fn from_captures<'a>(captures: &Captures<'a, 't>, offset: usize) -> Result<Self, Error> {
        if captures.get(offset).is_some() {
            T::from_captures(captures, offset).map(Some)
        } else {
            Ok(None)
        }
    }

    #[inline]
    fn parse(input: &'t str) -> Result<Self, Error> {
        match T::parse(input) {
            Ok(x) => Ok(Some(x)),
            Err(Error::DoesNotContainGroup) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

impl<'t> Reformation<'t> for &'t str {
    #[inline]
    fn regex_str() -> &'static str {
        "(.*?)"
    }

    #[inline]
    fn captures_count() -> usize {
        1
    }

    #[inline]
    fn from_captures<'a>(captures: &Captures<'a, 't>, offset: usize) -> Result<Self, Error> {
        let res = captures
            .get(offset)
            .ok_or_else(|| Error::DoesNotContainGroup)?;
        Ok(res)
    }

    #[inline]
    fn parse(input: &'t str) -> Result<Self, Error> {
        Ok(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_float_parse() {
        // test regular expression for floating point numbers
        let re = regex::Regex::new(&format!("^{}$", f32::regex_str())).unwrap();
        // positive
        assert!(check_float_capture(&re, "10"));
        assert!(check_float_capture(&re, "10.2"));
        assert!(check_float_capture(&re, "10."));
        assert!(check_float_capture(&re, "0.34"));
        assert!(check_float_capture(&re, "00.34"));
        assert!(check_float_capture(&re, ".34"));
        assert!(check_float_capture(&re, ".34e2"));
        assert!(check_float_capture(&re, ".34e+2"));
        assert!(check_float_capture(&re, ".34e-2"));
        assert!(check_float_capture(&re, "-0.34e-2"));
        assert!(check_float_capture(&re, "5e-2"));
        assert!(check_float_capture(&re, "5.e-2")); // should this pass?

        // negative
        assert!(!re.is_match("5.."));
        assert!(!re.is_match("."));
        assert!(!re.is_match("--4."));
        assert!(!re.is_match("-.0"));
    }

    fn check_float_capture(r: &regex::Regex, s: &str) -> bool {
        r.captures(s)
            .map(|c| c.len() == 2 && c.get(1).map(|x| x.as_str()) == Some(s))
            .unwrap_or(false)
    }
}
