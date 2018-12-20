use std::fmt;
pub use regex;
pub use lazy_static;
use std::str::FromStr;


#[derive(Debug)]
pub struct NoRegexMatch{
    pub format: &'static str,
    pub request: String,
}

impl std::error::Error for NoRegexMatch{}
impl fmt::Display for NoRegexMatch{
    fn fmt(&self, f: &mut fmt::Formatter)->fmt::Result{
        write!(f, "String {:?} does not match format r{:?}", self.format, self.request)
    }
}


pub trait ParsePrimitive: FromStr{
    fn regex_str()->&'static str;
}

macro_rules! group_impl_parse_primitive{
    ($re: expr, $($name: ty),*) => {
        $(group_impl_parse_primitive!{@single $re, $name})*
    };

    (@single $re: expr, $name: ty) => {
        impl ParsePrimitive for $name{
            fn regex_str()->&'static str{
                $re
            }
        }
    };
}

group_impl_parse_primitive!{r"\d+", u8, u16, u32, u64, u128, usize}
group_impl_parse_primitive!{r"[\+-]?\d+", i8, i16, i32, i64, i128, isize}
group_impl_parse_primitive!{r".*", String}


#[macro_export]
macro_rules! create_parse_fn{
    ($name: ident, $re: expr, $($res: ty),*) => {
        fn $name(s: &str)->Result<($($res),*), Box<std::error::Error>>{
            create_parse_fn!(@body s, $re, $($res),*)
        }
    };
    (@body $str: expr, $re: expr, $($res: ty),*) => {
        {
            type OkType = ($($res),*);

            reparse::lazy_static::lazy_static!{
                static ref REGEX: reparse::regex::Regex = {
                    let re_str = format!($re, $(create_parse_fn!(@ty_capture $res)),*);
                    reparse::regex::Regex::new(&re_str).unwrap()
                };
            }

            let captures = REGEX.captures($str).ok_or_else(||{
                reparse::NoRegexMatch{
                    format: $re,
                    request: $str.to_string()
                }
            })?;
            let mut i=0;
            Ok((
                $({
                    i += 1;
                    captures.get(i).unwrap().as_str().parse::<$res>()?
                }),*
            ))
        }
    };

    (@ty_capture $t: ty) => {
        format!("({})", <$t as reparse::ParsePrimitive>::regex_str())
    };
}


#[cfg(test)]
mod tests {
}
