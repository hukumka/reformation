use std::iter::Peekable;
use std::str::CharIndices;

use std::error::Error;
use std::fmt;

pub struct Format {
    substrings: Vec<String>,
    arguments: Vec<Argument>,
}

#[derive(Debug, Eq, PartialEq)]
enum Argument {
    Positional(usize),
    Named(String),
}

#[derive(Debug)]
pub enum FormatError {
    NoClosing,
    NoOpening(usize),
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormatError::NoClosing => {
                write!(f, "FormatError: Bracket was opened, but never closed")
            }
            FormatError::NoOpening(i) => write!(
                f,
                "FormatError: Bracket was closed at {}, but no matching opening bracket found",
                i
            ),
        }
    }
}

impl Error for FormatError {}

struct FormatBuilder<'a> {
    format: Format,
    string: &'a str,
    iter: Peekable<CharIndices<'a>>,
    positional_count: usize,
}

impl Format {
    pub fn build(string: &str) -> Result<Self, FormatError> {
        let mut res = FormatBuilder::new(string).build()?;
        res.map_substrings(|x| x.replace("{{", "{").replace("}}", "}"));
        Ok(res)
    }
    pub fn build_empty(&self) -> String {
        self.substrings.join("")
    }

    pub fn no_arguments(&self) -> bool {
        self.arguments.is_empty()
    }

    pub fn map_substrings<T: Fn(&str) -> String>(&mut self, map: T) {
        for i in &mut self.substrings {
            *i = map(&i);
        }
    }

    pub fn named_arguments(&self) -> Vec<String> {
        let set: Vec<_> = self
            .arguments
            .iter()
            .filter_map(|a| match a {
                Argument::Named(s) => Some(s.clone()),
                _ => None,
            })
            .collect();
        set
    }

    pub fn positional_arguments(&self) -> usize {
        self.arguments
            .iter()
            .filter_map(|a| match a {
                Argument::Positional(_) => Some(()),
                _ => None,
            })
            .count()
    }
}

impl ToString for Format {
    fn to_string(&self) -> String {
        let escape = |s: &str| s.replace("{", "{{").replace("}", "}}");
        let mut res = String::new();
        for (s, a) in self.substrings.iter().zip(&self.arguments) {
            res.push_str(&escape(s));
            res.push_str(&a.to_string());
        }
        res.push_str(&escape(self.substrings.last().unwrap()));
        res
    }
}

impl ToString for Argument {
    #[inline]
    fn to_string(&self) -> String {
        match self {
            Argument::Named(s) => format!("{{{}}}", s),
            Argument::Positional(_) => "{}".to_string(),
        }
    }
}

impl<'a> FormatBuilder<'a> {
    fn new(string: &'a str) -> Self {
        Self {
            format: Format {
                substrings: vec![],
                arguments: vec![],
            },
            string,
            iter: string.char_indices().peekable(),
            positional_count: 0,
        }
    }

    fn build(mut self) -> Result<Format, FormatError> {
        let mut substr_start = 0;
        while let Some((i, c)) = self.iter.next() {
            let is_argument = c == '{'
                && self
                    .iter
                    .peek() // check if '{' is not part of "{{" escaping
                    .map(|(_, c)| c)
                    != Some(&'{');

            let is_closing = c == '}'
                && self
                    .iter
                    .peek() // check if '{' is not part of "{{" escaping
                    .map(|(_, c)| c)
                    != Some(&'}');

            if is_argument {
                let substr = self.string.get(substr_start..i).unwrap().to_string();
                self.format.substrings.push(substr);
                let arg = self.parse_argument()?;
                self.format.arguments.push(arg);

                if let Some((i, _)) = self.iter.peek() {
                    substr_start = *i;
                } else {
                    substr_start = self.string.len();
                }
            } else if is_closing {
                return Err(FormatError::NoOpening(i));
            } else if c == '{' || c == '}' {
                self.iter.next();
            }
        }
        let last_substr = self.string.get(substr_start..).unwrap().to_string();
        self.format.substrings.push(last_substr);

        Ok(self.format)
    }

    fn parse_argument(&mut self) -> Result<Argument, FormatError> {
        let start;
        if let Some((i, c)) = self.iter.next() {
            start = i;
            if c == '}' {
                // positional argument
                let res = Argument::Positional(self.positional_count);
                self.positional_count += 1;
                return Ok(res);
            }
        } else {
            return Err(FormatError::NoClosing);
        }

        let end = self
            .iter
            .find(|(_, x)| x == &'}')
            .ok_or_else(|| FormatError::NoClosing)?
            .0;

        let name = self.string.get(start..end).unwrap().to_string();
        Ok(Argument::Named(name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_new() {
        let f = Format::build("a = {}, b = {}").unwrap();
        assert_eq!(f.substrings, &["a = ", ", b = ", ""]);
        assert_eq!(
            f.arguments,
            &[Argument::Positional(0), Argument::Positional(1),]
        );

        let f = Format::build("Vec{{ {x}, {}, {z}, {}}}").unwrap();
        assert_eq!(f.substrings, &["Vec{ ", ", ", ", ", ", ", "}"]);
        assert_eq!(
            f.arguments,
            &[
                Argument::Named("x".to_string()),
                Argument::Positional(0),
                Argument::Named("z".to_string()),
                Argument::Positional(1),
            ]
        );
    }
}
