use std::str::CharIndices;
use std::iter::Peekable;

use std::collections::{HashSet, HashMap};

pub struct Format{
    substrings: Vec<String>,
    arguments: Vec<Argument>,
}

#[derive(Debug, Eq, PartialEq)]
enum Argument{
    Positional(usize),
    Named(String),
}

#[derive(Debug)]
pub enum FormatError{
    NoClosing,
    NoOpening(usize),
    NoSuchPositionalArgument(usize),
    NoSuchNamedArgument(String),
}

struct FormatBuilder<'a>{
    format: Format,
    string: &'a str,
    iter: Peekable<CharIndices<'a>>,
    positional_count: usize,
}

impl Format{
    pub fn new(string: &str) -> Result<Self, FormatError>{
        let mut res = FormatBuilder::new(string).build()?;
        res.map_substrings(|x|{
            x.replace("{{", "{").replace("}}", "}")
        });
        Ok(res)
    }

    pub fn build(&self, args: &[String], kwargs: &HashMap<String, String>) -> Result<String, FormatError>{
        let sub_len: usize = self.substrings.iter()
            .map(|x| x.len())
            .sum();
        let arg_len: usize = self.arguments.iter()
            .map(|x| x.len(args, kwargs))
            .fold(Ok(0), |a, b| a.and(b))?;

        let mut res = String::with_capacity(sub_len + arg_len);

        for (string, arg) in self.substrings.iter().zip(&self.arguments){
            res.push_str(string);
            res.push_str(arg.to_string(args, kwargs)?);
        }
        res.push_str(self.substrings.last().unwrap());
        Ok(res)
    }

    pub fn map_substrings<T: Fn(&str)->String>(&mut self, map: T){
        for i in &mut self.substrings{
            *i = map(&i);
        }
    }
}

impl Argument{
    fn len(&self, args: &[String], kwargs: &HashMap<String, String>) -> Result<usize, FormatError>{
        match self {
            Argument::Positional(i) => {
                args.get(*i)
                    .map(|s| s.len())
                    .ok_or_else(|| FormatError::NoSuchPositionalArgument(*i))
            },
            Argument::Named(k) => {
                kwargs.get(k)
                    .map(|s| s.len())
                    .ok_or_else(|| FormatError::NoSuchNamedArgument(k.to_string()))
            }
        }
    }

    fn to_string<'a>(&self, args: &'a [String], kwargs: &'a HashMap<String, String>) -> Result<&'a String, FormatError>{
        match self {
            Argument::Positional(i) => {
                args.get(*i)
                    .ok_or_else(|| FormatError::NoSuchPositionalArgument(*i))
            },
            Argument::Named(k) => {
                kwargs.get(k)
                    .ok_or_else(|| FormatError::NoSuchNamedArgument(k.to_string()))
            }
        }
    }
}

impl<'a> FormatBuilder<'a>{
    fn new(string: &'a str) -> Self{
        Self{
            format: Format{
                substrings: vec![],
                arguments: vec![]
            },
            string,
            iter: string.char_indices().peekable(),
            positional_count: 0,
        }
    }

    fn build(mut self) -> Result<Format, FormatError>{
        let mut substr_start = 0;
        while let Some((i, c)) = self.iter.next(){
            let is_argument = c == '{'
                && self.iter.peek() // check if '{' is not part of "{{" escaping
                    .map(|(_, c)| c) != Some(&'{');

            let is_closing = c == '}'
                && self.iter.peek() // check if '{' is not part of "{{" escaping
                .map(|(_, c)| c) != Some(&'}');

            if is_argument{
                let substr = self.string.get(substr_start..i).unwrap().to_string();
                self.format.substrings.push(substr);
                let arg = self.parse_argument()?;
                self.format.arguments.push(arg);

                if let Some((i, _)) = self.iter.peek(){
                    substr_start = *i;
                }else{
                    substr_start = self.string.len();
                }
            }else if is_closing{
                return Err(FormatError::NoOpening(i));
            }else if c == '{' || c == '}'{
                self.iter.next();
            }
        }
        let last_substr = self.string.get(substr_start..).unwrap().to_string();
        self.format.substrings.push(last_substr);

        Ok(self.format)
    }

    fn parse_argument(&mut self) -> Result<Argument, FormatError>{
        let start;
        if let Some((i, c)) = self.iter.next(){
            start = i;
            if c == '}'{
                // positional argument
                let res = Argument::Positional(self.positional_count);
                self.positional_count += 1;
                return Ok(res);
            }
        }else{
            return Err(FormatError::NoClosing);
        }

        let end = self.iter.find(|(_, x)| x == &'}')
            .ok_or_else(|| FormatError::NoClosing)?.0;

        let name = self.string.get(start..end).unwrap().to_string();
        Ok(Argument::Named(name))
    }
}

/// parse which fields present in format string
pub fn arguments(format_string: &str) -> HashSet<String> {
    let mut curly_bracket_stack = vec![];
    let mut map = HashSet::new();

    let mut iter = format_string.char_indices().peekable();
    loop {
        match iter.next() {
            Some((i, c)) if c == '{' => {
                if iter.peek().map(|(_, c)| *c) != Some('{') {
                    curly_bracket_stack.push(i + c.len_utf8());
                }
            }
            Some((i, c)) if c == '}' => {
                if let Some(start) = curly_bracket_stack.pop() {
                    let end = i;
                    let substr = format_string.get(start..end).unwrap().to_string();
                    map.insert(substr);
                }
            }
            Some(_) => {}
            None => {
                break;
            }
        }
    }
    map
}


#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_format_new(){
        let f = Format::new("a = {}, b = {}").unwrap();
        assert_eq!(f.substrings, &[
            "a = ",
            ", b = ",
            ""
        ]);
        assert_eq!(f.arguments, &[
            Argument::Positional(0),
            Argument::Positional(1),
        ]);

        let f = Format::new("Vec{{ {x}, {}, {z}, {}}}").unwrap();
        assert_eq!(f.substrings, &[
            "Vec{ ",
            ", ",
            ", ",
            ", ",
            "}"
        ]);
        assert_eq!(f.arguments, &[
            Argument::Named("x".to_string()),
            Argument::Positional(0),
            Argument::Named("z".to_string()),
            Argument::Positional(1),
        ]);
    }

    #[test]
    fn test_format_build(){
        let f = Format::new("a = {{{}}}, b = {}").unwrap();
        let s = f.build(&["x".to_string(), "y".to_string()], &HashMap::new()).unwrap();
        assert_eq!(&s, "a = {x}, b = y");
        let s = f.build(&["Eras}".to_string(), "y{".to_string()], &HashMap::new()).unwrap();
        assert_eq!(&s, "a = {Eras}}, b = y{");

        let f = Format::new("Vec{{{x}, {y}, {z}}}").unwrap();
        let mut map = HashMap::new();
        map.insert("x".to_string(), "x".to_string());
        map.insert("y".to_string(), "y".to_string());
        map.insert("z".to_string(), "z".to_string());
        let s = f.build(&[], &map).unwrap();
        assert_eq!(&s, "Vec{x, y, z}");
    }
}