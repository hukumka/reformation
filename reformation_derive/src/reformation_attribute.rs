use crate::syn_helpers::expr_bool_lit;
use lazy_static::lazy_static;
use proc_macro2::Span;
use regex::{escape, Regex};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{AttrStyle, Attribute, Expr, Ident, Lit};

/// Struct representing attribute `#[reformation(...)]`
#[derive(Clone)]
pub struct ReformationAttribute {
    pub span: Span,
    pub regex_string: Option<String>,

    pub slack: bool,
    pub no_regex: bool,
    pub fromstr: bool,
}

impl ReformationAttribute {
    fn new(span: Span) -> Self {
        Self {
            span,
            regex_string: None,
            slack: false,
            no_regex: false,
            fromstr: false,
        }
    }

    pub fn combine(&self, other: &Self) -> Self {
        let s = other
            .regex_string
            .clone()
            .or_else(|| self.regex_string.clone());
        Self {
            span: other.span,
            regex_string: s,
            fromstr: false, // Only makes sence on top level #TODO
            slack: other.slack | self.slack,
            no_regex: other.no_regex | self.no_regex,
        }
    }

    pub fn substr_mode(&self) -> impl Fn(&str) -> String + '_ {
        move |s| {
            let s = if self.no_regex {
                escape(s)
            } else {
                no_capturing_groups(s)
            };
            if self.slack {
                slack(&s)
            } else {
                s
            }
        }
    }

    /// Parse ReformationAttribute from set of attributes on DeriveInput
    pub fn parse(span: Span, attrs: &[Attribute]) -> syn::Result<Self> {
        let attr = if let Some(a) = Self::find_attribute(attrs) {
            a
        } else {
            return Ok(Self::new(span));
        };
        let tts = &attr.tokens;
        let stream_str = quote!(#tts).to_string();
        let res: Self = syn::parse_str(&stream_str).map_err(|e| syn::Error::new(span, e))?;
        Ok(res)
    }

    pub fn find_attribute(attrs: &[Attribute]) -> Option<&Attribute> {
        attrs.iter().find(|a| is_reformation_attr(a))
    }

    pub fn regex(&self) -> syn::Result<&str> {
        self.regex_string
            .as_ref()
            .map(|s| s.as_str())
            .ok_or_else(|| syn::Error::new(self.span, "No format string specified by attribute"))
    }
}

impl Parse for ReformationAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let params: Punctuated<Mode, Token![,]> = content.parse_terminated(Mode::parse)?;
        let mut res = Self::new(Span::call_site());
        for mode in params {
            res.apply(mode)?;
        }
        Ok(res)
    }
}

enum Mode {
    Str(String),
    BoolParam(Ident),
}

impl Parse for Mode {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            let ident: Ident = input.parse()?;
            let _eq: Token![=] = input.parse()?;
            let true_: Expr = input.parse()?;
            expect_true(ident.span(), &ident.to_string(), &true_)?;
            Ok(Mode::BoolParam(ident))
        } else {
            let regex: Lit = input.parse()?;
            match regex {
                Lit::Str(s) => Ok(Mode::Str(s.value())),
                _ => Err(syn::Error::new_spanned(regex, "Expected string literal.")),
            }
        }
    }
}

impl ReformationAttribute {
    fn apply(&mut self, mode: Mode) -> syn::Result<()> {
        match mode {
            Mode::BoolParam(ident) => match ident.to_string().as_str() {
                "no_regex" => {
                    self.no_regex = true;
                    Ok(())
                }
                "slack" => {
                    self.slack = true;
                    Ok(())
                }
                "fromstr" => {
                    self.fromstr = true;
                    Ok(())
                }
                _ => Err(syn::Error::new(
                    self.span,
                    format!("Unknown mode: {:?}", &ident.to_string()),
                )),
            },
            Mode::Str(s) => {
                self.regex_string = Some(s);
                Ok(())
            }
        }
    }
}

fn expect_true(span: Span, name: &str, value: &Expr) -> syn::Result<()> {
    if expr_bool_lit(value) != Some(true) {
        Err(syn::Error::new(
            span,
            format!(
                "Error expected `true` for mode `{}`, found `{}`",
                name,
                quote! {value}.to_string()
            ),
        ))
    } else {
        Ok(())
    }
}

fn is_reformation_attr(a: &Attribute) -> bool {
    let pound = &a.pound_token;
    let path = &a.path;
    let style_cmp = match a.style {
        AttrStyle::Outer => true,
        _ => false,
    };
    quote!(#pound).to_string() == "#" && style_cmp && quote!(#path).to_string() == "reformation"
}

fn no_capturing_groups(input: &str) -> String {
    let mut prev = None;
    let mut res = String::new();
    let mut iter = input.chars().peekable();
    while let Some(c) = iter.next() {
        if prev != Some('\\') && c == '(' && iter.peek() != Some(&'?') {
            res.push_str("(?:");
        } else {
            res.push(c);
        }
        prev = Some(c);
    }
    res
}

fn slack(input: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"([,:;])\s+").unwrap();
    }
    RE.replace_all(input, r"$1\s*").to_string()
}
