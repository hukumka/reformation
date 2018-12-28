#![recursion_limit = "128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

mod syn_helpers;

use crate::syn_helpers::*;

use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream;
use regex::{Captures, Regex};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Expr;
use syn::{AttrStyle, Attribute};
use syn::{Data, DeriveInput, Field, Fields};
use syn::{GenericParam, Generics};
use syn::{Ident, Type};

#[proc_macro_derive(Reformation, attributes(reformation))]
pub fn reformation_derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ds = parse_macro_input!(item as DeriveInput);
    let expanded = match reformation_derive_do(ds) {
        Ok(ok) => ok,
        Err(errors) => errors.to_compile_error(),
    };
    proc_macro::TokenStream::from(expanded)
}

fn reformation_derive_do(mut ds: DeriveInput) -> syn::Result<TokenStream> {
    add_trait_bounds(&mut ds.generics);
    let attributes = get_attributes(&ds)?;
    impl_trait(attributes, &ds)
}

fn get_attributes(ds: &DeriveInput) -> syn::Result<ReAttribute> {
    let attr_tts = ds.attrs.iter().filter_map(get_re_parse_attribute).next();
    let attr_tts = attr_tts
        .ok_or_else(|| syn::Error::new_spanned(ds, "Expected #[reformation(..)] attribute"))?;
    // ugly workaround against inability to construct ParseBuffer
    let attr: ReAttribute = syn::parse_str(&attr_tts.to_string())?;
    Ok(attr)
}

fn add_trait_bounds(generics: &mut Generics) {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param
                .bounds
                .push(parse_quote!(::reformation::Reformation));
        }
    }
}

fn get_re_parse_attribute(a: &Attribute) -> Option<&TokenStream> {
    let pound = &a.pound_token;
    let path = &a.path;
    let style_cmp = match a.style {
        AttrStyle::Outer => true,
        _ => false,
    };
    let is_re_parse = quote!(#pound).to_string() == "#"
        && style_cmp
        && quote!(#path).to_string() == "reformation";
    if is_re_parse {
        Some(&a.tts)
    } else {
        None
    }
}

fn impl_trait(mut re: ReAttribute, ds: &DeriveInput) -> syn::Result<TokenStream> {
    re.apply_no_regex();
    re.apply_slack();

    let re_str = re.regex;
    let args = arguments(&re_str);
    let fields = get_fields(&ds)?;

    let (items_to_parse, types_to_parse): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|x| (x.ident.as_ref().unwrap(), &x.ty))
        .filter(|(ident, _ty)| args.contains(&ident.to_string()))
        .unzip();

    let (items_default, types_default): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|x| (x.ident.as_ref().unwrap(), &x.ty))
        .filter(|(ident, _ty)| !args.contains(&ident.to_string()))
        .unzip();

    let generics = &ds.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let name = &ds.ident;
    let re_parse_body = quote_impl_reformation(
        &re_str,
        (&items_to_parse, &types_to_parse),
        (&items_default, &types_default),
    );
    let from_str_body = quote_impl_from_str(&ds);

    Ok(quote! {
        impl #impl_generics ::reformation::Reformation for #name #ty_generics #where_clause{
            #re_parse_body
        }

        #from_str_body
    })
}

fn quote_impl_reformation(
    re_str: &str,
    items_to_parse: (&[&Ident], &[&Type]),
    items_default: (&[&Ident], &[&Type]),
) -> TokenStream {
    let (names, types) = items_to_parse;
    let (names_default, types_default) = items_default;

    // hack over unability of quote to use same variable multiple times
    let types1 = types;
    let types2 = types;
    let types3 = types;
    let types4 = types;

    let names1 = names;
    let names2 = names;
    let names3 = names;
    quote! {
        fn regex_str()->&'static str{
            ::reformation::lazy_static!{
                static ref STR: String = {
                    format!(#re_str, #(#names1 = <#types1 as ::reformation::Reformation>::regex_str()),*)
                };
            }
            &STR
        }

        fn captures_count()->usize{
            let mut count = 0;
            #(count += <#types2 as ::reformation::Reformation>::captures_count();)*
            count
        }

        fn from_captures(captures: &::reformation::Captures, mut offset: usize)->Result<Self, Box<std::error::Error>>{
            #(
                let #names2 = <#types3 as ::reformation::Reformation>::from_captures(&captures, offset)?;
                offset += <#types4 as ::reformation::Reformation>::captures_count();
            )*
            Ok(Self{
                #(#names3,)*
                #(#names_default: <#types_default as Default>::default()),*
            })
        }
    }
}

fn quote_impl_from_str(ds: &DeriveInput) -> TokenStream {
    let (impl_generics, ty_generics, where_clause) = ds.generics.split_for_impl();
    let ty_generics2 = &ty_generics;
    let name = &ds.ident;
    let name2 = &ds.ident;
    quote! {

        impl #impl_generics std::str::FromStr for #name #ty_generics #where_clause{
            type Err = Box<std::error::Error>;

            fn from_str(input_str: &str)->Result<Self, Self::Err>{
                reformation::lazy_static!{
                    static ref RE: reformation::Regex = {
                        reformation::Regex::new(#name2 #ty_generics2::regex_str())
                            .unwrap_or_else(|x| panic!("Cannot compile regex {:?}", ))
                    };
                }

                let captures = RE.captures(input_str).ok_or_else(||{
                        ::reformation::NoRegexMatch{
                            format: Self::regex_str(),
                            request: input_str.to_string()
                        }
                    })?;
                Self::from_captures(&captures, 1)
            }
        }

    }
}

fn get_fields(struct_: &DeriveInput) -> syn::Result<Vec<&Field>> {
    if let Data::Struct(ref ds) = struct_.data {
        let fields: Vec<_> = ds.fields.iter().collect();

        if let Fields::Named(_) = ds.fields {
            Ok(fields)
        } else {
            Err(syn::Error::new_spanned(
                &ds.fields,
                "regex_parse supports only structs with named fields.",
            ))
        }
    } else {
        Err(syn::Error::new_spanned(
            &struct_,
            "regex_parse supports only structs.",
        ))
    }
}

struct ReAttribute {
    regex: String,
    params: HashMap<String, Expr>,
}

impl ReAttribute {
    /// if param no_regex specified, escape all characters, related to regular expressions
    fn apply_no_regex(&mut self) {
        match self.params.get("no_regex") {
            Some(ref expr) if expr_bool_lit(&expr) == Some(true) => {
                // escape '\\', '[', ']', '*', '|', '+', '?', '.', "{{"
                let re = Regex::new(r"([\\\[\]\*\|\+\?\.\(\)\^\&]|\{\{)").unwrap();
                let s = re.replace_all(&self.regex, |cap: &Captures| format!(r"\{}", &cap[0]));
                // escape }} with \}}. Applied to reversed string, since they must be replaced from
                // left to right: {}}} -> {}\}}
                let s: String = s.chars().rev().collect();
                let s = s.replace("}}", r"}}\");
                self.regex = s.chars().rev().collect();
            }
            _ => {
                // replace () with (:?)
                self.regex = replace_capturing_groups_with_no_capturing(&self.regex);
            }
        }
    }

    fn apply_slack(&mut self) {
        match self.params.get("slack") {
            Some(ref expr) if expr_bool_lit(&expr) == Some(true) => {
                let re = Regex::new(r"([,:;])\s+").unwrap();
                let s = re.replace_all(&self.regex, |cap: &Captures| format!(r"{}\s*", &cap[1]));
                self.regex = s.to_string();
            }
            _ => {}
        }
    }
}

impl Parse for ReAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let params: Punctuated<Expr, Token![,]> = content.parse_terminated(Expr::parse)?;
        let mut iter = params.pairs();
        let regex = iter
            .next()
            .ok_or_else(|| syn::Error::new_spanned(&params, "Expected format string"))
            .and_then(|pair| get_regex_str(pair.value()))?;

        let params: syn::Result<HashMap<_, _>> = iter
            .map(|pair| {
                let expr = pair.value();
                expr_into_attribute_param(expr)
                    .map(|(a, b)| (a.to_string(), b.clone()))
                    .ok_or_else(|| syn::Error::new_spanned(expr, "Expected `param=expr`"))
            })
            .collect();

        Ok(Self {
            regex,
            params: params?,
        })
    }
}

fn get_regex_str(re: &Expr) -> syn::Result<String> {
    expr_lit(re)
        .and_then(lit_str)
        .ok_or_else(|| syn::Error::new_spanned(re, "regex_parse argument must be string literal."))
}

fn replace_capturing_groups_with_no_capturing(s: &str) -> String {
    let mut prev = None;
    let mut res = String::new();
    let mut iter = s.chars().peekable();
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

/// parse which fields present in format string
fn arguments(format_string: &str) -> HashSet<String> {
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
mod tests {
    use super::*;

    #[test]
    fn test_no_regex_mode() {
        let mut re_attr = reattributes_with_mode("Vec{{{}, {}}}", "no_regex");
        re_attr.apply_no_regex();
        assert_eq!(re_attr.regex, r"Vec\{{{}, {}\}}");
        assert_eq!(
            r"Vec\{(x), (y)\}",
            format!(r"Vec\{{{}, {}\}}", "(x)", "(y)")
        );

        let s = r"\[T]/ *+* -.- (\|)(^_^)(|/) &&";
        let mut re_attr = reattributes_with_mode(s, "no_regex");
        re_attr.apply_no_regex();
        assert_eq!(
            re_attr.regex,
            r"\\\[T\]/ \*\+\* -\.- \(\\\|\)\(\^_\^\)\(\|/\) \&\&"
        );
        let re = Regex::new(&re_attr.regex).unwrap();
        assert!(re.is_match(s));
    }

    #[test]
    fn test_slack_mode() {
        let mut re_attr = reattributes_with_mode(r"Vec\({a}, {b}\)", "slack");
        re_attr.apply_slack();
        assert_eq!(re_attr.regex, r"Vec\({a},\s*{b}\)");

        let mut re_attr = reattributes_with_mode(r"Vec\({a},ax {b}\)", "slack");
        re_attr.apply_slack();
        assert_eq!(re_attr.regex, r"Vec\({a},ax {b}\)");
    }

    fn reattributes_with_mode(s: &str, mode: &str) -> ReAttribute {
        let mut params = HashMap::new();
        let expr_true: Expr = parse_quote!(true);
        params.insert(mode.to_string(), expr_true);
        ReAttribute {
            regex: s.to_string(),
            params,
        }
    }
}
