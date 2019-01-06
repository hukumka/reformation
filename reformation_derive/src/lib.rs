#![recursion_limit = "128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

mod syn_helpers;
mod format;

use crate::syn_helpers::*;
use crate::format::Format;

use std::collections::{HashMap, HashSet};

use proc_macro2::{Span, TokenStream};
use regex::{Captures, Regex};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Expr;
use syn::{AttrStyle, Attribute};
use syn::{Data, DataEnum, DataStruct, DeriveInput, Field, Fields};
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
    impl_trait(attributes, ds)
}

fn get_attributes(ds: &DeriveInput) -> syn::Result<ReAttribute> {
    let attr_tts = ds.attrs.iter().filter(|&x| is_reformation_attr(x)).next();
    let attr_tts = attr_tts
        .ok_or_else(|| syn::Error::new_spanned(ds, "Expected #[reformation(..)] attribute"))?;
    // ugly workaround against inability to construct ParseBuffer
    let mut attr: ReAttribute = syn::parse_str(&attr_tts.tts.to_string())?;
    attr.span = attr_tts.span();
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

fn is_reformation_attr(a: &Attribute) -> bool{
    let pound = &a.pound_token;
    let path = &a.path;
    let style_cmp = match a.style {
        AttrStyle::Outer => true,
        _ => false,
    };
    let is_re_parse = quote!(#pound).to_string() == "#"
        && style_cmp
        && quote!(#path).to_string() == "reformation";
    is_re_parse
}

fn impl_trait(re: ReAttribute, ds: DeriveInput) -> syn::Result<TokenStream> {
    let from_str = quote_impl_from_str(&ds);

    let generics = &ds.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let name = &ds.ident;

    let reformation_body = match ds.data {
        Data::Struct(struct_) => {
            match struct_.fields {
                Fields::Named(_) => impl_struct(&struct_, re),
                Fields::Unnamed(_) => impl_tuple_struct(name, &struct_, re),
                Fields::Unit => impl_empty_struct(name, re),
            }
        },
        Data::Enum(enum_) => impl_enum(name, &enum_, re),
        _ => unimplemented!(),
    }?;

    Ok(quote! {
        impl #impl_generics ::reformation::Reformation for #name #ty_generics #where_clause{
            #reformation_body
        }

        #from_str
    })
}

fn impl_enum(ident: &Ident, enum_: &DataEnum, mut re: ReAttribute) -> syn::Result<TokenStream> {
    re.prepare_enum()?;
    re.test_enum(&enum_)?;
    let re_str = re.regex;
    let (variants, types): (Vec<_>, Vec<_>) = enum_
        .variants
        .iter()
        .map(|x| {
            let values: Vec<_> = x.fields.iter().map(|x| &x.ty).collect();
            (&x.ident, values)
        })
        .unzip();

    let types_flat = types.iter().flatten();

    let types1 = &types;

    let variants: Vec<_> = variants
        .iter()
        .zip(&types)
        .map(|(v, t)| quote_variant_from_capture(ident, v, &t))
        .collect();

    Ok(quote! {
        fn regex_str()->&'static str{
            ::reformation::lazy_static!{
                static ref STR: String = {
                    format!(#re_str, #(<#types_flat as ::reformation::Reformation>::regex_str()),*)
                };
            }
            &STR
        }

        fn captures_count()->usize{
            let mut count = 1;
            #(
                count += 1;
                #(
                    count += <#types1 as ::reformation::Reformation>::captures_count();
                )*
            )*
            count
        }

        fn from_captures(captures: &::reformation::Captures, mut offset: usize)->Result<Self, Box<std::error::Error>>{
            // check if capture group of variant is present until such is found;
            #(
                #variants
            )*

            // TODO: gracefull error, or prove that this is unreachable!()
            panic!("No variants match")
        }
    })
}

fn quote_variant_from_capture(ident: &Ident, variant: &Ident, values: &[&Type]) -> TokenStream {
    let values1 = values;
    let values2 = values;
    let values3 = values;
    if values.is_empty() {
        quote! {
            if captures.get(offset).is_some(){
                return Ok(#ident::#variant);
            }else{
                offset += 1;
            }
        }
    } else {
        quote! {
            if captures.get(offset).is_some(){
                offset += 1;
                return Ok(#ident::#variant(
                    #(
                        {
                            let res = <#values1 as ::reformation::Reformation>::from_captures(captures, offset)?;
                            offset += <#values2 as ::reformation::Reformation>::captures_count();
                            res
                        }
                    ),*
                ));
            }else{
                offset += 1;
                #(
                    offset += <#values3 as ::reformation::Reformation>::captures_count();
                )*
            }
        }
    }
}

fn impl_empty_struct(ident: &Ident, mut re: ReAttribute) -> syn::Result<TokenStream>{
    re.prepare_struct()?;
    let re_str = re.regex;
    let span = re.span;
    let f = Format::new(&re_str)
        .map_err(|x| syn::Error::new(span, x))?;

    let count = f.positional_arguments();
    if count != 0{
        let msg = format!(
            "Struct {} does not contain any fields, but {} were specified in format string",
            ident, count
        );
        return Err(syn::Error::new(span, msg));
    }
    let named = f.named_arguments();
    if !named.is_empty(){
        let msg = format!(
            "Struct {} must not have any fields, specified by name",
            ident
        );
        return Err(syn::Error::new(span, msg));
    }

    Ok(quote!{
        fn regex_str()->&'static str{
            #re_str
        }

        fn captures_count()->usize{
            0
        }

        fn from_captures(captures: &::reformation::Captures, mut offset: usize) -> Result<Self, Box<std::error::Error>>{
            Ok(#ident)
        }
    })
}

fn impl_tuple_struct(ident: &Ident, struct_: &DataStruct, mut re: ReAttribute) -> syn::Result<TokenStream>{
    re.prepare_struct()?;
    let re_str = re.regex;
    let span = re.span;
    let types: Vec<_> = struct_.fields.iter()
        .map(|x| &x.ty)
        .collect();

    let f = Format::new(&re_str)
        .map_err(|x| syn::Error::new(span, x))?;
    let count = f.positional_arguments();
    if count != types.len(){
        let msg = format!(
            "Struct {} contains {} fields, but {} were specified in format string",
            ident, types.len(), count
        );
        return Err(syn::Error::new(span, msg));
    }
    let named = f.named_arguments();
    if !named.is_empty(){
        let msg = format!(
            "Struct {} must not have any fields, specified by name",
            ident
        );
        return Err(syn::Error::new(span, msg));
    }

    let types1 = &types;
    let types2 = &types;
    let types3 = &types;
    let types4 = &types;

    Ok(quote!{
        fn regex_str()->&'static str{
            ::reformation::lazy_static!{
                static ref STR: String = {
                    format!(#re_str, #(<#types1 as ::reformation::Reformation>::regex_str()),*)
                };
            }
            &STR
        }

        fn captures_count()->usize{
            let mut count = 0;
            #(count += <#types2 as ::reformation::Reformation>::captures_count();)*
            count
        }

        fn from_captures(captures: &::reformation::Captures, mut offset: usize) -> Result<Self, Box<std::error::Error>>{
            Ok(#ident(
                #({
                    let res = <#types3 as ::reformation::Reformation>::from_captures(&captures, offset)?;
                    offset += <#types4 as ::reformation::Reformation>::captures_count();
                    res
                }),*
            ))
        }
    })
}

fn impl_struct(struct_: &DataStruct, mut re: ReAttribute) -> syn::Result<TokenStream> {
    re.prepare_struct()?;

    let span = re.span;

    let f = Format::new(&re.regex)
        .map_err(|x| syn::Error::new(span, x))?;
    let count = f.positional_arguments();
    if count != 0{
        let msg = "Structs with named fields must specify they fields in format string as named arguments.";
        return Err(syn::Error::new(span, msg));
    }

    let re_str = re.regex;
    let span = re.span;
    let args = Format::new(&re_str)
        .map_err(|x| {
            syn::Error::new(span, x)
        })?
        .named_arguments();
    let fields = get_fields(struct_)?;

    let fields_set: HashSet<_> = fields.iter()
        .map(|&x| x.ident.as_ref().unwrap().to_string())
        .collect();

    let mut not_in_fields = args.iter()
        .filter(|&x| !fields_set.contains(x));
    if let Some(x) = not_in_fields.next(){
        let msg = format!(
            "Format string features field {{{}}}, but field with name {:?} does not exist",
            x, x
        );
        return Err(syn::Error::new(span, msg));
    }

    // split fields into two categories:
    // items to be parsed from string
    let (items_to_parse, types_to_parse): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|x| (x.ident.as_ref().unwrap(), &x.ty))
        .filter(|(ident, _ty)| args.contains(&ident.to_string()))
        .unzip();
    // items to be initialized from Default trait
    let (items_default, types_default): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|x| (x.ident.as_ref().unwrap(), &x.ty))
        .filter(|(ident, _ty)| !args.contains(&ident.to_string()))
        .unzip();

    // hack over unability of quote to use same variable multiple times
    let types1 = &types_to_parse;
    let types2 = &types_to_parse;
    let types3 = &types_to_parse;
    let types4 = &types_to_parse;

    let names1 = &items_to_parse;
    let names2 = &items_to_parse;
    let names3 = &items_to_parse;
    Ok(quote! {
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
                #(#items_default: <#types_default as Default>::default()),*
            })
        }
    })
}

fn quote_impl_from_str(ds: &DeriveInput) -> TokenStream {
    let (impl_generics, ty_generics, where_clause) = ds.generics.split_for_impl();
    let ty_generics2 = &ty_generics;
    let ty_generics3 = &ty_generics;
    let name = &ds.ident;
    let name2 = &ds.ident;
    let name3 = &ds.ident;
    quote! {

        impl #impl_generics std::str::FromStr for #name #ty_generics #where_clause{
            type Err = Box<std::error::Error>;

            fn from_str(input_str: &str)->Result<Self, Self::Err>{
                reformation::lazy_static!{
                    static ref RE: reformation::Regex = {
                        reformation::Regex::new(#name2 #ty_generics2::regex_str())
                            .unwrap_or_else(|x| panic!("Cannot compile regex {:?}", #name3 #ty_generics3::regex_str()))
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

fn get_fields(ds: &DataStruct) -> syn::Result<Vec<&Field>> {
    let fields: Vec<_> = ds.fields.iter().collect();

    if let Fields::Named(_) = ds.fields {
        Ok(fields)
    } else {
        Err(syn::Error::new_spanned(
            &ds.fields,
            "regex_parse supports only structs with named fields.",
        ))
    }
}

struct ReAttribute {
    span: Span,
    regex: String,
    params: HashMap<String, Expr>,
}

impl ReAttribute {
    fn prepare_enum(&mut self) -> syn::Result<()> {
        let variants = Self::enum_variants(self.span, &self.regex)?;
        let variants: Vec<_> = variants
            .iter()
            .map(|x| self.apply_no_regex(x))
            .map(|x| self.apply_slack(&x))
            .collect();
        self.regex = "(?:(".to_string() + &variants.join(")|(") + "))";
        Ok(())
    }

    fn test_enum(&self, enum_: &DataEnum) -> syn::Result<()>{
        let variants = Self::enum_variants(self.span, &self.regex)?;
        let variants: Vec<_> = variants
            .iter()
            .map(|x| self.apply_no_regex(x))
            .map(|x| self.apply_slack(&x))
            .collect();
        self.test_enum_variants(&variants, enum_)?;
        self.test_enum_regex()
    }

    fn test_enum_regex(&self) -> syn::Result<()>{
        let r = Format::new(&self.regex).unwrap().build_empty();
        let r= Regex::new(&r).map_err(|x|{
            syn::Error::new(self.span, x.to_string())
        })?;
        if let Some(x) = r.capture_names().flatten().next(){
            let msg = format!("Named capture groups not allowed in format string. ({:?})", x);
            Err(syn::Error::new(self.span, msg))
        }else if let Some(_) = r.capture_locations().get(2){
            unreachable!("Capture location found in {:?}, which should have been escaped by crate.", self.regex)
        }else{
            Ok(())
        }
    }

    fn test_enum_variants(&self, variants: &[String], enum_: &DataEnum) -> syn::Result<()>{
        let ev: Vec<_> = enum_.variants.iter()
            .collect();
        if variants.len() != ev.len(){
            let msg = format!(
                "Enum has {} variants, but format string covers {} of them.",
                variants.len(),
                ev.len()
            );
            return Err(syn::Error::new(self.span, msg));
        }
        for (s, v) in variants.iter().zip(&enum_.variants){
            let fields = v.fields.iter().count();
            let values = Format::new(s)
                .map_err(|x| syn::Error::new(self.span, x))?
                .positional_arguments();
            if fields != values{
                let msg = format!(
                    "Enum variant {} has {} values, but {} were specified in format.",
                    &v.ident, fields, values
                );
                return Err(syn::Error::new(self.span, msg));
            }
        }
        Ok(())
    }

    fn enum_variants(span: Span, s: &str) -> syn::Result<Vec<String>> {
        let mut variants = vec![];
        let mut current_variant = String::new();

        let mut iter = s.chars().peekable();

        if iter.next() != Some('(') {
            let message = "Enum format string must be r\"(variant1|...|variantN)\", but prefix is found";
            return Err(syn::Error::new(span, message));
        }
        let mut bracket_depth = 1;

        while let Some(x) = iter.next() {
            if bracket_depth == 1 && x == ')' {
                bracket_depth = 0;
                break;
            }
            if '|' == x && bracket_depth == 1 {
                variants.push(current_variant);
                current_variant = String::new();
            } else {
                current_variant.push(x);
                if "(".contains(x) {
                    bracket_depth += 1;
                }
                if ")".contains(x) {
                    bracket_depth -= 1;
                }
                if x == '\\' {
                    if let Some(c) = iter.next() {
                        current_variant.push(c);
                    }
                }
            }
        }
        if iter.next() != None{
            let message = "Enum format string must be r\"(variant1|...|variantN)\", but suffix is found";
            return Err(syn::Error::new(span, message));
        }else if bracket_depth > 0 {
            let message ="Enum group bracket is not closed";
            return Err(syn::Error::new(span, message));
        }
        variants.push(current_variant);
        Ok(variants)
    }

    fn prepare_struct(&mut self) -> syn::Result<()>{
        self.regex = self.apply_no_regex(&self.regex);
        self.regex = self.apply_slack(&self.regex);

        let r = Format::new(&self.regex)
            .map_err(|x| syn::Error::new(self.span, x))?
            .build_empty();
        Regex::new(&r).map_err(|x|{
            syn::Error::new(self.span, x.to_string())
        })?;
        Ok(())
    }

    /// if param no_regex specified, escape all characters, related to regular expressions
    fn apply_no_regex(&self, s: &str) -> String {
        match self.params.get("no_regex") {
            Some(ref expr) if expr_bool_lit(&expr) == Some(true) => {
                // escape '\\', '[', ']', '*', '|', '+', '?', '.', "{{"
                let re = Regex::new(r"([\\\[\]\*\|\+\?\.\(\)\^\&]|\{\{)").unwrap();
                let s = re.replace_all(s, |cap: &Captures| format!(r"\{}", &cap[0]));
                // escape }} with \}}. Applied to reversed string, since they must be replaced from
                // left to right: {}}} -> {}\}}
                let s: String = s.chars().rev().collect();
                let s = s.replace("}}", r"}}\");

                let res: String = s.chars().rev().collect();
                res
            }
            _ => {
                // replace () with (:?)
                replace_capturing_groups_with_no_capturing(s)
            }
        }
    }

    fn apply_slack(&self, s: &str) -> String {
        match self.params.get("slack") {
            Some(ref expr) if expr_bool_lit(&expr) == Some(true) => Self::slack(s),
            _ => s.to_string(),
        }
    }

    fn slack(s: &str) -> String {
        let mut escape = false;
        let mut is_braced = false;
        let mut res = String::new();
        let mut iter = s.chars().peekable();

        let mut slack = None;

        while let Some(c) = iter.next() {
            if slack.is_some() && c == ' ' {
                // pass
            } else if slack.is_some() {
                res.push_str(r"\s*");
                res.push(c);
                slack = None;
            } else {
                res.push(c);
            }
            if escape {
                escape = false;
            } else if c == '\\' {
                escape = true;
            } else if c == '[' {
                is_braced = true;
            } else if c == ']' {
                is_braced = false;
            } else if ";,:".contains(c) && iter.peek() == Some(&' ') && !is_braced {
                slack = Some(c);
            }
        }
        res
    }
}

impl Parse for ReAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let params: Punctuated<Expr, Token![,]> = content.parse_terminated(Expr::parse)?;
        let mut iter = params.pairs();

        let expr = iter
            .next()
            .ok_or_else(|| syn::Error::new_spanned(&params, "Expected format string"))?;
        let expr = expr.value();

        let regex = get_regex_str(expr)?;
        let span = expr.span();

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
            span,
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


#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::Span;

    #[test]
    fn prepare_enum() {
        let mut re_attr = ReAttribute {
            span: Span::call_site(),
            regex: r"(a={}|b={}|{}\|)".to_string(),
            params: HashMap::new(),
        };
        re_attr.prepare_enum().unwrap();
        assert_eq!(re_attr.regex, r"(?:(a={})|(b={})|({}\|))");
    }

    #[test]
    fn test_no_regex_mode() {
        let mut re_attr = reattributes_with_mode("Vec{{{}, {}}}", "no_regex");
        re_attr.regex = re_attr.apply_no_regex(&re_attr.regex);
        assert_eq!(re_attr.regex, r"Vec\{{{}, {}\}}");
        assert_eq!(
            r"Vec\{(x), (y)\}",
            format!(r"Vec\{{{}, {}\}}", "(x)", "(y)")
        );

        let s = r"\[T]/ *+* -.- (\|)(^_^)(|/) &&";
        let mut re_attr = reattributes_with_mode(s, "no_regex");
        re_attr.regex = re_attr.apply_no_regex(&re_attr.regex);
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
        re_attr.regex = re_attr.apply_slack(&re_attr.regex);
        assert_eq!(re_attr.regex, r"Vec\({a},\s*{b}\)");

        let mut re_attr = reattributes_with_mode(r"Vec\({a},ax {b}\)", "slack");
        re_attr.regex = re_attr.apply_slack(&re_attr.regex);
        assert_eq!(re_attr.regex, r"Vec\({a},ax {b}\)");

        // slack mode should not be applied to characters in [] groups
        let mut re_attr = reattributes_with_mode(r"{a}[, ]{b}", "slack");
        re_attr.regex = re_attr.apply_slack(&re_attr.regex);
        assert_eq!(re_attr.regex, r"{a}[, ]{b}");

        let mut re_attr = reattributes_with_mode(r"{a}\[, \]{b}", "slack");
        re_attr.regex = re_attr.apply_slack(&re_attr.regex);
        assert_eq!(re_attr.regex, r"{a}\[,\s*\]{b}");

        let mut re_attr = reattributes_with_mode(r"{a}(, |; ){b}", "slack");
        re_attr.regex = re_attr.apply_slack(&re_attr.regex);
        assert_eq!(re_attr.regex, r"{a}(,\s*|;\s*){b}");
    }

    fn reattributes_with_mode(s: &str, mode: &str) -> ReAttribute {
        let mut params = HashMap::new();
        let expr_true: Expr = parse_quote!(true);
        params.insert(mode.to_string(), expr_true);
        ReAttribute {
            span: Span::call_site(),
            regex: s.to_string(),
            params,
        }
    }
}
