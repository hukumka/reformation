use crate::format::Format;
use crate::reformation_attribute::ReformationAttribute;
use lazy_static::lazy_static;
use proc_macro2::Span;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use syn::spanned::Spanned;
use syn::{
    Attribute, Data, DataEnum, DataStruct, Generics, Ident,
    Type, Variant,
};

mod errors {
    pub use crate::error_messages::*;
}

/// DeriveInput parsed into easily manipulated view
pub struct DeriveInput {
    ident: Ident,
    generics: Generics,
    arguments: Arguments,
    final_regex_str: String,
}

/// Fields specification of derive input
pub enum Arguments {
    Named(ArgumentsNamed),
    Pos(ArgumentsPos),
    Empty,
    Cases(ArgumentsCases),
}

/// Fields specification of struct
pub struct ArgumentsNamed {
    fields: Vec<Ident>,
    types: Vec<ReType>,
    defaults: HashMap<Ident, ReType>,
}

/// Fields specification of tuplestruct
pub struct ArgumentsPos(Vec<ReType>);

/// Field specification of enums
pub struct ArgumentsCases(Vec<EnumVariant>);

/// Single variant of enum
pub struct EnumVariant {
    ident: Ident,
    types: Vec<ReType>,
}

pub struct ReType{
    pub ty: Type,
    pub attr: Option<ReformationAttribute>,
}

impl quote::ToTokens for ReType{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream){
        self.ty.to_tokens(tokens);
    }
}

// ------------------------------------
// Methods required for generating implementation

impl DeriveInput {
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
    pub fn generics(&self) -> &Generics {
        &self.generics
    }
    pub fn arguments(&self) -> &Arguments {
        &self.arguments
    }

    pub fn regex_format_string(&self) -> &str {
        &self.final_regex_str
    }

    fn apply_where(&mut self, attributes: ReformationAttribute){
        if let Some(clause) = attributes.override_where{
            self.generics.where_clause = clause;
        }else{
            for p in self.generics.type_params_mut(){
                p.bounds.push(parse_quote!(::reformation::Reformation<'input>));
            }
        }
    }
}

impl Arguments {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn len(&self) -> usize {
        match self {
            Arguments::Cases(c) => c.len(),
            Arguments::Named(n) => n.len(),
            Arguments::Pos(p) => p.len(),
            Arguments::Empty => 0,
        }
    }

    pub fn types(&self) -> Vec<&ReType> {
        let res: Vec<_>;
        match self {
            Arguments::Cases(c) => {
                res = c.variants().iter().map(|x| x.fields()).flatten().collect();
            }
            Arguments::Pos(p) => {
                res = p.0.iter().collect();
            }
            Arguments::Named(n) => {
                res = n.types.iter().collect();
            }
            Arguments::Empty => res = vec![],
        }
        res
    }
}

impl ArgumentsNamed {
    fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn split_names_types(&self) -> (&[Ident], &[ReType]) {
        (&self.fields, &self.types)
    }

    pub fn default_fields(&self) -> (Vec<&Ident>, Vec<&ReType>) {
        let (names, types): (Vec<_>, Vec<_>) = self.defaults.iter().unzip();
        (names, types)
    }
}

impl ArgumentsPos {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> IntoIterator for &'a ArgumentsPos {
    type Item = &'a ReType;
    type IntoIter = ::std::slice::Iter<'a, ReType>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl ArgumentsCases {
    fn len(&self) -> usize {
        self.variants().len()
    }
    pub fn variants(&self) -> &[EnumVariant] {
        self.0.as_slice()
    }
}

impl EnumVariant {
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
    pub fn fields(&self) -> &[ReType] {
        self.types.as_slice()
    }
}

// ------------------
// Methods required for parsing DeriveInput from syn::DeriveInput
impl DeriveInput {
    pub fn parse(input: syn::DeriveInput) -> syn::Result<DeriveInput> {
        let span = input.span();
        let ident = input.ident;
        let generics = input.generics;

        let final_regex_str;
        let arguments;
        let attributes;

        let data = input.data;
        let attrs = input.attrs;
        match data {
            Data::Struct(struct_) => {
                let args = StructArguments::parse(&struct_)?;
                let format = StructFormat::parse(span, attrs)?;
                arguments = args.to_arguments(&format)?;
                final_regex_str = format.build_regex()?;
                attributes = format.attributes;
            }
            Data::Enum(enum_) => {
                let args = ArgumentsCases::parse(&enum_)?;
                let format = EnumFormat::parse(span, attrs)?;
                final_regex_str = args.build_regex(&format)?;
                arguments = Arguments::Cases(args);
                attributes = format.attributes;
            }
            Data::Union(_) => {
                return Err(errors::unions_are_not_supported(span));
            }
        }
        let mut res = Self{
            ident,
            generics,
            arguments,
            final_regex_str,
        };
        res.apply_where(attributes);
        Ok(res)
    }
}

struct StructArguments{
    names: Option<Vec<Ident>>,
    types: Vec<ReType>,
}

impl StructArguments{
    fn parse(struct_: &DataStruct) -> syn::Result<Self>{
        let names: Option<Vec<_>> = struct_.fields.iter()
            .map(|f| f.ident.clone())
            .collect();
        let types: syn::Result<Vec<_>> = struct_.fields.iter()
            .map(|f| ReType::new(&f.ty, &f.attrs))
            .collect();
        Ok(Self{
            names,
            types: types?
        })
    }

    fn to_arguments(self, format: &StructFormat) -> syn::Result<Arguments>{
        if self.names.is_some(){
            self.to_input_named(format)
        }else{
            self.to_input_pos(format)
        }
    }

    fn to_input_pos(self, format: &StructFormat) -> syn::Result<Arguments>{
        if !format.format.named_arguments().is_empty(){
            return Err(errors::unnamed_struct_named_argumens(format.span));
        }
        let real = format.format.positional_arguments();
        let expected = self.types.len();
        if expected != real{
            return Err(errors::unnamed_struct_wrong_argument_count(format.span, real, expected));
        }

        Ok(Arguments::Pos(ArgumentsPos(self.types)))
    }

    fn to_input_named(self, format: &StructFormat) -> syn::Result<Arguments>{
        let names = self.names.unwrap();
        if format.format.positional_arguments() != 0{
            return Err(errors::named_struct_unnamed_arguments(format.span));
        }
        let format_names = format.format.named_arguments();
        // check if every name mention in format string is a field
        let nameset: HashSet<_> = names.iter().map(|x| x.to_string()).collect();
        for name in &format_names{
            if !nameset.contains(name){
                return Err(errors::named_struct_argument_with_no_field(format.span, name));
            }
        }
        // split into default and parsed fields
        let mut fields = vec![];
        let mut types = vec![];
        let mut defaults = HashMap::new();
        for (name, ty) in names.into_iter().zip(self.types){
            if format_names.contains(&name.to_string()){
                fields.push(name);
                types.push(ty);
            }else{
                defaults.insert(name, ty);
            }
        }
        let args = ArgumentsNamed{
            fields,
            types,
            defaults
        };
        Ok(Arguments::Named(args))
    }
}

struct StructFormat {
    span: Span,
    format: Format,
    attributes: ReformationAttribute,
}

impl StructFormat {
    fn parse(span: Span, attrs: Vec<Attribute>) -> syn::Result<Self> {
        let attr = ReformationAttribute::parse(span, attrs)?;
        let format = Format::build(&attr.regex()?)
            .map_err(|e| errors::format_error(span, e))?;

        let format = apply_modes(format, &attr);
        Ok(Self { span, format, attributes: attr })
    }

    fn build_regex(&self) -> syn::Result<String> {
        test_format_regex(self.span, &self.format)?;
        Ok(self.format.to_string())
    }
}

fn test_format_regex(span: Span, format: &Format) -> syn::Result<()> {
    match Regex::new(&format.build_empty()) {
        Ok(r) => {
            if r.capture_names().flatten().next().is_some() {
                return Err(errors::format_string_contains_named_capture(span));
            }
        }
        Err(e) => {
            return Err(syn::Error::new(span, e));
        }
    }
    Ok(())
}

struct EnumFormat {
    span: Span,
    format: Vec<Format>,
    attributes: ReformationAttribute,
}

impl EnumFormat {
    fn parse(span: Span, attrs: Vec<Attribute>) -> syn::Result<Self> {
        let attr = ReformationAttribute::parse(span, attrs)?;
        let variants;
        if attr.regex_string.is_some(){
            variants = Self::split_by_pipe(&attr)?;
        }else{
            variants = vec![];
        }
        let format: syn::Result<Vec<_>> = variants
            .into_iter()
            .map(|s| {
                let format = Format::build(s).map_err(|e| errors::format_error(span, e))?;
                let format = apply_modes(format, &attr);
                test_format_regex(span, &format)?;
                Ok(format)
            })
            .collect();
        let format = format?;
        Ok(Self { span, format, attributes: attr })
    }

    // regular split cannot be used, because it will break at following cases:
    // "(a(b|c)|d)" -> ["a(b|c)", "d"]
    // (a\||d) -> ["a|", "d"]
    fn split_by_pipe(attr: &ReformationAttribute) -> syn::Result<Vec<&str>> {
        let mut iter = attr.regex()?.char_indices();
        let first = iter.next().map(|(_, c)| c);
        if first != Some('(') {
            return Err(errors::format_string_enum_start(attr.span, first));
        }

        let mut res = vec![];
        let mut start = '('.len_utf8();
        let mut bracket_level = 1;

        while let Some((i, c)) = iter.next() {
            if c == '(' {
                bracket_level += 1;
            } else if c == ')' {
                bracket_level -= 1;
                if bracket_level == 0 {
                    res.push(attr.regex()?.get(start..i).unwrap());
                    break;
                }
            } else if c == '\\' {
                iter.next();
            } else if c == '|' {
                res.push(attr.regex()?.get(start..i).unwrap());
                start = i + c.len_utf8();
            }
        }

        if let Some((i, _)) = iter.next() {
            let syffix = attr.regex()?.get(i..).unwrap();
            return Err(errors::format_string_enum_end(attr.span, syffix));
        } else if bracket_level != 0 {
            let variant = attr.regex()?.get(start..).unwrap();
            return Err(errors::format_string_enum_unclosed(attr.span, variant));
        }

        Ok(res)
    }
}

fn apply_modes(mut format: Format, modes: &ReformationAttribute) -> Format {
    if modes.no_regex {
        format.map_substrings(escape_regex);
    } else {
        format.map_substrings(no_capturing_groups);
    }
    if modes.slack {
        format.map_substrings(slack);
    }
    format
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

fn escape_regex(input: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[\|\[\]\(\)\{\}\.\?\+\*\^\\]").unwrap();
    }
    RE.replace_all(input, r"\$0").to_string()
}

fn slack(input: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"([,:;])\s+").unwrap();
    }
    RE.replace_all(input, r"$1\s*").to_string()
}


impl ArgumentsCases {
    fn parse(enum_: &DataEnum) -> syn::Result<Self> {
        let res: syn::Result<Vec<_>> = enum_
            .variants
            .iter()
            .map(EnumVariant::parse)
            .collect();
        let res = res?;
        Ok(ArgumentsCases(res))
    }

    fn build_regex(&self, format: &EnumFormat) -> syn::Result<String>{
        if format.format.is_empty(){
            return self.build_regex_new(format);
        }
        let real = format.format.len();
        let expected = self.variants().len();
        if real != expected{
            return Err(errors::enum_wrong_number_of_variants_covered(format.span, real, expected));
        }
        let mut result = String::new();
        for (v, f) in self.variants().iter().zip(&format.format){
            result.push_str("(");
            if !f.named_arguments().is_empty(){
                return Err(errors::enum_named_argumens(format.span));
            }
            let real = f.positional_arguments();
            let expected = v.types.len();
            if real != expected{
                return Err(errors::enum_variant_wrong_number_of_values(format.span, &v.ident, real, expected));
            }
            result.push_str(&f.to_string());
            result.push_str(")|");
        }
        // we have "|" one the end. Lets get rid of it
        result.pop();

        Ok(result)
    }

    fn build_regex_new(&self, format: &EnumFormat) -> syn::Result<String>{
        unimplemented!();
    }
}

impl EnumVariant {
    fn parse(variant: &Variant) -> syn::Result<Self> {
        let fields: syn::Result<Vec<_>> = variant.fields
            .iter()
            .map(|x| ReType::new(&x.ty, &x.attrs))
            .collect();
        Ok(Self {
            ident: variant.ident.clone(),
            types: fields?,
        })
    }
}


impl ReType{
    fn new(ty: &Type, attrs: &[Attribute]) -> syn::Result<Self>{
        let attr = ReformationAttribute::parse(Span::call_site(), attrs.to_vec()).ok();
        Ok(Self{
            ty: ty.clone(),
            attr,
        })
    }
}