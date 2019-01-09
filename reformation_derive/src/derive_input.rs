use crate::format::Format;
use crate::reformation_attribute::{Modes, ReformationAttribute};
use lazy_static::lazy_static;
use proc_macro2::Span;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use syn::spanned::Spanned;
use syn::{
    Attribute, Data, DataEnum, DataStruct, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident,
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
    types: Vec<Type>,
    defaults: HashMap<Ident, Type>,
}

/// Fields specification of tuplestruct
pub struct ArgumentsPos(Vec<Type>);

/// Field specification of enums
pub struct ArgumentsCases(Vec<EnumVariant>);

/// Single variant of enum
pub struct EnumVariant {
    ident: Ident,
    types: Vec<Type>,
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

    pub fn types(&self) -> Vec<&Type> {
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

    pub fn split_names_types(&self) -> (&[Ident], &[Type]) {
        (&self.fields, &self.types)
    }

    pub fn default_fields(&self) -> (Vec<&Ident>, Vec<&Type>) {
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
    type Item = &'a Type;
    type IntoIter = ::std::slice::Iter<'a, Type>;

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
    pub fn fields(&self) -> &[Type] {
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

        let data = input.data;
        let attrs = input.attrs;
        match data {
            Data::Struct(struct_) => {
                let format = StructFormat::parse(span, attrs)?;
                arguments = Arguments::parse_struct(struct_, &format)?;
                final_regex_str = format.build_string()?;
            }
            Data::Enum(enum_) => {
                let format = EnumFormat::parse(span, attrs)?;
                arguments = Arguments::parse_enum(enum_, &format)?;
                final_regex_str = format.build_string()?;
            }
            Data::Union(_) => {
                return Err(errors::unions_are_not_supported(span));
            }
        }
        Ok(Self {
            ident,
            generics,
            arguments,
            final_regex_str,
        })
    }
}

struct StructFormat {
    span: Span,
    format: Format,
}

impl StructFormat {
    fn parse(span: Span, attrs: Vec<Attribute>) -> syn::Result<Self> {
        let attr = ReformationAttribute::parse(span, attrs)?;
        let format = Format::new(&attr.regex_string).map_err(|e| errors::format_error(span, e))?;

        let format = apply_modes(format, &attr.modes);
        Ok(Self { span, format })
    }

    fn build_string(&self) -> syn::Result<String> {
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
}

impl EnumFormat {
    fn parse(span: Span, attrs: Vec<Attribute>) -> syn::Result<Self> {
        let attr = ReformationAttribute::parse(span, attrs)?;
        let variants = Self::split_by_pipe(&attr)?;
        let format: syn::Result<Vec<_>> = variants
            .into_iter()
            .map(|s| {
                let format = Format::new(s).map_err(|e| errors::format_error(span, e))?;
                let format = apply_modes(format, &attr.modes);
                Ok(format)
            })
            .collect();
        let format = format?;
        Ok(Self { span, format })
    }

    fn build_string(&self) -> syn::Result<String> {
        let mut res = String::new();
        res.push_str("(?:");
        for (i, f) in self.format.iter().enumerate() {
            test_format_regex(self.span, f)?;
            if i != 0 {
                res.push_str("|");
            }
            res.push_str("(");
            res.push_str(&f.to_string());
            res.push_str(")");
        }
        res.push_str(")");
        Ok(res)
    }

    // regular split cannot be used, because it will break at following cases:
    // "(a(b|c)|d)" -> ["a(b|c)", "d"]
    // (a\||d) -> ["a|", "d"]
    fn split_by_pipe(attr: &ReformationAttribute) -> syn::Result<Vec<&str>> {
        let mut iter = attr.regex_string.char_indices();
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
                    res.push(attr.regex_string.get(start..i).unwrap());
                    break;
                }
            } else if c == '\\' {
                iter.next();
            } else if c == '|' {
                res.push(attr.regex_string.get(start..i).unwrap());
                start = i + c.len_utf8();
            }
        }

        if let Some((i, _)) = iter.next() {
            let syffix = attr.regex_string.get(i..).unwrap();
            return Err(errors::format_string_enum_end(attr.span, syffix));
        } else if bracket_level != 0 {
            let variant = attr.regex_string.get(start..).unwrap();
            return Err(errors::format_string_enum_unclosed(attr.span, variant));
        }

        Ok(res)
    }
}

fn apply_modes(mut format: Format, modes: &Modes) -> Format {
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
    let res = RE.replace_all(input, r"\$0").to_string();
    res
}

fn slack(input: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"([,:;])\s+").unwrap();
    }
    RE.replace_all(input, r"$1\s*").to_string()
}

impl Arguments {
    fn parse_struct(struct_: DataStruct, format: &StructFormat) -> syn::Result<Self> {
        match struct_.fields {
            Fields::Named(fields) => {
                let args = ArgumentsNamed::parse(fields, format)?;
                Ok(Arguments::Named(args))
            }
            Fields::Unnamed(fields) => {
                let args = ArgumentsPos::parse(fields, format)?;
                Ok(Arguments::Pos(args))
            }
            Fields::Unit => {
                if !format.format.no_arguments() {
                    return Err(errors::empty_struct_with_arguments(
                        format.span,
                        &format.format,
                    ));
                }
                Ok(Arguments::Empty)
            }
        }
    }

    fn parse_enum(enum_: DataEnum, format: &EnumFormat) -> syn::Result<Self> {
        ArgumentsCases::parse(enum_, format).map(|x| Arguments::Cases(x))
    }
}

impl ArgumentsCases {
    fn parse(enum_: DataEnum, format: &EnumFormat) -> syn::Result<Self> {
        let enum_variant_count = enum_.variants.len();
        let format_variant_count = format.format.len();
        if enum_variant_count != format_variant_count {
            return Err(errors::enum_wrong_number_of_variants_covered(
                format.span,
                format_variant_count,
                enum_variant_count,
            ));
        }
        let res: syn::Result<Vec<_>> = enum_
            .variants
            .iter()
            .zip(&format.format)
            .map(|(v, f)| EnumVariant::parse(format.span, v, f))
            .collect();
        let res = res?;
        Ok(ArgumentsCases(res))
    }
}

impl EnumVariant {
    fn parse(span: Span, variant: &Variant, format: &Format) -> syn::Result<Self> {
        let fields: Vec<_> = variant.fields.iter().map(|x| x.clone().ty).collect();
        let expected = fields.len();
        let real = format.positional_arguments();
        if expected != real {
            return Err(errors::enum_variant_wrong_number_of_values(
                span,
                &variant.ident,
                real,
                expected,
            ));
        }
        if !format.named_arguments().is_empty() {
            return Err(errors::enum_named_argumens(span));
        }
        Ok(Self {
            ident: variant.ident.clone(),
            types: fields,
        })
    }
}

impl ArgumentsNamed {
    fn parse(struct_: FieldsNamed, format: &StructFormat) -> syn::Result<Self> {
        if format.format.positional_arguments() > 0 {
            return Err(errors::named_struct_unnamed_arguments(format.span));
        }
        let args = format.format.named_arguments();
        let mut fields = vec![];
        let mut types = vec![];
        let mut defaults = HashMap::new();
        let mut fields_set = HashSet::new();

        for field in struct_.named {
            let ident = field.ident.unwrap();
            let type_ = field.ty;
            let name = ident.to_string();
            if args.contains(&name) {
                fields.push(ident);
                types.push(type_);
                fields_set.insert(name);
            } else {
                defaults.insert(ident, type_);
            }
        }

        for name in &args {
            if !fields_set.contains(name) {
                return Err(errors::named_struct_argument_with_no_field(
                    format.span,
                    name,
                ));
            }
        }

        Ok(Self {
            fields,
            types,
            defaults,
        })
    }
}

impl ArgumentsPos {
    fn parse(struct_: FieldsUnnamed, format: &StructFormat) -> syn::Result<Self> {
        let fields: Vec<_> = struct_.unnamed.into_iter().map(|x| x.ty).collect();
        let real = fields.len();
        let expected = format.format.positional_arguments();
        if real != expected {
            return Err(errors::unnamed_struct_wrong_argument_count(
                format.span,
                real,
                expected,
            ));
        }
        if !format.format.named_arguments().is_empty() {
            return Err(errors::unnamed_struct_named_argumens(format.span));
        }
        Ok(ArgumentsPos(fields))
    }
}
