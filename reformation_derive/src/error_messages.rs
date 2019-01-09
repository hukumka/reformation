use syn::{Error, Expr, Ident};
use proc_macro2::Span;
use syn::parse::ParseStream;
use crate::format::{Format, FormatError};

pub fn no_reformation_attribute(span: Span) -> Error{
    let msg = "Expected attribute `#[reformation(r\"...\")], containing format string.";
    Error::new(span, msg)
}

pub fn no_format_string_in_attribute(stream: ParseStream) -> Error{
    let msg = "Attribute `#[reformation(...)]` must containt format string as first item.";
    stream.error(msg)
}

pub fn attribute_mode_is_not_assignment(span: Span) -> Error{
    let msg = "Modes specified in #[reformation(\"...\", ..modes)]\
                      correspond to following syntax: `mode_name=expression`.";
    Error::new(span, msg)
}

pub fn attribute_mode_expected_true(span: Span, name: &str, value: &Expr) -> Error{
    let msg = format!(
        "Mode `{name}` must be specified as `{name}=true`, but value `{value}` was supplied.",
        name=name, value=quote!(#value).to_string()
    );
    Error::new(span, msg)
}

pub fn attribute_unknown_mode(span: Span, name: &str) -> Error{
    let msg = format!(
        "Unknown mode `{}`.", name
    );
    Error::new(span, msg)
}

pub fn format_error(span: Span, error: FormatError) -> Error{
    Error::new(span, error)
}

pub fn format_string_enum_start(span: Span, c: Option<char>) -> Error{
    let msg;
    if let Some(c) = c {
        msg = format!("Enum format string must be \"(variant1|variant2|...|variantN)\", but starts with char {:?}.", c);
    }else{
        msg = "Enum format string must be \"(variant1|variant2|...|variantN)\", but it is empty.".to_string();
    }
    Error::new(span, msg)
}

pub fn format_string_enum_end(span: Span, syffix: &str) -> Error{
    let msg = format!(
        "Enum format string must be \"(variant1|variant2|...|variantN)\",\
         but syffix {:?} found after closing bracket",
        syffix
    );
    Error::new(span, msg)
}

pub fn format_string_enum_unclosed(span: Span, syffix: &str) -> Error{
    let msg = format!(
        "Enum format string must be \"(variant1|variant2|...|variantN)\".\
         Expected `)` after variant {:?}.",
        syffix,
    );
    Error::new(span, msg)
}

pub fn empty_struct_with_arguments(span: Span, _format: &Format) -> Error{
    let msg = "Unit structs cannot contain arguments in format string.";
    Error::new(span, msg)
}

pub fn named_struct_unnamed_arguments(span: Span) -> Error{
    let msg = "Structs with named fields cannot have positional arguments in format string.";
    Error::new(span, msg)
}

pub fn named_struct_argument_with_no_field(span: Span, name: &str) -> Error{
    let msg = format!("Format string contains argument {{{0}}}, but no corresponding field `{0}` found.", name);
    Error::new(span, msg)
}

pub fn unnamed_struct_wrong_argument_count(span: Span, real: usize, expected: usize) -> Error{
    let msg = format!("Struct contains {} fields, but format string has {}.", expected, real);
    Error::new(span, msg)
}

pub fn unnamed_struct_named_argumens(span: Span) -> Error{
    let msg = "Named struct must not contain named arguments.";
    Error::new(span, msg)
}

pub fn format_string_contains_named_capture(span: Span) -> Error{
    let msg = "Format string must not contain any named capture groups.";
    Error::new(span, msg)
}

pub fn enum_wrong_number_of_variants_covered(span: Span, real: usize, expected: usize) -> Error{
    let msg = format!(
        "Format string covers {} variants, but enum contains {}",
        real, expected
    );
    Error::new(span, msg)
}

pub fn enum_variant_wrong_number_of_values(span: Span, ident: &Ident, real: usize, expected: usize) -> Error{
    let msg = format!(
        "Variant {} contains {} values, but {} were specified in format string",
        ident, expected, real,
    );
    Error::new(span, msg)
}

pub fn enum_named_argumens(span: Span) -> Error{
    let msg = "Enum must not contain named arguments.";
    Error::new(span, msg)
}

pub fn unions_are_not_supported(span: Span) -> Error{
    let msg = "Unions are not supported.";
    Error::new(span, msg)
}