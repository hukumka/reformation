use crate::syn_helpers::{expr_bool_lit, expr_into_attribute_param, get_regex_str};
use proc_macro2::Span;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{AttrStyle, Attribute, Expr};

// to use error::error_name instead of crate::error_messages::error_name
mod errors {
    pub use crate::error_messages::*;
}

/// Struct representing attribute `#[reformation(...)]`
pub struct ReformationAttribute {
    pub span: Span,
    pub regex_string: String,
    pub modes: Modes,
}

/// Mode applied to ReformationAttribute
pub struct Modes {
    pub slack: bool,
    pub no_regex: bool,
}

impl ReformationAttribute {
    /// Parse ReformationAttribute from set of attributes on DeriveInput
    pub fn parse(span: Span, attrs: Vec<Attribute>) -> syn::Result<Self> {
        let attr = Self::find_attribute(span, attrs)?;
        let tts = attr.tts;
        let stream_str = quote!(#tts).to_string();
        let res: Self = syn::parse_str(&stream_str)?;
        Ok(res)
    }

    fn find_attribute(span: Span, attrs: Vec<Attribute>) -> syn::Result<Attribute> {
        attrs
            .into_iter()
            .find(|a| is_reformation_attr(a))
            .ok_or_else(|| errors::no_reformation_attribute(span))
    }
}

impl Parse for ReformationAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let params: Punctuated<Expr, Token![,]> = content.parse_terminated(Expr::parse)?;
        let mut iter = params.pairs();

        let expr = iter
            .next()
            .ok_or_else(|| errors::no_format_string_in_attribute(input))?;
        let expr = expr.value();

        let regex_string = get_regex_str(expr)?;
        let span = expr.span();

        let mut modes = Modes::default();
        for e in iter {
            modes.apply(*e.value())?;
        }

        Ok(Self {
            regex_string,
            span,
            modes,
        })
    }
}

impl Default for Modes {
    fn default() -> Self {
        Modes {
            no_regex: false,
            slack: false,
        }
    }
}

impl Modes {
    fn apply(&mut self, expr: &Expr) -> syn::Result<()> {
        let (name, value) = expr_into_attribute_param(expr)
            .ok_or_else(|| errors::attribute_mode_is_not_assignment(expr.span()))?;

        match name.as_str() {
            "no_regex" => {
                Self::expect_true(expr.span(), &name, value)?;
                self.no_regex = true;
                Ok(())
            }
            "slack" => {
                Self::expect_true(expr.span(), &name, value)?;
                self.slack = true;
                Ok(())
            }
            _ => Err(errors::attribute_unknown_mode(expr.span(), &name)),
        }
    }

    fn expect_true(span: Span, name: &str, value: &Expr) -> syn::Result<()> {
        if expr_bool_lit(value) != Some(true) {
            Err(errors::attribute_mode_expected_true(span, name, value))
        } else {
            Ok(())
        }
    }
}

fn is_reformation_attr(a: &Attribute) -> bool {
    let pound = &a.pound_token;
    let path = &a.path;
    let style_cmp = match a.style {
        AttrStyle::Outer => true,
        _ => false,
    };
    quote!(#pound).to_string() == "#"
        && style_cmp
        && quote!(#path).to_string() == "reformation"
}
