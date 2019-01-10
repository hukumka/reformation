use proc_macro2::Span;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{AttrStyle, Attribute, Expr, WhereClause, Lit, Ident};
use crate::syn_helpers::expr_bool_lit;

// to use error::error_name instead of crate::error_messages::error_name
mod errors {
    pub use crate::error_messages::*;
}

/// Struct representing attribute `#[reformation(...)]`
pub struct ReformationAttribute {
    pub span: Span,
    pub regex_string: Option<String>,

    pub slack: bool,
    pub no_regex: bool,
    pub override_where: Option<WhereClause>,
}

impl ReformationAttribute {
    fn new(span: Span) -> Self{
        Self{
            span,
            regex_string: None,
            slack: false,
            no_regex: false,
            override_where: None,
        }
    }

    /// Parse ReformationAttribute from set of attributes on DeriveInput
    pub fn parse(span: Span, attrs: Vec<Attribute>) -> syn::Result<Self> {
        let attr = Self::find_attribute(span, attrs)?;
        let tts = attr.tts;
        let stream_str = quote!(#tts).to_string();
        let res: Self = syn::parse_str(&stream_str)
            .map_err(|e| syn::Error::new(span, e))?;
        Ok(res)
    }

    fn find_attribute(span: Span, attrs: Vec<Attribute>) -> syn::Result<Attribute> {
        attrs
            .into_iter()
            .find(|a| is_reformation_attr(a))
            .ok_or_else(|| errors::no_reformation_attribute(span))
    }

    pub fn regex(&self) -> syn::Result<&str>{
        self.regex_string.as_ref()
            .map(|s| s.as_str())
            .ok_or_else(||{
                errors::no_format_string_in_attribute(self.span)
            })
    }
}

impl Parse for ReformationAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let params: Punctuated<Mode, Token![,]> = content.parse_terminated(Mode::parse)?;
        let mut res = Self::new(Span::call_site());
        for mode in params{
            res.apply(mode)?;
        }
        Ok(res)
    }
}

enum Mode{
    Str(String),
    BoolParam(Ident),
    WhereClauseParam(WhereClause)
}

impl Parse for Mode{
    fn parse(input: ParseStream) -> syn::Result<Self>{
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident){
            let ident: Ident = input.parse()?;
            let _eq: Token![=] = input.parse()?;
            if ident.to_string() == "override_where"{
                // #[reformation("blabla", override_where={where T: })]
                let clause;
                bracketed!(clause in input);
                let clause: WhereClause = clause.parse()?;
                Ok(Mode::WhereClauseParam(clause))
            }else{
                let true_: Expr = input.parse()?;
                expect_true(ident.span(), &ident.to_string(), &true_)?;
                Ok(Mode::BoolParam(ident))
            }
        }else{
            let regex: Lit = input.parse()?;
            match regex{
                Lit::Str(s) => Ok(Mode::Str(s.value())),
                _ => Err(syn::Error::new_spanned(regex, "Expected string literal."))
            }
        }
    }
}

impl ReformationAttribute{
    fn apply(&mut self, mode: Mode) -> syn::Result<()> {
        match mode {
            Mode::BoolParam(ident) => {
                match ident.to_string().as_str(){
                    "no_regex" => {
                        self.no_regex = true;
                        Ok(())
                    },
                    "slack" => {
                        self.slack = true;
                        Ok(())
                    },
                    _ => {
                        Err(errors::attribute_unknown_mode(self.span, &ident.to_string()))
                    }
                }
            }
            Mode::Str(s) => {
                self.regex_string = Some(s);
                Ok(())
            }
            Mode::WhereClauseParam(clause) => {
                self.override_where = Some(clause);
                Ok(())
            },
        }
    }
}

fn expect_true(span: Span, name: &str, value: &Expr) -> syn::Result<()> {
    if expr_bool_lit(value) != Some(true) {
        Err(errors::attribute_mode_expected_true(span, name, value))
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
    quote!(#pound).to_string() == "#"
        && style_cmp
        && quote!(#path).to_string() == "reformation"
}
