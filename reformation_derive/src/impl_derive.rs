use proc_macro2::TokenStream;
use quote::quote;

use crate::derive_input::{
    Arguments, ArgumentsCases, ArgumentsNamed, ArgumentsPos, DeriveInput, EnumVariant, ReType
};

/// Generate ```TokenStream``` with implementation of ```Reformation``` and ```FromStr``` for `input`
pub fn impl_all(input: &DeriveInput) -> TokenStream {
    let reformation = impl_reformation(input);
    let res = quote! {
        #reformation
    };
    res
}

/// Generate ```TokenStream``` with implementation of ```Reformation``` for `input`
fn impl_reformation(input: &DeriveInput) -> TokenStream {
    let ident = input.ident();
    let mut impl_gen = input.generics().clone();
    impl_gen.params.push(parse_quote!('input));
    let (impl_gen, _, _) = impl_gen.split_for_impl();
    let (_, type_gen, where_clause) = input.generics().split_for_impl();

    let regex_tts = fn_regex_token_stream(&input);
    let count = fn_captures_count_token_stream(&input);
    let from_captures = fn_from_captures_token_stream(&input);
    let parse = fn_parse_token_stream(&input);

    if input.generics().type_params().next().is_some(){
        return quote!{
            compile_error!{"Generic type parameters not supported. "}
        };
    }
    quote! {
        impl #impl_gen ::reformation::Reformation<'input> for #ident #type_gen #where_clause{
            #regex_tts
            #count
            #from_captures
            #parse
        }
    }
}

fn fn_parse_token_stream(input: &DeriveInput) -> TokenStream{
    if input.use_tls_for_parse(){
        fn_parse_token_stream_tls(input)
    }else{
        fn_parse_token_stream_regular(input)
    }
}

fn fn_parse_token_stream_tls(input: &DeriveInput) -> TokenStream{
    let ident = input.ident();
    let ident2 = ident;
    let ident_str = ident.to_string();
    quote! {
        fn parse(string: &'input str) -> Result<Self, ::reformation::Error>{
            ::reformation::lazy_static!{
                static ref RE: ::reformation::Regex = {
                    let s = format!(r"\A{}\z", <#ident2 as ::reformation::Reformation>::regex_str());
                    ::reformation::Regex::new(&s)
                        .unwrap_or_else(|e| panic!("Cannot compile regex for {}: {}", #ident_str, e))
                };
            }

            use std::cell::RefCell;
            use std::ops::{Deref, DerefMut};
            thread_local!(static LOC: RefCell<::reformation::CaptureLocations> = RefCell::new(RE.capture_locations()));

            LOC.with(|loc|{
                let is_ok = {
                    let res = RE.captures_read(loc.borrow_mut().deref_mut(), string);
                    res.is_some()
                };
                if is_ok{
                    let guard = loc.borrow();
                    let r = guard.deref();
                    let captures = ::reformation::Captures::new(r, string);
                    Self::from_captures(&captures, 1)
                }else{
                    Err(
                        ::reformation::Error::NoRegexMatch(::reformation::NoRegexMatch{
                            format: Self::regex_str(),
                            request: string.to_string(),
                        })
                    )
                }
            })
        }
    }
}

fn fn_parse_token_stream_regular(input: &DeriveInput) -> TokenStream{
    let ident = input.ident();
    let ident2 = ident;
    let ident_str = ident.to_string();
    quote! {
        fn parse(string: &'input str) -> Result<Self, ::reformation::Error>{
            ::reformation::lazy_static!{
                static ref RE: ::reformation::Regex = {
                    let s = format!(r"\A{}\z", <#ident2 as ::reformation::Reformation>::regex_str());
                    ::reformation::Regex::new(&s)
                        .unwrap_or_else(|e| panic!("Cannot compile regex for {}: {}", #ident_str, e))
                };
            }
            let mut loc = RE.capture_locations();
            if let Some(_) = RE.captures_read(&mut loc, string){
                let captures = ::reformation::Captures::new(&loc, string);
                Self::from_captures(&captures, 1)
            }else{
                Err(
                    ::reformation::Error::NoRegexMatch(::reformation::NoRegexMatch{
                        format: Self::regex_str(),
                        request: string.to_string(),
                    })
                )
            }
        }
    }
}

/// Generate ```TokenStream``` representing method `regex_str`
fn fn_regex_token_stream(input: &DeriveInput) -> TokenStream {
    let base = input.regex_format_string();
    let args = input.arguments();
    if args.is_empty() {
        return quote! {
            fn regex_str() -> &'static str{
                #base
            }
        };
    }
    let regex_args = args.regex_arguments();
    let res = quote! {
        #[inline]
        fn regex_str() -> &'static str{
            // Cannot pass lifetime through types via lazy_static
            let re = unsafe{
                static mut RE: Option<String> = None;
                static INIT: std::sync::Once = std::sync::Once::new();
                INIT.call_once(||{
                    RE = Some(format!(#base, #regex_args));
                });
                &RE
            };
            re.as_ref().unwrap_or_else(|| unreachable!())
        }
    };
    res
}

/// Generate ```TokenStream``` representing method `captures_count`
fn fn_captures_count_token_stream(input: &DeriveInput) -> TokenStream {
    let args = input.arguments();
    if args.is_empty() {
        return quote! {
            fn captures_count() -> usize{
                0
            }
        };
    }

    let mut base_count = 0;
    if let Arguments::Cases(ref cases) = &args {
        base_count = cases.variants().len();
    }
    let types = args.types();
    quote! {
        #[inline]
        fn captures_count() -> usize{
            let mut acc = #base_count;
            #(
                acc += <#types as ::reformation::Reformation>::captures_count();
            )*
            acc
        }
    }
}

/// Generate ```TokenStream``` representing method `from_captures`
fn fn_from_captures_token_stream(input: &DeriveInput) -> TokenStream {
    match input.arguments() {
        Arguments::Empty => empty_struct_from_captures(input),
        Arguments::Pos(ref args) => tuple_struct_from_captures(input, &args),
        Arguments::Named(ref args) => struct_from_captures(input, &args),
        Arguments::Cases(ref args) => enum_from_captures(input, &args),
    }
}

/// Generate token stream with `from_captures` method for structs of type `struct Ident;`
fn empty_struct_from_captures(input: &DeriveInput) -> TokenStream {
    let name = input.ident();
    quote! {
        #[inline]
        fn from_captures<'_a>(captures: &::reformation::Captures<'_a, 'input>, offset: usize) -> Result<Self, ::reformation::Error>{
            Ok(#name)
        }
    }
}

/// Generate token stream with `from_captures` method for structs of type `struct Ident(arg1, arg2, arg3);`
fn tuple_struct_from_captures(input: &DeriveInput, args: &ArgumentsPos) -> TokenStream {
    let (_, ty_gen, _) = input.generics().split_for_impl();
    let ty_gen = ty_gen.as_turbofish();
    let ident = input.ident();
    let args2 = args;
    quote! {
        #[inline]
        fn from_captures<'_a>(captures: &::reformation::Captures<'_a, 'input>, mut offset: usize) -> Result<Self, ::reformation::Error>{
            let res = #ident #ty_gen(
                #({
                    let res = <#args as ::reformation::Reformation>::from_captures(captures, offset)?;
                    offset += <#args2 as ::reformation::Reformation>::captures_count();
                    res
                }),*
            );
            Ok(res)
        }
    }
}

/// Generate token stream with `from_captures` method for structs of type
/// `
/// struct Ident{
///     name1: ty1,
///     name2: ty2,
///     ...
/// };
/// `
fn struct_from_captures(input: &DeriveInput, args: &ArgumentsNamed) -> TokenStream {
    let (_, ty_gen, _) = input.generics().split_for_impl();
    let ty_gen = ty_gen.as_turbofish();
    let ident = input.ident();
    let (arg_names, arg_types) = args.split_names_types();
    let arg_types = arg_types;
    let arg_types2 = arg_types;
    let (default_arg, default_type) = args.default_fields();
    let res = quote! {
        #[inline]
        fn from_captures<'_a>(captures: &::reformation::Captures<'_a, 'input>, mut offset: usize) -> Result<Self, ::reformation::Error>{
            let res = #ident #ty_gen{
                #(
                    #arg_names: {
                        let res = <#arg_types as ::reformation::Reformation>::from_captures(captures, offset)?;
                        offset += <#arg_types2 as ::reformation::Reformation>::captures_count();
                        res
                    },
                )*

                #(
                    #default_arg: <#default_type as std::default::Default>::default(),
                )*
            };
            Ok(res)
        }
    };
    res
}

/// Generate token stream with `from_captures` method for enum
fn enum_from_captures(input: &DeriveInput, args: &ArgumentsCases) -> TokenStream {
    let variants = args
        .variants()
        .iter()
        .map(|x| enum_variant_from_captures(input, x));
    quote! {
        #[inline]
        fn from_captures<'a>(captures: &::reformation::Captures<'a, 'input>, mut offset: usize) -> Result<Self, ::reformation::Error>{
            #(#variants)*

            panic!("No mathing variants")
        }
    }
}

fn enum_variant_from_captures(derive_input: &DeriveInput, variant: &EnumVariant) -> TokenStream {
    let fields = variant.fields();
    let fields2 = fields;
    let fields3 = fields;
    let ident = variant.ident();
    let enum_ident = derive_input.ident();
    let (_, type_gen, _) = derive_input.generics().split_for_impl();
    if fields.is_empty() {
        quote! {
            if captures.get(offset).is_some(){
                return Ok(#enum_ident #type_gen :: #ident);
            }
            offset += 1;
        }
    } else {
        let res = quote! {
            if captures.get(offset).is_some(){
                offset += 1;
                let res = #enum_ident #type_gen :: #ident(
                    #({
                        let res = <#fields as ::reformation::Reformation>::from_captures(captures, offset)?;
                        offset += <#fields2 as ::reformation::Reformation>::captures_count();
                        res
                    }),*
                );
                return Ok(res);
            }
            #(
                offset += <#fields3 as ::reformation::Reformation>::captures_count();
            )*
            offset += 1;
        };
        res
    }
}

trait GetRegexArguments {
    fn regex_arguments(&self) -> TokenStream;
}

impl GetRegexArguments for Arguments {
    fn regex_arguments(&self) -> TokenStream {
        match self {
            Arguments::Cases(c) => c.regex_arguments(),
            Arguments::Named(n) => n.regex_arguments(),
            Arguments::Pos(p) => p.regex_arguments(),
            Arguments::Empty => quote!(),
        }
    }
}

impl GetRegexArguments for ArgumentsNamed {
    fn regex_arguments(&self) -> TokenStream {
        let (names, types) = self.split_names_types();
        let types = types.iter().map(regex_for_type);
        quote! {
            #(#names = #types),*
        }
    }
}

impl GetRegexArguments for ArgumentsPos {
    fn regex_arguments(&self) -> TokenStream {
        let types = (&self).into_iter()
            .map(regex_for_type);
        quote! {
            #(#types),*
        }
    }
}

impl GetRegexArguments for ArgumentsCases {
    fn regex_arguments(&self) -> TokenStream {
        let variants = self.variants().iter().map(|x| x.regex_arguments());

        let res = quote! {
            #(#variants)*
        };
        res
    }
}

impl GetRegexArguments for EnumVariant {
    fn regex_arguments(&self) -> TokenStream {
        let types = self.fields().iter().map(regex_for_type);
        let res = quote! {
            #(#types,)*
        };
        res
    }
}

fn regex_for_type(ty: &ReType) -> TokenStream{
    let override_ = ty.attr.regex_string.as_ref();
    if let Some(s) = override_{
        let s = format!("({})", s);
        quote!{
            {
                ::reformation::assert_primitive::<#ty>();
                #s
            }
        }
    }else{
        quote!{
            <#ty as ::reformation::Reformation>::regex_str()
        }
    }
}
