use proc_macro2::TokenStream;
use quote::quote;

use crate::derive_input::{
    Arguments, ArgumentsCases, ArgumentsNamed, ArgumentsPos, DeriveInput, EnumVariant,
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
    let ident = input.ident();
    let ident_str = ident.to_string();
    quote! {
        fn parse<'b: 'input>(string: &'b str) -> Result<Self, ::reformation::Error>{
            // cannot use lazy_static, since it cannot access generic types
            // Mutating static mut is unsafe, but ok then used with `Once` sync primitive
            // see example at https://doc.rust-lang.org/std/sync/struct.Once.html
            let re = unsafe{
                static mut RE: Option<::reformation::Regex> = None;
                static ONCE: std::sync::Once = std::sync::Once::new();
                ONCE.call_once(||{
                    let re = Self::regex_str();
                    let re = ::reformation::Regex::new(re)
                        .unwrap_or_else(|x|{
                            // Panicking is allowed due to 'poisoning'
                            // docs reference: https://doc.rust-lang.org/std/sync/struct.Once.html#method.call_once
                            // poisoning explanation: https://doc.rust-lang.org/std/sync/struct.Mutex.html#poisoning
                            panic!("Cannot compile regex for {}: {}", #ident_str, x)
                        });
                    RE = Some(re);
                });
                &RE
            };

            let re = re.as_ref().unwrap_or_else(|| unreachable!());

            // get captures for regular expression and delegete to from_captures method
            let captures = re.captures(string).ok_or_else(||{
                ::reformation::Error::NoRegexMatch(::reformation::NoRegexMatch{
                    format: Self::regex_str(),
                    request: string.to_string(),
                })
            })?;
            // ignore capture group mathing entire expression
            Self::from_captures(&captures, 1)
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
            // cannot use lazy_static, since it cannot access generic types
            // Mutating static mut is unsafe, but ok then used with `Once` sync primitive
            // see example at https://doc.rust-lang.org/std/sync/struct.Once.html
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
        fn from_captures(captures: &::reformation::Captures<'input>, offset: usize) -> Result<Self, ::reformation::Error>{
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
        fn from_captures(captures: &::reformation::Captures<'input>, mut offset: usize) -> Result<Self, ::reformation::Error>{
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
        fn from_captures(captures: &::reformation::Captures<'input>, mut offset: usize) -> Result<Self, ::reformation::Error>{
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
        fn from_captures(captures: &::reformation::Captures<'input>, mut offset: usize) -> Result<Self, ::reformation::Error>{
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
        quote! {
            #(#names = <#types as ::reformation::Reformation>::regex_str()),*
        }
    }
}

impl GetRegexArguments for ArgumentsPos {
    fn regex_arguments(&self) -> TokenStream {
        let types = (&self).into_iter();
        quote! {
            #(<#types as ::reformation::Reformation>::regex_str()),*
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
        let types = self.fields();
        let res = quote! {
            #(<#types as ::reformation::Reformation>::regex_str(),)*
        };
        res
    }
}
