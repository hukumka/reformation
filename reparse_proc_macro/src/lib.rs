#![recursion_limit="128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

use std::collections::HashSet;
use std::ops::Deref;

use proc_macro2::TokenStream;
use syn::spanned::Spanned;
use syn::{Attribute, AttrStyle};
use syn::{DeriveInput, Data, Field, Fields};
use syn::{GenericParam, Generics};
use syn::{Expr, Lit};


#[proc_macro_derive(ReParse, attributes(re_parse))]
pub fn re_parse_derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream{
    let ds = parse_macro_input!(item as DeriveInput);

    // find #[re_parse] a
    let regex_tts = ds.attrs.iter()
        .filter_map(get_re_parse_attribute)
        .next();
    let regex_tts = if let Some(regex_tts) = regex_tts{
        proc_macro::TokenStream::from(regex_tts.clone())
    }else{
        return proc_macro::TokenStream::from(quote!{
            compile_error!{"Attribute #[re_parse(r\"..\")] containing format string not found."}
        });
    };
    let re = parse_macro_input!(regex_tts as Expr);

    let expanded = match impl_from_str_body(re, &ds){
        Ok(from_str_token_stream) => {
            let generics = add_trait_bounds(ds.generics);
            let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

            let name = &ds.ident;
            quote!{
                impl #impl_generics std::str::FromStr for #name #ty_generics #where_clause{
                    type Err = Box<std::error::Error>;

                    fn from_str(input_str: &str)->Result<Self, Self::Err>{
                        #from_str_token_stream
                    }
                }
            }
        },
        Err(errors) => {
            quote!{
                #errors
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(::reparse::ReParse));
        }
    }
    generics
}


fn get_re_parse_attribute(a: &Attribute)->Option<&TokenStream>{
    let pound = &a.pound_token;
    let path = &a.path;
    let style_cmp = match a.style{
        AttrStyle::Outer => true,
        _ => false
    };
    let is_re_parse = quote!(#pound).to_string() == "#"
        && style_cmp
        && quote!(#path).to_string() == "re_parse";
    if is_re_parse{
        Some(&a.tts)
    }else{
        None
    }
}


fn impl_from_str_body(re: Expr, ds: &DeriveInput)->Result<TokenStream, TokenStream>{
    let re_str = get_regex_str(&re)?;
    let args = arguments(&re_str);
    let fields = get_fields(&ds)?;

    let (names, types): (Vec<_>, Vec<_>) = fields.iter()
        .map(|x| (x.ident.as_ref().unwrap(), &x.ty))
        .filter(|(ident, _ty)| args.contains(&ident.to_string()))
        .unzip();

    // hack over unability of quote to use same variable multiple times
    let types1 = &types;
    let names1 = &names;
    let names2 = &names;
    let types2 = &types;
    let names3 = &names;
    let types3 = &types;

    let res = quote!{
        reparse::lazy_static!{
            static ref RE: reparse::Regex = {
                let re_str = &format!(#re_str, #(#names1=<#types1 as reparse::ReParse>::regex_str()),*);
                reparse::Regex::new(re_str)
                    .unwrap_or_else(|x| panic!("Cannot compile regex {:?}", ))
            };
        }
        let captures = RE.captures(input_str).unwrap();
        let mut offset = 1;
        #(
            let #names2 = <#types2 as reparse::ReParse>::from_captures(&captures, offset)?;
            offset += <#types3 as reparse::ReParse>::captures_count();
        )*
        Ok(Self{
            #(#names3,)*
        })
    };

    Ok(res)
}


fn get_fields(struct_: &DeriveInput)->Result<Vec<&Field>, TokenStream>{
    if let Data::Struct(ref ds) = struct_.data{
        let fields: Vec<_> = ds.fields.iter().collect();

        if let Fields::Named(_) = ds.fields{
            Ok(fields)
        }else{
            Err(quote_spanned!{ds.fields.span()=>
                compile_error!{"regex_parse supports only structs with named fields."}
            })
        }
    }else{
        Err(quote_spanned!{struct_.span()=>
            compile_error!{"regex_parse supports only structs."}
        })
    }
}


fn get_regex_str(re: &Expr)->Result<String, TokenStream>{
    expr_par(re)
        .and_then(expr_lit)
        .and_then(lit_str)
        .ok_or_else(||{
            quote_spanned!{re.span()=>
                compile_error!{"regex_parse argument must be string literal."}
            }
        })
}

fn expr_par(x: &Expr)->Option<&Expr>{
    if let Expr::Paren(ref i) = x{
        Some(i.expr.deref())
    }else{
        None
    }
}

fn expr_lit(x: &Expr)->Option<&Lit>{
    if let Expr::Lit(ref i) = x{
        Some(&i.lit)
    }else{
        None
    }
}

fn lit_str(x: &Lit)->Option<String>{
    if let Lit::Str(ref s) = x{
        Some(s.value())
    }else{
        None
    }
}


/// parse which fields present in format string
fn arguments(format_string: &str)->HashSet<String>{
    let mut curly_bracket_stack = vec![];
    let mut map = HashSet::new();

    let mut iter = format_string.char_indices().peekable();
    loop{
        match iter.next(){
            Some((i, c)) if c == '{' => {
                if iter.peek().map(|(_, c)| *c) != Some('{'){
                    curly_bracket_stack.push(i + c.len_utf8());
                }
            },
            Some((i, c)) if c == '}' => {
                if let Some(start) = curly_bracket_stack.pop(){
                    let end = i;
                    let substr = format_string.get(start..end).unwrap().to_string();
                    map.insert(substr);
                }
            },
            Some(_) => {},
            None => {break;}
        }
    }
    map
}
