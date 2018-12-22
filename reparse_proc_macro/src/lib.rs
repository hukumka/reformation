#![recursion_limit="128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

use std::collections::HashSet;

use proc_macro2::TokenStream;
use syn::{DeriveInput, Data, Field};
use syn::{Expr, Lit};


#[proc_macro_attribute]
pub fn regex_parse(re: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream{
    let re = parse_macro_input!(re as Expr);
    let ds = parse_macro_input!(item as DeriveInput);
    let from_str_token_stream = impl_from_str_body(re, &ds);

    let name = &ds.ident;

    let expanded = quote!{
        #ds

        impl std::str::FromStr for #name{
            type Err = Box<std::error::Error>;

            fn from_str(input_str: &str)->Result<Self, Self::Err>{
                #from_str_token_stream
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}


fn impl_from_str_body(re: Expr, ds: &DeriveInput)->TokenStream{
    let re2 = re.clone();
    let re_str = get_regex_str(re);
    let args = arguments(&re_str);
    let fields = get_fields(&ds);

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

    quote!{
        reparse::lazy_static!{
            static ref RE: reparse::Regex = {
                let re_str = &format!(#re2, #(#names1=format!("({})", <#types1 as reparse::ParsePrimitive>::regex_str())),*);
                reparse::Regex::new(re_str)
                    .unwrap_or_else(|x| panic!("Cannot compile regex {:?}", ))
            };
        }
        let captures = RE.captures(input_str).unwrap();
        let mut i=0;
        #(
            let #names2 = captures.get({i += 1; i})
                .unwrap_or_else(|| panic!("Capture group with id={} not found. {:?}", i, captures))
                .as_str().parse::<#types2>()
                .unwrap_or_else(|x| panic!("Cannot parse {:?}", x));
        )*
        Ok(Self{
            #(#names3,)*
            //..Default::default()
        })
    }
}


fn get_fields(struct_: &DeriveInput)->Vec<&Field>{
    if let Data::Struct(ref ds) = struct_.data{
        let fields: Vec<_> = ds.fields.iter().collect();
        fields
    }else{
        panic!("regex_parse macro support struct only variants.")
    }
}


fn get_regex_str(re: Expr)->String{
    let lit = if let Expr::Lit(re) = re{
        re
    }else{
        panic!("regex_parse argument must be string literal.")
    };
    if let Lit::Str(s) = lit.lit{
        s.value()
    }else{
        panic!("regex_parse argument must be string literal.")
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
