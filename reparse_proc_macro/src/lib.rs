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

    let names2 = names.clone();
    let types2 = types.clone();
    let names3 = names.clone();

    quote!{
        reparse::lazy_static::lazy_static!{
            static ref RE: reparse::regex::Regex = {
                reparse::regex::Regex::new(
                    &format!(#re2, #(#names=format!("({})", <#types as reparse::ParsePrimitive>::regex_str())),*)
                ).unwrap()
            };
        }
        let captures = RE.captures(input_str).unwrap();
        let mut i=0;
        #(let #names2 = captures.get({i += 1; i}).unwrap().as_str().parse::<#types2>().unwrap();)*
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


fn arguments(format_string: &str)->HashSet<String>{
    let mut curly_bracket_stack = vec![];
    let mut map = HashSet::new();

    let mut iter = format_string.char_indices();
    loop{
        match iter.next(){
            Some((i, c)) if c == '{' => {
                curly_bracket_stack.push(i + c.len_utf8());
            },
            Some((i, c)) if c == '}' => {
                let start = curly_bracket_stack.pop().unwrap();
                let end = i;
                map.insert(format_string.get(start..end).unwrap().to_string());
            },
            Some(_) => {},
            None => {break;}
        }
    }
    map
}
