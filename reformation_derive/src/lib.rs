#![recursion_limit = "128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

mod format;
mod reformation_attribute;
mod syn_helpers;

use crate::format::Format;
use crate::reformation_attribute::ReformationAttribute;
use proc_macro2::{Span, TokenStream};
use std::collections::HashMap;
use syn::spanned::Spanned;

#[proc_macro_derive(Reformation, attributes(reformation))]
pub fn reformation_derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ds = parse_macro_input!(item as syn::DeriveInput);
    let expanded = match reformation_derive_do(ds) {
        Ok(ok) => ok,
        Err(errors) => errors.to_compile_error(),
    };
    proc_macro::TokenStream::from(expanded)
}

fn reformation_derive_do(mut ds: syn::DeriveInput) -> syn::Result<TokenStream> {
    let ds = DeriveInput::new(&mut ds)?;
    Ok(ds.impl_all())
}

struct DeriveInput<'a> {
    input: &'a syn::DeriveInput,
    data: InputData<'a>,
    attr: ReformationAttribute,
}

enum InputData<'a> {
    Struct(Item<'a>),
    Enum(Vec<Item<'a>>),
}

struct Item<'a> {
    format: String,
    name: TokenStream,
    defaults: Vec<&'a syn::Field>,
    fields: Vec<&'a syn::Field>,
}

impl<'a> InputData<'a> {
    

    fn new_enum(
        name: &TokenStream,
        attrs: &ReformationAttribute,
        enum_: &'a syn::DataEnum,
    ) -> syn::Result<Self> {
        let variants: syn::Result<Vec<_>> = enum_
            .variants
            .iter()
            .filter_map(|v| {
                ReformationAttribute::find_attribute(&v.attrs)?; // Variants with no attribute should be unparseable
                let new_attrs = match ReformationAttribute::parse(v.span(), &v.attrs) {
                    Ok(a) => a,
                    Err(e) => {
                        return Some(Err(e));
                    }
                };
                let mut attrs = attrs.clone();
                attrs.regex_string = None;
                let new_attrs = attrs.combine(&new_attrs);
                let ident = &v.ident;
                let name = quote! {#name::#ident};
                Some(Item::from_fields(name, &new_attrs, &v.fields))
            })
            .collect();
        Ok(InputData::Enum(variants?))
    }
}

impl<'a> Item<'a> {
    fn from_fields(
        name: TokenStream,
        attrs: &ReformationAttribute,
        fields: &'a syn::Fields,
    ) -> syn::Result<Self> {
        match fields {
            syn::Fields::Named(_) => Self::named(name, attrs, fields),
            syn::Fields::Unnamed(_) => Self::unnamed(name, attrs, fields),
            syn::Fields::Unit => Self::empty(name, attrs),
        }
    }

    fn named(
        name: TokenStream,
        attrs: &ReformationAttribute,
        fields: &'a syn::Fields,
    ) -> syn::Result<Self> {
        let format = format_from_attribute(attrs)?;
        if format.positional_arguments() > 0 {
            return Err(syn::Error::new(
                attrs.span,
                "Reformation error: arguments must be named",
            ));
        }
        let mut map: HashMap<String, &'a syn::Field> = fields
            .iter()
            .map(|f| (f.ident.as_ref().unwrap().to_string(), f))
            .collect();
        let fields: syn::Result<Vec<_>> = format
            .named_arguments()
            .map(|s| {
                map.remove(&s)
                    .ok_or_else(|| syn::Error::new(attrs.span, format!("No field named '{}'", s)))
            })
            .collect();
        let defaults: Vec<_> = map.into_iter().map(|(_, v)| v).collect();
        let fields = fields?;
        Ok(Self {
            format: format.linearize(),
            fields,
            defaults,
            name,
        })
    }

    fn unnamed(
        name: TokenStream,
        attrs: &ReformationAttribute,
        fields: &'a syn::Fields,
    ) -> syn::Result<Self> {
        let format = format_from_attribute(attrs)?;
        if format.named_arguments().next().is_some() {
            return Err(syn::Error::new(
                attrs.span,
                "Reformation error: arguments must be positional",
            ));
        }
        let fields: Vec<_> = fields.iter().collect();
        if format.positional_arguments() != fields.len() {
            return Err(syn::Error::new(
                attrs.span,
                "Reformation error: wrong number of arguments",
            ));
        }
        Ok(Self {
            name,
            defaults: vec![],
            format: format.linearize(),
            fields,
        })
    }

    fn empty(name: TokenStream, attrs: &ReformationAttribute) -> syn::Result<Self> {
        let format = format_from_attribute(attrs)?;
        if format.no_arguments() {
            let format = format.linearize();
            Ok(Self {
                name,
                format,
                defaults: vec![],
                fields: vec![],
            })
        } else {
            Err(syn::Error::new(
                attrs.span,
                "Reformation error: no arguments expected in format string for Unit.",
            ))
        }
    }
}

impl<'a> DeriveInput<'a> {
    fn new(derive_input: &'a syn::DeriveInput) -> syn::Result<DeriveInput<'a>> {
        let attr = ReformationAttribute::parse(derive_input.span(), &derive_input.attrs)?;
        let ident = &derive_input.ident;
        let name = quote! { #ident };
        if attr.fromstr && derive_input.generics.lifetimes().next().is_some() {
            return Err(syn::Error::new(ident.span(), "FromStr can only be implemented by `T: 'static` types."))
        }
        let data = match derive_input.data {
            syn::Data::Struct(ref s) => {
                let item = Item::from_fields(name, &attr, &s.fields)?;
                Ok(InputData::Struct(item))
            }
            syn::Data::Enum(ref e) => InputData::new_enum(&name, &attr, &e),
            syn::Data::Union(_) => Err(syn::Error::new(
                ident.span(),
                "Reformation does not support unions",
            )),
        }?;
        Ok(DeriveInput{
            data,
            input: derive_input,
            attr
        })
    }

    fn unique_lifetime(&self, syffix: &str) -> syn::Lifetime {
        let last = self
            .input
            .generics
            .lifetimes()
            .map(|x| x.lifetime.ident.to_string())
            .max()
            .unwrap_or_default();

        let total = format!("'{}_{}", last, syffix);
        syn::Lifetime::new(&total, Span::call_site())
    }

    /// Generate ```TokenStream``` with implementation of ```Reformation``` for `input`
    fn impl_all(&self) -> TokenStream {
        let ident = &self.input.ident;

        // Add lifetime " 'input: 'a + 'b + ... " to impl_gen
        let mut impl_gen = self.input.generics.clone();
        let mut lifetimes = impl_gen.lifetimes();
        let first = lifetimes.next().into_iter();
        let input_lifetime = self.unique_lifetime("input");
        let input = parse_quote!(#input_lifetime #(: #first)* #(+ #lifetimes)* );
        impl_gen.params.push(syn::GenericParam::Lifetime(input));
        // add 'input bound on all generic types
        for t in impl_gen.type_params_mut() {
            t.bounds
                .push(parse_quote!(::reformation::Reformation<#input_lifetime>))
        }
        let (impl_gen, _, where_clause) = impl_gen.split_for_impl();
        let (_, type_gen, _) = self.input.generics.split_for_impl();

        let regex_str = self.regex_str_quote();
        let count = self.captures_count_quote();
        let from_captures = self.impl_from_captures_quote();
        let parse = self.parse_quote();

        let fromstr = if self.attr.fromstr {
            self.fromstr_quote()
        } else {
            quote! {}
        };

        let res = quote! {
            #[allow(clippy::eval_order_dependence)]
            impl #impl_gen ::reformation::Reformation<#input_lifetime> for #ident #type_gen #where_clause{
                #regex_str
                #count
                #from_captures
                #parse
            }

            #fromstr
        };
        res
    }

    fn fromstr_quote(&self) -> TokenStream {
        let ident = &self.input.ident;
        let (impl_gen, type_gen, where_clause) = self.input.generics.split_for_impl();

        quote! {
            impl #impl_gen std::str::FromStr for #ident #type_gen #where_clause {
                type Err = ::reformation::Error;
                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    Self::parse(s)
                }
            }
        }
    }

    fn regex_str_quote(&self) -> TokenStream {
        let fmt = match self.data {
            InputData::Struct(ref i) => {
                let substr = i.substrings();
                let fmt = &i.format;
                quote! {
                    format!(#fmt, #(#substr),*)
                }
            }
            InputData::Enum(ref e) => {
                let substr = e.iter().flat_map(|i| i.substrings());
                let fmt = Self::enum_format_str(&e);
                quote! {
                    format!(#fmt, #(#substr),*)
                }
            }
        };

        let gen = self.generic_type_params();
        let static_ = self.maybe_generic_static(
            quote! {String},
            quote! {create_regex},
            quote! {|x: &str| x.to_string()},
        );
        let res = quote! {
            fn regex_str() -> &'static str{
                fn create_regex #gen () -> String {
                    #fmt
                }
                #static_.as_str()
            }
        };
        res
    }

    fn generic_type_params(&self) -> TokenStream {
        let mut generics = self.input.generics.clone();
        let mut lifetimes = generics.lifetimes();
        let first = lifetimes.next().into_iter();
        let input_lifetime = self.unique_lifetime("input");
        let input = parse_quote!(#input_lifetime #(: #first)* #(+ #lifetimes)* );
        generics.params.push(syn::GenericParam::Lifetime(input));
        let input_lifetime = self.unique_lifetime("input");
        for t in generics.type_params_mut() {
            t.bounds
                .push(parse_quote!(::reformation::Reformation<#input_lifetime>))
        }
        let (impl_gen, _, _) = generics.split_for_impl();
        quote! { #impl_gen }
    }

    fn maybe_generic_static(
        &self,
        type_: TokenStream,
        key: TokenStream,
        map: TokenStream,
    ) -> TokenStream {
        if self.input.generics.type_params().next().is_some() {
            let (_, ty, _) = self.input.generics.split_for_impl();
            let turbo = ty.as_turbofish();
            quote! {
                {
                    static RE: ::reformation::OnceCell<::reformation::GenericStaticStr<#type_>> = ::reformation::OnceCell::new();
                    let re = RE.get_or_init(||{
                        ::reformation::GenericStaticStr::new()
                    });
                    re.call_once(#key #turbo, #map)
                }
            }
        } else {
            // No generics, fall back to regular statics for better perfomance
            quote! {
                static RE: ::reformation::OnceCell<#type_> = ::reformation::OnceCell::new();
                RE.get_or_init(||{
                    (#map)(&#key())
                })
            }
        }
    }

    fn enum_format_str(items: &[Item]) -> String {
        let mut fmt = "(?:(".to_string();
        for i in items {
            fmt += &i.format;
            fmt += ")|(";
        }
        // After we added last group we added separator ")|("
        // where "|(" must be removed
        fmt.pop();
        fmt.pop();
        fmt += ")";
        fmt
    }

    fn captures_count_quote(&self) -> TokenStream {
        let count = match self.data {
            InputData::Struct(ref i) => i.size_quote(),
            InputData::Enum(ref e) => {
                let counts = e.iter().flat_map(|i| i.captures_count());
                let variant_count = e.len();
                quote! {
                    {
                        let mut count = #variant_count;
                        #(count += #counts;)*
                        count
                    }
                }
            }
        };
        quote! {
            #[inline]
            fn captures_count() -> usize {
                #count
            }
        }
    }

    fn impl_from_captures_quote(&self) -> TokenStream {
        let body = match self.data {
            InputData::Struct(ref i) => i.impl_from_captures(&quote! {captures}, &quote! {offset}),
            InputData::Enum(ref variants) => {
                let items = variants
                    .iter()
                    .map(|i| i.impl_from_captures(&quote! {captures}, &quote! {offset}));
                let sizes = variants.iter().map(|i| i.size_quote());
                quote! {
                    #(
                        if captures.get(offset).is_some() {
                            offset += 1;
                            return #items;
                        }else{
                            offset += #sizes + 1;
                        }
                    )*
                    Err(::reformation::Error::DoesNotContainGroup)
                }
            }
        };
        let input_lifetime = self.unique_lifetime("input");
        let cap_lifetime = self.unique_lifetime("cap");
        let res = quote! {
            fn from_captures<#cap_lifetime>(captures: &::reformation::Captures<#cap_lifetime, #input_lifetime>, mut offset: usize) -> Result<Self, ::reformation::Error> {
                #body
            }
        };
        res
    }

    fn parse_quote(&self) -> TokenStream {
        let ident = &self.input.ident;
        let (_, ty_gen, _) = self.input.generics.split_for_impl();
        let name = quote! { #ident #ty_gen};
        let input_lifetime = self.unique_lifetime("input");

        let gen = self.generic_type_params();
        let static_ = self.maybe_generic_static(
            quote! {::reformation::Regex},
            quote! {create_regex},
            quote! {|x: &str| ::reformation::Regex::new(x).unwrap()},
        );
        let static_ = quote! {
            {
                fn create_regex #gen () -> String {
                    format!(r"\A{}\z", <#name as ::reformation::Reformation>::regex_str())
                }
                #static_
            }
        };

        quote! {
            fn parse(string: &#input_lifetime str) -> Result<Self, ::reformation::Error>{
                use ::reformation::{Regex, Captures, Error};
                let re = #static_;
                let mut loc = re.capture_locations();
                if re.captures_read(&mut loc, string).is_some(){
                    let captures = Captures::new(&loc, string);
                    Self::from_captures(&captures, 1)
                }else{
                    Err(
                        Error::NoRegexMatch(::reformation::NoRegexMatch{
                            format: Self::regex_str(),
                            request: string.to_string(),
                        })
                    )
                }
            }
        }
    }

}

impl<'a> Item<'a> {
    fn substrings<'b>(&'b self) -> impl Iterator<Item = TokenStream> + 'b {
        self.fields.iter().map(|f| {
            let ty = &f.ty;
            let r = Self::field_regex_override(f);
            match r {
                Ok(Some(regex)) => quote! { format!("({})", #regex) },
                Ok(None) => quote! {<#ty as ::reformation::Reformation>::regex_str()},
                Err(e) => e.to_compile_error(),
            }
        })
    }

    fn field_regex_override(field: &syn::Field) -> syn::Result<Option<String>> {
        if field.attrs.is_empty() {
            return Ok(None);
        }
        let attr = ReformationAttribute::parse(field.span(), &field.attrs)?;
        let res = attr.regex().ok().map(|s| (attr.substr_mode())(s));
        Ok(res)
    }

    fn size_quote(&self) -> TokenStream {
        if self.fields.is_empty() {
            return quote! {0};
        }
        let count = self.captures_count();
        quote! {{
            let mut count = 0;
            #(count += #count;)*
            count
        }}
    }

    fn captures_count<'b>(&'b self) -> impl Iterator<Item = TokenStream> + 'b {
        self.fields.iter().map(|f| {
            let ty = &f.ty;
            if Item::field_regex_override(f).unwrap().is_some() {
                quote! {1}
            } else {
                quote! {<#ty as ::reformation::Reformation>::captures_count()}
            }
        })
    }

    /// Returns token stream representing code which construct `Item` from captures
    ///
    /// + `captures` - name of captures variable
    /// + `counter` - name of counter variable
    fn impl_from_captures(&self, captures: &TokenStream, counter: &TokenStream) -> TokenStream {
        let constr = &self.name;
        if self.fields.is_empty() {
            return quote! {
                Ok(#constr)
            };
        }

        let item_from_captures = self.fields.iter()
            .map(|f|{
                let type1 = &f.ty;
                let type2 = &f.ty;
                let counter1 = &counter;
                let counter2 = &counter;
                if Item::field_regex_override(f).unwrap().is_some(){
                    quote! {
                        {
                            let str = #captures.get(#counter1)
                                .ok_or_else(|| ::reformation::Error::DoesNotContainGroup)?;
                            let x = <#type1 as ::reformation::ParseOverride>::parse_override(str)?;
                            #counter2 += 1;
                            x
                        }
                    }
                }else{
                    quote! {
                        {
                            let x = <#type1 as ::reformation::Reformation>::from_captures(#captures, #counter1)?;
                            #counter2 += <#type2 as ::reformation::Reformation>::captures_count();
                            x
                        }
                    }
                }
            });

        let defaults = self.defaults.iter().map(|x| &x.ident);
        if self.fields[0].ident.is_some() {
            let names = self.fields.iter().map(|f| f.ident.as_ref().unwrap());
            quote! {
                Ok(#constr{
                    #(
                        #names: #item_from_captures,
                    )*
                    #(#defaults: Default::default(),)*
                })
            }
        } else {
            quote! {
                Ok(#constr( #(
                    #item_from_captures,
                )* ))
            }
        }
    }
}

fn format_from_attribute(attr: &ReformationAttribute) -> syn::Result<Format> {
    let mut format = Format::build(&attr.regex()?).map_err(|e| syn::Error::new(attr.span, e))?;
    format.map_substrings(attr.substr_mode());
    Ok(format)
}
