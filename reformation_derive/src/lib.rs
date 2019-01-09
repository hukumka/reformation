#![recursion_limit = "128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

mod syn_helpers;
mod format;
mod impl_derive;
mod derive_input;
mod reformation_attribute;
mod error_messages;

use proc_macro2::TokenStream;
use crate::derive_input::DeriveInput;
use crate::impl_derive::impl_all;

#[proc_macro_derive(Reformation, attributes(reformation))]
pub fn reformation_derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ds = parse_macro_input!(item as syn::DeriveInput);
    let expanded = match reformation_derive_do(ds) {
        Ok(ok) => ok,
        Err(errors) => errors.to_compile_error(),
    };
    proc_macro::TokenStream::from(expanded)
}

fn reformation_derive_do(ds: syn::DeriveInput) -> syn::Result<TokenStream>{
    let ds = DeriveInput::parse(ds)?;
    Ok(impl_all(&ds))
}
