use syn::{Expr, Lit, Path};

pub fn expr_into_attribute_param(expr: &Expr) -> Option<(String, &Expr)> {
    match expr {
        Expr::Assign(a) => {
            let ident = expr_into_ident_name(&a.left)?;
            Some((ident, &a.right))
        }
        _ => None,
    }
}

fn expr_into_ident_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Path(p) => path_to_ident_name(&p.path),
        _ => None,
    }
}

fn path_to_ident_name(path: &Path) -> Option<String> {
    let ident = &path.segments.first()?.value().ident;
    let name = quote!(#ident).to_string();
    if path.is_ident(&name) {
        Some(name)
    } else {
        None
    }
}

pub fn get_regex_str(re: &Expr) -> syn::Result<String> {
    expr_lit(re)
        .and_then(lit_str)
        .ok_or_else(|| syn::Error::new_spanned(re, "regex_parse argument must be string literal."))
}

pub fn expr_lit(x: &Expr) -> Option<&Lit> {
    if let Expr::Lit(ref i) = x {
        Some(&i.lit)
    } else {
        None
    }
}

pub fn expr_bool_lit(x: &Expr) -> Option<bool> {
    if let Some(Lit::Bool(ref c)) = expr_lit(x) {
        Some(c.value)
    } else {
        None
    }
}

pub fn lit_str(x: &Lit) -> Option<String> {
    if let Lit::Str(ref s) = x {
        Some(s.value())
    } else {
        None
    }
}
