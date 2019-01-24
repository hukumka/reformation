use syn::{Expr, Lit};

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
