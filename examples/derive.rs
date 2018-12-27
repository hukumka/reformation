extern crate reformation;

use reformation::Reformation;

#[derive(Debug, Reformation)]
#[reformation(r"Vec\{{{x}, {y}, {z}\}}")]
struct Vec {
    x: f32,
    y: f32,
    z: f32,
}

// note that capture group (,|;) is replaced with non-capturing (:?,|;) in order
// to avoid accidental break of expression. Note that named capture groups
// (?P<name>expr) will still cause logical error and hopefully panic.
#[derive(Debug, Reformation)]
#[reformation(r"Rect\{{{a}(,|;)\s+{b}\}}")]
struct Rect {
    a: Vec,
    b: Vec,

    // Note what zero does not appear in format string, but
    // initialized from `Default` trait implementation
    zero: usize,
}

// One may choose to use plain format syntax (with no regular expressions)
// by providing `no_regex=true` mode
#[derive(Debug, Reformation)]
#[reformation(r"(\|)({left_eye}_{right_eye})(|/)", no_regex=true)]
struct Crab{
    left_eye: String,
    right_eye: String,
}

fn main() {
    let a: Vec = "Vec{1, 2, 3}".parse().unwrap();
    println!("{:?}", a);

    let r: Rect = "Rect{Vec{1, 1, 0}; Vec{-3.e-5, 0.03, 3}}".parse().unwrap();
    println!("{:?}", r);

    // Even through such structs can be combined, but do not overuse it, since it will produce horrific regular expressions
    println!("{:?}", Rect::regex_str());

    let pirate_crab: Crab = r"(\|)(x_^)(|/)".parse().unwrap();
    println!("{:?}", pirate_crab);
}
