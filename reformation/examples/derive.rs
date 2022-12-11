extern crate reformation;

use reformation::Reformation;

#[derive(Debug, Reformation)]
#[reformation(r"Vec\{{{x}, {y}, {z}\}}", slack = true)]
struct Vec {
    x: f32,
    y: f32,
    z: f32,
}

// By default, regular expression items are escaped
#[derive(Debug, Reformation)]
#[reformation(r"(\|)({left_eye}_{right_eye})(|/)")]
struct Crab {
    left_eye: String,
    right_eye: String,
}

// But regular expressions can be enabled by providing `regex=true` attribute
// note that capture group (,|;) is replaced with non-capturing (:?,|;) in order
// to avoid accidental break of expression. Note that named capture groups
// (?P<name>expr) will still cause logical error and hopefully panic.
#[derive(Debug, Reformation)]
#[reformation(r"Rect\{{{a}(,|;)\s+{b}\}}", regex = true)]
struct Rect {
    a: Vec,
    b: Vec,

    // Note what zero does not appear in format string, but
    // initialized from `Default` trait implementation
    zero: usize,
}

#[derive(Debug, Reformation)]
#[reformation(r"hm( vec might be here: {})?")]
struct Ovec(Option<Vec>);

fn main() {
    let a = Vec::parse("Vec{1, 2, 3}").unwrap();
    println!("{:?}", a);

    // Even through such structs can be combined, but do not overuse it, since it will produce horrific regular expressions
    println!("{:?}", Rect::regex_str());

    // Vec regex is in slack mode, allowing arbitrary amount of spaces after coma.
    let r = Rect::parse("Rect{Vec{1, 1, 0}; Vec{-3.e-5,  0.03,3}}").unwrap();
    println!("{:?}", r);

    let pirate_crab = Crab::parse(r"(\|)(x_^)(|/)").unwrap();
    println!("{:?}", pirate_crab);

    let b = Ovec::parse("hm vec might be here: Vec{0, 0, 0}").unwrap();
    println!("{:?}", b);

    let c = Ovec::parse("hm").unwrap();
    println!("{:?}", c);
}
