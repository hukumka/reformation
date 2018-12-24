extern crate reparse;

use reparse::ReParse;

#[derive(Debug, ReParse)]
#[re_parse(r"Vec\{{{x}, {y}, {z}\}}")]
struct Vec{
    x: f32,
    y: f32,
    z: f32
}

#[derive(Debug, ReParse)]
#[re_parse(r"Rect\{{{a}, {b}\}}")]
struct Rect{
    a: Vec,
    b: Vec,
}

fn main(){
    let a: Vec = "Vec{1, 2, 3}".parse().unwrap();
    println!("{:?}", a);

    let r: Rect = "Rect{Vec{1, 1, 0}, Vec{-3.e-5, 0.03, 3}}".parse().unwrap();
    println!("{:?}", r);

    // Even through such structs can be combined, but do not overuse it, since it will produce horrific regular expressions
    println!("{:?}", Rect::regex_str());
}