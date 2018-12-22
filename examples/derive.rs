extern crate reparse;

use reparse::reparse_proc_macro::regex_parse;

#[regex_parse(r"Vec\{{{x}, {y}, {z}\}}")]
#[derive(Debug)]
struct Date{
    x: f32,
    y: f32,
    z: f32
}

fn main(){
    let v: Date = "Vec{1, 2, 3}".parse().unwrap();

    println!("{:?}", v);
}