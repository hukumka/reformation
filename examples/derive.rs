extern crate reparse;

use reparse::prelude::*;

#[derive(Debug, ReParse)]
#[re_parse(r"Vec\{{{x}, {y}, {z}\}}")]
struct Date{
    x: f32,
    y: f32,
    z: f32
}

fn main(){
    let v: Date = "Vec{1, 2, 3}".parse().unwrap();

    println!("{:?}", v);
}