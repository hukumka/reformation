/// Since static variables created for one per lexical function:
/// ```
/// fn gen<T>(){
///     static X: i32 = 0;
/// }
/// // gen::<i32> and gen::<i64> will share same static variable
/// ```
/// instance of regex automata is held inside `HashMap` protected by
/// `RwLock`.
/// 
/// This creates overhead for accessing it in each call of `Reformation::parse`
///
/// But, on the other hand, wrapping generic structure inside concrete one will
/// eliminate this overhead, since `reformation` constructs regex automation
/// on per struct basis, which means `AIntErase::parse` never uses automation
/// of `AInt::<i32>`. Also, unless `AInt::<i32>::parse` is called at least once
/// such automation is never constructed.
///
/// This benchmark aims to give general idea of how much overhead it could cause.
#[macro_use]
extern crate criterion;

use criterion::{Criterion, Fun};
use reformation::Reformation;


// This struct being strait up parsed
#[derive(Reformation, Debug, PartialEq)]
#[reformation("{value}")]
struct AInt{
    value: i32
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation("{value}")]
struct A<T>{
    value: T
}

// Erase generic to remove overhead
#[derive(Reformation, Debug, PartialEq)]
#[reformation("{}")]
struct AIntErase(A<i32>);

fn parse(inputs: &Vec<&str>) {
    for i in inputs{
        criterion::black_box(AInt::parse(i).unwrap());
    }
}

fn parse_generic(inputs: &Vec<&str>) {
    for i in inputs{
        criterion::black_box(A::<i32>::parse(i).unwrap());
    }
}

fn parse_generic_erased(inputs: &Vec<&str>) {
    for i in inputs{
        criterion::black_box(AIntErase::parse(i).unwrap());
    }
}

fn compare(c: &mut Criterion) {
    let inputs = vec!["134", "-13", "9999932", "0", "3"];
    let a_i32 = Fun::new("parse_int", |b, i| b.iter(|| parse(i)));
    let a_gen = Fun::new("parse::<int>", |b, i| b.iter(|| parse_generic(i)));
    let a_gen_erased = Fun::new("parse::<int> with erased generic", |b, i| b.iter(|| parse_generic_erased(i)));
    c.bench_functions("reformation parse gen", vec![a_i32, a_gen, a_gen_erased], inputs);
}

criterion_group!(benches, compare);
criterion_main!(benches);
