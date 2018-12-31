[![Build Status](https://travis-ci.org/hukumka/reformation.svg?branch=master)](https://travis-ci.org/hukumka/reformation)

# reformation
 Parsing via regular expressions using format syntax

 Deriving trait `Reformation` will also implement
 trait `FromStr`, with `Err=Box<Error>`

 Derive will require attribute reformation to specify format string,
 which will be treated as format string -> regular expression string

 Types implementing `Reformation` by default:

 + signed integers: `i8` `i16` `i32` `i64` `i128` `isize`
 + unsigned integers: `u8` `u16` `u32` `u64` `u128` `usize`
 + floats: `f32` `f64`
 + `String`
 + `char`
 
 It can be used to parse:
 

## Structs
 ```rust
 use reformation::Reformation;

 #[derive(Reformation, Debug)]
 #[reformation(r"{year}-{month}-{day} {hour}:{minute}")]
 struct Date{
     year: u16,
     month: u8,
     day: u8,
     hour: u8,
     minute: u8,
 }

 fn main(){
     let date: Date = "2018-12-22 20:23".parse().unwrap();

     assert_eq!(date.year, 2018);
     assert_eq!(date.month, 12);
     assert_eq!(date.day, 22);
     assert_eq!(date.hour, 20);
     assert_eq!(date.minute, 23);
 }
 ```

## Tuple Structs

```
use reformation::Reformation;

#[derive(Reformation)]
#[reformation(r"{} -> {}")]
struct Predicate(Empty, char);

#[derive(Reformation, Debug, PartialEq)]
#[reformation(r"Empty")]
struct Empty;

fn main(){
    let p: Predicate = "Empty -> X".parse().unwrap();
    assert_eq!(p.0, Empty);
    assert_eq!(p.1, 'X');
}
```

## Enums
Current enum supports only following pattern: `r"(variant1|variant2|variant_with_value\({}\)|other_variant_with_value{})"`
```
use reformation::Reformation;

#[derive(Reformation, Eq, PartialEq, Debug)]
#[reformation(r"(Queen\({}\)|Worker\({}\)|Warrior)")]
enum Ant{
    Queen(String),
    Worker(i32),
    Warrior
}

 fn main(){
     let queen: Ant = "Queen(We are swarm)".parse().unwrap();
     assert_eq!(queen, Ant::Queen("We are swarm".to_string()));

     let worker: Ant = "Worker(900000)".parse().unwrap();
     assert_eq!(worker, Ant::Worker(900000));

     let warrior: Ant = "Warrior".parse().unwrap();
     assert_eq!(warrior, Ant::Warrior);
 }
 ```

## Modes

There are some modes, that can be applied to regular expression.

Order, in which modes are specified does not matter.

### no_regex

Makes format string behave as regular string (in contrast with being regular expression),
by escaping all special regex characters.

```rust
use reformation::Reformation;

#[derive(Reformation, Debug)]
#[reformation("Vec{{{x}, {y}}}", no_regex=true)]
struct Vec{
    x: i32,
    y: i32,
}

fn main(){
    let v: Vec = "Vec{-1, 1}".parse().unwrap();
    assert_eq!(v.x, -1);
    assert_eq!(v.y, 1);
}
```

### slack

Allow arbitrary number of spaces after separators: ',', ';', ':'. For separator to be recognized
as slack, it must be followed by at least one space in format string.

```rust
use reformation::Reformation;

#[derive(Reformation, Debug)]
#[reformation(r"Vec\{{{x}, {y}\}}", slack=true)]
struct Vec{
    x: i32,
    y: i32,
}

fn main(){
    let v: Vec = "Vec{-1,1}".parse().unwrap();
    assert_eq!(v.x, -1);
    assert_eq!(v.y, 1);

    let r: Vec = "Vec{15,   2}".parse().unwrap();
    assert_eq!(r.x, 15);
    assert_eq!(r.y, 2);
}
```

Combination of no_regex and slack behaves as expected:

```rust
use reformation::Reformation;

#[derive(Reformation, Debug)]
#[reformation(r"Vec({x}; {y})", slack=true, no_regex=true)]
struct Vec{
    x: i32,
    y: i32,
}

fn main(){
    let v: Vec = "Vec(-1;1)".parse().unwrap();
    assert_eq!(v.x, -1);
    assert_eq!(v.y, 1);

    let r: Vec = "Vec(15;   2)".parse().unwrap();
    assert_eq!(r.x, 15);
    assert_eq!(r.y, 2);
}
```

