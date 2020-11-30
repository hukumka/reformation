[![Build Status](https://travis-ci.org/hukumka/reformation.svg?branch=master)](https://travis-ci.org/hukumka/reformation)

# reformation

Parsing via regular expressions using format syntax

Derive will require attribute reformation to specify format string,
which will be treated as format string -> regular expression string

Types implementing `Reformation` by default:

+ signed integers: `i8` `i16` `i32` `i64` `i128` `isize`
+ unsigned integers: `u8` `u16` `u32` `u64` `u128` `usize`
+ floats: `f32` `f64`
+ `String`, &str
+ `char`

### Structs

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
    let date = Date::parse("2018-12-22 20:23").unwrap();

    assert_eq!(date.year, 2018);
    assert_eq!(date.month, 12);
    assert_eq!(date.day, 22);
    assert_eq!(date.hour, 20);
    assert_eq!(date.minute, 23);
}
```

### Tuple Structs

```rust
use reformation::Reformation;

#[derive(Reformation)]
#[reformation(r"{} -> {}")]
struct Predicate(Empty, char);

#[derive(Reformation, Debug, PartialEq)]
#[reformation(r"Empty")]
struct Empty;

fn main(){
    let p = Predicate::parse("Empty -> X").unwrap();
    assert_eq!(p.0, Empty);
    assert_eq!(p.1, 'X');
}
```

### Enums
```rust
use reformation::Reformation;

#[derive(Reformation, Eq, PartialEq, Debug)]
enum Ant{
    #[reformation(r"Queen\({}\)")]
    Queen(String),
    #[reformation(r"Worker\({}\)")]
    Worker(i32),
    #[reformation(r"Warrior")]
    Warrior
}

fn main(){
    let queen = Ant::parse("Queen(We are swarm)").unwrap();
    assert_eq!(queen, Ant::Queen("We are swarm".to_string()));

    let worker = Ant::parse("Worker(900000)").unwrap();
    assert_eq!(worker, Ant::Worker(900000));

    let warrior = Ant::parse("Warrior").unwrap();
    assert_eq!(warrior, Ant::Warrior);
}
```

### In place parsing
```rust
use reformation::Reformation;

#[derive(Reformation, Eq, PartialEq, Debug)]
#[reformation("{a} {b}")]
struct InPlace<'a, 'b>{
    #[reformation("[a-z]*")]
    a: &'a str,
    #[reformation("[a-z]*")]
    b: &'b str,
}

fn main(){
    // Then parsed from &'x str value will have type
    // InPlace<'x, 'x>
    let inplace = InPlace::parse("aval bval").unwrap();
    assert_eq!(inplace, InPlace{a: "aval", b: "bval"})
}
```

### Modes

Order, in which modes are specified does not matter.

#### fromstr

Generate implementation of `FromStr` trait. 

Not compatible with lifetime annotated structs.

#### no_regex

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
    let v= Vec::parse("Vec{-1, 1}").unwrap();
    assert_eq!(v.x, -1);
    assert_eq!(v.y, 1);
}
```

#### slack

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
    let v = Vec::parse("Vec{-1,1}").unwrap();
    assert_eq!(v.x, -1);
    assert_eq!(v.y, 1);

    let r = Vec::parse("Vec{15,   2}").unwrap();
    assert_eq!(r.x, 15);
    assert_eq!(r.y, 2);
}
```

License: MIT
