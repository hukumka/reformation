use reformation::{Error, ParseOverride, Reformation};

#[derive(Debug, Reformation, PartialEq)]
#[reformation(r"Vec\{{{x}, {y}, {z}\}}", slack = true)]
struct Vect {
    x: f32,
    y: f32,
    z: f32,
}

#[test]
fn test_vec() {
    let real = Vect::parse("Vec{1, 2, 3}");
    let expected = Vect {
        x: 1.0,
        y: 2.0,
        z: 3.0,
    };
    assert_eq!(real, Ok(expected));
}

// note that capture group (,|;) is replaced with non-capturing (:?,|;) in order
// to avoid accidental break of expression. Note that named capture groups
// (?P<name>expr) will still cause logical error and hopefully panic.
#[derive(Debug, Reformation, PartialEq)]
#[reformation(r"Rect\{{{a}(,|;)\s+{b}\}}")]
struct Rect {
    a: Vect,
    b: Vect,

    // Note what zero does not appear in format string, but
    // initialized from `Default` trait implementation
    zero: usize,
}

#[test]
fn test_rect() {
    let real = Rect::parse("Rect{Vec{1, 1, 0}; Vec{-3.e-5,  0.03,3}}");
    let expected = Rect {
        a: Vect {
            x: 1.0,
            y: 1.0,
            z: 0.0,
        },
        b: Vect {
            x: -3.0e-5,
            y: 0.03,
            z: 3.0,
        },
        zero: 0,
    };
    assert_eq!(real, Ok(expected));
}

#[derive(Reformation, PartialEq, Debug)]
#[reformation("{b}, {a}")]
struct Order {
    a: i32,
    b: i32,
}

#[test]
fn test_order() {
    let real = Order::parse("1, 3");
    let expected = Order { a: 3, b: 1 };
    assert_eq!(real, Ok(expected));
}

#[derive(Reformation, PartialEq, Debug)]
#[reformation("unit")]
struct Unit;

#[derive(Reformation, PartialEq, Debug)]
#[reformation("{a}, {b}")]
struct InPlace<'a, 'b> {
    a: &'a str,
    b: &'b str,
}

#[derive(Reformation, PartialEq, Debug)]
#[reformation("{}, {}")]
struct Generic<T>(T, Option<T>);

#[test]
fn in_place() {
    let ab = InPlace::parse("wqr, asdfg");
    assert_eq!(
        ab,
        Ok(InPlace {
            a: "wqr",
            b: "asdfg"
        })
    )
}

#[test]
fn test_unit() {
    let u = Unit::parse("unit");
    assert_eq!(u, Ok(Unit));
    let u = Unit::parse("u");
    assert!(u.is_err());
}

#[test]
fn test_generic() {
    let a = Generic::<i32>::parse("12, ");
    assert_eq!(a, Ok(Generic::<i32>(12, None)));
    let a = Generic::<String>::parse("stringoo, strongo");
    assert_eq!(
        a,
        Ok(Generic::<String>(
            "stringoo".to_string(),
            Some("strongo".to_string())
        ))
    );
}

#[derive(Reformation, PartialEq, Debug)]
#[reformation("{a} {b}")]
struct Override {
    #[reformation(r".")]
    a: i32,

    #[reformation(r"\d( \d)*")]
    b: VecWrapper,
}

#[derive(Debug, PartialEq)]
struct VecWrapper(Vec<u32>);

impl<'a> ParseOverride<'a> for VecWrapper {
    fn parse_override(s: &'a str) -> Result<VecWrapper, Error> {
        let v: Result<Vec<_>, Error> = s
            .split_whitespace()
            .map(|i| i.parse::<u32>().map_err(|e| Error::Other(e.to_string())))
            .collect();
        v.map(|v| VecWrapper(v))
    }
}

#[test]
fn test_override() {
    let a = Override::parse("13 1");
    assert_eq!(r"(.) (\d(?: \d)*)", Override::regex_str());
    if a.is_ok() {
        panic!("{:?}", a)
    }
    let b = Override::parse("1 2 3 9 0");
    assert_eq!(
        b,
        Ok(Override {
            a: 1,
            b: VecWrapper(vec![2, 3, 9, 0])
        })
    );
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation("{a}: {c}")]
struct ManyDefaults {
    a: i32,
    b: i32,
    c: i32,
    d: i32,
    e: i32,
}

#[test]
fn test_default() {
    let x = ManyDefaults::parse("1: 2").unwrap();
    assert_eq!(
        x,
        ManyDefaults {
            a: 1,
            b: 0,
            c: 2,
            d: 0,
            e: 0,
        }
    )
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation("{}:{}:{}", fromstr = true)]
struct Time(i32, i32, i32);

#[test]
fn test_fromstr() {
    let x: Time = "1:2:3".parse().unwrap();
    assert_eq!(x, Time(1, 2, 3));
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation("$", no_regex = true)]
struct Dollar;

#[test]
fn test_dollar() {
    let x = Dollar::parse("$").unwrap();
    assert_eq!(x, Dollar);
}
