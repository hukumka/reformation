use reformation::Reformation;

#[derive(Reformation, Debug)]
enum Color {
    #[reformation("red")]
    Red,
    #[reformation("green")]
    Green,
    #[reformation(r"grey\({}\)")]
    Grey(i32),
    #[reformation("yellow")]
    Yellow,
    #[reformation(r"blue={}, {}")]
    Blue(i32, i32),
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation(no_regex = true)]
enum Superman {
    #[reformation("Bird({})")]
    Bird(f32),
    #[reformation("Plane({})")]
    Plane(f32),
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation(no_regex = true, slack = true)]
enum MiniList {
    #[reformation("[]")]
    Zero,
    #[reformation("[{}]")]
    One(i32),
    #[reformation("[{}, {}]")]
    Two(i32, i32),
    #[reformation("[{}, {}, {}]")]
    Three(i32, i32, i32),
}

fn main() {
    let c = Color::parse("grey(64)").unwrap();
    println!("{:?}", c);

    let c = Color::parse("blue=11, -23").unwrap();
    println!("{:?}", c);

    let s = Superman::parse("Bird(-1.3)").unwrap();
    assert_eq!(s, Superman::Bird(-1.3));

    let l = MiniList::parse("[34]").unwrap();
    assert_eq!(l, MiniList::One(34));

    let l = MiniList::parse("[34,22]").unwrap();
    assert_eq!(l, MiniList::Two(34, 22));
    let l = MiniList::parse("[34,   22]").unwrap();
    assert_eq!(l, MiniList::Two(34, 22));

    let l = MiniList::parse("[]").unwrap();
    assert_eq!(l, MiniList::Zero);
}
