use reformation::Reformation;

#[derive(Reformation, Debug)]
#[reformation(r"(red|green|grey\({}\)|yellow|blue={}, {})")]
enum Color{
    Red,
    Green,
    Grey(i32),
    Yellow,
    Blue(i32, i32)
}


#[derive(Reformation, Debug, PartialEq)]
#[reformation(r"(Bird({})|Plane({}))", no_regex=true)]
enum Superman{
    Bird(f32),
    Plane(f32),
}

#[derive(Reformation, Debug, PartialEq)]
#[reformation(r"([]|[{}]|[{}, {}]|[{}, {}, {}])", no_regex=true, slack=true)]
enum MiniList{
    Zero,
    One(i32),
    Two(i32, i32),
    Three(i32, i32, i32),
}


fn main(){
    let c: Color = "grey(64)".parse().unwrap();
    println!("{:?}", c);

    let c: Color = "blue=11, -23".parse().unwrap();
    println!("{:?}", c);

    let s: Superman = "Bird(-1.3)".parse().unwrap();
    assert_eq!(s, Superman::Bird(-1.3));

    let l: MiniList = "[34]".parse().unwrap();
    assert_eq!(l, MiniList::One(34));

    let l: MiniList = "[34,22]".parse().unwrap();
    assert_eq!(l, MiniList::Two(34, 22));
    let l: MiniList = "[34,   22]".parse().unwrap();
    assert_eq!(l, MiniList::Two(34, 22));

    let l: MiniList = "[]".parse().unwrap();
    assert_eq!(l, MiniList::Zero);
}