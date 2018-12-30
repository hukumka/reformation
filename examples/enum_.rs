use reformation::Reformation;

#[derive(Reformation, Debug)]
#[reformation(r"(red|green|grey\({}\)|yellow|blue={}, {}")]
enum Color{
    Red,
    Green,
    Grey(i32),
    Yellow,
    Blue(i32, i32)
}


fn main(){
    let c: Color = "red".parse().unwrap();
    println!("{:?}", c);

    let c: Color = "grey(64)".parse().unwrap();
    println!("{:?}", c);

    let c: Color = "blue=11, -23".parse().unwrap();
    println!("{:?}", c);
}