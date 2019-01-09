use reformation::Reformation;

#[derive(Reformation)]
#[reformation(r"A\({x}\)")]
struct A<T>{
    x: T
}

fn main(){
    let x: A<usize> = "A(32)".parse().unwrap();
    assert_eq!(x.x, 32);
}