use reformation::Reformation;


// This example is expected to work, since type std::marker::Marker<T>
// implements default, but as macro bounds T: Reformation for all generated
// methods, it does not compile.
#[derive(Reformation)]
#[reformation("{x}")]
struct A<T>{
    x: usize,
    _marker: std::marker::PhantomData<T>,
}

fn main(){
    let x: A<Vec<usize>> = "13".parse().unwrap();
}