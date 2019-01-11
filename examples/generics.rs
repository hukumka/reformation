use reformation::Reformation;

#[derive(Reformation)]
#[reformation(r"A\({x}\)")]
struct A<T>{
    x: T,
}

// Sometimes macro cannot determine correct trait bounds by itself.
// In such sorrowful cases one can explicitly specify where clause
// for generated trait implementations.
// By default bound T: Reformation is put on every generic argument,
// but this is not desired behaviour here. By using `override_where=""`
// we remove such bound.
#[derive(Reformation)]
#[reformation("{x}", override_where = "")]
struct B<T>{
    x: usize,
    _marker: std::marker::PhantomData<T>,
}

#[derive(Reformation)]
#[reformation("{x}", override_where = "where T1: Reformation")]
struct C<T1, T2>{
    x: T1,
    y: Vec<T2>, // constructed with Default::default method
}

fn main(){
    let x: A<usize> = "A(32)".parse().unwrap();
    assert_eq!(x.x, 32);

    let y: B<Vec<usize>> = "11".parse().unwrap();
    assert_eq!(y.x, 11);

    let z: C<i32, (u8, u8)> = "44".parse().unwrap();
    assert_eq!(z.x, 44);
    assert_eq!(z.y, vec![]);
}