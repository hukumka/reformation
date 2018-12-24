use reformation::create_parse_fn;


fn main(){
    let (x, y, z) = parse_vec("3, -4.2, +1e3").unwrap();
    println!("{:?}", (x, y, z));
}

create_parse_fn!{
    parse_vec,
    r"^{}, {}, {}$",
    f32, f32, f32
}