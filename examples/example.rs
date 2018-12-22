use reparse::create_parse_fn;


fn main(){
    let (year, month, day, hour, minute) = parse_datetime("2018-12-20 12:51").unwrap();
    println!("{}-{}-{} {}:{}", year, month, day, hour, minute);
}

create_parse_fn!{
    parse_datetime,
    r"^{}-{}-{} {}:{}$",
    u16, u8, u8, u8, u8
}