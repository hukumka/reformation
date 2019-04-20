use reformation::Reformation;

#[derive(Reformation, PartialEq, Debug)]
enum A {
    #[reformation("variant1")]
    V1,
    #[reformation("variant2{}")]
    V2(usize),
}

#[derive(Reformation, PartialEq, Debug)]
enum B {
    #[reformation("{}")]
    One(u8),
    #[reformation("{}..{}", no_regex = true)]
    Pair(u8, u8),
}

#[test]
fn test_base_enum() -> Result<(), reformation::Error> {
    dbg!(A::regex_str());
    let a = A::parse("variant1")?;
    assert_eq!(a, A::V1);
    let b = A::parse("variant259")?;
    assert_eq!(b, A::V2(59));
    Ok(())
}

#[test]
fn test_enum_separation() -> Result<(), reformation::Error> {
    dbg!(B::regex_str());
    let a = B::parse("8")?;
    assert_eq!(a, B::One(8));
    let b = B::parse("1..88")?;
    assert_eq!(b, B::Pair(1, 88));
    Ok(())
}
