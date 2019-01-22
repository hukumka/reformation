use reformation::Reformation;

#[derive(Reformation, PartialEq, Debug)]
enum A{
    #[reformation("variant1")]
    V1,
    #[reformation("variant2{}")]
    V2(usize)
}

#[test]
fn test_base_enum() -> Result<(), reformation::Error>{
    let a = A::parse("variant1")?;
    assert_eq!(a, A::V1);
    let b = A::parse("variant259")?;
    assert_eq!(b, A::V2(59));
    Ok(())
}