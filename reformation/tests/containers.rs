#[cfg(feature="containers")]
mod tests{
    use reformation::containers::VecDelimited;
    use reformation::Reformation;

    #[derive(Reformation)]
    #[reformation(r"{header}\r\n{rows}")]
    struct Nested<'a>{
        header: &'a str,
        #[reformation(r"(?s).*")]
        rows: VecDelimited<Record<'a>, "\r\n">
    }

    #[derive(Reformation, Debug, PartialEq)]
    #[reformation("{name}: {numbers}")]
    struct Record<'a>{
        name: &'a str,
        #[reformation(r"[^\n]*")]
        numbers: VecDelimited<u32, ", ">,
    }

    #[test]
    fn test_nested_record(){
        let data = include_str!("nested_record.txt");
        let nested = Nested::parse(data).unwrap();
        assert_eq!(nested.header, "Apparitions");
        assert_eq!(nested.rows.0.len(), 3);
        let rows = nested.rows.0;
        assert_eq!(rows[0], Record{
            name: "Ghost",
            numbers: VecDelimited(vec![1, 2, 3, 4, 5, 6, 7])
        });
        assert_eq!(rows[1], Record{
            name: "Specter",
            numbers: VecDelimited(vec![3, 9]),
        });
        assert_eq!(rows[2], Record{
            name: "Lost soul",
            numbers: VecDelimited(vec![0, 334, 21]),
        });
    }
}
