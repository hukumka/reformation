use crate::ParseOverride;

#[derive(Debug, Eq, PartialEq)]
pub struct VecDelimited<T, const SEP: &'static str>(pub Vec<T>);

impl<'a, T: ParseOverride<'a>, const SEP: &'static str> ParseOverride<'a> for VecDelimited<T, {SEP}>{
    fn parse_override(s: &'a str) -> Result<Self, crate::Error>{
        let data: Result<Vec<_>, _> = s.split(SEP)
            .map(|s| {
                println!("{:?}", s);
                T::parse_override(s)
            })
            .collect();
        Ok(Self(data?))
    }
}