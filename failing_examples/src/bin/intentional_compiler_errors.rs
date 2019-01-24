use reformation::Reformation;

// Should not compile without reformation attribute
#[derive(Reformation)]
struct A {}

// Should not compile, since format string is not correct regular expression
#[derive(Reformation)]
#[reformation("(")]
struct B {}

// Format string does not cover fields, which does not present in struct
#[derive(Reformation)]
#[reformation("{x}")]
struct C {}

// Format string features unnamed argument
// TODO: figure out why span is broken
#[derive(Reformation)]
#[reformation("{}")]
struct D {
    a: i32,
}

// Format string does not contain enough arguments
#[derive(Reformation)]
#[reformation("")]
struct E(usize);

// Format string must not contain named arguments
#[derive(Reformation)]
#[reformation("{x}")]
struct F(usize);

// Format string contains to many arguments
#[derive(Reformation)]
#[reformation("{} {}")]
struct G(usize);

// Format string does not cover all variants
#[derive(Reformation)]
#[reformation("(RED|GREEN)")]
enum H {
    RED,
    GREEN,
    BLUE,
}

// Format string does not cover variant values
#[derive(Reformation)]
#[reformation("({})")]
enum J {
    A(i32, i32),
}

#[derive(Reformation)]
#[reformation("{a}")]
struct K {
    #[reformation("(")]
    a: usize,
}

fn main() {}
