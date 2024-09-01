use super::expression::Literal;

pub struct Callable {
    pub name: String,
    pub arity: u32,
    pub function: Box<dyn Fn() -> Box<Literal>>,
}
impl Callable {
    pub fn new(name: String, arity: u32, function: Box<dyn Fn() -> Box<Literal>>) -> Callable {
        Callable { name, arity, function }
    }
}
