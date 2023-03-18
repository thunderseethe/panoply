mod expr;
pub mod lexer;
pub mod parser;

#[salsa::jar(db = Db)]
pub struct Jar();
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db {}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
