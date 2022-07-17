pub mod cst;
pub mod error;
pub mod lexer;
pub mod loc;
pub mod parser;
pub mod span;
pub mod token;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
