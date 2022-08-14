#[cfg(test)]
#[macro_use]
extern crate assert_matches;

pub mod cst;
pub mod error;
mod expr;
pub mod lexer;
pub mod parser;
pub mod token;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
