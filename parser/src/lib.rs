#[cfg(test)]
#[macro_use]
extern crate assert_matches;

pub mod error;
mod expr;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
