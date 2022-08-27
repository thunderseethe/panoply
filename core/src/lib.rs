pub mod cst;
pub mod diagnostic;
mod display_iter;
pub mod handle;
pub mod loc;
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
