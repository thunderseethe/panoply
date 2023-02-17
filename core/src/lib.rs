pub mod ast;
pub mod cst;
pub mod diagnostic;
mod display_iter;
pub mod displayer;
pub mod graph;
pub mod id;
pub mod ir;
pub mod loc;
pub mod memory;
pub mod modules;
pub mod nst;
pub mod option;
pub mod span;
pub mod spanner;
pub mod token;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
