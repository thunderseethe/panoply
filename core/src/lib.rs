use self::diagnostic::aiahr;

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

pub mod ident {
    /// An interned identifier.
    #[salsa::interned]
    pub struct Ident {
        #[return_ref]
        pub text: String,
    }
}

#[salsa::jar(db = Db)]
pub struct Jar(ident::Ident, modules::Module);
pub trait Db: salsa::DbWithJar<Jar> {}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

// TODO: We can remove this once trait upcasting is stabilized.
pub trait AsCoreDb {
    fn as_core_db(&self) -> &dyn crate::Db;
}
impl AsCoreDb for dyn crate::Db {
    fn as_core_db(&self) -> &dyn crate::Db {
        self
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
