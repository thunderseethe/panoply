pub mod ast;
pub mod cst;
pub mod diagnostic;
mod display_iter;
pub mod displayer;
pub mod file;
pub mod graph;
pub mod id;
pub mod indexed;
pub mod ir;
pub mod loc;
pub mod memory;
pub mod modules;
pub mod nst;
pub mod option;
pub mod span;
pub mod spanner;
pub mod token;
pub mod ty;

pub mod ident {
    /// An interned identifier.
    #[salsa::interned]
    #[derive(PartialEq)]
    pub struct Ident {
        #[return_ref]
        pub text: String,
    }
}

#[salsa::jar(db = Db)]
pub struct Jar(
    ast::AstModule,
    ast::indexed::SalsaItem,
    diagnostic::aiahr::AiahrcErrors,
    file::SourceFile,
    file::SourceFileSet,
    file::module_source_file,
    ident::Ident,
    ir::indexed::IrTy,
    modules::Module,
    modules::SalsaModuleTree,
    modules::all_modules,
    modules::module_of,
    modules::module_id_of,
    ty::TyData,
    ty::SalsaRowFields,
    ty::SalsaRowValues,
    Top,
);
pub trait Db: salsa::DbWithJar<Jar> {
    fn as_core_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn ident(&self, text: String) -> ident::Ident {
        ident::Ident::new(self.as_core_db(), text)
    }

    // Since this must be trait object safe we're not allowed to use generics.
    // So instead we have a manual specialization for `&str`
    fn ident_str(&self, text: &str) -> ident::Ident {
        self.ident(text.to_string())
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

/// Trivial type to satisfy salsa's requirements
/// Eventually all salsa tracked functions should be in terms of salsa tracked structs and we
/// shouldn't need this anymore.
#[salsa::tracked]
pub struct Top {}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
