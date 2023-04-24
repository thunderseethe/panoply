use self::file::{file_for_id, FileId, SourceFile};

pub mod diagnostic;
mod display_iter;
pub mod displayer;
pub mod file;
pub mod graph;
pub mod id;
pub mod indexed;
pub mod loc;
pub mod memory;
pub mod modules;
pub mod option;
pub mod span;
pub mod spanner;

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
    diagnostic::aiahr::AiahrcErrors,
    file::FileId,
    file::SourceFile,
    file::SourceFileSet,
    file::file_for_id,
    id::TermName,
    id::EffectName,
    id::EffectOpName,
    ident::Ident,
    modules::Module,
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

    fn file_for_id(&self, file_id: FileId) -> SourceFile {
        file_for_id(self.as_core_db(), file_id)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}
