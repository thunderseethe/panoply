use aiahr_core::{
    cst::indexed::CstModule,
    diagnostic::aiahr::{AiahrcError, AiahrcErrors},
    file::SourceFile,
    id::ModuleId,
    modules::Module,
    Top,
};
use bumpalo::Bump;
use chumsky::Parser;

use crate::{
    lexer::aiahr_lexer,
    parser::{aiahr_parser, to_stream},
};

mod expr;
pub mod lexer;
pub mod parser;

#[salsa::jar(db = Db)]
pub struct Jar(parse_module, parse_module_of, ParseModule);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {
    fn as_parser_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn parse_module(&self, file: SourceFile) -> ParseModule {
        parse_module(self.as_parser_db(), file)
    }

    fn parse_module_of(&self, mod_id: ModuleId) -> ParseModule {
        parse_module_of(self.as_parser_db(), self.top(), mod_id)
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_core::Db {}

#[salsa::tracked]
#[derive(DebugWithDb)]
pub struct ParseModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub data: CstModule,
}

#[salsa::tracked]
fn parse_module_of(db: &dyn Db, top: Top, mod_id: ModuleId) -> ParseModule {
    let file = aiahr_core::file::module_source_file(db.as_core_db(), top, mod_id);
    db.parse_module(file)
}

#[salsa::tracked]
fn parse_module(db: &dyn Db, file: SourceFile) -> ParseModule {
    let core_db = db.as_core_db();
    let mod_id = file.module(core_db);
    let module = Module::new(core_db, mod_id, file.path(core_db).clone());
    let lexer = aiahr_lexer(db);
    let (tokens, eoi) = match lexer.lex(mod_id, file.contents(core_db)) {
        Ok(tokens) => tokens,
        Err(err) => {
            AiahrcErrors::push(db, AiahrcError::from(err));
            return ParseModule::new(db, module, CstModule::default());
        }
    };

    let arena = Bump::new();
    let parser = aiahr_parser(&arena);
    let cst_module = parser
        .parse(to_stream(tokens, eoi))
        .map_err(|errs| {
            for err in errs.into_iter().flat_map(|list| list.0.into_iter()) {
                AiahrcErrors::push(db, AiahrcError::from(err))
            }
        })
        .map(CstModule::from)
        .unwrap_or_default();

    ParseModule::new(db, module, cst_module)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
