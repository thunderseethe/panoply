use aiahr_core::{
    cst::indexed::Module,
    diagnostic::aiahr::{AiahrcError, AiahrcErrors},
    file::SourceFile,
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
pub struct Jar(parse_module);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db {}

#[salsa::tracked]
pub fn parse_module(db: &dyn Db, file: SourceFile) -> Option<Module> {
    let lexer = aiahr_lexer(db);
    let (tokens, eoi) = lexer
        .lex(file.module(db.as_core_db()), file.contents(db.as_core_db()))
        .map_err(|err| AiahrcErrors::push(db, AiahrcError::from(err)))
        .ok()?;

    let arena = Bump::new();
    let parser = aiahr_parser(&arena);
    parser
        .parse(to_stream(tokens, eoi))
        .map_err(|errs| {
            for err in errs.into_iter().flat_map(|list| list.0.into_iter()) {
                AiahrcErrors::push(db, AiahrcError::from(err))
            }
        })
        .ok()
        .map(Module::from)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
