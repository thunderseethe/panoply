use aiahr_core::{
    diagnostic::aiahr::{AiahrcError, AiahrcErrors},
    file::{FileId, SourceFile, SourceFileSet},
    modules::Module,
};
use aiahr_cst::{CstIndxAlloc, CstModule};
use chumsky::Parser;

use crate::{
    lexer::aiahr_lexer,
    parser::{aiahr_parser, to_stream},
};

mod expr;
pub mod lexer;
pub mod parser;

pub(crate) mod locator;

pub mod error {
    use std::collections::LinkedList;
    use std::fmt::Display;

    use crate::lexer::Token;
    use aiahr_core::diagnostic::parser::ParseError;
    use aiahr_core::span::Span;

    #[derive(Debug)]
    pub struct ParseErrors(pub LinkedList<ParseError>);

    struct TokenOrEOFByName(Option<Token>);

    impl Display for TokenOrEOFByName {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(t) = self.0 {
                write!(f, "'{}'", t.name())
            } else {
                write!(f, "EOF")
            }
        }
    }

    impl chumsky::Error<Token> for ParseErrors {
        type Span = Span;
        type Label = ();

        fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
            span: Self::Span,
            expected: Iter,
            found: Option<Token>,
        ) -> Self {
            ParseErrors(LinkedList::from([ParseError::WrongToken {
                span,
                got: TokenOrEOFByName(found).to_string(),
                want_any: expected
                    .into_iter()
                    .map(|token| TokenOrEOFByName(token).to_string())
                    .collect(),
            }]))
        }

        fn with_label(self, _: Self::Label) -> Self {
            self
        }

        fn merge(mut self, other: Self) -> Self {
            let mut other = other;
            self.0.append(&mut other.0);
            self
        }
    }
}

#[salsa::jar(db = Db)]
pub struct Jar(all_modules, parse_module, ParseFile);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {
    fn as_parser_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn all_modules(&self) -> &[Module] {
        all_modules(self.as_parser_db())
    }

    fn root_module_for_path(&self, path: std::path::PathBuf) -> Module {
        let file_id = FileId::new(self.as_core_db(), path);
        let file = aiahr_core::file::file_for_id(self.as_core_db(), file_id);
        self.root_module_for_file(file)
    }

    fn root_module_for_file(&self, file: SourceFile) -> Module {
        self.parse_module(file).module(self.as_parser_db())
    }

    fn parse_module(&self, file: SourceFile) -> ParseFile {
        parse_module(self.as_parser_db(), file)
    }

    fn parse_module_of(&self, module: Module) -> ParseFile {
        let file = aiahr_core::file::module_source_file(self.as_core_db(), module);
        self.parse_module(file)
    }

    fn parse_errors(&self) -> Vec<AiahrcError> {
        let file_set = SourceFileSet::get(self.as_core_db());
        file_set
            .files(self.as_core_db())
            .into_iter()
            .flat_map(|file| {
                parse_module::accumulated::<AiahrcErrors>(self.as_parser_db(), file).into_iter()
            })
            .collect()
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_core::Db {}

#[salsa::tracked]
#[derive(DebugWithDb)]
pub struct ParseFile {
    #[id]
    pub file: FileId,
    /// Root module of file
    pub module: Module,
    #[return_ref]
    pub data: CstModule,
}

#[salsa::tracked(return_ref)]
fn all_modules(db: &dyn Db) -> Vec<Module> {
    let source_file_set = SourceFileSet::get(db.as_core_db());
    source_file_set
        .files(db.as_core_db())
        .iter()
        .map(|file| {
            let parse_file = db.parse_module(*file);
            parse_file.module(db)
        })
        .collect::<Vec<_>>()
}

#[salsa::tracked]
fn parse_module(db: &dyn Db, file: SourceFile) -> ParseFile {
    let core_db = db.as_core_db();
    let file_id = file.path(core_db);
    let path = file_id.path(core_db);
    let file_name = path
        .file_name()
        .and_then(|os_str| os_str.to_str())
        .expect("Expected file name to exist and be valid UTF8");
    let mod_name = db.ident_str(file_name);
    let module = Module::new(core_db, mod_name, file_id);
    let lexer = aiahr_lexer(db);
    let (tokens, eoi) = match lexer.lex(file_id, file.contents(core_db)) {
        Ok(tokens) => tokens,
        Err(err) => {
            AiahrcErrors::push(db, AiahrcError::from(err));
            return ParseFile::new(db, file_id, module, CstModule::default());
        }
    };

    let cst_module = aiahr_parser()
        .parse(to_stream(tokens, eoi))
        .map_err(|errs| {
            for err in errs.into_iter().flat_map(|list| list.0.into_iter()) {
                AiahrcErrors::push(db, AiahrcError::from(err))
            }
        })
        .map(|items| {
            let mut indices = CstIndxAlloc::default();
            let items = items.apply(&mut indices);
            CstModule { items, indices }
        })
        .unwrap_or_default();

    ParseFile::new(db, file_id, module, cst_module)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
