use base::{
    diagnostic::aiahr::{AiahrcError, AiahrcErrors},
    file::{file_for_id, FileId, SourceFile, SourceFileSet},
    ident::Ident,
    loc::Loc,
    modules::Module,
    span::{SpanOf, Spanned},
};
use chumsky::Parser;
use cst::{CstIndxAlloc, CstModule, IdField, Pattern, Row, Term, Type};
use la_arena::Idx;

use crate::{
    lexer::aiahr_lexer,
    parser::{aiahr_parser, to_stream},
};

use self::locator::Locator;

mod expr;
pub mod lexer;
pub mod parser;

pub(crate) mod locator;

pub mod error {
    use std::collections::LinkedList;
    use std::fmt::Display;

    use crate::lexer::Token;
    use base::{diagnostic::parser::ParseError, span::Span};

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
pub struct Jar(
    all_modules,
    locator_of_file,
    ident_starting_at,
    parse_module,
    ParseFile,
);
pub trait Db: salsa::DbWithJar<Jar> + base::Db {
    fn as_parser_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn all_modules(&self) -> &[Module] {
        all_modules(self.as_parser_db())
    }

    fn root_module_for_path(&self, path: std::path::PathBuf) -> Module {
        let file_id = FileId::new(self.as_core_db(), path);
        let file = base::file::file_for_id(self.as_core_db(), file_id);
        self.root_module_for_file(file)
    }

    fn root_module_for_file(&self, file: SourceFile) -> Module {
        self.parse_module(file).module(self.as_parser_db())
    }

    fn parse_module(&self, file: SourceFile) -> ParseFile {
        parse_module(self.as_parser_db(), file)
    }

    fn parse_module_of(&self, module: Module) -> ParseFile {
        let file = base::file::module_source_file(self.as_core_db(), module);
        self.parse_module(file)
    }

    fn all_parse_errors(&self) -> Vec<AiahrcError> {
        let file_set = SourceFileSet::get(self.as_core_db());
        file_set
            .files(self.as_core_db())
            .into_iter()
            .flat_map(|file| {
                parse_module::accumulated::<AiahrcErrors>(self.as_parser_db(), file).into_iter()
            })
            .collect()
    }

    fn parse_errors(&self, file_id: FileId) -> Vec<AiahrcError> {
        let file = self.file_for_id(file_id);
        parse_module::accumulated::<AiahrcErrors>(self.as_parser_db(), file)
    }

    fn ident_starting_at(&self, file_id: FileId, line: u32, col: u32) -> Option<Ident> {
        ident_starting_at(self.as_parser_db(), file_id, line, col)
    }

    fn locate(&self, file: FileId, line: u32, col: u32) -> Option<Loc> {
        let locator = locator_of_file(self.as_parser_db(), self.file_for_id(file));
        let byte = locator.unlocate(line as usize, col as usize)?;
        Some(Loc {
            file,
            byte,
            line: line as usize,
            col: col as usize,
        })
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + base::Db {}

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

#[salsa::tracked(return_ref)]
fn locator_of_file(db: &dyn crate::Db, file: SourceFile) -> Locator {
    let core_db = db.as_core_db();
    let file_id = file.path(core_db);
    Locator::new(file_id, file.contents(core_db).as_str())
}

#[salsa::tracked]
fn parse_module(db: &dyn crate::Db, file: SourceFile) -> ParseFile {
    let core_db = db.as_core_db();
    let file_id = file.path(core_db);
    let path = file_id.path(core_db);
    let file_name = path
        .file_stem()
        .and_then(|os_str| os_str.to_str())
        .expect("Expected file name to exist and be valid UTF8");
    let mod_name = db.ident_str(file_name);
    let module = Module::new(core_db, mod_name, file_id);
    let lexer = aiahr_lexer(db);
    let (tokens, eoi) = match lexer.lex(locator_of_file(db, file), file.contents(core_db)) {
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

#[salsa::tracked]
fn ident_starting_at(db: &dyn crate::Db, file_id: FileId, line: u32, col: u32) -> Option<Ident> {
    let file = file_for_id(db.as_core_db(), file_id);
    let locator = locator_of_file(db, file);
    let byte = locator
        .unlocate(line as usize, col as usize)
        .expect("Expected line and col to exist within file but they did not");
    let loc = Loc {
        file: file_id,
        line: line as usize,
        col: col as usize,
        byte,
    };

    let cst_module = db.parse_module(file).data(db);
    let item_idx = cst_module
        .items
        .binary_search_by(|item| {
            item.span(&cst_module.indices)
                .by_precedence()
                .partial_cmp(&loc)
                .unwrap()
        })
        .ok()?;
    match &cst_module.items[item_idx] {
        cst::Item::Effect(eff) => {
            if eff.name.span().contains(loc) {
                return Some(eff.name.value);
            }
            eff.ops
                .iter()
                .find(|op| op.name.span().contains(loc))
                .map(|op| op.name.value)
        }
        cst::Item::Term(term) => {
            if term.name.span().contains(loc) {
                return Some(term.name.value);
            }

            struct DfsFindSpan<'a> {
                indices: &'a CstIndxAlloc,
                needle: Loc,
            }
            // We want to end our dfs early when we find the ident at our span, or if no such ident
            // exists. To represent this early exit we use the Err behavior of result to bubble our
            // result up the stack.
            type ShortCircuit<T> = Result<(), T>;
            fn continue_search<T>() -> ShortCircuit<T> {
                // Ok(()) represents check the next thing in our dfs traversal.
                // We use it as an early return to signal check the next item.
                Ok(())
            }
            impl<'a> DfsFindSpan<'a> {
                fn search_ident(&self, ident: &SpanOf<Ident>) -> ShortCircuit<Option<Ident>> {
                    if ident.span().contains(self.needle) {
                        Err(Some(ident.value))
                    } else {
                        Ok(())
                    }
                }

                fn search_row(
                    &self,
                    row: &Row<Ident, IdField<Idx<Type<Ident>>>>,
                ) -> ShortCircuit<Option<Ident>> {
                    if !row.spanned(self.indices).contains(self.needle) {
                        return continue_search();
                    }
                    match row {
                        cst::Row::Concrete(closed) => {
                            for field in closed.elements() {
                                self.search_ident(&field.label)?;
                                self.search_type(field.target)?;
                            }
                        }
                        cst::Row::Variable(vars) => {
                            for var in vars.elements() {
                                self.search_ident(var)?;
                            }
                        }
                        cst::Row::Mixed {
                            concrete,
                            variables,
                            ..
                        } => {
                            for field in concrete.elements() {
                                self.search_ident(&field.label)?;
                                self.search_type(field.target)?;
                            }
                            for var in variables.elements() {
                                self.search_ident(var)?;
                            }
                        }
                    }
                    Err(None)
                }

                fn search_type(&self, idx: Idx<Type<Ident>>) -> ShortCircuit<Option<Ident>> {
                    let ty = &self.indices[idx];
                    if !ty.spanned(self.indices).contains(self.needle) {
                        return continue_search();
                    }
                    match ty {
                        Type::Named(var) => self.search_ident(var)?,
                        Type::Sum { variants, .. } => self.search_row(variants)?,
                        Type::Product { fields, .. } => {
                            if let Some(row) = fields {
                                self.search_row(row)?;
                            }
                        }
                        Type::Function {
                            domain, codomain, ..
                        } => {
                            self.search_type(*domain)?;
                            self.search_type(*codomain)?;
                        }
                        Type::Parenthesized { type_, .. } => self.search_type(*type_)?,
                    };
                    Err(None)
                }

                fn search_pat(&self, idx: Idx<Pattern>) -> ShortCircuit<Option<Ident>> {
                    let pat = &self.indices[idx];
                    if !pat.span().contains(self.needle) {
                        return continue_search();
                    }
                    match pat {
                        Pattern::ProductRow(prod) => {
                            if let Some(fields) = &prod.fields {
                                for field in fields.elements() {
                                    self.search_ident(&field.label)?;
                                    self.search_pat(field.target)?;
                                }
                            }
                        }
                        Pattern::SumRow(sum) => {
                            self.search_ident(&sum.field.label)?;
                            self.search_pat(sum.field.target)?;
                        }
                        Pattern::Whole(var) => self.search_ident(var)?,
                    }
                    Err(None)
                }

                fn search_term(&self, idx: Idx<Term>) -> ShortCircuit<Option<Ident>> {
                    let term = &self.indices[idx];
                    if term.spanned(self.indices).span().contains(self.needle) {
                        return continue_search();
                    }
                    match term {
                        cst::Term::Binding {
                            var,
                            annotation,
                            expr,
                            ..
                        } => {
                            self.search_ident(var)?;
                            if let Some(ann) = annotation {
                                self.search_type(ann.type_)?;
                            }
                            self.search_term(*expr)?;
                        }
                        cst::Term::Handle { handler, expr, .. } => {
                            self.search_term(*handler)?;
                            self.search_term(*expr)?;
                        }
                        cst::Term::Abstraction {
                            arg,
                            annotation,
                            body,
                            ..
                        } => {
                            self.search_ident(arg)?;
                            if let Some(ann) = annotation {
                                self.search_type(ann.type_)?;
                            }
                            self.search_term(*body)?;
                        }
                        cst::Term::Application { func, arg, .. } => {
                            self.search_term(*func)?;
                            self.search_term(*arg)?;
                        }
                        cst::Term::ProductRow(prod) => {
                            if let Some(fields) = &prod.fields {
                                for field in fields.elements() {
                                    self.search_ident(&field.label)?;
                                    self.search_term(field.target)?;
                                }
                            }
                        }
                        cst::Term::Concat { left, right, .. } => {
                            self.search_term(*left)?;
                            self.search_term(*right)?;
                        }
                        cst::Term::SumRow(sum) => {
                            self.search_ident(&sum.field.label)?;
                            self.search_term(sum.field.target)?;
                        }
                        cst::Term::DotAccess { base, field, .. } => {
                            self.search_ident(field)?;
                            self.search_term(*base)?;
                        }
                        cst::Term::Match { cases, .. } => {
                            // TODO: Technically we could be a little smarter by skipping the field
                            // as one check if the entire field's span doesn't contain `needle`.
                            // Need benchmarks to tell if this is worth or not.
                            for field in cases.elements() {
                                self.search_pat(field.label)?;
                                self.search_term(field.target)?;
                            }
                        }
                        cst::Term::SymbolRef(symbol) => self.search_ident(symbol)?,
                        cst::Term::Parenthesized { term, .. } => self.search_term(*term)?,
                        cst::Term::Int(_) => {}
                    }
                    Err(None)
                }
            }
            let dfs = DfsFindSpan {
                indices: &cst_module.indices,
                needle: loc,
            };
            dfs.search_term(term.value).err().flatten()
        }
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
