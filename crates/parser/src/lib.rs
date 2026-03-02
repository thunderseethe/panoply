use base::{
  diagnostic::parser::ParseDiagnostic,
  file::{FileId, SourceFile, SourceFileSet, file_for_id},
  ident::Ident,
  loc::Loc,
  modules::Module,
};
/*use rowan::{
  ast::{support::{child, children}, AstChildren, AstPtr}, GreenNode, Language, SyntaxNode, TextRange, TextSize
};*/
pub use rowan::ast::AstNode;

use self::locator::Locator;
use cst::{Panoply, Syntax};
use rowan::{SyntaxNode, TextRange, TextSize};

//pub mod lexer;
pub mod parser;

pub(crate) mod locator;

pub mod error {
  use std::collections::LinkedList;

  use base::diagnostic::parser::ParseError;

  #[derive(Debug)]
  pub struct ParseErrors(pub LinkedList<ParseError>);
}

pub fn root_module_for_file(db: &dyn salsa::Database, file: SourceFile) -> Module {
  parse_module(db, file).module(db)
}

pub fn root_module_for_path(db: &dyn salsa::Database, path: std::path::PathBuf) -> Module {
  let file_id = FileId::new(db, path);
  let file = base::file::file_for_id(db, file_id);
  root_module_for_file(db, file)
}

#[salsa::tracked(debug)]
pub struct ParseFile<'db> {
  pub file: FileId,
  /// Root module of file
  pub module: Module,
  pub data: rowan::GreenNode,
}

pub fn locate(db: &dyn salsa::Database, file: FileId, line: u32, col: u32) -> Option<Loc> {
  let locator = locator_of_file(db, file_for_id(db, file));
  let byte = locator.unlocate(line as usize, col as usize)?;
  Some(Loc { byte })
}

pub fn unlocate(db: &dyn salsa::Database, file: FileId, byte: Loc) -> Option<(u32, u32)> {
  let locator = locator_of_file(db, file_for_id(db, file));
  let (line, col) = locator.locate(byte.byte)?;
  Some((line as u32, col as u32))
}

#[salsa::tracked(returns(ref))]
pub fn all_modules<'db>(db: &'db dyn salsa::Database) -> Vec<Module> {
  let source_file_set = SourceFileSet::get(db);
  source_file_set
    .files(db)
    .iter()
    .map(|file| {
      let parse_file = parse_module(db, *file);
      parse_file.module(db)
    })
    .collect::<Vec<_>>()
}

pub fn parse_errors(db: &dyn salsa::Database, file_id: FileId) -> Vec<ParseDiagnostic> {
  let file = file_for_id(db, file_id);
  let diag = parse_module::accumulated::<ParseDiagnostic>(db, file);
  diag.into_iter().cloned().collect()
}

#[salsa::tracked]
fn locator_of_file<'db>(db: &'db dyn salsa::Database, file: SourceFile) -> Locator {
  Locator::new(file.contents(db).as_str())
}

#[salsa::tracked]
pub fn parse_module_of<'db>(db: &'db dyn salsa::Database, module: Module) -> ParseFile<'db> {
  let file = base::file::module_source_file(db, module);
  parse_module(db, file)
}

#[salsa::tracked]
pub fn parse_module<'db>(db: &'db dyn salsa::Database, file: SourceFile) -> ParseFile<'db> {
  let core_db = db;
  let file_id = file.path(core_db);
  let path = file_id.path(core_db);
  let file_name = path
    .file_stem()
    .and_then(|os_str| os_str.to_str())
    .expect("Expected file name to exist and be valid UTF8");
  let mod_name = Ident::new(db, file_name);
  let module = Module::new(core_db, mod_name, file_id);

  let mut parser = parser::Parser::new(file.contents(core_db));
  parser.items();
  let (cst, errors) = parser.finish();

  use salsa::Accumulator;
  for error in errors {
    ParseDiagnostic { error }.accumulate(db)
  }

  ParseFile::new(db, file_id, module, cst)
}

#[salsa::tracked]
fn ident_starting_at<'db>(
  db: &'db dyn salsa::Database,
  file_id: FileId,
  line: u32,
  col: u32,
) -> Option<Ident> {
  let file = file_for_id(db, file_id);
  let locator = locator_of_file(db, file);
  let byte = locator
    .unlocate(line as usize, col as usize)
    .expect("Expected line and col to exist within file but they did not");
  let byte: u32 = byte.try_into().unwrap();
  let cst = parse_module(db, file).data(db);
  let node = SyntaxNode::<Panoply>::new_root(cst)
    .covering_element(TextRange::new(TextSize::new(byte), TextSize::new(byte + 1)));

  let Syntax::Identifier = node.kind() else {
    return None;
  };

  let id = node.into_token().unwrap();
  Some(Ident::new(db, id.text()))
}
