use base::{
  diagnostic::error::{PanoplyError, PanoplyErrors},
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

  fn all_parse_errors(&self) -> Vec<PanoplyError> {
    let file_set = SourceFileSet::get(self.as_core_db());
    file_set
      .files(self.as_core_db())
      .into_iter()
      .flat_map(|file| {
        parse_module::accumulated::<PanoplyErrors>(self.as_parser_db(), file).into_iter()
      })
      .collect()
  }

  fn parse_errors(&self, file_id: FileId) -> Vec<PanoplyError> {
    let file = self.file_for_id(file_id);
    parse_module::accumulated::<PanoplyErrors>(self.as_parser_db(), file)
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
  pub data: rowan::GreenNode,
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

  let mut parser = parser::Parser::new(file.contents(core_db));
  parser.items();
  let (cst, errors) = parser.finish();

  for error in errors {
    PanoplyErrors::push(db, PanoplyError::from(error));
  }

  ParseFile::new(db, file_id, module, cst)
}

#[salsa::tracked]
fn ident_starting_at(db: &dyn crate::Db, file_id: FileId, line: u32, col: u32) -> Option<Ident> {
  let file = file_for_id(db.as_core_db(), file_id);
  let locator = locator_of_file(db, file);
  let byte = locator
    .unlocate(line as usize, col as usize)
    .expect("Expected line and col to exist within file but they did not");
  let byte: u32 = byte.try_into().unwrap();
  let cst = db.parse_module(file).data(db);
  let node = SyntaxNode::<Panoply>::new_root(cst)
    .covering_element(TextRange::new(TextSize::new(byte), TextSize::new(byte + 1)));

  let Syntax::Identifier = node.kind() else {
    return None;
  };

  let id = node.into_token().unwrap();
  Some(db.ident_str(id.text()))
}
