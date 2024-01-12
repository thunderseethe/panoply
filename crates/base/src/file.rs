use crate::modules::Module;

#[salsa::interned]
pub struct FileId {
  #[return_ref]
  pub path: std::path::PathBuf,
}

#[salsa::input]
pub struct SourceFile {
  #[id]
  pub path: FileId,
  #[return_ref]
  pub contents: String,
}

#[salsa::input(singleton)]
pub struct SourceFileSet {
  pub files: Vec<SourceFile>,
}

#[salsa::tracked]
pub fn file_for_id(db: &dyn crate::Db, file_id: FileId) -> SourceFile {
  let source_file_set = SourceFileSet::get(db);
  *source_file_set
    .files(db)
    .iter()
    .find(|file| file.path(db) == file_id)
    .unwrap_or_else(|| panic!("Did not find file for {}", file_id.path(db).display()))
}

pub fn module_source_file(db: &dyn crate::Db, module: Module) -> SourceFile {
  let file_id = module.uri(db);
  file_for_id(db, file_id)
}
