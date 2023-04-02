use std::path::PathBuf;

use crate::id::ModuleId;

#[salsa::input]
pub struct SourceFile {
    pub module: ModuleId,
    #[return_ref]
    pub path: std::path::PathBuf,
    #[return_ref]
    pub contents: String,
}

#[salsa::input(singleton)]
pub struct SourceFileSet {
    pub files: Vec<SourceFile>,
}

#[salsa::tracked]
pub fn module_source_file(db: &dyn crate::Db, _top: crate::Top, mod_id: ModuleId) -> SourceFile {
    let source_file_set = SourceFileSet::get(db);
    *source_file_set
        .files(db)
        .iter()
        .find(|file| file.module(db) == mod_id)
        .unwrap_or_else(|| {
            panic!(
                "ICE: Module {:?} constructed with no accompanying source file",
                mod_id
            )
        })
}

#[salsa::tracked]
pub fn module_id_for_path(db: &dyn crate::Db, _top: crate::Top, path: PathBuf) -> ModuleId {
    let source_file_set = SourceFileSet::get(db);
    source_file_set
        .files(db)
        .iter()
        .find(|file| file.path(db) == &path)
        .map(|file| file.module(db))
        .unwrap_or_else(|| panic!("ICE: No source file for path {:?}", path))
}
