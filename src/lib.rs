use std::path::{Path, PathBuf};

use base::{
    displayer::Displayer,
    file::{FileId, SourceFile, SourceFileSet},
    ident::Ident,
    modules::Module,
    Db,
};
use clap::Parser;

#[salsa::db(
    ast::Jar,
    base::Jar,
    desugar::Jar,
    emit_wasm::Jar,
    reducir::Jar,
    lower_reducir::Jar,
    lower_medir::Jar,
    medir::Jar,
    nameres::Jar,
    optimize_reducir::Jar,
    parser::Jar,
    tc::Jar,
    ty::Jar
)]
#[derive(Default)]
pub struct PanoplyDatabase {
    storage: salsa::Storage<Self>,
}
impl salsa::Database for PanoplyDatabase {}
impl salsa::ParallelDatabase for PanoplyDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
        })
    }
}

impl Displayer<Module> for PanoplyDatabase {
    type Output<'a> = String;

    fn show<'a>(&self, value: &'a Module) -> Self::Output<'a> {
        value
            .name(self.as_core_db())
            .text(self.as_core_db())
            .clone()
    }
}
impl Displayer<Ident> for PanoplyDatabase {
    type Output<'a> = String;

    fn show<'a>(&self, value: &'a Ident) -> Self::Output<'a> {
        value.text(self.as_core_db()).clone()
    }
}

#[derive(Parser, Debug)]
pub struct Args {
    /// Specify which passed in file contains the `main` function for execution.
    #[arg(short, long)]
    pub main_file: PathBuf,
    /// Files to be interpreted
    pub files: Vec<PathBuf>,
}

pub fn canonicalize_path_set<P: AsRef<Path>>(
    paths: impl IntoIterator<Item = P>,
) -> eyre::Result<Vec<PathBuf>> {
    let mut uniq_files = paths
        .into_iter()
        .map(|path| path.as_ref().canonicalize().map_err(Into::into))
        .collect::<eyre::Result<Vec<_>>>()?;

    uniq_files.dedup();

    Ok(uniq_files)
}

pub fn create_source_file_set(
    db: &PanoplyDatabase,
    paths: impl IntoIterator<Item = PathBuf>,
) -> eyre::Result<SourceFileSet> {
    let files = paths
        .into_iter()
        .map(|path| {
            let contents = std::fs::read_to_string(&path)?;
            let file = SourceFile::new(db, FileId::new(db, path), contents);
            Ok(file)
        })
        .collect::<eyre::Result<Vec<_>>>()?;

    Ok(SourceFileSet::new(db, files))
}
