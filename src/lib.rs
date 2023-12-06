use std::path::{Path, PathBuf};

use aiahr_core::displayer::Displayer;
use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
use aiahr_core::ident::Ident;
use aiahr_core::modules::Module;
use aiahr_core::Db;
use clap::Parser;

#[salsa::db(
    aiahr_ast::Jar,
    aiahr_core::Jar,
    aiahr_desugar::Jar,
    aiahr_emit_wasm::Jar,
    aiahr_reducir::Jar,
    aiahr_lower_reducir::Jar,
    aiahr_lower_medir::Jar,
    aiahr_medir::Jar,
    aiahr_nameres::Jar,
    aiahr_optimize_reducir::Jar,
    aiahr_parser::Jar,
    aiahr_tc::Jar,
    aiahr_ty::Jar
)]
#[derive(Default)]
pub struct AiahrDatabase {
    storage: salsa::Storage<Self>,
}
impl salsa::Database for AiahrDatabase {}
impl salsa::ParallelDatabase for AiahrDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
        })
    }
}

impl Displayer<Module> for AiahrDatabase {
    type Output<'a> = String;

    fn show<'a>(&self, value: &'a Module) -> Self::Output<'a> {
        value
            .name(self.as_core_db())
            .text(self.as_core_db())
            .clone()
    }
}
impl Displayer<Ident> for AiahrDatabase {
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
    db: &AiahrDatabase,
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
