use std::path::PathBuf;

use aiahr::AiahrDatabase;
use aiahr_core::file::{SourceFile, SourceFileSet};
use aiahr_core::id::ModuleId;
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    /// Specify which passed in file contains the `main` function for execution.
    #[arg(short, long)]
    main_file: PathBuf,
    /// Files to be interpreted
    files: Vec<PathBuf>,
}

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    let db = AiahrDatabase::default();
    let mut next_mod_id = 0;

    let files = args
        .files
        .into_iter()
        .map(|path| {
            let contents = std::fs::read_to_string(&path)?;
            let file: SourceFile = SourceFile::new(&db, ModuleId(next_mod_id), path, contents);
            next_mod_id += 1;
            Ok(file)
        })
        .collect::<eyre::Result<Vec<_>>>()?;

    // This is a singleton so we just need to call constructor
    let _ = SourceFileSet::new(&db, files);

    //TODO: Query for IR of main function and execute it

    Ok(())
}
