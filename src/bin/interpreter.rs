use std::path::PathBuf;

use aiahr::AiahrDatabase;
use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
use aiahr_core::id::ModuleId;
use aiahr_core::Db as CoreDb;
use aiahr_interpreter::Machine;
use aiahr_lower_ir::Db as LowerIrDb;
use aiahr_parser::Db;
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

    let mut uniq_files = args
        .files
        .into_iter()
        .chain(std::iter::once(args.main_file.clone()))
        .collect::<Vec<_>>();

    uniq_files.dedup();

    let files = uniq_files
        .into_iter()
        .map(|path| {
            let contents = std::fs::read_to_string(&path)?;
            let file: SourceFile =
                SourceFile::new(&db, ModuleId(next_mod_id), FileId::new(&db, path), contents);
            next_mod_id += 1;
            Ok(file)
        })
        .collect::<eyre::Result<Vec<_>>>()?;

    // This is a singleton so we just need to call constructor
    let _ = SourceFileSet::new(&db, files);

    let opt_ir = db.lower_item_for_file_name(args.main_file, db.ident_str("main"));

    let ir = match opt_ir {
        Some(ir) => ir,
        None => {
            for err in db.parse_errors() {
                println!("{:?}", err);
            }
            return Ok(());
        }
    };

    let mut interpreter = Machine::default();
    let value = interpreter.interpret(ir.item(&db).clone());

    println!("{:?}", value);

    Ok(())
}
