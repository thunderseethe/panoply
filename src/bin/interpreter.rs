use std::ops::Deref;

use aiahr::{canonicalize_path_set, create_source_file_set, AiahrDatabase, Args};
use aiahr_core::Db as CoreDb;
use aiahr_interpreter::Machine;
use aiahr_lower_ir::Db as LowerIrDb;
use aiahr_parser::Db;
use clap::Parser;
use pretty::BoxDoc;

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    let db = AiahrDatabase::default();

    let uniq_paths =
        canonicalize_path_set(args.files.iter().chain(std::iter::once(&args.main_file)))?;
    let _ = create_source_file_set(&db, uniq_paths)?;

    let opt_ir = db.lower_item_for_file_name(args.main_file, db.ident_str("main"));

    let ir = match opt_ir {
        Some(ir) => ir,
        None => {
            for err in db.all_parse_errors() {
                println!("{:?}", err);
            }
            return Ok(());
        }
    };

    let doc: BoxDoc<'_, ()> = ir.item(&db).pretty(&db, &pretty::BoxAllocator).into_doc();
    println!("{}", doc.deref().pretty(80));

    let mut interpreter = Machine::default();

    let value = interpreter.interpret(ir.item(&db).clone());
    println!("\n\nINTERPRETS INTO\n");

    let doc: BoxDoc<'_, ()> = value.pretty(&db, &pretty::BoxAllocator).into_doc();
    println!("{}", doc.deref().pretty(80));

    Ok(())
}
