use aiahr::{canonicalize_path_set, create_source_file_set, AiahrDatabase, Args};
use base::pretty::{PrettyPrint, PrettyWithCtx};
use base::Db as BaseDb;
use clap::Parser;
use interpreter::Machine;
use lower_reducir::Db as LowerReducIrDb;
use parser::Db;

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    let db = AiahrDatabase::default();

    let uniq_paths =
        canonicalize_path_set(args.files.iter().chain(std::iter::once(&args.main_file)))?;
    let _ = create_source_file_set(&db, uniq_paths)?;

    let opt_ir = db.lower_reducir_item_for_file_name(args.main_file, db.ident_str("main"));

    let ir = match opt_ir {
        Some(ir) => ir,
        None => {
            for err in db.all_parse_errors() {
                println!("{:?}", err);
            }
            return Ok(());
        }
    };

    println!("{}", ir.item(&db).pretty_with(&db).pprint().pretty(80));

    let mut interpreter = Machine::default();

    let value = interpreter.interpret(ir.item(&db).clone());
    println!("\n\nINTERPRETS INTO\n");

    println!("{}", value.pretty_with(&db).pprint().pretty(80));

    Ok(())
}
