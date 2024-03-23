use clap::Parser;
use emit_wasm::Db as EmitWasmDb;
use nameres::Db;
use panoply::{canonicalize_path_set, create_source_file_set, Args, PanoplyDatabase};
use parser::Db as NameResDb;
use wasmparser::WasmFeatures;
use wasmtime::{Config, Engine, Linker, Module, Store};

fn main() -> eyre::Result<()> {
  let args = Args::parse();

  let db = PanoplyDatabase::default();

  let uniq_paths =
    canonicalize_path_set(args.files.iter().chain(std::iter::once(&args.main_file)))?;
  let main_file = uniq_paths.iter().find(|path| path.ends_with(&args.main_file)).unwrap().clone();
  let _ = create_source_file_set(&db, uniq_paths)?;

  let wasm_module = db.emit_module_for_path(main_file);
  for err in db.all_parse_errors() {
    println!("{:?}", err);
  }
  for err in db.all_nameres_errors() {
    println!("{:?}", err);
  }

  let bytes = wasm_module.finish();

  let mut validator = wasmparser::Validator::new_with_features(WasmFeatures::default());
  let tys = validator.validate_all(&bytes);
  let mut printer = wasmprinter::Printer::default();
  printer.print_offsets(true);
  let wat = printer.print(&bytes).unwrap();
  println!("{}", wat);

  match tys {
    Ok(_) => {}
    Err(err) => {
      eprintln!("{}", err);
    }
  }

  let mut file = std::fs::OpenOptions::new()
    .truncate(true)
    .write(true)
    .open("./testbed/wand.wat")
    .unwrap();
  use std::io::Write;
  writeln!(file, "{}", wat).unwrap();
  let mut file = std::fs::OpenOptions::new()
    .truncate(true)
    .write(true)
    .open("./testbed/wand.wasm")
    .unwrap();
  file.write_all(&bytes).unwrap();
  /*let mut file = std::fs::OpenOptions::new()
    .read(true)
    .open("./testbed/wand.wat")
    .unwrap();
  use std::io::Read;
  let mut wat = String::new();
  file.read_to_string(&mut wat).unwrap();*/

  let mut config = Config::new();
  config
    .debug_info(true)
    .wasm_bulk_memory(true)
    .wasm_multi_value(true)
    .wasm_multi_memory(true)
    .coredump_on_trap(true);
  let engine = Engine::new(&config).unwrap();

  let mut store = Store::new(&engine, ());

  let linker = Linker::new(&engine);

  let module = Module::new(&engine, wat).unwrap();

  let instance = linker.instantiate(&mut store, &module).unwrap();

  let main = instance
    .get_typed_func::<(), i32>(&mut store, "main")
    .unwrap();

  match main.call(&mut store, ()) {
    Ok(val) => {
      println!("Success: {}", val);
    }
    Err(err) => {
      eprintln!("{}", err.root_cause());
      eprintln!("{}", err);
    }
  };

  Ok(())
}
