use clap::Parser;
use emit_wasm::Db as EmitWasmDb;
use nameres::Db;
use panoply::{canonicalize_path_set, create_source_file_set, Args, PanoplyDatabase};
use parser::Db as NameResDb;
use wasmparser::WasmFeatures;
use wasmtime::{Config, Engine, FuncType, Linker, Memory, MemoryType, Module, Store, Val, ValType};

fn main() -> eyre::Result<()> {
  let args = Args::parse();

  let db = PanoplyDatabase::default();

  let uniq_paths =
    canonicalize_path_set(args.files.iter().chain(std::iter::once(&args.main_file)))?;
  let _ = create_source_file_set(&db, uniq_paths)?;

  let wasm_module = db.emit_module_for_path(args.main_file);
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
  /*let mut file = std::fs::OpenOptions::new()
    .read(true)
    .open("./testbed/wand.wat")
    .unwrap();
  use std::io::Read;
  let mut wat = String::new();
  file.read_to_string(&mut wat).unwrap();*/

  let mut config = Config::new();
  config
    .wasm_bulk_memory(true)
    .wasm_multi_value(true)
    .wasm_multi_memory(true)
    .coredump_on_trap(true);
  let engine = Engine::new(&config).unwrap();

  #[derive(Default)]
  struct Data {
    bump_alloc: usize,
    marker: i32,
  }

  let mut store = Store::new(&engine, Data::default());

  let mut linker = Linker::new(&engine);

  let main_mem = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
  linker.define(&mut store, "main", "mem", main_mem).unwrap();

  linker
    .func_new(
      "intrinsic",
      "__mon_generate_marker",
      FuncType::new([], [ValType::I32]),
      |mut call, _args, ret| {
        let data: &mut Data = call.data_mut();
        let marker = data.marker;
        data.marker += 1;
        ret[0] = Val::I32(marker);
        Ok(())
      },
    )
    .unwrap();

  linker
    .func_new(
      "intrinsic",
      "alloc",
      FuncType::new([ValType::I32], [ValType::I32]),
      |mut call, args, ret| {
        let len: i32 = match args[0] {
          Val::I32(i) => i,
          _ => {
            return Err(wasmtime::Error::msg(
              "Expected an i32 as parameter to alloc",
            ))
          }
        };
        let data: &mut Data = call.data_mut();
        let addr = data.bump_alloc;
        data.bump_alloc += len as usize;
        ret[0] = Val::I32(addr.try_into()?);
        Ok(())
      },
    )
    .unwrap();

  linker
    .func_new(
      "intrinsic",
      "trace",
      FuncType::new([ValType::I32], [ValType::I32]),
      |_call, args, ret| {
        println!("trace: {:?}", args[0]);
        ret[0] = args[0].clone();
        Ok(())
      },
    )
    .unwrap();

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

  let mem = instance.get_memory(&mut store, "mem").unwrap();
  let mem_ref = mem.data(&store);

  println!("{:?}", &mem_ref[84..92]);
  println!("{:?}", &mem_ref[136..152]);

  Ok(())
}
