use aiahr::{canonicalize_path_set, create_source_file_set, AiahrDatabase, Args};
use aiahr_emit_wasm::Db as EmitWasmDb;
use clap::Parser;
use wasmtime::{Config, Engine, FuncType, Linker, Store, Val, ValType};

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    let db = AiahrDatabase::default();

    let uniq_paths =
        canonicalize_path_set(args.files.iter().chain(std::iter::once(&args.main_file)))?;
    let _ = create_source_file_set(&db, uniq_paths)?;

    let wasm_module = db.emit_module_for_path(args.main_file);

    let bytes = wasm_module.finish();

    let wat = wasmprinter::print_bytes(bytes).unwrap();
    println!("{}", wat);

    let engine = Engine::new(&Config::new()).unwrap();

    let module = wasmtime::Module::new(&engine, wat).unwrap();

    let mut store = Store::new(&engine, 4);

    let mut linker = Linker::new(&engine);

    linker
        .func_new(
            "intrinsic",
            "__mon_generate_marker",
            FuncType::new([], [ValType::I32]),
            |_call, _args, ret| {
                ret[0] = Val::I32(1);
                Ok(())
            },
        )
        .unwrap();

    linker
        .func_new(
            "intrinsic",
            "__mon_prompt",
            FuncType::new([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]),
            |_call, _args, ret| {
                ret[0] = Val::I32(2);
                Ok(())
            },
        )
        .unwrap();

    linker
        .func_new(
            "intrinsic",
            "__mon_bind",
            FuncType::new([ValType::I32, ValType::I32], [ValType::I32]),
            |_call, _args, ret| {
                ret[0] = Val::I32(3);
                Ok(())
            },
        )
        .unwrap();

    let instance = linker.instantiate(&mut store, &module).unwrap();

    let main = instance
        .get_typed_func::<(), i32>(&mut store, "main")
        .unwrap();

    match main.call(&mut store, ()) {
        Ok(val) => {
            println!("We did it reddit! {} {}", val, val << 8);
        }
        Err(err) => eprintln!("{}", err),
    };

    Ok(())
}
