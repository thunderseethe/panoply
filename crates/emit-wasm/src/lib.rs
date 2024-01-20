use base::{id::MedIrVarId, modules::Module};
use medir::{
  Atom, ClosureArities, Locals, MedIr, MedIrItem, MedIrItemName, MedIrKind, MedIrModule,
  MedIrTraversal, MedIrTy, MedIrTyKind,
};
use reducir::ReducIrTermName;
use rustc_hash::FxHashMap;
use wasm_encoder::{
  CodeSection, ConstExpr, ElementSection, Elements, EntityType, FuncType, Function, GlobalSection,
  GlobalType, Instruction, MemArg, MemorySection, MemoryType, NameSection, TypeSection, ValType,
};

#[salsa::jar(db = Db)]
pub struct Jar();

pub trait Db: salsa::DbWithJar<Jar> + medir::Db + lower_medir::Db {
  fn as_emit_wasm_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }

  fn emit_module(&self, module: Module) -> wasm_encoder::Module {
    let medir_module = self.lower_medir_module_of(module);
    emit_wasm_module(self.as_emit_wasm_db(), medir_module)
  }

  fn emit_module_for_path(&self, path: std::path::PathBuf) -> wasm_encoder::Module {
    let module = self.root_module_for_path(path);
    self.emit_module(module)
  }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + medir::Db + lower_medir::Db {}

struct TypeSect {
  section: TypeSection,
  indices: FxHashMap<FuncType, u32>,
  names: wasm_encoder::NameMap,
}
impl Default for TypeSect {
  fn default() -> Self {
    Self {
      section: TypeSection::new(),
      indices: FxHashMap::default(),
      names: wasm_encoder::NameMap::default(),
    }
  }
}
impl TypeSect {
  fn new(section: TypeSection) -> Self {
    Self {
      section,
      ..Default::default()
    }
  }

  fn insert_fun_ty(&mut self, ty: wasm_encoder::FuncType) -> u32 {
    let indx = self.indices.entry(ty).or_insert_with_key(|ty| {
      let indx = self.section.len();
      self.names.append(
        indx,
        &format!("fun_{}_{}", ty.params().len(), ty.results().len()),
      );
      self
        .section
        .function(ty.params().iter().copied(), ty.results().iter().copied());
      indx
    });
    *indx
  }

  fn emit_fun_ty<DB: ?Sized + crate::Db>(&mut self, db: &DB, ty: MedIrTy) -> u32 {
    let ty = try_wasm_fun_ty(db, ty).expect("Expected function type");
    self.insert_fun_ty(ty)
  }
}

fn try_wasm_fun_ty<DB: ?Sized + crate::Db>(db: &DB, ty: MedIrTy) -> Result<FuncType, MedIrTy> {
  let db = db.as_medir_db();
  match ty.kind(db) {
    MedIrTyKind::FunTy(args, _) => Ok(FuncType::new(
      args.iter().map(|_| ValType::I32),
      [ValType::I32],
    )),
    _ => Err(ty),
  }
}

fn emit_wasm_module(db: &dyn crate::Db, medir_module: MedIrModule) -> wasm_encoder::Module {
  let medir_db = db.as_medir_db();
  let mut funcs = wasm_encoder::FunctionSection::new();
  let mut types = wasm_encoder::TypeSection::new();

  let mut name_map = wasm_encoder::NameMap::new();
  let mut exports = wasm_encoder::ExportSection::new();

  let mut item_indices = FxHashMap::default();

  struct Import<'a> {
    name: MedIrItemName,
    module: &'a str,
    func: &'a str,
    params: Vec<ValType>,
    returns: Vec<ValType>,
  }

  let module = medir_module.module(medir_db);
  let alloc_indx = 1;
  let import_items = [
    Import {
      name: MedIrItemName::new(ReducIrTermName::gen(db, "__mon_generate_marker", module)),
      module: "intrinsic",
      func: "__mon_generate_marker",
      params: vec![],
      returns: vec![ValType::I32],
    },
    Import {
      name: MedIrItemName::new(ReducIrTermName::gen(db, "alloc", module)),
      module: "intrinsic",
      func: "alloc",
      params: vec![ValType::I32],
      returns: vec![ValType::I32],
    },
    Import {
      name: MedIrItemName::new(ReducIrTermName::gen(db, "trace", module)),
      module: "intrinsic",
      func: "trace",
      params: vec![ValType::I32],
      returns: vec![ValType::I32],
    },
    /*Import {
        name: MedIrItemName::new(ReducIrTermName::gen(db, "__mon_prompt", module)),
        module: "mon",
        func: "prompt",
        params: vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        returns: vec![ValType::I32],
    },
    Import {
        name: MedIrItemName::new(ReducIrTermName::gen(db, "__mon_bind", module)),
        module: "mon",
        func: "bind",
        params: vec![ValType::I32, ValType::I32, ValType::I32],
        returns: vec![ValType::I32],
    },
    Import {
        name: MedIrItemName::new(ReducIrTermName::gen(db, "__mon_eqm", module)),
        module: "mon",
        func: "eqm",
        params: vec![ValType::I32, ValType::I32],
        returns: vec![ValType::I32],
    },*/
  ];

  let mut imports = wasm_encoder::ImportSection::new();

  for (indx, import) in import_items.into_iter().enumerate() {
    let indx = indx as u32;
    types.function(import.params, import.returns);
    imports.import(import.module, import.func, EntityType::Function(indx));
    name_map.append(indx, import.func);
    item_indices.insert(import.name, indx);
  }
  let num_imports = imports.len();

  let mut closure_arities = ClosureArities::new(db);
  medir_module
    .items(db.as_medir_db())
    .iter()
    .for_each(|item| item.item(db.as_medir_db()).body.visit(&mut closure_arities));

  let mut type_sect = TypeSect::new(types);
  let mut codes = CodeSection::new();

  // Builtin mon_eqm function
  {
    let indx = funcs.len();
    let ty_indx = type_sect.insert_fun_ty(fun_n_i32s(2));
    funcs.function(ty_indx);
    let name_str = "__mon_eqm";
    let name = MedIrItemName::new(ReducIrTermName::gen(
      db,
      name_str,
      medir_module.module(db.as_medir_db()),
    ));
    name_map.append(indx + num_imports, name_str);
    item_indices.insert(name, indx + num_imports);

    let alloc = MedIrItemName::new(ReducIrTermName::gen(
      db,
      "alloc",
      medir_module.module(db.as_medir_db()),
    ));
    let ins = [
      Instruction::I32Const(4),
      Instruction::Call(item_indices[&alloc]),
      Instruction::LocalTee(2),
      Instruction::I32Const(1),
      Instruction::I32Const(0),
      Instruction::LocalGet(0),
      Instruction::LocalGet(1),
      Instruction::I32Eq,
      Instruction::Select,
      Instruction::I32Store(default_memarg(0)),
      Instruction::LocalGet(2),
      Instruction::Return,
      Instruction::End,
    ];
    let mut f = Function::new([(1, ValType::I32)]);
    ins.iter().for_each(|ins| {
      f.instruction(ins);
    });
    codes.function(&f);
  }

  for pap in closure_arities.into_arities() {
    let indx = funcs.len();
    let num_params = pap.arity - pap.provided_args;
    let apply_ty_indx = type_sect.insert_fun_ty(fun_n_i32s(num_params + 1));
    funcs.function(apply_ty_indx);
    let name = format!("__apply_{}_{}", pap.arity, pap.provided_args);
    let name = MedIrItemName::new(ReducIrTermName::gen(
      db,
      name,
      medir_module.module(db.as_medir_db()),
    ));
    name_map.append(indx + num_imports, name.0.name(db).text(db.as_core_db()));
    item_indices.insert(name, indx + num_imports);

    let mut f = Function::new([]);
    for i in (0u64..pap.provided_args.try_into().unwrap()).map(|i| i + 1) {
      f.instruction(&Instruction::LocalGet(0));
      f.instruction(&Instruction::I32Load(default_memarg(i)));
    }
    for i in (0u32..num_params.try_into().unwrap()).map(|i| i + 1) {
      f.instruction(&Instruction::LocalGet(i));
    }
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load(default_memarg(0)));
    let ty = type_sect.insert_fun_ty(fun_n_i32s(pap.arity));
    f.instruction(&Instruction::CallIndirect { ty, table: 0 });
    f.instruction(&Instruction::End);
    codes.function(&f);
  }

  for item in medir_module.items(medir_db).iter() {
    let defn = item.item(medir_db);
    let ty_indx = type_sect.emit_fun_ty(db, defn.type_of(db));
    let indx = funcs.len();
    funcs.function(ty_indx);
    let name = item.name(medir_db);
    let text = name.0.name(db).text(db.as_core_db());
    // TODO: Entry point detection. Fix this hack
    if text == "main" {
      exports.export(text, wasm_encoder::ExportKind::Func, indx + num_imports);
    }
    name_map.append(indx + num_imports, text);
    item_indices.insert(name, indx + num_imports);
  }

  for item in medir_module.items(medir_db).iter() {
    let f = emit_wasm_item(db, item, &item_indices, &mut type_sect, alloc_indx);
    codes.function(&f);
  }

  let mut names = NameSection::new();
  let core_db = db.as_core_db();
  names.module(module.name(core_db).text(core_db));
  names.functions(&name_map);
  names.types(&type_sect.names);

  let mut globals = GlobalSection::new();
  globals.global(
    GlobalType {
      val_type: ValType::I32,
      mutable: true,
    },
    &ConstExpr::i32_const(0),
  );

  let mut memory = MemorySection::new();
  memory.memory(MemoryType {
    minimum: 1,
    maximum: None,
    memory64: false,
    shared: false,
  });
  exports.export("mem", wasm_encoder::ExportKind::Memory, 0);

  let func_len = (0..item_indices.len())
    .map(|i| i.try_into().unwrap())
    .collect::<Vec<u32>>();

  let mut tables = wasm_encoder::TableSection::new();
  tables.table(wasm_encoder::TableType {
    element_type: wasm_encoder::RefType::FUNCREF,
    minimum: func_len.len().try_into().unwrap(),
    maximum: Some(func_len.len().try_into().unwrap()),
  });

  let mut elems = ElementSection::new();
  elems.active(
    Some(0),
    &ConstExpr::i32_const(0),
    Elements::Functions(&func_len),
  );

  let mut module = wasm_encoder::Module::new();
  module.section(&type_sect.section);
  module.section(&imports);
  module.section(&funcs);
  module.section(&tables);
  module.section(&memory);
  module.section(&globals);
  module.section(&exports);
  module.section(&elems);
  module.section(&codes);
  module.section(&names);
  module
}

fn fun_n_i32s(n: usize) -> FuncType {
  FuncType::new(std::iter::repeat(ValType::I32).take(n), [ValType::I32])
}

fn default_memarg(offset_in_i32s: u64) -> MemArg {
  MemArg {
    offset: offset_in_i32s * 4,
    align: 2,
    memory_index: 0,
  }
}

fn emit_wasm_item(
  db: &dyn crate::Db,
  opt_item: &MedIrItem,
  item_indices: &FxHashMap<MedIrItemName, u32>,
  type_sect: &mut TypeSect,
  alloc_fn: u32,
) -> wasm_encoder::Function {
  struct InstrEmitter<'a, 'i> {
    db: &'a dyn crate::Db,
    module: Module,
    ins: Vec<Instruction<'i>>,
    locals: FxHashMap<MedIrVarId, u32>,
    item_indices: &'a FxHashMap<MedIrItemName, u32>,
    type_sect: &'a mut TypeSect,
    alloc_fn: u32,
  }
  impl<'i> InstrEmitter<'_, 'i> {
    fn ins(&mut self, ins: Instruction<'i>) {
      self.ins.push(ins);
    }

    fn inss(&mut self, inss: impl IntoIterator<Item = Instruction<'i>>) {
      self.ins.extend(inss);
    }

    fn emit_atom(&self, atom: &Atom) -> Instruction<'i> {
      match atom {
        Atom::Var(v) => {
          let local = self
            .locals
            .get(&v.id)
            .expect("Local should have been set before it was got");
          Instruction::LocalGet(*local)
        }
        Atom::Int(i) => Instruction::I32Const((*i).try_into().unwrap()),
      }
    }

    fn emit(&mut self, medir: &MedIr) {
      match &medir.kind {
        MedIrKind::Atom(atom) => self.ins(self.emit_atom(atom)),
        MedIrKind::Blocks(elems) => {
          let addr = self.locals.len() as u32;
          let alloc_len: i32 = elems.len().try_into().unwrap();
          self.inss([
            Instruction::I32Const(alloc_len * 4),
            Instruction::Call(self.alloc_fn),
            Instruction::LocalSet(addr),
          ]);
          for (i, elem) in elems.iter().enumerate() {
            self.inss([
              Instruction::LocalGet(addr),
              self.emit_atom(elem),
              Instruction::I32Store(default_memarg(i.try_into().unwrap())),
            ]);
          }
          // Leave a copy of the start of blocks on the stack after everything
          // is done.
          self.ins(Instruction::LocalGet(addr));
        }
        MedIrKind::BlockAccess(var, indx) => self.inss([
          self.emit_atom(&Atom::Var(*var)),
          Instruction::I32Load(default_memarg((*indx).try_into().unwrap())),
        ]),
        MedIrKind::Switch(scrutinee, branches) => {
          let end: u32 = branches.len().try_into().unwrap();
          self.ins(Instruction::Block(wasm_encoder::BlockType::Result(
            ValType::I32,
          )));
          self.inss((0..=end).map(|_| Instruction::Block(wasm_encoder::BlockType::Empty)));
          let atom = self.emit_atom(scrutinee);
          self.inss([
            atom,
            Instruction::BrTable((0u32..end).collect(), end),
            Instruction::End,
          ]);
          for (i, branch) in branches.iter().enumerate() {
            let i: u32 = i.try_into().unwrap();
            self.emit_locals(branch);
            // Only emit the break if this isn't the outermost block
            self.inss([Instruction::Br(end - i), Instruction::End]);
          }
          self.inss([Instruction::Unreachable, Instruction::End]);
        }
        MedIrKind::Closure(item, env) => {
          let fun_ty =
            try_wasm_fun_ty(self.db, item.ty).expect("Closure has to have function type");
          let arity = fun_ty.params().len();
          let apply_name_str = format!("__apply_{}_{}", arity, env.len());
          let apply_name =
            MedIrItemName::new(ReducIrTermName::gen(self.db, &apply_name_str, self.module));
          let apply_indx = self
            .item_indices
            .get(&apply_name)
            .unwrap_or_else(|| panic!("__apply_n_m indices not found for {}", apply_name_str));
          let fn_indx = self.item_indices.get(&item.name).unwrap_or_else(|| {
            panic!(
              "Item indices not found for {}",
              item.name.0.name(self.db).text(self.db.as_core_db())
            )
          });

          let addr = self.locals.len() as u32;
          let env_len: i32 = env.len().try_into().unwrap();
          self.inss([
            Instruction::I32Const((env_len + 2) * 4),
            Instruction::Call(self.alloc_fn),
            Instruction::LocalSet(addr),
          ]);

          self.inss([
            // Store apply_n indx in slot 0
            Instruction::LocalGet(addr),
            Instruction::I32Const((*apply_indx).try_into().unwrap()),
            Instruction::I32Store(default_memarg(0)),
            // Store fn indx in slot 1
            Instruction::LocalGet(addr),
            Instruction::I32Const((*fn_indx).try_into().unwrap()),
            Instruction::I32Store(default_memarg(1)),
          ]);

          // Store each env capture in slot 2..n
          for (i, capt) in env.iter().enumerate() {
            let i: u64 = i.try_into().unwrap();
            self.inss([
              Instruction::LocalGet(addr),
              self.emit_atom(&Atom::Var(*capt)),
              Instruction::I32Store(default_memarg(i + 2)),
            ]);
          }
          // Leave
          self.ins(Instruction::LocalGet(addr));
        }
        MedIrKind::Call(fun, args) => match fun {
          medir::Call::Known(item) => {
            for arg in args.iter() {
              self.ins(self.emit_atom(arg));
            }
            let indx = self.item_indices.get(&item.name).unwrap_or_else(|| {
              panic!(
                "Item indice not found for {} {:?}",
                item.name.0.name(self.db).text(self.db.as_core_db()),
                item.name
              )
            });
            self.ins(Instruction::Call(*indx))
          }
          medir::Call::Unknown(v) => {
            let get_local = self.emit_atom(&Atom::Var(*v));
            self.inss([
              get_local.clone(),
              Instruction::I32Const(4),
              Instruction::I32Add,
            ]);
            for arg in args.iter() {
              self.ins(self.emit_atom(arg));
            }
            let fun_ty_indx = self.type_sect.insert_fun_ty(fun_n_i32s(args.len() + 1));
            self.inss([
              get_local.clone(),
              Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
              }),
              Instruction::CallIndirect {
                ty: fun_ty_indx,
                table: 0,
              },
            ]);
          }
        },
        MedIrKind::Typecast(_, term) => self.emit(term),
      }
    }

    fn emit_locals(&mut self, locals: &Locals) {
      for (var, defn) in locals.binds.iter() {
        self.emit(defn);
        let local_len = self.locals.len().try_into().unwrap();
        let local = *self.locals.entry(var.id).or_insert(local_len);
        self.ins(Instruction::LocalSet(local));
      }
      self.emit(&locals.body);
    }
  }

  let defn = opt_item.item(db.as_medir_db());
  let locals = defn
    .params
    .iter()
    .enumerate()
    .map(|(i, var)| (var.id, i.try_into().unwrap()))
    .collect();
  let mut emitter = InstrEmitter {
    db,
    module: opt_item.name(db.as_medir_db()).0.module(db),
    ins: vec![],
    locals,
    item_indices,
    type_sect,
    // TODO: Replace magic number with an actual lookup of the alloc function index
    // This is annoying because we can't create alloc name here because we don't have module
    alloc_fn,
  };

  emitter.emit_locals(&defn.body);
  emitter.ins(Instruction::Return);
  emitter.ins(Instruction::End);

  let num_locals = emitter.locals.len() - defn.params.len() + 1;

  let mut f = Function::new([(num_locals.try_into().unwrap(), ValType::I32)]);
  for ins in emitter.ins.iter() {
    f.instruction(ins);
  }
  f
}

#[cfg(test)]
mod tests {
  use base::file::{FileId, SourceFile, SourceFileSet};
  use expect_test::expect;
  use wasmparser::Validator;

  use crate::Db;

  #[derive(Default)]
  #[salsa::db(
    crate::Jar,
    ast::Jar,
    base::Jar,
    desugar::Jar,
    lower_medir::Jar,
    lower_reducir::Jar,
    medir::Jar,
    nameres::Jar,
    optimize_reducir::Jar,
    parser::Jar,
    reducir::Jar,
    tc::Jar,
    ty::Jar
  )]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  impl salsa::Database for TestDatabase {}

  fn emit_module(db: &TestDatabase, input: &str) -> wasm_encoder::Module {
    let path = std::path::PathBuf::from("test");
    let mut contents = r#"
effect State {
    put : Int -> {},
    get : {} -> Int
}

effect Reader {
    ask : {} -> {}
}

"#
    .to_string();
    contents.push_str(input);
    let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
    SourceFileSet::new(db, vec![file]);

    db.emit_module_for_path(path)
  }

  #[test]
  fn test_prj() {
    let db = TestDatabase::default();

    let wasm_module = emit_module(&db, "f = { x = 5678, y = 1234 }.x");

    let bytes = wasm_module.finish();
    let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
      function_references: true,
      ..Default::default()
    });
    let validate_res = validator.validate_all(&bytes);
    let string = wasmprinter::print_bytes(bytes).unwrap();
    let expect = expect![[r#"
        (module $test
          (type (;0;) (func (result i32)))
          (type (;1;) (func (param i32) (result i32)))
          (type (;2;) (func (param i32) (result i32)))
          (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
          (type $fun_3_1 (;4;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;5;) (func (param i32 i32 i32 i32) (result i32)))
          (type $fun_1_1 (;6;) (func (param i32) (result i32)))
          (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
          (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
          (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
          (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 4
            call $alloc
            local.tee 2
            i32.const 1
            i32.const 0
            local.get 0
            local.get 1
            i32.eq
            select
            i32.store
            local.get 2
            return
          )
          (func $__apply_3_2 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_4_3 (;5;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 0
            i32.load offset=12
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_4_1)
          )
          (func $f (;6;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            i32.const 5678
            i32.store offset=4
            local.get 1
            return
          )
          (func $__mon_bind (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 3
            i32.load
            local.set 4
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 4
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 3
                  i32.load offset=4
                  local.set 5
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 5
                  local.get 2
                  local.get 1
                  i32.load
                  call_indirect (type $fun_3_1)
                  br 2 (;@1;)
                end
                local.get 3
                i32.load offset=4
                local.set 6
                local.get 6
                local.set 7
                local.get 7
                i32.load
                local.set 8
                local.get 7
                i32.load offset=4
                local.set 9
                i32.const 16
                call $alloc
                local.set 10
                local.get 10
                i32.const 4
                i32.store
                local.get 10
                i32.const 8
                i32.store offset=4
                local.get 10
                local.get 1
                i32.store offset=8
                local.get 10
                local.get 7
                i32.store offset=12
                local.get 10
                local.set 10
                i32.const 12
                call $alloc
                local.set 11
                local.get 11
                local.get 8
                i32.store
                local.get 11
                local.get 9
                i32.store offset=4
                local.get 11
                local.get 10
                i32.store offset=8
                local.get 11
                local.set 11
                i32.const 8
                call $alloc
                local.set 12
                local.get 12
                i32.const 1
                i32.store
                local.get 12
                local.get 11
                i32.store offset=4
                local.get 12
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_bind_lam_0 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 1
            i32.load offset=8
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 4
            i32.store
            local.get 5
            i32.const 7
            i32.store offset=4
            local.get 5
            local.get 3
            i32.store offset=8
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_prompt (;9;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            local.get 2
            i32.const 4
            i32.add
            local.get 4
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            local.get 5
            i32.load
            local.set 6
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 6
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 5
                  i32.load offset=4
                  local.set 7
                  i32.const 8
                  call $alloc
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.store
                  local.get 8
                  local.get 7
                  i32.store offset=4
                  local.get 8
                  br 2 (;@1;)
                end
                local.get 5
                i32.load offset=4
                local.set 8
                local.get 8
                i32.load
                local.set 9
                local.get 0
                local.get 9
                call $__mon_eqm
                local.set 10
                local.get 10
                i32.load
                local.set 11
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 11
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 10
                      i32.load offset=4
                      local.set 12
                      local.get 8
                      i32.load
                      local.set 13
                      local.get 8
                      i32.load offset=4
                      local.set 14
                      i32.const 20
                      call $alloc
                      local.set 15
                      local.get 15
                      i32.const 5
                      i32.store
                      local.get 15
                      i32.const 10
                      i32.store offset=4
                      local.get 15
                      local.get 0
                      i32.store offset=8
                      local.get 15
                      local.get 1
                      i32.store offset=12
                      local.get 15
                      local.get 8
                      i32.store offset=16
                      local.get 15
                      local.set 15
                      local.get 8
                      i32.load offset=8
                      local.set 16
                      i32.const 16
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 13
                      i32.store
                      local.get 17
                      local.get 14
                      i32.store offset=4
                      local.get 17
                      local.get 15
                      i32.store offset=8
                      local.get 17
                      local.get 16
                      i32.store offset=12
                      local.get 17
                      local.set 17
                      i32.const 8
                      call $alloc
                      local.set 18
                      local.get 18
                      i32.const 1
                      i32.store
                      local.get 18
                      local.get 17
                      i32.store offset=4
                      local.get 18
                      br 2 (;@3;)
                    end
                    local.get 10
                    i32.load offset=4
                    local.set 12
                    i32.const 20
                    call $alloc
                    local.set 18
                    local.get 18
                    i32.const 5
                    i32.store
                    local.get 18
                    i32.const 11
                    i32.store offset=4
                    local.get 18
                    local.get 0
                    i32.store offset=8
                    local.get 18
                    local.get 1
                    i32.store offset=12
                    local.get 18
                    local.get 8
                    i32.store offset=16
                    local.get 18
                    local.set 18
                    local.get 8
                    i32.load offset=4
                    local.set 19
                    local.get 19
                    i32.const 4
                    i32.add
                    local.get 18
                    local.get 3
                    local.get 19
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_prompt_lam_0 (;10;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 5
            i32.store
            local.get 6
            i32.const 9
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (func $__mon_prompt_lam_1 (;11;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 5
            i32.store
            local.get 6
            i32.const 9
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (table (;0;) 12 12 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_3_2 $__apply_4_3 $f $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
        )"#]];
    expect.assert_eq(&string);

    validate_res.expect("Validation Failed");
  }

  #[test]
  fn test_wand() {
    let db = TestDatabase::default();

    let wasm_module = emit_module(
      &db,
      r#"f = |m||n| (m ,, n).x

g = f({ x = {} })({ y = {} })
"#,
    );

    let bytes = wasm_module.finish();
    let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
      function_references: true,
      ..Default::default()
    });
    let validate_res = validator.validate_all(&bytes);
    let string = wasmprinter::print_bytes(bytes).unwrap();
    let expect = expect![[r#"
        (module $test
          (type (;0;) (func (result i32)))
          (type (;1;) (func (param i32) (result i32)))
          (type (;2;) (func (param i32) (result i32)))
          (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
          (type $fun_1_1 (;4;) (func (param i32) (result i32)))
          (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
          (type $fun_5_1 (;7;) (func (param i32 i32 i32 i32 i32) (result i32)))
          (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
          (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
          (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
          (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 4
            call $alloc
            local.tee 2
            i32.const 1
            i32.const 0
            local.get 0
            local.get 1
            i32.eq
            select
            i32.store
            local.get 2
            return
          )
          (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_1_1)
          )
          (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_3_0 (;6;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 3
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_3_2 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_4_3 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 0
            i32.load offset=12
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_4_1)
          )
          (func $f (;9;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.load
            local.set 5
            local.get 5
            i32.const 4
            i32.add
            local.get 2
            local.get 3
            local.get 5
            i32.load
            call_indirect (type $fun_3_1)
            local.set 6
            local.get 1
            i32.load offset=12
            local.set 7
            local.get 7
            i32.load
            local.set 8
            local.get 8
            i32.const 4
            i32.add
            local.get 6
            local.get 8
            i32.load
            call_indirect (type $fun_2_1)
            local.set 9
            i32.const 8
            call $alloc
            local.set 10
            local.get 10
            i32.const 0
            i32.store
            local.get 10
            local.get 9
            i32.store offset=4
            local.get 10
            return
          )
          (func $g (;10;) (type $fun_1_1) (param i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 5
            i32.store
            local.get 1
            i32.const 11
            i32.store offset=4
            local.get 1
            local.set 1
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 6
            i32.store
            local.get 2
            i32.const 12
            i32.store offset=4
            local.get 2
            local.set 2
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 4
            i32.store
            local.get 3
            i32.const 13
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 4
            i32.store
            local.get 4
            i32.const 14
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            local.get 3
            i32.store
            local.get 5
            local.get 4
            i32.store offset=4
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            i32.const 4
            i32.store
            local.get 6
            i32.const 15
            i32.store offset=4
            local.get 6
            local.set 6
            i32.const 8
            call $alloc
            local.set 7
            local.get 7
            i32.const 4
            i32.store
            local.get 7
            i32.const 16
            i32.store offset=4
            local.get 7
            local.set 7
            i32.const 8
            call $alloc
            local.set 8
            local.get 8
            local.get 6
            i32.store
            local.get 8
            local.get 7
            i32.store offset=4
            local.get 8
            local.set 8
            i32.const 16
            call $alloc
            local.set 9
            local.get 9
            local.get 1
            i32.store
            local.get 9
            local.get 2
            i32.store offset=4
            local.get 9
            local.get 5
            i32.store offset=8
            local.get 9
            local.get 8
            i32.store offset=12
            local.get 9
            local.set 9
            i32.const 8
            call $alloc
            local.set 10
            local.get 10
            i32.const 5
            i32.store
            local.get 10
            i32.const 17
            i32.store offset=4
            local.get 10
            local.set 10
            i32.const 8
            call $alloc
            local.set 11
            local.get 11
            i32.const 6
            i32.store
            local.get 11
            i32.const 18
            i32.store offset=4
            local.get 11
            local.set 11
            i32.const 8
            call $alloc
            local.set 12
            local.get 12
            i32.const 4
            i32.store
            local.get 12
            i32.const 19
            i32.store offset=4
            local.get 12
            local.set 12
            i32.const 8
            call $alloc
            local.set 13
            local.get 13
            i32.const 4
            i32.store
            local.get 13
            i32.const 20
            i32.store offset=4
            local.get 13
            local.set 13
            i32.const 8
            call $alloc
            local.set 14
            local.get 14
            local.get 12
            i32.store
            local.get 14
            local.get 13
            i32.store offset=4
            local.get 14
            local.set 14
            i32.const 8
            call $alloc
            local.set 15
            local.get 15
            i32.const 4
            i32.store
            local.get 15
            i32.const 21
            i32.store offset=4
            local.get 15
            local.set 15
            i32.const 8
            call $alloc
            local.set 16
            local.get 16
            i32.const 4
            i32.store
            local.get 16
            i32.const 22
            i32.store offset=4
            local.get 16
            local.set 16
            i32.const 8
            call $alloc
            local.set 17
            local.get 17
            local.get 15
            i32.store
            local.get 17
            local.get 16
            i32.store offset=4
            local.get 17
            local.set 17
            i32.const 16
            call $alloc
            local.set 18
            local.get 18
            local.get 10
            i32.store
            local.get 18
            local.get 11
            i32.store offset=4
            local.get 18
            local.get 14
            i32.store offset=8
            local.get 18
            local.get 17
            i32.store offset=12
            local.get 18
            local.set 18
            i32.const 0
            call $alloc
            local.set 19
            local.get 19
            local.set 19
            i32.const 0
            call $alloc
            local.set 20
            local.get 20
            local.set 20
            local.get 9
            local.get 18
            local.get 19
            local.get 20
            local.get 0
            call $f
            return
          )
          (func $g_lam_0 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 0
            i32.store
            local.get 2
            local.get 1
            i32.store offset=4
            local.get 2
            return
          )
          (func $g_lam_1 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load
            local.set 3
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 3
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 2
                  i32.load offset=4
                  local.set 4
                  local.get 0
                  i32.const 4
                  i32.add
                  local.get 4
                  local.get 0
                  i32.load
                  call_indirect (type $fun_2_1)
                  br 2 (;@1;)
                end
                local.get 2
                i32.load offset=4
                local.set 4
                local.get 1
                i32.const 4
                i32.add
                local.get 4
                local.get 1
                i32.load
                call_indirect (type $fun_2_1)
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $g_lam_2 (;13;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load
            return
          )
          (func $g_lam_3 (;14;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $g_lam_4 (;15;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load offset=4
            return
          )
          (func $g_lam_5 (;16;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 1
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $g_lam_6 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 1
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $g_lam_7 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load
            local.set 3
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 3
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 2
                  i32.load offset=4
                  local.set 4
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 4
                  local.get 1
                  i32.load
                  call_indirect (type $fun_2_1)
                  br 2 (;@1;)
                end
                local.get 2
                i32.load offset=4
                local.set 4
                local.get 0
                i32.const 4
                i32.add
                local.get 4
                local.get 0
                i32.load
                call_indirect (type $fun_2_1)
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $g_lam_8 (;19;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load offset=4
            return
          )
          (func $g_lam_9 (;20;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 1
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $g_lam_10 (;21;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load
            return
          )
          (func $g_lam_11 (;22;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $__mon_bind (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 3
            i32.load
            local.set 4
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 4
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 3
                  i32.load offset=4
                  local.set 5
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 5
                  local.get 2
                  local.get 1
                  i32.load
                  call_indirect (type $fun_3_1)
                  br 2 (;@1;)
                end
                local.get 3
                i32.load offset=4
                local.set 6
                local.get 6
                local.set 7
                local.get 7
                i32.load
                local.set 8
                local.get 7
                i32.load offset=4
                local.set 9
                i32.const 16
                call $alloc
                local.set 10
                local.get 10
                i32.const 7
                i32.store
                local.get 10
                i32.const 24
                i32.store offset=4
                local.get 10
                local.get 1
                i32.store offset=8
                local.get 10
                local.get 7
                i32.store offset=12
                local.get 10
                local.set 10
                i32.const 12
                call $alloc
                local.set 11
                local.get 11
                local.get 8
                i32.store
                local.get 11
                local.get 9
                i32.store offset=4
                local.get 11
                local.get 10
                i32.store offset=8
                local.get 11
                local.set 11
                i32.const 8
                call $alloc
                local.set 12
                local.get 12
                i32.const 1
                i32.store
                local.get 12
                local.get 11
                i32.store offset=4
                local.get 12
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_bind_lam_0 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 1
            i32.load offset=8
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 7
            i32.store
            local.get 5
            i32.const 23
            i32.store offset=4
            local.get 5
            local.get 3
            i32.store offset=8
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_prompt (;25;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            local.get 2
            i32.const 4
            i32.add
            local.get 4
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            local.get 5
            i32.load
            local.set 6
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 6
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 5
                  i32.load offset=4
                  local.set 7
                  i32.const 8
                  call $alloc
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.store
                  local.get 8
                  local.get 7
                  i32.store offset=4
                  local.get 8
                  br 2 (;@1;)
                end
                local.get 5
                i32.load offset=4
                local.set 8
                local.get 8
                i32.load
                local.set 9
                local.get 0
                local.get 9
                call $__mon_eqm
                local.set 10
                local.get 10
                i32.load
                local.set 11
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 11
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 10
                      i32.load offset=4
                      local.set 12
                      local.get 8
                      i32.load
                      local.set 13
                      local.get 8
                      i32.load offset=4
                      local.set 14
                      i32.const 20
                      call $alloc
                      local.set 15
                      local.get 15
                      i32.const 8
                      i32.store
                      local.get 15
                      i32.const 26
                      i32.store offset=4
                      local.get 15
                      local.get 0
                      i32.store offset=8
                      local.get 15
                      local.get 1
                      i32.store offset=12
                      local.get 15
                      local.get 8
                      i32.store offset=16
                      local.get 15
                      local.set 15
                      local.get 8
                      i32.load offset=8
                      local.set 16
                      i32.const 16
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 13
                      i32.store
                      local.get 17
                      local.get 14
                      i32.store offset=4
                      local.get 17
                      local.get 15
                      i32.store offset=8
                      local.get 17
                      local.get 16
                      i32.store offset=12
                      local.get 17
                      local.set 17
                      i32.const 8
                      call $alloc
                      local.set 18
                      local.get 18
                      i32.const 1
                      i32.store
                      local.get 18
                      local.get 17
                      i32.store offset=4
                      local.get 18
                      br 2 (;@3;)
                    end
                    local.get 10
                    i32.load offset=4
                    local.set 12
                    i32.const 20
                    call $alloc
                    local.set 18
                    local.get 18
                    i32.const 8
                    i32.store
                    local.get 18
                    i32.const 27
                    i32.store offset=4
                    local.get 18
                    local.get 0
                    i32.store offset=8
                    local.get 18
                    local.get 1
                    i32.store offset=12
                    local.get 18
                    local.get 8
                    i32.store offset=16
                    local.get 18
                    local.set 18
                    local.get 8
                    i32.load offset=4
                    local.set 19
                    local.get 19
                    i32.const 4
                    i32.add
                    local.get 18
                    local.get 3
                    local.get 19
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_prompt_lam_0 (;26;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 8
            i32.store
            local.get 6
            i32.const 25
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (func $__mon_prompt_lam_1 (;27;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 8
            i32.store
            local.get 6
            i32.const 25
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (table (;0;) 28 28 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_3_0 $__apply_3_2 $__apply_4_3 $f $g $g_lam_0 $g_lam_1 $g_lam_2 $g_lam_3 $g_lam_4 $g_lam_5 $g_lam_6 $g_lam_7 $g_lam_8 $g_lam_9 $g_lam_10 $g_lam_11 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
        )"#]];
    expect.assert_eq(&string);

    validate_res.expect("Validation Failed");
  }

  #[test]
  fn test_simple_get() {
    let db = TestDatabase::default();

    let wasm_module = emit_module(
      &db,
      r#"
f = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(825)"#,
    );

    let bytes = wasm_module.finish();
    /*let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
        function_references: true,
        ..Default::default()
    });
    let validate_res = validator.validate_all(&bytes);*/
    let string = wasmprinter::print_bytes(bytes).unwrap();
    let expect = expect![[r#"
        (module $test
          (type (;0;) (func (result i32)))
          (type (;1;) (func (param i32) (result i32)))
          (type (;2;) (func (param i32) (result i32)))
          (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
          (type $fun_1_1 (;4;) (func (param i32) (result i32)))
          (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
          (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
          (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
          (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
          (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 4
            call $alloc
            local.tee 2
            i32.const 1
            i32.const 0
            local.get 0
            local.get 1
            i32.eq
            select
            i32.store
            local.get 2
            return
          )
          (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_1_1)
          )
          (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_2_1 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_3_0 (;7;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 3
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_3_2 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_4_3 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 0
            i32.load offset=12
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_4_1)
          )
          (func $f (;10;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            i32.const 0
            call $alloc
            local.set 2
            local.get 2
            local.set 2
            local.get 2
            call $__mon_generate_marker
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 7
            i32.store
            local.get 4
            i32.const 11
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            i32.const 7
            i32.store
            local.get 5
            i32.const 12
            i32.store offset=4
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            local.get 4
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            local.set 6
            i32.const 8
            call $alloc
            local.set 7
            local.get 7
            local.get 3
            i32.store
            local.get 7
            local.get 6
            i32.store offset=4
            local.get 7
            local.set 7
            local.get 0
            i32.load
            local.set 8
            local.get 8
            i32.const 4
            i32.add
            local.get 1
            local.get 7
            local.get 8
            i32.load
            call_indirect (type $fun_3_1)
            local.set 1
            local.get 0
            i32.load offset=12
            local.set 9
            local.get 9
            i32.load
            local.set 10
            local.get 10
            i32.const 4
            i32.add
            local.get 1
            local.get 10
            i32.load
            call_indirect (type $fun_2_1)
            local.set 11
            i32.const 12
            call $alloc
            local.set 12
            local.get 12
            i32.const 6
            i32.store
            local.get 12
            i32.const 15
            i32.store offset=4
            local.get 12
            local.get 11
            i32.store offset=8
            local.get 12
            local.set 12
            i32.const 8
            call $alloc
            local.set 13
            local.get 13
            i32.const 4
            i32.store
            local.get 13
            i32.const 18
            i32.store offset=4
            local.get 13
            local.set 13
            local.get 12
            local.get 13
            local.get 1
            call $__mon_bind
            local.set 14
            local.get 14
            i32.load
            local.set 15
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 15
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 14
                  i32.load offset=4
                  local.set 16
                  i32.const 8
                  call $alloc
                  local.set 17
                  local.get 17
                  i32.const 0
                  i32.store
                  local.get 17
                  local.get 16
                  i32.store offset=4
                  local.get 17
                  br 2 (;@1;)
                end
                local.get 14
                i32.load offset=4
                local.set 17
                local.get 17
                i32.load
                local.set 18
                local.get 3
                local.get 18
                call $__mon_eqm
                local.set 19
                local.get 19
                i32.load
                local.set 20
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 20
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 19
                      i32.load offset=4
                      local.set 21
                      local.get 17
                      i32.load
                      local.set 22
                      local.get 17
                      i32.load offset=4
                      local.set 23
                      i32.const 20
                      call $alloc
                      local.set 24
                      local.get 24
                      i32.const 9
                      i32.store
                      local.get 24
                      i32.const 22
                      i32.store offset=4
                      local.get 24
                      local.get 0
                      i32.store offset=8
                      local.get 24
                      local.get 3
                      i32.store offset=12
                      local.get 24
                      local.get 17
                      i32.store offset=16
                      local.get 24
                      local.set 24
                      local.get 17
                      i32.load offset=8
                      local.set 25
                      i32.const 16
                      call $alloc
                      local.set 26
                      local.get 26
                      local.get 22
                      i32.store
                      local.get 26
                      local.get 23
                      i32.store offset=4
                      local.get 26
                      local.get 24
                      i32.store offset=8
                      local.get 26
                      local.get 25
                      i32.store offset=12
                      local.get 26
                      local.set 26
                      i32.const 8
                      call $alloc
                      local.set 27
                      local.get 27
                      i32.const 1
                      i32.store
                      local.get 27
                      local.get 26
                      i32.store offset=4
                      local.get 27
                      br 2 (;@3;)
                    end
                    local.get 19
                    i32.load offset=4
                    local.set 21
                    i32.const 20
                    call $alloc
                    local.set 27
                    local.get 27
                    i32.const 9
                    i32.store
                    local.get 27
                    i32.const 26
                    i32.store offset=4
                    local.get 27
                    local.get 0
                    i32.store offset=8
                    local.get 27
                    local.get 3
                    i32.store offset=12
                    local.get 27
                    local.get 17
                    i32.store offset=16
                    local.get 27
                    local.set 27
                    local.get 17
                    i32.load offset=4
                    local.set 28
                    local.get 28
                    i32.const 4
                    i32.add
                    local.get 27
                    local.get 1
                    local.get 28
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            local.set 29
            local.get 29
            i32.load
            local.set 30
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 30
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 29
                  i32.load offset=4
                  local.set 31
                  local.get 31
                  i32.const 4
                  i32.add
                  i32.const 825
                  local.get 31
                  i32.load
                  call_indirect (type $fun_2_1)
                  local.set 32
                  i32.const 8
                  call $alloc
                  local.set 33
                  local.get 33
                  i32.const 0
                  i32.store
                  local.get 33
                  local.get 32
                  i32.store offset=4
                  local.get 33
                  br 2 (;@1;)
                end
                local.get 29
                i32.load offset=4
                local.set 33
                local.get 33
                local.set 34
                local.get 34
                i32.load
                local.set 35
                local.get 34
                i32.load offset=4
                local.set 36
                i32.const 12
                call $alloc
                local.set 37
                local.get 37
                i32.const 6
                i32.store
                local.get 37
                i32.const 28
                i32.store offset=4
                local.get 37
                local.get 34
                i32.store offset=8
                local.get 37
                local.set 37
                i32.const 12
                call $alloc
                local.set 38
                local.get 38
                local.get 35
                i32.store
                local.get 38
                local.get 36
                i32.store offset=4
                local.get 38
                local.get 37
                i32.store offset=8
                local.get 38
                local.set 38
                i32.const 8
                call $alloc
                local.set 39
                local.get 39
                i32.const 1
                i32.store
                local.get 39
                local.get 38
                i32.store offset=4
                local.get 39
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $f_lam_0 (;11;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32)
            i32.const 0
            call $alloc
            local.set 3
            local.get 3
            local.set 3
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 0
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_1 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 2
            local.get 2
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_2 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 0
            call $alloc
            local.set 2
            local.get 2
            local.set 2
            local.get 0
            i32.load offset=4
            local.set 3
            local.get 3
            i32.load offset=4
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 2
            local.get 1
            local.get 4
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_3 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 0
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_4 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32 i32)
            local.get 0
            i32.load
            local.set 2
            i32.const 12
            call $alloc
            local.set 3
            local.get 3
            i32.const 6
            i32.store
            local.get 3
            i32.const 13
            i32.store offset=4
            local.get 3
            local.get 0
            i32.store offset=8
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 5
            i32.store
            local.get 4
            i32.const 14
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 12
            call $alloc
            local.set 5
            local.get 5
            local.get 2
            i32.store
            local.get 5
            local.get 3
            i32.store offset=4
            local.get 5
            local.get 4
            i32.store offset=8
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            i32.const 1
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            return
          )
          (func $f_lam_5 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 1
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_6 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32)
            i32.const 12
            call $alloc
            local.set 2
            local.get 2
            i32.const 6
            i32.store
            local.get 2
            i32.const 16
            i32.store offset=4
            local.get 2
            local.get 0
            i32.store offset=8
            local.get 2
            local.set 2
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 0
            i32.store
            local.get 3
            local.get 2
            i32.store offset=4
            local.get 3
            return
          )
          (func $f_lam_7 (;18;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 12
            call $alloc
            local.set 1
            local.get 1
            i32.const 6
            i32.store
            local.get 1
            i32.const 17
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            local.get 1
            return
          )
          (func $f_lam_8 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32)
            i32.const 0
            call $alloc
            local.set 3
            local.get 3
            local.set 3
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 0
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_9 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 2
            local.get 2
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_10 (;21;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 7
            i32.store
            local.get 3
            i32.const 19
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 7
            i32.store
            local.get 4
            i32.const 20
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            local.get 3
            i32.store
            local.get 5
            local.get 4
            i32.store offset=4
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            local.get 1
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            local.set 6
            local.get 0
            i32.load
            local.set 7
            local.get 7
            i32.const 4
            i32.add
            local.get 2
            local.get 6
            local.get 7
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_11 (;22;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 16
            call $alloc
            local.set 4
            local.get 4
            i32.const 8
            i32.store
            local.get 4
            i32.const 21
            i32.store offset=4
            local.get 4
            local.get 0
            i32.store offset=8
            local.get 4
            local.get 1
            i32.store offset=12
            local.get 4
            local.set 4
            local.get 2
            i32.load offset=8
            local.set 5
            local.get 5
            i32.const 4
            i32.add
            local.get 3
            local.get 5
            i32.load
            call_indirect (type $fun_2_1)
            local.set 6
            i32.const 20
            call $alloc
            local.set 7
            local.get 7
            i32.const 9
            i32.store
            local.get 7
            i32.const 31
            i32.store offset=4
            local.get 7
            local.get 1
            i32.store offset=8
            local.get 7
            local.get 4
            i32.store offset=12
            local.get 7
            local.get 6
            i32.store offset=16
            local.get 7
            return
          )
          (func $f_lam_12 (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32)
            i32.const 0
            call $alloc
            local.set 3
            local.get 3
            local.set 3
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 0
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_13 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 2
            local.get 2
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_14 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 7
            i32.store
            local.get 3
            i32.const 23
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 7
            i32.store
            local.get 4
            i32.const 24
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            local.get 3
            i32.store
            local.get 5
            local.get 4
            i32.store offset=4
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            local.get 1
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            local.set 6
            local.get 0
            i32.load
            local.set 7
            local.get 7
            i32.const 4
            i32.add
            local.get 2
            local.get 6
            local.get 7
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_15 (;26;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 16
            call $alloc
            local.set 4
            local.get 4
            i32.const 8
            i32.store
            local.get 4
            i32.const 25
            i32.store offset=4
            local.get 4
            local.get 0
            i32.store offset=8
            local.get 4
            local.get 1
            i32.store offset=12
            local.get 4
            local.set 4
            local.get 2
            i32.load offset=8
            local.set 5
            local.get 5
            i32.const 4
            i32.add
            local.get 3
            local.get 5
            i32.load
            call_indirect (type $fun_2_1)
            local.set 6
            i32.const 20
            call $alloc
            local.set 7
            local.get 7
            i32.const 9
            i32.store
            local.get 7
            i32.const 31
            i32.store offset=4
            local.get 7
            local.get 1
            i32.store offset=8
            local.get 7
            local.get 4
            i32.store offset=12
            local.get 7
            local.get 6
            i32.store offset=16
            local.get 7
            return
          )
          (func $f_lam_16 (;27;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 0
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_17 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            i32.const 825
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 2
            i32.const 12
            call $alloc
            local.set 3
            local.get 3
            i32.const 6
            i32.store
            local.get 3
            i32.const 27
            i32.store offset=4
            local.get 3
            local.get 2
            i32.store offset=8
            local.get 3
            local.set 3
            local.get 0
            i32.load offset=8
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 8
            i32.store
            local.get 5
            i32.const 29
            i32.store offset=4
            local.get 5
            local.get 3
            i32.store offset=8
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_bind (;29;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 3
            i32.load
            local.set 4
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 4
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 3
                  i32.load offset=4
                  local.set 5
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 5
                  local.get 2
                  local.get 1
                  i32.load
                  call_indirect (type $fun_3_1)
                  br 2 (;@1;)
                end
                local.get 3
                i32.load offset=4
                local.set 6
                local.get 6
                local.set 7
                local.get 7
                i32.load
                local.set 8
                local.get 7
                i32.load offset=4
                local.set 9
                i32.const 16
                call $alloc
                local.set 10
                local.get 10
                i32.const 8
                i32.store
                local.get 10
                i32.const 30
                i32.store offset=4
                local.get 10
                local.get 1
                i32.store offset=8
                local.get 10
                local.get 7
                i32.store offset=12
                local.get 10
                local.set 10
                i32.const 12
                call $alloc
                local.set 11
                local.get 11
                local.get 8
                i32.store
                local.get 11
                local.get 9
                i32.store offset=4
                local.get 11
                local.get 10
                i32.store offset=8
                local.get 11
                local.set 11
                i32.const 8
                call $alloc
                local.set 12
                local.get 12
                i32.const 1
                i32.store
                local.get 12
                local.get 11
                i32.store offset=4
                local.get 12
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_bind_lam_0 (;30;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 1
            i32.load offset=8
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 8
            i32.store
            local.get 5
            i32.const 29
            i32.store offset=4
            local.get 5
            local.get 3
            i32.store offset=8
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_prompt (;31;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            local.get 2
            i32.const 4
            i32.add
            local.get 4
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            local.get 5
            i32.load
            local.set 6
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 6
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 5
                  i32.load offset=4
                  local.set 7
                  i32.const 8
                  call $alloc
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.store
                  local.get 8
                  local.get 7
                  i32.store offset=4
                  local.get 8
                  br 2 (;@1;)
                end
                local.get 5
                i32.load offset=4
                local.set 8
                local.get 8
                i32.load
                local.set 9
                local.get 0
                local.get 9
                call $__mon_eqm
                local.set 10
                local.get 10
                i32.load
                local.set 11
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 11
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 10
                      i32.load offset=4
                      local.set 12
                      local.get 8
                      i32.load
                      local.set 13
                      local.get 8
                      i32.load offset=4
                      local.set 14
                      i32.const 20
                      call $alloc
                      local.set 15
                      local.get 15
                      i32.const 9
                      i32.store
                      local.get 15
                      i32.const 32
                      i32.store offset=4
                      local.get 15
                      local.get 0
                      i32.store offset=8
                      local.get 15
                      local.get 1
                      i32.store offset=12
                      local.get 15
                      local.get 8
                      i32.store offset=16
                      local.get 15
                      local.set 15
                      local.get 8
                      i32.load offset=8
                      local.set 16
                      i32.const 16
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 13
                      i32.store
                      local.get 17
                      local.get 14
                      i32.store offset=4
                      local.get 17
                      local.get 15
                      i32.store offset=8
                      local.get 17
                      local.get 16
                      i32.store offset=12
                      local.get 17
                      local.set 17
                      i32.const 8
                      call $alloc
                      local.set 18
                      local.get 18
                      i32.const 1
                      i32.store
                      local.get 18
                      local.get 17
                      i32.store offset=4
                      local.get 18
                      br 2 (;@3;)
                    end
                    local.get 10
                    i32.load offset=4
                    local.set 12
                    i32.const 20
                    call $alloc
                    local.set 18
                    local.get 18
                    i32.const 9
                    i32.store
                    local.get 18
                    i32.const 33
                    i32.store offset=4
                    local.get 18
                    local.get 0
                    i32.store offset=8
                    local.get 18
                    local.get 1
                    i32.store offset=12
                    local.get 18
                    local.get 8
                    i32.store offset=16
                    local.get 18
                    local.set 18
                    local.get 8
                    i32.load offset=4
                    local.set 19
                    local.get 19
                    i32.const 4
                    i32.add
                    local.get 18
                    local.get 3
                    local.get 19
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_prompt_lam_0 (;32;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 9
            i32.store
            local.get 6
            i32.const 31
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (func $__mon_prompt_lam_1 (;33;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 9
            i32.store
            local.get 6
            i32.const 31
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (table (;0;) 34 34 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $__apply_4_3 $f $f_lam_0 $f_lam_1 $f_lam_2 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_6 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_10 $f_lam_11 $f_lam_12 $f_lam_13 $f_lam_14 $f_lam_15 $f_lam_16 $f_lam_17 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
        )"#]];
    expect.assert_eq(&string);

    /*match validate_res {
        Ok(_) => {},
        Err(bin_reader_err) => {
            panic!("{}", bin_reader_err.message());
        },
    }*/
  }
}
