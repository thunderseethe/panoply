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

pub(crate) fn emit_wasm_module(
  db: &dyn crate::Db,
  medir_module: MedIrModule,
) -> wasm_encoder::Module {
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
        let fun_ty = try_wasm_fun_ty(self.db, item.ty).expect("Closure has to have function type");
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
