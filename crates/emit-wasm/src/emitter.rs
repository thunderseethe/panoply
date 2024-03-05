use base::pretty::{PrettyPrint, PrettyWithCtx};
use base::{id::MedIrVarId, modules::Module};
use medir::{
  Atom, CallAritys, Locals, MedIr, MedIrItem, MedIrItemName, MedIrKind, MedIrModule,
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
  let import_items: [Import<'_>; 0] = [];

  let mut imports = wasm_encoder::ImportSection::new();

  for (indx, import) in import_items.into_iter().enumerate() {
    let indx = indx as u32;
    types.function(import.params, import.returns);
    imports.import(import.module, import.func, EntityType::Function(indx));
    name_map.append(indx, import.func);
    item_indices.insert(import.name, indx);
  }
  let num_imports = imports.len();

  let mut closure_arities = CallAritys::new();
  medir_module
    .items(db.as_medir_db())
    .iter()
    .for_each(|item| item.item(db.as_medir_db()).body.visit(&mut closure_arities));

  let mut type_sect = TypeSect::new(types);
  let mut codes = CodeSection::new();

  // Builtin __mon_generate_marker function
  {
    let indx = funcs.len();
    let ty_indx = type_sect.insert_fun_ty(fun_n_i32s(0));
    funcs.function(ty_indx);
    let name_str = "__mon_generate_marker";
    let name = MedIrItemName::new(ReducIrTermName::gen(
      db,
      name_str,
      medir_module.module(db.as_medir_db()),
    ));
    name_map.append(indx + num_imports, name_str);
    item_indices.insert(name, indx + num_imports);

    let ins = [
      Instruction::GlobalGet(1),
      Instruction::GlobalGet(1),
      Instruction::I32Const(1),
      Instruction::I32Add,
      Instruction::GlobalSet(1),
      Instruction::Return,
      Instruction::End,
    ];
    let mut f = Function::new([(1, ValType::I32)]);
    ins.iter().for_each(|ins| {
      f.instruction(ins);
    });
    codes.function(&f);
  }

  // Builtin alloc function
  {
    let indx = funcs.len();
    let ty_indx = type_sect.insert_fun_ty(fun_n_i32s(1));
    funcs.function(ty_indx);
    let name_str = "alloc";
    let name = MedIrItemName::new(ReducIrTermName::gen(
      db,
      name_str,
      medir_module.module(db.as_medir_db()),
    ));
    name_map.append(indx + num_imports, name_str);
    item_indices.insert(name, indx + num_imports);

    let ins = [
      Instruction::GlobalGet(0),
      Instruction::GlobalGet(0),
      Instruction::LocalGet(0),
      Instruction::I32Add,
      Instruction::GlobalSet(0),
      Instruction::Return,
      Instruction::End,
    ];
    let mut f = Function::new([(1, ValType::I32)]);
    ins.iter().for_each(|ins| {
      f.instruction(ins);
    });
    codes.function(&f);
  }

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

  let calls = closure_arities.into_calls();

  let call_1_indx = funcs.len();
  for call in 1..=calls {
    let indx = funcs.len();
    let call_ty_indx = type_sect.insert_fun_ty(fun_n_i32s(call + 1));
    funcs.function(call_ty_indx);
    let name_str = format!("__call_{}", call);
    let name = MedIrItemName::new(ReducIrTermName::gen(
      db,
      name_str.as_str(),
      medir_module.module(db.as_medir_db()),
    ));
    name_map.append(indx + num_imports, &name_str);
    item_indices.insert(name, indx + num_imports);

    let mut f = Function::new([(1, ValType::I32)]);

    // Calculate pointer to the env of closure
    let env_ptr_local = call as u32 + 1;
    f.instruction(&Instruction::LocalGet(0))
      .instruction(&Instruction::I32Const(12))
      .instruction(&Instruction::I32Add)
      .instruction(&Instruction::LocalSet(env_ptr_local));
    for ins in std::iter::repeat(Instruction::Block(wasm_encoder::BlockType::Empty)).take(call + 1)
    {
      f.instruction(&ins);
    }
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty))
      .instruction(&Instruction::LocalGet(0))
      .instruction(&Instruction::I32Load(default_memarg(0)))
      .instruction(&Instruction::BrTable(
        (0..=(call as u32)).collect::<Vec<_>>().into(),
        (call as u32) + 1,
      ))
      .instruction(&Instruction::End);
    for i in 0..=call {
      // Pass our env pointer
      f.instruction(&Instruction::LocalGet(env_ptr_local));
      // Pass args based on arity
      (0..(i as u32))
        .map(|i| Instruction::LocalGet(i + 1))
        .for_each(|ins| {
          f.instruction(&ins);
        });
      let fun_ty = type_sect.insert_fun_ty(fun_n_i32s(i + 1));
      // Load our fn pointer and call it
      f.instruction(&Instruction::LocalGet(0))
        .instruction(&Instruction::I32Load(default_memarg(2)))
        .instruction(&Instruction::CallIndirect {
          ty: fun_ty,
          table: 0,
        });
      // Dispatch to call based on remaining args
      let rest = call - i;
      ((i as u32)..(call as u32))
        .map(|i| Instruction::LocalGet(i + 1))
        .for_each(|ins| {
          f.instruction(&ins);
        });
      if rest > 0 {
        f.instruction(&Instruction::Call(call_1_indx - 1 + rest as u32));
      }
      f.instruction(&Instruction::Return);
      f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::Unreachable)
      .instruction(&Instruction::End);

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
    let f = emit_wasm_item(db, item, &item_indices, alloc_indx);
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
        let local = self.locals.get(&v.id).unwrap_or_else(|| {
          panic!(
            "Local should have been set before it was got: {}",
            MedIr::new(MedIrKind::Atom(Atom::Var(*v)))
              .pretty_with(self.db)
              .pprint()
              .pretty(80)
          );
        });
        //.expect("Local should have been set before it was got");
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
        let fn_indx = self.item_indices.get(&item.name).unwrap_or_else(|| {
          panic!(
            "Item indices not found for {}",
            item.name.0.name(self.db).text(self.db.as_core_db())
          )
        });

        let env_len = match item.ty.kind(self.db) {
          MedIrTyKind::FunTy(args, _) => {
            // Env parameter type
            match args[0].kind(self.db) {
              MedIrTyKind::BlockTy(env_ty) => env_ty.len(),
              _ => 0,
            }
          }
          _ => unreachable!(),
        } as i32;

        let addr = self.locals.len() as u32;
        //let env_len: i32 = env.len().try_into().unwrap();
        self.inss([
          Instruction::I32Const((env_len + 3) * 4),
          Instruction::Call(self.alloc_fn),
          Instruction::LocalSet(addr),
        ]);

        self.inss([
          // Store call arity in slot 0
          Instruction::LocalGet(addr),
          // Remove 1 from arity for env param
          Instruction::I32Const((arity - 1).try_into().unwrap()),
          Instruction::I32Store(default_memarg(0)),
          // Store env len in slot 1
          Instruction::LocalGet(addr),
          Instruction::I32Const(env.len().try_into().unwrap()),
          Instruction::I32Store(default_memarg(1)),
          // Store fn indx in slot 2
          Instruction::LocalGet(addr),
          Instruction::I32Const((*fn_indx).try_into().unwrap()),
          Instruction::I32Store(default_memarg(2)),
        ]);

        // Store each env capture in slot 2..n
        for (i, capt) in env.iter().enumerate() {
          let i: u64 = i.try_into().unwrap();
          self.inss([
            Instruction::LocalGet(addr),
            self.emit_atom(&Atom::Var(*capt)),
            Instruction::I32Store(default_memarg(i + 3)),
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
          self.inss([get_local.clone()]);
          for arg in args.iter() {
            self.ins(self.emit_atom(arg));
          }
          let name_str = format!("__call_{}", args.len());
          let name = MedIrItemName::new(ReducIrTermName::gen(
            self.db,
            name_str.as_str(),
            self.module,
          ));
          self.ins(Instruction::Call(self.item_indices[&name]));
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
