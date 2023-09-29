use std::ops::Deref;

use aiahr_core::id::{IdSupply, ReducIrVarId};
use aiahr_core::modules::Module;
use aiahr_medir::{
    Atom, Db as MedirDb, Defn, Locals, MedIr, MedIrItem, MedIrItemName, MedIrKind, MedIrModule,
    MedIrVar,
};
use aiahr_reducir::ty::{ReducIrTy, ReducIrTyKind};
use aiahr_reducir::{
    Lets, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar, TypeCheck, P,
};
use rustc_hash::FxHashMap;
use wasm_encoder::{
    ConstExpr, EntityType, FieldType, FuncType, Function, GlobalSection, GlobalType, Instruction,
    MemArg, NameSection, StorageType, StructType, StructuralType, TypeSection, ValType,
};

#[salsa::jar(db = Db)]
pub struct Jar();

pub trait Db: salsa::DbWithJar<Jar> + aiahr_medir::Db + aiahr_lower_medir::Db {
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
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_medir::Db + aiahr_lower_medir::Db {}

enum WasmType {
    Val(ValType),
    Comp(StructuralType),
}

fn wasm_ty(db: &dyn crate::Db, ty: ReducIrTy) -> WasmType {
    match ty.kind(db.as_reducir_db()) {
        ReducIrTyKind::IntTy | ReducIrTyKind::MarkerTy(_) => WasmType::Val(ValType::I32),
        ReducIrTyKind::FunTy(args, _) => {
            WasmType::Comp(StructuralType::Func(wasm_encoder::FuncType::new(
                args.iter().map(|_| ValType::I32),
                std::iter::once(ValType::I32),
            )))
        }
        ReducIrTyKind::ProductTy(elems) => WasmType::Comp(StructuralType::Struct(StructType {
            fields: elems
                .iter()
                .map(|_| FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: false,
                })
                .collect(),
        })),
        ReducIrTyKind::CoproductTy(_) => WasmType::Comp(StructuralType::Struct(StructType {
            fields: vec![
                FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: false,
                },
                FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: false,
                },
            ]
            .into_boxed_slice(),
        })),
        // Todo
        ReducIrTyKind::ControlTy(_, _) => todo!(),

        // Not do
        ReducIrTyKind::ForallTy(_, ty) => wasm_ty(db, ty),
        ReducIrTyKind::VarTy(_) => todo!(),
        ReducIrTyKind::ProdVarTy(_) => todo!(),
        ReducIrTyKind::CoprodVarTy(_) => todo!(),
    }
}

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

    fn emit_fun_ty(&mut self, defn: &Defn) -> u32 {
        let ty =
            wasm_encoder::FuncType::new(defn.params.iter().map(|_| ValType::I32), [ValType::I32]);
        let indx = self.indices.entry(ty.clone()).or_insert_with(|| {
            let indx = self.section.len();
            self.names.append(
                indx,
                &format!("fun_{}_{}", ty.params().len(), ty.results().len()),
            );
            self.section
                .function(ty.params().iter().copied(), ty.results().iter().copied());
            indx
        });
        *indx
    }
}

fn emit_wasm_module(db: &dyn crate::Db, medir_module: MedIrModule) -> wasm_encoder::Module {
    let medir_db = db.as_medir_db();
    let mut funcs = wasm_encoder::FunctionSection::new();
    let mut types = wasm_encoder::TypeSection::new();

    let mut name_map = wasm_encoder::NameMap::new();

    let mut imports = wasm_encoder::ImportSection::new();
    types.function([], [ValType::I32]);
    types.function(
        [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        [ValType::I32],
    );
    types.function([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]);
    imports.import(
        "intrinsic",
        "__mon_generate_marker",
        EntityType::Function(0),
    );
    imports.import("intrinsic", "__mon_prompt", EntityType::Function(1));
    imports.import("intrinsic", "__mon_bind", EntityType::Function(2));
    let num_imports = imports.len();

    let items = medir_module.items(medir_db);
    let mut type_sect = TypeSect::new(types);
    let mut item_indices = items
        .iter()
        .enumerate()
        .map(|(func_indx, item)| {
            let defn = item.item(medir_db);
            let ty_indx = type_sect.emit_fun_ty(defn);
            funcs.function(ty_indx);
            let indx: u32 = func_indx.try_into().unwrap();
            let name = item.name(medir_db);
            name_map.append(indx + num_imports, name.0.name(db).text(db.as_core_db()));
            (name, indx + num_imports)
        })
        .collect::<FxHashMap<_, _>>();

    let module = medir_module.module(medir_db);
    item_indices.insert(
        MedIrItemName::new(ReducIrTermName::gen(db, "__mon_generate_marker", module)),
        0,
    );
    item_indices.insert(
        MedIrItemName::new(ReducIrTermName::gen(db, "__mon_bind", module)),
        1,
    );
    item_indices.insert(
        MedIrItemName::new(ReducIrTermName::gen(db, "__mon_prompt", module)),
        2,
    );

    let mut codes = wasm_encoder::CodeSection::new();
    for item in items.iter() {
        let f = emit_wasm_item(db, item, &item_indices, &mut type_sect);
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

    let mut module = wasm_encoder::Module::new();
    module.section(&type_sect.section);
    module.section(&imports);
    module.section(&funcs);
    module.section(&globals);
    module.section(&codes);
    module.section(&names);
    module
}

fn emit_wasm_item(
    db: &dyn crate::Db,
    opt_item: &MedIrItem,
    item_indices: &FxHashMap<MedIrItemName, u32>,
    type_sect: &mut TypeSect,
) -> wasm_encoder::Function {
    struct InstrEmitter<'a, 'i> {
        db: &'a dyn crate::Db,
        ins: Vec<Instruction<'i>>,
        bump_alloc_global: u32,
        locals: FxHashMap<MedIrVar, u32>,
        item_indices: &'a FxHashMap<MedIrItemName, u32>,
        type_sect: &'a mut TypeSect,
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
                        .get(v)
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
                    for (i, elem) in elems.iter().enumerate() {
                        self.inss([
                            Instruction::GlobalGet(self.bump_alloc_global),
                            self.emit_atom(elem),
                            Instruction::I32Store(MemArg {
                                offset: i.try_into().unwrap(),
                                align: 1,
                                memory_index: 0,
                            }),
                        ]);
                    }
                    let alloc_len: i32 = elems.len().try_into().unwrap();
                    self.inss([
                        // Leave a copy of the start of blocks on the stack after everything
                        // is done.
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::I32Const(alloc_len),
                        Instruction::I32Add,
                        Instruction::GlobalSet(self.bump_alloc_global),
                    ])
                }
                MedIrKind::BlockAccess(var, indx) => self.inss([
                    self.emit_atom(&Atom::Var(*var)),
                    Instruction::I32Load(MemArg {
                        offset: (*indx).try_into().unwrap(),
                        align: 1,
                        memory_index: 0,
                    }),
                ]),
                MedIrKind::Switch(scrutinee, branches) => {
                    let end: u32 = branches.len().try_into().unwrap();
                    self.inss(
                        (0..=end).map(|_| Instruction::Block(wasm_encoder::BlockType::Empty)),
                    );
                    self.inss([
                        Instruction::Block(wasm_encoder::BlockType::Empty),
                        self.emit_atom(scrutinee),
                        Instruction::BrTable((0u32..end).map(|i| i + 1).collect(), end),
                        Instruction::Unreachable,
                        Instruction::End,
                    ]);
                    for (i, branch) in branches.iter().enumerate() {
                        let i: u32 = i.try_into().unwrap();
                        self.emit_locals(branch);
                        self.inss([Instruction::Br(end - i), Instruction::End]);
                    }
                }
                MedIrKind::Closure(item, env) => {
                    let indx = self.item_indices.get(item).expect("Item indices not found");
                    let env_len = env.len().try_into().unwrap();
                    self.inss([
                        // Store func ref in slot 0
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::RefFunc(*indx),
                        Instruction::I32Store(MemArg {
                            offset: 0,
                            align: 1,
                            memory_index: 0,
                        }),
                        // Store env length in slot 1
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::I32Const(env_len),
                        Instruction::I32Store(MemArg {
                            offset: 1,
                            align: 1,
                            memory_index: 0,
                        }),
                    ]);
                    // Store each env capture in slot 2..n
                    for (i, capt) in env.iter().rev().enumerate() {
                        let i: u64 = i.try_into().unwrap();
                        self.inss([
                            Instruction::GlobalGet(self.bump_alloc_global),
                            self.emit_atom(&Atom::Var(*capt)),
                            Instruction::I32Store(MemArg {
                                offset: i + 2,
                                align: 1,
                                memory_index: 0,
                            }),
                        ]);
                    }
                    // Leave
                    self.inss([
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::I32Const(env_len + 2),
                        Instruction::I32Add,
                        Instruction::GlobalSet(self.bump_alloc_global),
                    ]);
                }
                MedIrKind::Call(fun, args) => {
                    match fun {
                        aiahr_medir::Call::Known(item) => {
                            for arg in args.iter() {
                                self.ins(self.emit_atom(arg));
                            }
                            let indx = self.item_indices.get(item).expect("Item indices not found");
                            self.ins(Instruction::Call((*indx).try_into().unwrap()))
                        }
                        aiahr_medir::Call::Unknown(v) => {
                            let get_local = self.emit_atom(&Atom::Var(*v));
                            let i = self.locals.len().try_into().unwrap();
                            self.inss([
                                get_local.clone(),
                                Instruction::I32Load(MemArg {
                                    offset: 1,
                                    align: 1,
                                    memory_index: 0,
                                }),
                                Instruction::LocalSet(i),
                                Instruction::Loop(wasm_encoder::BlockType::Result(ValType::I32)),
                                Instruction::GlobalGet(self.bump_alloc_global),
                                Instruction::LocalGet(i),
                                Instruction::I32Add,
                                Instruction::I32Load(MemArg {
                                    offset: 0,
                                    align: 1,
                                    memory_index: 0,
                                }),
                                Instruction::LocalGet(i),
                                Instruction::I32Const(1),
                                Instruction::I32Sub,
                                Instruction::BrIf(0),
                                Instruction::End,
                            ]);
                            for arg in args.iter() {
                                self.ins(self.emit_atom(arg));
                            }
                            self.inss([
                                get_local,
                                Instruction::CallRef(0), // Todo get a real type for this
                            ]);
                        }
                    }
                }
            }
        }

        fn emit_locals(&mut self, locals: &Locals) {
            for (var, defn) in locals.binds.iter() {
                self.emit(defn);
                let local_len = self.locals.len().try_into().unwrap();
                let local = *self.locals.entry(*var).or_insert(local_len);
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
        .map(|(i, var)| (*var, i.try_into().unwrap()))
        .collect();
    let mut emitter = InstrEmitter {
        db,
        ins: vec![],
        bump_alloc_global: 0u32,
        locals,
        item_indices,
        type_sect,
    };

    emitter.emit_locals(&defn.body);

    let mut f = Function::new([(emitter.locals.len().try_into().unwrap(), ValType::I32)]);
    for ins in emitter.ins.iter() {
        f.instruction(ins);
    }
    f
}

/*fn emit_wasm_module(
    db: &dyn crate::Db,
    opt_module: OptimizedReducIrModule,
) -> wasm_encoder::Module {
    let reducir_db = db.as_reducir_db();
    let mut funcs = wasm_encoder::FunctionSection::new();
    let mut types = wasm_encoder::TypeSection::new();

    let mut name_map = wasm_encoder::NameMap::new();

    let mut imports = wasm_encoder::ImportSection::new();
    types.function([], [ValType::I32]);
    types.function(
        [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        [ValType::I32],
    );
    types.function([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]);
    imports.import(
        "intrinsic",
        "__mon_generate_marker",
        EntityType::Function(0),
    );
    imports.import("intrinsic", "__mon_prompt", EntityType::Function(1));
    imports.import("intrinsic", "__mon_bind", EntityType::Function(2));
    let num_imports = imports.len();

    let items = opt_module.items(db.as_reducir_db());
    let mut type_sect = TypeSect::new(types);
    let mut item_indices = items
        .iter()
        .enumerate()
        .map(|(func_indx, item)| {
            let ir = item.item(reducir_db);
            let ty = ir.type_check(reducir_db).unwrap();
            let ty_indx = type_sect.emit_fun_ty(db, ty);
            funcs.function(ty_indx);
            let indx: u32 = func_indx.try_into().unwrap();
            let name = item.name(reducir_db);
            name_map.append(indx + num_imports, name.name(db).text(db.as_core_db()));
            (name, indx + num_imports)
        })
        .collect::<FxHashMap<_, _>>();

    item_indices.insert(
        ReducIrTermName::gen(db, "__mon_generate_marker", opt_module.module(reducir_db)),
        0,
    );
    item_indices.insert(
        ReducIrTermName::gen(db, "__mon_bind", opt_module.module(reducir_db)),
        1,
    );
    item_indices.insert(
        ReducIrTermName::gen(db, "__mon_prompt", opt_module.module(reducir_db)),
        2,
    );

    let mut codes = wasm_encoder::CodeSection::new();
    for item in items.iter() {
        let f = emit_wasm_item(db, *item, &item_indices, &mut type_sect);
        codes.function(&f);
    }

    let mut names = NameSection::new();
    let core_db = db.as_core_db();
    names.module(opt_module.module(reducir_db).name(core_db).text(core_db));
    names.functions(&name_map);

    let mut globals = GlobalSection::new();
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const(0),
    );

    let mut module = wasm_encoder::Module::new();
    module.section(&type_sect.section);
    module.section(&imports);
    module.section(&funcs);
    module.section(&globals);
    module.section(&codes);
    module.section(&names);
    module
}*/

/*fn emit_wasm_item(
    db: &dyn crate::Db,
    opt_item: OptimizedReducIrItem,
    item_indices: &FxHashMap<ReducIrTermName, u32>,
    type_sect: &mut TypeSect,
) -> Function {
    let item = opt_item.item(db.as_reducir_db());

    struct InstrEmitter<'a, 'i> {
        db: &'a dyn crate::Db,
        ins: Vec<Instruction<'i>>,
        locals: FxHashMap<ReducIrLocal, u32>,
        item_indices: &'a FxHashMap<ReducIrTermName, u32>,
        type_sect: &'a mut TypeSect,
    }
    impl<'i> InstrEmitter<'_, 'i> {
        fn ins(&mut self, ins: Instruction<'i>) {
            self.ins.push(ins);
        }

        fn inss(&mut self, inss: impl IntoIterator<Item = Instruction<'i>>) {
            self.ins.extend(inss);
        }

        fn type_index(&mut self, ty: ReducIrTy) -> u32 {
            self.type_sect.emit_fun_ty(self.db, ty)
        }

        fn emit(&mut self, ir: &ReducIr<Lets>) {
            match ir.kind() {
                ReducIrKind::Int(lit) => {
                    self.ins(Instruction::I32Const((*lit).try_into().unwrap()));
                }
                ReducIrKind::Var(var) => {
                    let param_indx = *self.locals.get(&var.var).unwrap_or_else(|| {
                        panic!("Var {:?} not found in {:?}", var, self.locals);
                    });
                    self.ins(Instruction::LocalGet(param_indx));
                }
                ReducIrKind::Item(name, _) => {
                    let item_indx = self.item_indices.get(name).unwrap_or_else(|| {
                        panic!(
                            "No index for item: {} {:?}",
                            name.name(self.db).text(self.db.as_core_db()),
                            name
                        );
                    });
                    self.ins(Instruction::RefFunc(*item_indx));
                }
                ReducIrKind::Abs(_, _) => todo!("I think this shouldn't happen"),
                ReducIrKind::App(head, spine) => {
                    for arg in spine.iter().rev() {
                        self.emit(arg);
                    }
                    match head.kind() {
                        // Indirect call
                        ReducIrKind::Var(var) => {
                            self.emit(head);
                            let ty_index = self.type_index(var.ty);
                            self.ins(Instruction::CallRef(ty_index));
                        }
                        // Known call
                        ReducIrKind::Item(name, _) => {
                            let item_indx = self.item_indices.get(name).unwrap_or_else(|| {
                                panic!(
                                    "No index for item: {} {:?}",
                                    name.name(self.db).text(self.db.as_core_db()),
                                    name
                                );
                            });
                            self.ins(Instruction::Call(*item_indx));
                        }
                        _ => panic!("Application head expected to be var or item"),
                    }
                }
                ReducIrKind::TyAbs(_, ir) | ReducIrKind::TyApp(ir, _) => self.emit(ir),
                ReducIrKind::Struct(elems) => {
                    let num_elems = elems.len();
                    for elem in elems.iter().rev() {
                        self.emit(elem);
                    }
                    // TODO: Figure out a real way to do this address, prolly a bump allocator
                    for i in 0..num_elems {
                        self.inss([
                            Instruction::GlobalGet(0),
                            Instruction::I32Store(MemArg {
                                offset: i.try_into().unwrap(), /* TODO: is this bytes or bits? */
                                align: 1,
                                memory_index: 0,
                            }),
                        ]);
                    }
                    self.inss([
                        Instruction::I32Const(num_elems.try_into().unwrap()),
                        Instruction::GlobalGet(0),
                        Instruction::I32Add,
                        Instruction::GlobalSet(0),
                        // After allocating the struct the value of the struct on the stack is it's
                        // address
                        Instruction::GlobalGet(0),
                    ]);
                }
                ReducIrKind::FieldProj(indx, strukt) => {
                    self.emit(strukt);
                    // After emitting strukt value on the stack should be the address of strukt
                    self.ins(Instruction::I32Load(MemArg {
                        offset: (*indx).try_into().unwrap(),
                        align: 4,
                        memory_index: 0,
                    }));
                }
                ReducIrKind::Tag(_, tag, val) => {
                    // TODO: Figure out a real way to do this address, prolly a bump allocator
                    self.inss([
                        Instruction::I32Const((*tag).try_into().unwrap()),
                        Instruction::GlobalGet(0),
                        Instruction::I32Store(MemArg {
                            offset: 0,
                            align: 1,
                            memory_index: 0,
                        }),
                    ]);

                    self.emit(val);
                    self.inss([
                        Instruction::GlobalGet(0),
                        Instruction::I32Store(MemArg {
                            offset: 1,
                            align: 1,
                            memory_index: 0,
                        }),
                        Instruction::GlobalGet(0),
                        Instruction::I32Const(2),
                        Instruction::GlobalSet(0),
                        Instruction::GlobalGet(0),
                    ]);
                }
                ReducIrKind::Case(_, discr, branches) => {
                    self.ins(Instruction::Block(wasm_encoder::BlockType::Empty));

                    for branch in branches.iter() {
                        self.ins(Instruction::Loop(wasm_encoder::BlockType::Empty));
                        self.emit(branch);
                        self.ins(Instruction::Br(0));
                        self.ins(Instruction::End);
                    }

                    // TODO: Figure out block types
                    self.emit(discr);
                    let local = match discr.kind() {
                        ReducIrKind::Var(var) => {
                            let local = self.locals.len().try_into().unwrap();
                            self.locals.insert(var.var, local);
                            local
                        }
                        _ => unreachable!(),
                    };
                    self.ins(Instruction::LocalTee(local));
                    self.ins(Instruction::I32Load(MemArg {
                        offset: 1,
                        align: 4,
                        memory_index: 0,
                    }));
                    self.ins(Instruction::LocalGet(local));
                    self.ins(Instruction::I32Load(MemArg {
                        offset: 0,
                        align: 4,
                        memory_index: 0,
                    }));

                    self.ins(Instruction::BrTable(
                        (0u32..branches.len().try_into().unwrap())
                            .collect::<Vec<u32>>()
                            .into(),
                        branches.len().try_into().unwrap(),
                    ));

                    self.ins(Instruction::End);
                }
                ReducIrKind::X(Lets { binds, body }) => {
                    for (var, defn) in binds.iter() {
                        self.emit(defn);
                        let local = self.locals.len().try_into().unwrap();
                        self.locals.insert(var.var, local);
                        self.ins(Instruction::LocalSet(local));
                    }
                    self.emit(body);
                }
            };
        }
    }

    match item.try_top_level_def() {
        Ok(top_level) => {
            let locals = top_level
                .vars
                .iter()
                .enumerate()
                .map(|(indx, var)| (var.var, indx.try_into().unwrap()))
                .collect();
            let reducir_db = db.as_reducir_db();
            let name = opt_item.name(reducir_db);
            let mut supply = match opt_item.name(reducir_db) {
                ReducIrTermName::Term(term_name) => {
                    IdSupply::start_from(db.reducir_var_supply(term_name))
                }
                ReducIrTermName::Gen(_) => {
                    // TODO: Ensure this actually works
                    // We can start fresh because we don't generate any names that refer the
                    // generated name at the moment. Only the name that the generated item comes
                    // from.
                    IdSupply::default()
                }
            };

            let anf_body = anf(db, name, &mut supply, top_level.body);
            let mut emitter = InstrEmitter {
                db,
                ins: vec![],
                locals,
                item_indices,
                type_sect,
            };
            let mut func =
                Function::new([(emitter.locals.len().try_into().unwrap(), ValType::I32)]);
            emitter.emit(&anf_body);
            for ins in emitter.ins {
                func.instruction(&ins);
            }
            func
        }
        Err(ir) => panic!("Top level item expected to be function def: {:?}", ir),
    }
}*/

#[cfg(test)]
mod tests {
    use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
    use expect_test::expect;
    use wasmparser::Validator;

    use crate::Db;

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_lower_medir::Jar,
        aiahr_lower_reducir::Jar,
        aiahr_medir::Jar,
        aiahr_nameres::Jar,
        aiahr_optimize_reducir::Jar,
        aiahr_parser::Jar,
        aiahr_reducir::Jar,
        aiahr_tc::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn emit_function(db: &TestDatabase, input: &str) -> wasm_encoder::Module {
        let path = std::path::PathBuf::from("test.aiahr");
        let mut contents = r#"
effect State {
    put : {} -> {},
    get : {} -> {}
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
    //#[ignore = "Turn off this test until we can emit-wasm from medir"]
    fn test_simple_get() {
        let db = TestDatabase::default();

        let wasm_module = emit_function(
            &db,
            r#"
f = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))({})"#,
        );

        let bytes = wasm_module.finish();
        /*let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
            function_references: true,
            ..Default::default()
        });*/
        //let validate_res = validator.validate_all(&bytes);
        let string = wasmprinter::print_bytes(bytes).unwrap();
        let expect = expect![[r#"
            (module $test
              (type (;0;) (func (result i32)))
              (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
              (type (;2;) (func (param i32 i32 i32) (result i32)))
              (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
              (type $fun_3_1 (;4;) (func (param i32 i32 i32) (result i32)))
              (type $fun_1_1 (;5;) (func (param i32) (result i32)))
              (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
              (import "intrinsic" "__mon_prompt" (func (;1;) (type 1)))
              (import "intrinsic" "__mon_bind" (func (;2;) (type 2)))
              (func $f (;3;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 2
                local.get 2
                call 0
                local.set 3
                global.get 0
                ref.func $f_lam_2
                i32.store align=2
                global.get 0
                i32.const 2
                i32.store offset=1 align=2
                global.get 0
                local.get 3
                i32.store offset=2 align=2
                global.get 0
                local.get 0
                i32.store offset=3 align=2
                global.get 0
                global.get 0
                i32.const 4
                i32.add
                global.set 0
                local.set 4
                global.get 0
                ref.func $f_lam_5
                i32.store align=2
                global.get 0
                i32.const 1
                i32.store offset=1 align=2
                global.get 0
                local.get 0
                i32.store offset=2 align=2
                global.get 0
                global.get 0
                i32.const 3
                i32.add
                global.set 0
                local.set 5
                global.get 0
                ref.func $f_lam_8
                i32.store align=2
                global.get 0
                i32.const 0
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 6
                local.get 5
                local.get 6
                call 1
                local.set 7
                local.get 3
                local.get 4
                local.get 7
                call 2
                local.set 8
                global.get 0
                ref.func $f_lam_10
                i32.store align=2
                global.get 0
                i32.const 0
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 9
                local.get 8
                local.get 9
                call 1
              )
              (func $f_lam_0 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32 i32)
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 3
                local.get 1
                i32.load offset=1 align=2
                local.set 4
                loop (result i32) ;; label = @1
                  global.get 0
                  local.get 4
                  i32.add
                  i32.load align=2
                  local.get 4
                  i32.const 1
                  i32.sub
                  br_if 0 (;@1;)
                end
                local.get 3
                local.get 0
                local.get 1
                call_ref 0
              )
              (func $f_lam_1 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32)
                local.get 1
                i32.load offset=1 align=2
                local.set 3
                loop (result i32) ;; label = @1
                  global.get 0
                  local.get 3
                  i32.add
                  i32.load align=2
                  local.get 3
                  i32.const 1
                  i32.sub
                  br_if 0 (;@1;)
                end
                local.get 2
                local.get 2
                local.get 1
                call_ref 0
              )
              (func $f_lam_2 (;6;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32 i32)
                global.get 0
                ref.func $f_lam_0
                i32.store align=2
                global.get 0
                i32.const 0
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 3
                global.get 0
                ref.func $f_lam_1
                i32.store align=2
                global.get 0
                i32.const 0
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 4
                global.get 0
                local.get 3
                i32.store align=2
                global.get 0
                local.get 4
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 5
                global.get 0
                local.get 1
                i32.store align=2
                global.get 0
                local.get 5
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 6
                local.get 0
                i32.load align=2
                local.set 7
                local.get 7
                i32.load offset=1 align=2
                local.set 8
                loop (result i32) ;; label = @1
                  global.get 0
                  local.get 8
                  i32.add
                  i32.load align=2
                  local.get 8
                  i32.const 1
                  i32.sub
                  br_if 0 (;@1;)
                end
                local.get 2
                local.get 6
                local.get 7
                call_ref 0
              )
              (func $f_lam_3 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32 i32)
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 2
                local.get 0
                i32.load offset=1 align=2
                local.set 3
                local.get 3
                i32.load offset=1 align=2
                local.set 4
                local.get 4
                i32.load offset=1 align=2
                local.set 5
                loop (result i32) ;; label = @1
                  global.get 0
                  local.get 5
                  i32.add
                  i32.load align=2
                  local.get 5
                  i32.const 1
                  i32.sub
                  br_if 0 (;@1;)
                end
                local.get 2
                local.get 1
                local.get 4
                call_ref 0
              )
              (func $f_lam_4 (;8;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                local.get 0
              )
              (func $f_lam_5 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
                local.get 0
                i32.load offset=3 align=2
                local.set 2
                local.get 2
                i32.load align=2
                local.set 3
                local.get 3
                i32.load offset=1 align=2
                local.set 4
                loop (result i32) ;; label = @1
                  global.get 0
                  local.get 4
                  i32.add
                  i32.load align=2
                  local.get 4
                  i32.const 1
                  i32.sub
                  br_if 0 (;@1;)
                end
                local.get 1
                local.get 3
                call_ref 0
                local.set 4
                local.get 4
                i32.load align=2
                local.set 5
                global.get 0
                ref.func $f_lam_3
                i32.store align=2
                global.get 0
                i32.const 1
                i32.store offset=1 align=2
                global.get 0
                local.get 4
                i32.store offset=2 align=2
                global.get 0
                global.get 0
                i32.const 3
                i32.add
                global.set 0
                local.set 6
                global.get 0
                ref.func $f_lam_4
                i32.store align=2
                global.get 0
                i32.const 0
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 7
                global.get 0
                local.get 5
                i32.store align=2
                global.get 0
                local.get 6
                i32.store offset=1 align=2
                global.get 0
                local.get 7
                i32.store offset=2 align=2
                global.get 0
                global.get 0
                i32.const 3
                i32.add
                global.set 0
                local.set 8
                global.get 0
                i32.const 1
                i32.store align=2
                global.get 0
                local.get 8
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (func $f_lam_6 (;10;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32)
                global.get 0
                local.get 1
                i32.store align=2
                global.get 0
                local.get 0
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (func $f_lam_7 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32)
                global.get 0
                ref.func $f_lam_6
                i32.store align=2
                global.get 0
                i32.const 1
                i32.store offset=1 align=2
                global.get 0
                local.get 0
                i32.store offset=2 align=2
                global.get 0
                global.get 0
                i32.const 3
                i32.add
                global.set 0
                local.set 2
                global.get 0
                i32.const 0
                i32.store align=2
                global.get 0
                local.get 2
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (func $f_lam_8 (;12;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                ref.func $f_lam_7
                i32.store align=2
                global.get 0
                i32.const 1
                i32.store offset=1 align=2
                global.get 0
                local.get 0
                i32.store offset=2 align=2
                global.get 0
                global.get 0
                i32.const 3
                i32.add
                global.set 0
              )
              (func $f_lam_9 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32)
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 2
                local.get 0
                i32.load offset=1 align=2
                local.set 3
                loop (result i32) ;; label = @1
                  global.get 0
                  local.get 3
                  i32.add
                  i32.load align=2
                  local.get 3
                  i32.const 1
                  i32.sub
                  br_if 0 (;@1;)
                end
                local.get 2
                local.get 0
                call_ref 0
                local.set 3
                global.get 0
                i32.const 0
                i32.store align=2
                global.get 0
                local.get 3
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (func $f_lam_10 (;14;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                ref.func $f_lam_9
                i32.store align=2
                global.get 0
                i32.const 1
                i32.store offset=1 align=2
                global.get 0
                local.get 0
                i32.store offset=2 align=2
                global.get 0
                global.get 0
                i32.const 3
                i32.add
                global.set 0
              )
              (global (;0;) (mut i32) i32.const 0)
            )"#]];
        expect.assert_eq(&string);

        //validate_res.expect("Validation Failed");
    }
}
