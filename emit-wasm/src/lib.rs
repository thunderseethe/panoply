use aiahr_core::id::MedIrVarId;
use aiahr_core::modules::Module;
use aiahr_medir::{
    Atom, ClosureArities, Locals, MedIr, MedIrItem, MedIrItemName, MedIrKind, MedIrModule,
    MedIrTraversal, MedIrTy, MedIrTyKind,
};
use aiahr_reducir::ReducIrTermName;
use rustc_hash::FxHashMap;
use wasm_encoder::{
    CodeSection, ConstExpr, ElementSection, Elements, EntityType, FuncType, Function,
    GlobalSection, GlobalType, Instruction, MemArg, MemorySection, MemoryType, NameSection,
    TypeSection, ValType,
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
            self.section
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
            args.iter()
                // Remove 0 sized types
                //.filter(|a| !matches!(a.kind(db), MedIrTyKind::BlockTy(elems) if elems.is_empty()))
                .map(|_| ValType::I32),
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

    let mut imports = wasm_encoder::ImportSection::new();
    types.function([], [ValType::I32]);
    types.function([ValType::I32, ValType::I32], [ValType::I32]);
    types.function([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]);
    imports.import(
        "intrinsic",
        "__mon_generate_marker",
        EntityType::Function(0),
    );
    imports.import("intrinsic", "__mon_prompt", EntityType::Function(2));
    imports.import("intrinsic", "__mon_bind", EntityType::Function(1));
    let num_imports = imports.len();

    let mut closure_arities = ClosureArities::new(db);
    medir_module
        .items(db.as_medir_db())
        .iter()
        .for_each(|item| item.item(db.as_medir_db()).body.visit(&mut closure_arities));

    let mut type_sect = TypeSect::new(types);
    let mut codes = CodeSection::new();
    let mut item_indices = FxHashMap::default();

    for pap in closure_arities.into_arities() {
        let indx = funcs.len();
        let num_params = pap.arity - pap.num_args;
        let apply_ty_indx = type_sect.insert_fun_ty(fun_n_i32s(num_params + 1));
        funcs.function(apply_ty_indx);
        let name = format!("__apply_{}_{}", pap.arity, pap.num_args);
        let name = MedIrItemName::new(ReducIrTermName::gen(
            db,
            name,
            medir_module.module(db.as_medir_db()),
        ));
        name_map.append(indx + num_imports, name.0.name(db).text(db.as_core_db()));
        item_indices.insert(name, indx + num_imports);

        let mut f = Function::new([]);
        for i in (0u64..pap.num_args.try_into().unwrap()).map(|i| i + 1) {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::I32Load(MemArg {
                offset: i,
                align: 1,
                memory_index: 0,
            }));
        }
        for i in (0u32..num_params.try_into().unwrap()).map(|i| i + 1) {
            f.instruction(&Instruction::LocalGet(i));
        }
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 1,
            memory_index: 0,
        }));
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
        name_map.append(indx + num_imports, name.0.name(db).text(db.as_core_db()));
        item_indices.insert(name, indx + num_imports);
    }

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

    for item in medir_module.items(medir_db).iter() {
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

    let mut memory = MemorySection::new();
    memory.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
    });

    let func_len = (0u32..funcs.len())
        .map(|i| i + imports.len())
        .collect::<Vec<_>>();

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
    module.section(&elems);
    module.section(&codes);
    module.section(&names);
    module
}

fn fun_n_i32s(n: usize) -> FuncType {
    FuncType::new(std::iter::repeat(ValType::I32).take(n), [ValType::I32])
}

fn emit_wasm_item(
    db: &dyn crate::Db,
    opt_item: &MedIrItem,
    item_indices: &FxHashMap<MedIrItemName, u32>,
    type_sect: &mut TypeSect,
) -> wasm_encoder::Function {
    struct InstrEmitter<'a, 'i> {
        db: &'a dyn crate::Db,
        module: Module,
        ins: Vec<Instruction<'i>>,
        bump_alloc_global: u32,
        locals: FxHashMap<MedIrVarId, u32>,
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
                    self.inss((0..=end).map(|_| {
                        Instruction::Block(wasm_encoder::BlockType::Result(ValType::I32))
                    }));
                    self.inss([
                        Instruction::I32Const(1),
                        self.emit_atom(scrutinee),
                        Instruction::BrTable((0u32..end).collect(), end),
                        Instruction::End,
                    ]);
                    for (i, branch) in branches.iter().enumerate() {
                        let i: u32 = i.try_into().unwrap();
                        self.emit_locals(branch);
                        self.inss([Instruction::Br(end - i - 1), Instruction::End]);
                    }
                }
                MedIrKind::Closure(item, env) => {
                    let fun_ty = try_wasm_fun_ty(self.db, item.ty)
                        .expect("Closure has to have function type");
                    let arity = fun_ty.params().len();
                    let apply_name_str = format!("__apply_{}_{}", arity, env.len());
                    let apply_name = MedIrItemName::new(ReducIrTermName::gen(
                        self.db,
                        &apply_name_str,
                        self.module,
                    ));
                    let apply_indx = self.item_indices.get(&apply_name).unwrap_or_else(|| {
                        panic!("__apply_n_m indices not found for {}", apply_name_str)
                    });
                    let fn_indx = self
                        .item_indices
                        .get(&item.name)
                        .expect("Item indices not found");
                    self.inss([
                        // Store apply_n indx in slot 0
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::I32Const((*apply_indx).try_into().unwrap()),
                        Instruction::I32Store(MemArg {
                            offset: 0,
                            align: 1,
                            memory_index: 0,
                        }),
                        // Store fn indx in slot 1
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::I32Const((*fn_indx).try_into().unwrap()),
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
                    let env_len: i32 = env.len().try_into().unwrap();
                    // Leave
                    self.inss([
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::GlobalGet(self.bump_alloc_global),
                        Instruction::I32Const(env_len + 2),
                        Instruction::I32Add,
                        Instruction::GlobalSet(self.bump_alloc_global),
                    ]);
                }
                MedIrKind::Call(fun, args) => match fun {
                    aiahr_medir::Call::Known(item) => {
                        for arg in args.iter() {
                            self.ins(self.emit_atom(arg));
                        }
                        let indx = self
                            .item_indices
                            .get(&item.name)
                            .expect("Item indices not found");
                        self.ins(Instruction::Call(*indx))
                    }
                    aiahr_medir::Call::Unknown(v) => {
                        let get_local = self.emit_atom(&Atom::Var(*v));
                        self.inss([
                            get_local.clone(),
                            Instruction::I32Const(1),
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
                                align: 1,
                                memory_index: 0,
                            }),
                            Instruction::CallIndirect {
                                ty: fun_ty_indx,
                                table: 0,
                            },
                        ]);
                    }
                },
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
        bump_alloc_global: 0u32,
        locals,
        item_indices,
        type_sect,
    };

    emitter.emit_locals(&defn.body);
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

    use std::fs::OpenOptions;
    use std::io::Write;

    use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
    use aiahr_core::pretty::{PrettyPrint, PrettyWithCtx};
    use aiahr_optimize_reducir::Db as OptDb;
    use aiahr_parser::Db as ParseDb;
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

    fn emit_module(db: &TestDatabase, input: &str) -> wasm_encoder::Module {
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
    fn test_prj() {
        let db = TestDatabase::default();

        let wasm_module = emit_module(&db, "f = { x = 5678, y = 1234 }.x");

        let bytes = wasm_module.finish();
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/thunderseethe/aiahr/testbed/prj.wasm")
            .unwrap();
        f.write_all(&bytes).unwrap();
        let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
            function_references: true,
            ..Default::default()
        });
        let validate_res = validator.validate_all(&bytes);
        let string = wasmprinter::print_bytes(bytes).unwrap();
        let expect = expect![[r#"
            (module $test
              (type (;0;) (func (result i32)))
              (type (;1;) (func (param i32 i32) (result i32)))
              (type (;2;) (func (param i32 i32 i32) (result i32)))
              (type $fun_1_1 (;3;) (func (param i32) (result i32)))
              (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
              (import "intrinsic" "__mon_prompt" (func (;1;) (type 2)))
              (import "intrinsic" "__mon_bind" (func (;2;) (type 1)))
              (func $f (;3;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 0
                i32.store align=2
                global.get 0
                i32.const 5678
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (table (;0;) 1 1 funcref)
              (memory (;0;) 1)
              (global (;0;) (mut i32) i32.const 0)
              (elem (;0;) (i32.const 0) func $f)
            )"#]];
        expect.assert_eq(&string);

        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/thunderseethe/aiahr/testbed/prj.wat")
            .unwrap();
        writeln!(f, "{}", string).unwrap();

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
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/thunderseethe/aiahr/testbed/wand.wasm")
            .unwrap();
        f.write_all(&bytes).unwrap();
        let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
            function_references: true,
            ..Default::default()
        });
        let validate_res = validator.validate_all(&bytes);
        let string = wasmprinter::print_bytes(bytes).unwrap();
        let expect = expect![[r#"
            (module $test
              (type (;0;) (func (result i32)))
              (type (;1;) (func (param i32 i32) (result i32)))
              (type (;2;) (func (param i32 i32 i32) (result i32)))
              (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
              (type $fun_1_1 (;4;) (func (param i32) (result i32)))
              (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
              (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
              (type $fun_5_1 (;7;) (func (param i32 i32 i32 i32 i32) (result i32)))
              (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
              (import "intrinsic" "__mon_prompt" (func (;1;) (type 2)))
              (import "intrinsic" "__mon_bind" (func (;2;) (type 1)))
              (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
                local.get 1
                local.get 0
                i32.load align=2
                call_indirect (type $fun_1_1)
              )
              (func $__apply_2_0 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                local.get 1
                local.get 2
                local.get 0
                i32.load align=2
                call_indirect (type $fun_2_1)
              )
              (func $__apply_3_0 (;5;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
                local.get 1
                local.get 2
                local.get 3
                local.get 0
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $f (;6;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32)
                local.get 0
                i32.load align=2
                local.set 5
                local.get 5
                i32.const 1
                i32.add
                local.get 2
                local.get 3
                local.get 5
                i32.load align=2
                call_indirect (type $fun_3_1)
                local.set 6
                local.get 1
                i32.load offset=3 align=2
                local.set 7
                local.get 7
                i32.load align=2
                local.set 8
                local.get 8
                i32.const 1
                i32.add
                local.get 6
                local.get 8
                i32.load align=2
                call_indirect (type $fun_2_1)
                local.set 9
                global.get 0
                i32.const 0
                i32.store align=2
                global.get 0
                local.get 9
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (func $g (;7;) (type $fun_1_1) (param i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
                global.get 0
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 8
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 1
                global.get 0
                i32.const 5
                i32.store align=2
                global.get 0
                i32.const 9
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 2
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 10
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 3
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 11
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
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 12
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 6
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 13
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 7
                global.get 0
                local.get 6
                i32.store align=2
                global.get 0
                local.get 7
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 8
                global.get 0
                local.get 1
                i32.store align=2
                global.get 0
                local.get 2
                i32.store offset=1 align=2
                global.get 0
                local.get 5
                i32.store offset=2 align=2
                global.get 0
                local.get 8
                i32.store offset=3 align=2
                global.get 0
                global.get 0
                i32.const 4
                i32.add
                global.set 0
                local.set 9
                global.get 0
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 14
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 10
                global.get 0
                i32.const 5
                i32.store align=2
                global.get 0
                i32.const 15
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 11
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 16
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 12
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 17
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 13
                global.get 0
                local.get 12
                i32.store align=2
                global.get 0
                local.get 13
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 14
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 18
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 15
                global.get 0
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 19
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 16
                global.get 0
                local.get 15
                i32.store align=2
                global.get 0
                local.get 16
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 17
                global.get 0
                local.get 10
                i32.store align=2
                global.get 0
                local.get 11
                i32.store offset=1 align=2
                global.get 0
                local.get 14
                i32.store offset=2 align=2
                global.get 0
                local.get 17
                i32.store offset=3 align=2
                global.get 0
                global.get 0
                i32.const 4
                i32.add
                global.set 0
                local.set 18
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 19
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 20
                local.get 9
                local.get 18
                local.get 19
                local.get 20
                local.get 0
                call $f
              )
              (func $g_lam_0 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32)
                global.get 0
                local.get 0
                i32.store align=2
                global.get 0
                local.get 1
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
              )
              (func $g_lam_1 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32)
                local.get 2
                i32.load align=2
                local.set 3
                block (result i32) ;; label = @1
                  block (result i32) ;; label = @2
                    block (result i32) ;; label = @3
                      i32.const 1
                      local.get 3
                      br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
                    end
                    local.get 2
                    i32.load offset=1 align=2
                    local.set 4
                    local.get 0
                    i32.const 1
                    i32.add
                    local.get 4
                    local.get 0
                    i32.load align=2
                    call_indirect (type $fun_2_1)
                    br 1 (;@1;)
                  end
                  local.get 2
                  i32.load offset=1 align=2
                  local.set 4
                  local.get 1
                  i32.const 1
                  i32.add
                  local.get 4
                  local.get 1
                  i32.load align=2
                  call_indirect (type $fun_2_1)
                  br 0 (;@1;)
                end
              )
              (func $g_lam_2 (;10;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                local.get 0
                i32.load align=2
              )
              (func $g_lam_3 (;11;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 0
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
              (func $g_lam_4 (;12;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                local.get 0
                i32.load offset=1 align=2
              )
              (func $g_lam_5 (;13;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 1
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
              (func $g_lam_6 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32)
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
              (func $g_lam_7 (;15;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32)
                local.get 2
                i32.load align=2
                local.set 3
                block (result i32) ;; label = @1
                  block (result i32) ;; label = @2
                    block (result i32) ;; label = @3
                      i32.const 1
                      local.get 3
                      br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
                    end
                    local.get 2
                    i32.load offset=1 align=2
                    local.set 4
                    local.get 1
                    i32.const 1
                    i32.add
                    local.get 4
                    local.get 1
                    i32.load align=2
                    call_indirect (type $fun_2_1)
                    br 1 (;@1;)
                  end
                  local.get 2
                  i32.load offset=1 align=2
                  local.set 4
                  local.get 0
                  i32.const 1
                  i32.add
                  local.get 4
                  local.get 0
                  i32.load align=2
                  call_indirect (type $fun_2_1)
                  br 0 (;@1;)
                end
              )
              (func $g_lam_8 (;16;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                local.get 0
                i32.load offset=1 align=2
              )
              (func $g_lam_9 (;17;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 1
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
              (func $g_lam_10 (;18;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                local.get 0
                i32.load align=2
              )
              (func $g_lam_11 (;19;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 0
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
              (table (;0;) 17 17 funcref)
              (memory (;0;) 1)
              (global (;0;) (mut i32) i32.const 0)
              (elem (;0;) (i32.const 0) func $__apply_1_0 $__apply_2_0 $__apply_3_0 $f $g $g_lam_0 $g_lam_1 $g_lam_2 $g_lam_3 $g_lam_4 $g_lam_5 $g_lam_6 $g_lam_7 $g_lam_8 $g_lam_9 $g_lam_10 $g_lam_11)
            )"#]];
        expect.assert_eq(&string);

        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/thunderseethe/aiahr/testbed/wand.wat")
            .unwrap();
        writeln!(f, "{}", string).unwrap();

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
} do State.get({}))({})"#,
        );

        let bytes = wasm_module.finish();
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/thunderseethe/aiahr/testbed/test.wasm")
            .unwrap();
        f.write_all(&bytes).unwrap();
        /*let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
            function_references: true,
            ..Default::default()
        });*/
        //let validate_res = validator.validate_all(&bytes);
        let string = wasmprinter::print_bytes(bytes).unwrap();
        let expect = expect![[r#"
            (module $test
              (type (;0;) (func (result i32)))
              (type (;1;) (func (param i32 i32) (result i32)))
              (type (;2;) (func (param i32 i32 i32) (result i32)))
              (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
              (type $fun_1_1 (;4;) (func (param i32) (result i32)))
              (type $fun_4_1 (;5;) (func (param i32 i32 i32 i32) (result i32)))
              (type $fun_3_1 (;6;) (func (param i32 i32 i32) (result i32)))
              (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
              (import "intrinsic" "__mon_prompt" (func (;1;) (type 2)))
              (import "intrinsic" "__mon_bind" (func (;2;) (type 1)))
              (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
                local.get 1
                local.get 0
                i32.load align=2
                call_indirect (type $fun_1_1)
              )
              (func $__apply_2_1 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
                local.get 0
                i32.load offset=1 align=2
                local.get 1
                local.get 0
                i32.load align=2
                call_indirect (type $fun_2_1)
              )
              (func $__apply_3_0 (;5;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
                local.get 1
                local.get 2
                local.get 3
                local.get 0
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $__apply_3_2 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
                local.get 0
                i32.load offset=1 align=2
                local.get 0
                i32.load offset=2 align=2
                local.get 1
                local.get 0
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $f (;7;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
                i32.const 6
                i32.store align=2
                global.get 0
                i32.const 10
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
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 13
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
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 16
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
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 18
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 9
                local.get 8
                local.get 9
                local.get 1
                call 1
              )
              (func $f_lam_0 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32)
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 3
                local.get 1
                i32.const 1
                i32.add
                local.get 3
                local.get 0
                local.get 1
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $f_lam_1 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32)
                local.get 1
                i32.const 1
                i32.add
                local.get 2
                local.get 2
                local.get 1
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $f_lam_2 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32)
                global.get 0
                i32.const 5
                i32.store align=2
                global.get 0
                i32.const 8
                i32.store offset=1 align=2
                global.get 0
                global.get 0
                i32.const 2
                i32.add
                global.set 0
                local.set 3
                global.get 0
                i32.const 5
                i32.store align=2
                global.get 0
                i32.const 9
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
                i32.const 1
                i32.add
                local.get 2
                local.get 6
                local.get 7
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $f_lam_3 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
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
                local.get 3
                i32.load offset=1 align=2
                local.set 4
                local.get 4
                i32.const 1
                i32.add
                local.get 2
                local.get 1
                local.get 4
                i32.load align=2
                call_indirect (type $fun_3_1)
              )
              (func $f_lam_4 (;12;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                local.get 0
              )
              (func $f_lam_5 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32 i32)
                local.get 0
                i32.load offset=3 align=2
                local.set 2
                local.get 2
                i32.load align=2
                local.set 3
                local.get 3
                i32.const 1
                i32.add
                local.get 1
                local.get 3
                i32.load align=2
                call_indirect (type $fun_2_1)
                local.set 4
                local.get 4
                i32.load align=2
                local.set 5
                global.get 0
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 11
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
                i32.const 3
                i32.store align=2
                global.get 0
                i32.const 12
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
              (func $f_lam_6 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32)
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
              (func $f_lam_7 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32)
                global.get 0
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 14
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
              (func $f_lam_8 (;16;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 15
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
              (func $f_lam_9 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32)
                global.get 0
                global.get 0
                i32.const 0
                i32.add
                global.set 0
                local.set 2
                local.get 0
                i32.const 1
                i32.add
                local.get 2
                local.get 0
                i32.load align=2
                call_indirect (type $fun_2_1)
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
              (func $f_lam_10 (;18;) (type $fun_1_1) (param i32) (result i32)
                (local i32)
                global.get 0
                i32.const 4
                i32.store align=2
                global.get 0
                i32.const 17
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
              (table (;0;) 16 16 funcref)
              (memory (;0;) 1)
              (global (;0;) (mut i32) i32.const 0)
              (elem (;0;) (i32.const 0) func $__apply_1_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $f $f_lam_0 $f_lam_1 $f_lam_2 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_6 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_10)
            )"#]];
        expect.assert_eq(&string);

        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/thunderseethe/aiahr/testbed/test.wat")
            .unwrap();
        writeln!(f, "{}", string).unwrap();

        /*match validate_res {
            Ok(_) => {},
            Err(bin_reader_err) => {
                panic!("{}", bin_reader_err.message());
            },
        }*/
    }
}
