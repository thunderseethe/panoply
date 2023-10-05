use aiahr_core::id::MedIrVarId;
use aiahr_core::modules::Module;
use aiahr_core::pretty::{PrettyPrint, PrettyWithCtx};
use aiahr_medir::{
    Atom, Locals, MedIr, MedIrItem, MedIrItemName, MedIrKind, MedIrModule, MedIrTyKind,
};
use aiahr_reducir::ReducIrTermName;
use rustc_hash::FxHashMap;
use wasm_encoder::{
    ConstExpr, EntityType, FuncType, Function, GlobalSection, GlobalType, Instruction, MemArg,
    NameSection, TypeSection, ValType,
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

    fn emit_fun_ty(&mut self, ty: &MedIrTyKind) -> u32 {
        let ty = match ty {
            MedIrTyKind::FunTy(params, _) => {
                wasm_encoder::FuncType::new(params.iter().map(|_| ValType::I32), [ValType::I32])
            }
            _ => unreachable!(),
        };
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
            let ty_indx = type_sect.emit_fun_ty(defn.type_of(db).kind(db));
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
                    let indx = self
                        .item_indices
                        .get(&item.name)
                        .expect("Item indices not found");
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
                        let ty_kind = v.ty.kind(self.db);
                        let fun_ty_indx = self.type_sect.emit_fun_ty(ty_kind);
                        self.inss([get_local, Instruction::CallRef(fun_ty_indx)]);
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
        ins: vec![],
        bump_alloc_global: 0u32,
        locals,
        item_indices,
        type_sect,
    };

    emitter.emit_locals(&defn.body);

    let num_locals = emitter.locals.len() - defn.params.len();

    let mut f = Function::new([(num_locals.try_into().unwrap(), ValType::I32)]);
    for ins in emitter.ins.iter() {
        f.instruction(ins);
    }
    f
}

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
                (local i32 i32 i32 i32 i32 i32 i32 i32)
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
                (local i32)
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
                call_ref $fun_2_1
              )
              (func $f_lam_1 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
                call_ref $fun_2_1
              )
              (func $f_lam_2 (;6;) (type $fun_3_1) (param i32 i32 i32) (result i32)
                (local i32 i32 i32 i32 i32)
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
                call_ref $fun_2_1
              )
              (func $f_lam_3 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32)
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
                call_ref $fun_3_1
              )
              (func $f_lam_4 (;8;) (type $fun_1_1) (param i32) (result i32)
                local.get 0
              )
              (func $f_lam_5 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
                (local i32 i32 i32 i32 i32 i32 i32)
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
                call_ref $fun_1_1
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
                (local i32)
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
                (local i32 i32)
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
                call_ref $fun_1_1
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
