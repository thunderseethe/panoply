use std::ops::Deref;

use aiahr_core::id::{IdSupply, ReducIrVarId};
use aiahr_core::modules::Module;
use aiahr_optimize_reducir::{ReducIrOptimizedItem, ReducIrOptimizedModule};
use aiahr_reducir::ty::{ReducIrTy, ReducIrTyKind};
use aiahr_reducir::{
    Lets, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar, TypeCheck, P,
};
use rustc_hash::FxHashMap;
use wasm_encoder::{
    ConstExpr, EntityType, FieldType, Function, GlobalSection, GlobalType, Instruction, MemArg,
    NameSection, StorageType, StructType, StructuralType, TypeSection, ValType,
};

#[salsa::jar(db = Db)]
pub struct Jar();

pub trait Db: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db {
    fn as_emit_wasm_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn emit_module(&self, module: Module) -> wasm_encoder::Module {
        let opt_module = self.simple_reducir_module(module);
        emit_wasm_module(self.as_emit_wasm_db(), opt_module)
    }

    fn emit_module_for_path(&self, path: std::path::PathBuf) -> wasm_encoder::Module {
        let module = self.root_module_for_path(path);
        self.emit_module(module)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db {}

enum WasmType {
    Val(ValType),
    Comp(StructuralType),
}

fn wasm_ty(db: &dyn crate::Db, ty: ReducIrTy) -> WasmType {
    match ty.kind(db.as_ir_db()) {
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

fn anf(
    db: &dyn crate::Db,
    term_name: ReducIrTermName,
    supply: &mut IdSupply<ReducIrVarId>,
    ir: &ReducIr<Lets>,
) -> ReducIr<Lets> {
    struct Aux<'a> {
        db: &'a dyn crate::Db,
        term_name: ReducIrTermName,
        supply: &'a mut IdSupply<ReducIrVarId>,
    }
    impl<'a> Aux<'a> {
        fn is_imm<Ext>(&self, ir: &ReducIr<Ext>) -> bool {
            matches!(
                ir.kind(),
                ReducIrKind::Int(_) | ReducIrKind::Var(_) | ReducIrKind::Item(_, _)
            )
        }

        fn new_local(&mut self) -> ReducIrLocal {
            ReducIrLocal {
                top_level: self.term_name,
                id: self.supply.supply_id(),
            }
        }

        fn bind_compound(
            &mut self,
            binds: &mut Vec<(ReducIrVar, ReducIr<Lets>)>,
            ir: &ReducIr<Lets>,
        ) -> ReducIrVar {
            let ty = ir.type_check(self.db.as_ir_db()).expect("Arg to typecheck");
            let (ir_binds, body) = self.aux(ir);
            binds.extend(ir_binds);
            let var = ReducIrVar {
                var: self.new_local(),
                ty,
            };
            binds.push((var, body));
            var
        }

        fn aux(&mut self, ir: &ReducIr<Lets>) -> (Vec<(ReducIrVar, ReducIr<Lets>)>, ReducIr<Lets>) {
            match ir.kind() {
                ReducIrKind::Int(_) | ReducIrKind::Var(_) | ReducIrKind::Item(_, _) => {
                    (vec![], ir.clone())
                }
                ReducIrKind::Abs(vars, body) => {
                    let (binds, body) = self.aux(body);

                    (
                        vec![],
                        ReducIr::abss(vars.iter().copied(), ReducIr::locals(binds, body)),
                    )
                }
                ReducIrKind::TyAbs(ty, body) => {
                    let (binds, body) = self.aux(body);
                    // Might want these inside the TyAbs instead
                    (
                        vec![],
                        ReducIr::new(ReducIrKind::TyAbs(
                            *ty,
                            P::new(ReducIr::locals(binds, body)),
                        )),
                    )
                }
                ReducIrKind::App(head, spine) => {
                    let mut binds = vec![];
                    let imm_spine = spine
                        .iter()
                        .rev()
                        .map(|arg| {
                            if self.is_imm(arg) {
                                return arg.clone();
                            }
                            let var = self.bind_compound(&mut binds, arg);
                            ReducIr::var(var)
                        })
                        .rev()
                        .collect::<Vec<_>>();
                    let body = if self.is_imm(head) {
                        ReducIr::app(head.deref().clone(), imm_spine)
                    } else {
                        let var = self.bind_compound(&mut binds, head);
                        ReducIr::app(ReducIr::var(var), imm_spine)
                    };
                    (binds, body)
                }
                ReducIrKind::TyApp(body, ty_app) => {
                    let (binds, body) = self.aux(body);
                    (binds, ReducIr::ty_app(body, [ty_app.clone()]))
                }
                ReducIrKind::Struct(elems) => {
                    let mut binds = vec![];
                    let elems = elems
                        .iter()
                        .rev()
                        .map(|elem| {
                            if self.is_imm(elem) {
                                return elem.deref().clone();
                            }
                            let var = self.bind_compound(&mut binds, elem);
                            ReducIr::var(var)
                        })
                        .rev()
                        .collect();
                    (binds, ReducIr::new(ReducIrKind::Struct(elems)))
                }
                ReducIrKind::FieldProj(indx, val) => {
                    if self.is_imm(val) {
                        (vec![], ReducIr::field_proj(*indx, val.deref().clone()))
                    } else {
                        let mut binds = vec![];
                        let var = self.bind_compound(&mut binds, val);
                        (binds, ReducIr::field_proj(*indx, ReducIr::var(var)))
                    }
                }
                ReducIrKind::Tag(ty, tag, val) => {
                    if self.is_imm(val) {
                        (
                            vec![],
                            ReducIr::new(ReducIrKind::Tag(*ty, *tag, P::new(val.deref().clone()))),
                        )
                    } else {
                        let mut binds = vec![];
                        let var = self.bind_compound(&mut binds, val);
                        (
                            binds,
                            ReducIr::new(ReducIrKind::Tag(*ty, *tag, P::new(ReducIr::var(var)))),
                        )
                    }
                }
                ReducIrKind::Case(ty, discr, branches) => {
                    let branches = branches
                        .iter()
                        .map(|branch| {
                            let (binds, body) = self.aux(branch);
                            debug_assert!(binds.is_empty());
                            body
                        })
                        .collect::<Vec<_>>();
                    if self.is_imm(discr) {
                        (
                            vec![],
                            ReducIr::new(ReducIrKind::Case(
                                *ty,
                                P::new(discr.deref().clone()),
                                branches.into_boxed_slice(),
                            )),
                        )
                    } else {
                        let mut binds = vec![];
                        let var = self.bind_compound(&mut binds, discr);
                        (binds, ReducIr::case_on_var(*ty, var, branches))
                    }
                }
                ReducIrKind::X(Lets { binds, body }) => {
                    let mut new_binds = vec![];
                    for (var, defn) in binds.iter() {
                        let (binds, body) = self.aux(defn);
                        new_binds.extend(binds);
                        new_binds.push((*var, body));
                    }
                    if self.is_imm(body) {
                        (new_binds, body.deref().clone())
                    } else {
                        let (binds, body) = self.aux(body);
                        new_binds.extend(binds);
                        (new_binds, body)
                    }
                }
            }
        }
    }

    let mut aux = Aux {
        db,
        term_name,
        supply,
    };

    let (binds, body) = aux.aux(ir);
    ReducIr::locals(binds, body)
}

struct TypeSect {
    section: TypeSection,
    indices: FxHashMap<ReducIrTy, u32>,
}
impl Default for TypeSect {
    fn default() -> Self {
        Self {
            section: TypeSection::new(),
            indices: FxHashMap::default(),
        }
    }
}
impl TypeSect {
    fn new(section: TypeSection) -> Self {
        Self {
            section,
            indices: FxHashMap::default(),
        }
    }
    fn emit_fun_ty(&mut self, db: &dyn crate::Db, ty: ReducIrTy) -> u32 {
        let indx = self.indices.entry(ty).or_insert_with(|| {
            let indx = self.section.len();
            let func_ty = match wasm_ty(db, ty) {
                WasmType::Comp(StructuralType::Func(func_ty)) => func_ty,
                _ => panic!("Expected a function type for top level items"),
            };
            self.section.function(
                func_ty.params().iter().copied(),
                func_ty.results().iter().copied(),
            );
            indx
        });
        *indx
    }
}

fn emit_wasm_module(
    db: &dyn crate::Db,
    opt_module: ReducIrOptimizedModule,
) -> wasm_encoder::Module {
    let opt_db = db.as_opt_reducir_db();
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
    imports.import("intrinsic", "__mon_freshm", EntityType::Function(0));
    imports.import("intrinsic", "__mon_prompt", EntityType::Function(1));
    imports.import("intrinsic", "__mon_bind", EntityType::Function(2));
    let num_imports = imports.len();

    let items = opt_module.items(db.as_opt_reducir_db());
    let mut type_sect = TypeSect::new(types);
    let mut item_indices = items
        .iter()
        .enumerate()
        .map(|(func_indx, item)| {
            let ir = item.item(opt_db);
            let ty = ir.type_check(db.as_ir_db()).unwrap();
            let ty_indx = type_sect.emit_fun_ty(db, ty);
            funcs.function(ty_indx);
            let indx: u32 = func_indx.try_into().unwrap();
            let name = item.name(db.as_opt_reducir_db());
            name_map.append(indx + num_imports, name.name(db).text(db.as_core_db()));
            (name, indx + num_imports)
        })
        .collect::<FxHashMap<_, _>>();

    item_indices.insert(
        ReducIrTermName::gen(
            db,
            "__mon_freshm",
            opt_module.module(db.as_opt_reducir_db()),
        ),
        0,
    );
    item_indices.insert(
        ReducIrTermName::gen(db, "__mon_bind", opt_module.module(db.as_opt_reducir_db())),
        1,
    );
    item_indices.insert(
        ReducIrTermName::gen(
            db,
            "__mon_prompt",
            opt_module.module(db.as_opt_reducir_db()),
        ),
        2,
    );

    let mut codes = wasm_encoder::CodeSection::new();
    for item in items.iter() {
        let f = emit_wasm_item(db, *item, &item_indices, &mut type_sect);
        codes.function(&f);
    }

    let mut names = NameSection::new();
    let core_db = db.as_core_db();
    names.module(opt_module.module(opt_db).name(core_db).text(core_db));
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
}

fn emit_wasm_item(
    db: &dyn crate::Db,
    opt_item: ReducIrOptimizedItem,
    item_indices: &FxHashMap<ReducIrTermName, u32>,
    type_sect: &mut TypeSect,
) -> Function {
    let item = opt_item.item(db.as_opt_reducir_db());

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
            let opt_db = db.as_opt_reducir_db();
            let name = opt_item.name(opt_db);
            let mut supply = match opt_item.name(opt_db) {
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
        aiahr_lower_reducir::Jar,
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
    #[ignore = "type checking failures with new __mon_freshm inlining"]
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
        let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
            function_references: true,
            ..Default::default()
        });
        let validate_res = validator.validate_all(&bytes);
        let string = wasmprinter::print_bytes(bytes).unwrap();
        let expect = expect![[r#"
            (module $test
              (type (;0;) (func (result i32)))
              (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
              (type (;2;) (func (param i32 i32 i32) (result i32)))
              (type (;3;) (func (param i32 i32 i32) (result i32)))
              (type (;4;) (func (param i32 i32 i32) (result i32)))
              (type (;5;) (func (param i32 i32 i32) (result i32)))
              (type (;6;) (func (param i32 i32 i32) (result i32)))
              (type (;7;) (func (param i32) (result i32)))
              (type (;8;) (func (param i32 i32) (result i32)))
              (type (;9;) (func (param i32 i32) (result i32)))
              (type (;10;) (func (param i32 i32) (result i32)))
              (type (;11;) (func (param i32 i32 i32) (result i32)))
              (type (;12;) (func (param i32 i32) (result i32)))
              (type (;13;) (func (param i32 i32) (result i32)))
              (type (;14;) (func (param i32 i32 i32) (result i32)))
              (type (;15;) (func (param i32 i32) (result i32)))
              (type (;16;) (func (param i32) (result i32)))
              (type (;17;) (func (param i32 i32 i32) (result i32)))
              (type (;18;) (func (param i32 i32 i32 i32) (result i32)))
              (type (;19;) (func (param i32) (result i32)))
              (import "intrinsic" "__mon_freshm" (func (;0;) (type 0)))
              (import "intrinsic" "__mon_prompt" (func (;1;) (type 1)))
              (import "intrinsic" "__mon_bind" (func (;2;) (type 2)))
              (func $f (;3;) (type 3) (param i32 i32 i32) (result i32)
                (local i32 i32)
                local.get 0
                call $f_lam_9
                local.set 2
                ref.func 0
                local.set 3
                local.get 2
                local.get 3
                call_ref 13
                local.set 4
                ref.func 1
                local.set 5
                ref.func $f_lam_11
                local.get 4
                local.get 5
                call_ref 14
              )
              (func $f_lam_0 (;4;) (type 4) (param i32 i32 i32) (result i32)
                (local i32 i32 i32)
                i32.const 0
                global.get 0
                i32.add
                global.set 0
                global.get 0
                local.set 3
                local.get 0
                local.get 3
                local.get 1
                call_ref 9
              )
              (func $f_lam_1 (;5;) (type 4) (param i32 i32 i32) (result i32)
                (local i32 i32 i32)
                local.get 2
                local.get 2
                local.get 1
                call_ref 9
              )
              (func $f_lam_2 (;6;) (type 5) (param i32 i32 i32) (result i32)
                (local i32 i32 i32)
                ref.func $f_lam_1
                ref.func $f_lam_0
                global.get 0
                i32.store align=2
                global.get 0
                i32.store offset=1 align=2
                i32.const 2
                global.get 0
                i32.add
                global.set 0
                global.get 0
                local.set 3
                local.get 3
                local.get 1
                global.get 0
                i32.store align=2
                global.get 0
                i32.store offset=1 align=2
                i32.const 2
                global.get 0
                i32.add
                global.set 0
                global.get 0
                local.set 4
                local.get 0
                i32.load align=16
                local.set 5
                local.get 4
                local.get 2
                local.get 5
                call_ref 15
              )
              (func $f_lam_3 (;7;) (type 6) (param i32 i32 i32) (result i32)
                (local i32 i32)
                i32.const 0
                global.get 0
                i32.add
                global.set 0
                global.get 0
                local.set 2
                local.get 0
                i32.load offset=1 align=16
                local.set 3
                local.get 3
                i32.load offset=1 align=16
                local.set 4
                local.get 1
                local.get 2
                local.get 4
                call_ref 4
              )
              (func $f_lam_4 (;8;) (type 7) (param i32) (result i32)
                (local i32)
                local.get 0
              )
              (func $f_lam_5 (;9;) (type 8) (param i32 i32) (result i32)
                (local i32 i32)
                local.get 0
                i32.load offset=3 align=16
                local.set 2
                local.get 2
                i32.load align=16
                local.set 3
                local.get 1
                local.get 3
                call_ref 16
                local.set 4
                local.get 4
                i32.load align=16
                local.set 5
                local.get 4
                call $f_lam_3
                local.set 6
                ref.func $f_lam_4
                local.get 6
                local.get 5
                global.get 0
                i32.store align=2
                global.get 0
                i32.store offset=1 align=2
                global.get 0
                i32.store offset=2 align=2
                i32.const 3
                global.get 0
                i32.add
                global.set 0
                global.get 0
                local.set 7
                i32.const 1
                global.get 0
                i32.store align=2
                local.get 7
                global.get 0
                i32.store offset=1 align=2
                global.get 0
                i32.const 2
                global.set 0
                global.get 0
              )
              (func $f_lam_6 (;10;) (type 9) (param i32 i32) (result i32)
                (local i32 i32)
                local.get 0
                local.get 1
                global.get 0
                i32.store align=2
                global.get 0
                i32.store offset=1 align=2
                i32.const 2
                global.get 0
                i32.add
                global.set 0
                global.get 0
              )
              (func $f_lam_7 (;11;) (type 10) (param i32 i32) (result i32)
                (local i32 i32)
                local.get 0
                call $f_lam_6
                local.set 2
                i32.const 0
                global.get 0
                i32.store align=2
                local.get 2
                global.get 0
                i32.store offset=1 align=2
                global.get 0
                i32.const 2
                global.set 0
                global.get 0
              )
              (func $f_lam_8 (;12;) (type 10) (param i32 i32) (result i32)
                (local i32)
                local.get 0
                call $f_lam_7
              )
              (func $f_lam_9 (;13;) (type 11) (param i32 i32 i32) (result i32)
                (local i32 i32)
                local.get 1
                local.get 0
                call $f_lam_2
                local.set 2
                local.get 0
                call $f_lam_5
                local.set 3
                ref.func 1
                local.set 4
                ref.func $f_lam_8
                local.get 3
                local.get 4
                call_ref 17
                local.set 5
                ref.func 2
                local.set 6
                local.get 5
                local.get 2
                local.get 1
                local.get 6
                call_ref 18
              )
              (func $f_lam_10 (;14;) (type 12) (param i32 i32) (result i32)
                (local i32 i32)
                i32.const 0
                global.get 0
                i32.add
                global.set 0
                global.get 0
                local.set 2
                local.get 2
                local.get 0
                call_ref 19
                local.set 3
                i32.const 0
                global.get 0
                i32.store align=2
                local.get 3
                global.get 0
                i32.store offset=1 align=2
                global.get 0
                i32.const 2
                global.set 0
                global.get 0
              )
              (func $f_lam_11 (;15;) (type 12) (param i32 i32) (result i32)
                (local i32)
                local.get 0
                call $f_lam_10
              )
              (global (;0;) (mut i32) i32.const 0)
            )"#]];
        expect.assert_eq(&string);

        validate_res.expect("Validation Failed");
    }
}
