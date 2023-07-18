use aiahr_ast::{Ast, AstModule, AstTerm, Term};
use aiahr_core::{
    id::{EffectName, EffectOpName, Id, TermName, TyVarId, VarId},
    ident::Ident,
    modules::Module,
};
use aiahr_reducir::{
    ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyKind, ReducIrVarTy},
    ReducIr,
    ReducIrKind::*,
    P,
};
use aiahr_tc::{EffectInfo, TypedItem};
use aiahr_ty::{
    row::{Scoped, Simple},
    InDb, MkTy, RowFields, Ty, TyScheme, Wrapper,
};
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};

pub(crate) mod id_converter;
use id_converter::IdConverter;
use rustc_hash::FxHashMap;

use crate::lower::{LowerTyCtx, LowerTySchemeCtx};

use self::lower::{ItemWrappers, RowReducrIrEvidence, RowVarConvert};

pub(crate) mod evidence;

mod lower;

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait ReducIrEffectInfo: EffectInfo {
    fn effect_handler_return_index(&self, effect: EffectName) -> usize;
    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize;
    fn effect_vector_index(&self, effect: EffectName) -> usize;

    fn effect_handler_ir_ty(&self, effect: EffectName) -> ReducIrTy;
}

#[salsa::jar(db = Db)]
pub struct Jar(
    ReducIrModule,
    ReducIrItem,
    ReducIrRowEv,
    lower_module,
    lower_item,
    lower_row_ev,
    effect_handler_ir_ty,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_tc::Db + aiahr_reducir::Db {
    fn as_lower_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn lower_module(&self, module: AstModule) -> ReducIrModule {
        lower_module(self.as_lower_reducir_db(), module)
    }

    fn lower_item(&self, term_name: TermName) -> ReducIrItem {
        let ast_term = self.desugar_term_of(term_name);
        lower_item(self.as_lower_reducir_db(), ast_term)
    }

    fn lower_module_of(&self, module: Module) -> ReducIrModule {
        let ast_module = self.desugar_module_of(module);
        self.lower_module(ast_module)
    }

    fn lower_module_for_path(&self, path: std::path::PathBuf) -> ReducIrModule {
        let module = self.root_module_for_path(path);
        let ast_module = self.desugar_module_of(module);
        self.lower_module(ast_module)
    }

    fn lower_item_for_file_name(
        &self,
        path: std::path::PathBuf,
        item: Ident,
    ) -> Option<ReducIrItem> {
        let module = self.root_module_for_path(path);
        let term_name = self.id_for_name(module, item)?;
        Some(self.lower_item(term_name))
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_tc::Db + aiahr_reducir::Db {}

#[salsa::tracked]
pub struct ReducIrModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<ReducIrItem>,
}

#[salsa::tracked]
pub struct ReducIrItem {
    #[id]
    pub name: TermName,
    #[return_ref]
    pub item: ReducIr,
}

#[salsa::tracked]
pub struct ReducIrRowEv {
    pub simple: ReducIrItem,
    pub scoped: ReducIrItem,
}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: AstModule) -> ReducIrModule {
    let ast_db = db.as_ast_db();
    let items = module
        .terms(ast_db)
        .iter()
        .map(|term| lower_item(db, *term))
        .collect();
    ReducIrModule::new(db, module.module(ast_db), items)
}

#[salsa::tracked]
fn lower_row_ev(
    db: &dyn crate::Db,
    module: Module,
    left: RowFields,
    right: RowFields,
    goal: RowFields,
) -> ReducIrRowEv {
    ReducIrRowEv::new(
        db,
        lower_row_ev_item::<Simple>(db, module, "simple", left, right, goal),
        lower_row_ev_item::<Scoped>(db, module, "scoped", left, right, goal),
    )
}

fn lower_row_ev_item<Sema: RowReducrIrEvidence>(
    db: &dyn crate::Db,
    module: Module,
    mark: &str,
    left: RowFields,
    right: RowFields,
    goal: RowFields,
) -> ReducIrItem
where
    for<'a, 'b> LowerTyCtx<'a, 'b>: RowVarConvert<Sema>,
    Sema::Closed<InDb>: Copy,
    Sema::Open<InDb>: Copy,
{
    let ty_db = db.as_ty_db();
    let left_fields = left.fields(ty_db);
    let left_field_str: String = left_fields
        .iter()
        .map(|ident| ident.text(db.as_core_db()).as_str())
        .collect();
    let right_fields = right.fields(ty_db);
    let right_field_str: String = right_fields
        .iter()
        .map(|ident| ident.text(db.as_core_db()).as_str())
        .collect();
    let row_ev_ident = db.ident(format!(
        "_row_{}_{}_{}",
        mark, left_field_str, right_field_str
    ));
    let row_ev_name = TermName::new(db.as_core_db(), row_ev_ident, module);

    let mut id_count: i32 = -1;
    let mut left_values = left_fields
        .iter()
        .map(|_| {
            id_count += 1;
            id_count
        })
        .map(|id| TyVarId::from_raw(id.try_into().unwrap()))
        .collect::<Vec<_>>();
    let right_values = right_fields
        .iter()
        .map(|_| {
            id_count += 1;
            id_count
        })
        .map(|id| TyVarId::from_raw(id.try_into().unwrap()))
        .collect::<Vec<_>>();

    let left_row = db.mk_row_iter::<Sema::Closed<InDb>>(
        left_fields.iter().copied(),
        left_values
            .iter()
            .map(|id| db.mk_ty(aiahr_ty::TypeKind::VarTy(*id))),
    );
    let right_row = db.mk_row_iter::<Sema::Closed<InDb>>(
        right_fields.iter().copied(),
        right_values
            .iter()
            .map(|id| db.mk_ty(aiahr_ty::TypeKind::VarTy(*id))),
    );

    let goal_row_indices = Sema::merge(db, left_row, right_row);
    let goal_values = goal_row_indices
        .iter()
        .map(|indx| match indx {
            lower::RowIndx::Left(_, ty) => *ty,
            lower::RowIndx::Right(_, ty) => *ty,
        })
        .collect::<Vec<_>>();

    let goal_row =
        db.mk_row::<Sema::Closed<InDb>>(goal.fields(ty_db).as_slice(), goal_values.as_slice());

    left_values.extend(right_values);
    let order_of_tys = left_values;
    let tyvar_env = order_of_tys
        .iter()
        .enumerate()
        .map(|(i, ty)| (*ty, i as i32))
        .collect::<FxHashMap<_, _>>();

    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let mut lower_ctx = LowerCtx::new(
        db,
        &mut var_conv,
        LowerTyCtx::new(db, &mut tyvar_conv, tyvar_env),
        row_ev_name,
    );
    let ir = lower_ctx.row_evidence_ir::<Sema>(left_row, right_row, goal_row);

    let ir = order_of_tys.into_iter().rfold(ir, |ir, var| {
        let var = ReducIrVarTy {
            var: tyvar_conv.convert(var),
            kind: Kind::Type,
        };
        ReducIr::new(TyAbs(var, P::new(ir)))
    });

    ReducIrItem::new(db, row_ev_name, ir)
}

#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term: AstTerm) -> ReducIrItem {
    let ast_db = db.as_ast_db();
    let name = term.name(ast_db);
    let ast = term.data(ast_db);
    let typed_item = db.type_scheme_of(name);
    let ir = lower(db, name, typed_item, ast);
    ReducIrItem::new(db, name, ir)
}

#[salsa::tracked]
fn effect_handler_ir_ty(db: &dyn crate::Db, effect: EffectName) -> ReducIrTy {
    let mut tyvar_conv = IdConverter::new();

    let varp_ty = db.mk_reducir_ty(ReducIrTyKind::VarTy(0));
    // TODO: Produce members in order so we don't have to sort or get names here.
    let mut members = db
        .effect_members(effect)
        .iter()
        .map(|op| {
            let lower_ty_ctx = LowerTySchemeCtx::new(db.as_lower_reducir_db(), &mut tyvar_conv);
            let scheme = db.effect_member_sig(*op);
            let (ir_ty_scheme, _) =
                lower_ty_ctx.lower_scheme(effect.module(db.as_core_db()), &scheme);
            let ir_ty_scheme = match ir_ty_scheme.kind(db.as_ir_db()) {
                ReducIrTyKind::ForallTy(Kind::Type, ty) => match ty.kind(db.as_ir_db()) {
                    ReducIrTyKind::FunTy(arg, ret) => db.mk_binary_fun_ty(
                        arg,
                        db.mk_reducir_ty(ReducIrTyKind::FunTy(ret, varp_ty)),
                        varp_ty,
                    ),
                    ty => panic!("{:?}", ty),
                },
                ty => panic!("{:?}", ty),
            };
            (db.effect_member_name(*op), ir_ty_scheme)
        })
        .collect::<Vec<_>>();

    members.sort_by(|a, b| a.0.cmp(&b.0));

    db.mk_reducir_ty(ReducIrTyKind::ForallTy(
        Kind::Type,
        db.mk_reducir_ty(ReducIrTyKind::ProductTy(vec![
            db.mk_reducir_ty(ReducIrTyKind::IntTy),
            db.mk_reducir_ty(ReducIrTyKind::ProductTy(
                members.into_iter().map(|(_, ir_ty)| ir_ty).collect(),
            )),
        ])),
    ))
}

impl<DB> ReducIrEffectInfo for DB
where
    DB: ?Sized + crate::Db,
{
    fn effect_handler_return_index(&self, effect: EffectName) -> usize {
        aiahr_nameres::effect_handler_return_index(self.as_nameres_db(), effect)
    }

    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize {
        aiahr_nameres::effect_handler_op_index(self.as_nameres_db(), effect_op)
    }

    fn effect_vector_index(&self, effect: EffectName) -> usize {
        aiahr_nameres::effect_vector_index(self.as_nameres_db(), effect)
    }

    fn effect_handler_ir_ty(&self, effect: EffectName) -> ReducIrTy {
        effect_handler_ir_ty(self.as_lower_reducir_db(), effect)
    }
}
impl<DB> ItemWrappers for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_wrapper(&self, term_name: TermName, term: Idx<Term<VarId>>) -> &Wrapper {
        let typed_item = self.type_scheme_of(term_name);
        let wrappers = typed_item.item_to_wrappers(self.as_tc_db());
        &wrappers[&term]
    }
}

impl<DB> ItemSchemes for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_scheme(&self, term: TermName) -> TyScheme {
        let typed_item = self.type_scheme_of(term);
        typed_item.ty_scheme(self.as_tc_db())
    }
}
impl<DB> VarTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty {
        let typed_item = self.type_scheme_of(term);
        typed_item.var_to_tys(self.as_tc_db())[&var_id]
    }
}
impl<DB> TermTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_term(&self, name: TermName, term: Idx<Term<VarId>>) -> aiahr_tc::TyChkRes<InDb> {
        let typed_item = self.type_scheme_of(name);
        typed_item.term_to_tys(self.as_tc_db())[&term]
    }
}

/// Lower an `Ast` into an `ReducIr`.
/// TODO: Real documentation.
fn lower(db: &dyn crate::Db, name: TermName, typed_item: TypedItem, ast: &Ast<VarId>) -> ReducIr {
    let tc_db = db.as_tc_db();
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let scheme = typed_item.ty_scheme(db.as_tc_db());

    let (_, ty_ctx) = LowerTySchemeCtx::new(db, &mut tyvar_conv)
        .lower_scheme(name.module(db.as_core_db()), &scheme);

    let required_evidence = typed_item.required_evidence(tc_db);
    let (mut lower_ctx, ev_solved, ev_params) = LowerCtx::new(db, &mut var_conv, ty_ctx, name)
        .collect_evidence_params(required_evidence.iter());

    let body = lower_ctx.lower_top_level(ast, ast.root());
    // TODO: Bit of a hack. Eventually we'd like to generate our solved row ev in a central location.
    // Add row evidence as parameters of the term
    let body = ev_solved
        .into_iter()
        .rfold(body, |body, (arg, term)| ReducIr::local(arg, term, body));
    // Wrap our term in any unsolved row evidence params we need
    let body = ev_params
        .into_iter()
        .rfold(body, |body, arg| ReducIr::new(Abs(arg, P::new(body))));

    // Finally wrap our term in any type/row variables it needs to bind
    let body = scheme
        .bound_data_row
        .iter()
        .rfold(body, |acc, simple_row_var| {
            ReducIr::new(TyAbs(
                ReducIrVarTy {
                    var: tyvar_conv.convert(*simple_row_var),
                    kind: Kind::SimpleRow,
                },
                P::new(acc),
            ))
        });
    let body = scheme
        .bound_eff_row
        .iter()
        .rfold(body, |acc, scoped_row_var| {
            ReducIr::new(TyAbs(
                ReducIrVarTy {
                    var: tyvar_conv.convert(*scoped_row_var),
                    kind: Kind::ScopedRow,
                },
                P::new(acc),
            ))
        });
    scheme.bound_ty.iter().rfold(body, |acc, ty_var| {
        ReducIr::new(TyAbs(
            ReducIrVarTy {
                var: tyvar_conv.convert(*ty_var),
                kind: Kind::Type,
            },
            P::new(acc),
        ))
    })
}

#[cfg(test)]
mod tests {

    use crate::Db as LowerIrDb;

    use aiahr_core::{
        file::{FileId, SourceFile, SourceFileSet},
        Db,
    };
    use aiahr_parser::Db as ParserDb;
    use aiahr_reducir::{ty::ReducIrTy, ReducIr, ReducIrTyErr};
    use expect_test::expect;
    use pretty::{BoxAllocator, BoxDoc, Doc, RcAllocator};

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_reducir::Jar,
        aiahr_nameres::Jar,
        aiahr_parser::Jar,
        aiahr_tc::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn lower_function<'db>(db: &'db TestDatabase, input: &str, fn_name: &str) -> &'db ReducIr {
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

        match db.lower_item_for_file_name(path, db.ident_str(fn_name)) {
            Some(term) => term.item(db),
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }

    /// Lower a snippet and return the produced IR
    fn lower_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db ReducIr {
        let main = format!("f = {}", input);
        lower_function(db, &main, "f")
    }

    trait PrettyTyErr {
        fn to_pretty(self, db: &TestDatabase) -> String;
    }
    impl PrettyTyErr for Result<ReducIrTy, ReducIrTyErr> {
        fn to_pretty(self, db: &TestDatabase) -> String {
            match self {
                Ok(ty) => ty.pretty(db, &RcAllocator).pretty(80).to_string(),
                Err(err) => err.pretty(db, &RcAllocator).pretty(80).to_string(),
            }
        }
    }

    #[test]
    fn lower_id() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|x| x");
        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();

        let expect = expect!["(forall [(T1: Type) (T0: ScopedRow)] (fun [V0, V1] V1))"];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . T0 -> T1 -> T1"];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_product_literal() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|a| { x = a, y = a }");
        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();

        let expect = expect![[r#"
            (forall [(T1: Type) (T0: ScopedRow)] (let (V1 ((_row_simple_x_y @ T1) @ T1))
                (fun [V0, V2] (V1[0] V2 V2))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . T0 -> T1 -> {T1, T1}"];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_wand() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|m| |n| (m ,, n).x");
        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();

        let expect = expect![[r#"
            (forall
              [(T5: Type) (T4: ScopedRow) (T3: SimpleRow) (T2: SimpleRow) (T1: SimpleRow) (T0: SimpleRow)]
              (fun [V1, V2, V0, V3, V4] (V2[3][0] (V1[0] V3 V4))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect![[r#"
            forall Type .
              forall ScopedRow .
                forall SimpleRow .
                  forall SimpleRow .
                    forall SimpleRow .
                      forall SimpleRow .
                        { {4} -> {3} -> {2}
                        , forall Type . (<5> -> T0) -> (<4> -> T0) -> <3> -> T0
                        , {{2} -> {4}, <4> -> <2>}
                        , {{2} -> {3}, <3> -> <2>}
                        } -> { {1} -> T5 -> {2}
                             , forall Type . (<2> -> T0) -> (T6 -> T0) -> <3> -> T0
                             , {{2} -> {1}, <1> -> <2>}
                             , {{2} -> T5, T5 -> <2>}
                             } -> T0 -> {4} -> {3} -> T5"#]];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_state_get() {
        let db = TestDatabase::default();
        let ir = lower_snippet(
            &db,
            r#"
with {
    put = |x| |k| |s| k(s)(s),
    get = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({})"#,
        );
        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();

        let expect = expect![[r#"
            (forall [(T2: Type) (T1: ScopedRow) (T0: ScopedRow)] (fun [V1]
                (let
                  [ (V2 ((_row_simple_state_value @ {}) @ {}))
                  , (V3 (((_row_simple_return_putget @ {} -> ({} -> {} -> {{}, {}}) -> {} ->
                  {{}, {}}) @ {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}) @ {} -> {} ->
                  {{}, {}}))
                  , (V4 (((_row_simple_putget_return @ {} -> {} -> {{}, {}}) @ {} -> ({} ->
                  {} -> {{}, {}}) -> {} -> {{}, {}}) @ {} -> ({} -> {} -> {{}, {}}) -> {} ->
                  {{}, {}}))
                  , (V5 ((_row_simple_put_get @ {} -> ({} -> {} -> {{}, {}}) -> {} -> { {}
                                                                                      , {}
                                                                                      }) @ {}
                  -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}))
                  ]
                  (fun [V0]
                    (let
                      (V7 (V4[0]
                        (V5[0]
                          (fun [V8, V9, V10] (V9 V10 V10))
                          (fun [V11, V12, V13] (V12 {} V11)))
                        (fun [V14, V15] (V2[0] V15 V14))))
                      (new_prompt [V6] (let (V0 (V1[0] V0 {V6, (V4[2][0] V7)}))
                        (prompt V6 (V4[3][0]
                            V7
                            ((fun [V17, V16] (yield V17[0] (fun [V18] (V17[1][1] V16 V18))))
                              (V1[3][0] V0)
                              {}))))))))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect![[r#"
            forall Type .
              forall ScopedRow .
                forall ScopedRow .
                  { {1} -> { Int
                           , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             }
                           } -> {0}
                  , forall Type .
                    (<2> -> T0) -> ({ Int
                                    , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                      , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                      }
                                    } -> T0) -> <1> -> T0
                  , {{0} -> {1}, <1> -> <0>}
                  , { {0} -> { Int
                             , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                               , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                               }
                             }
                    , { Int
                      , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                        , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                        }
                      } -> <0>
                    }
                  } -> T1 -> {} -> {{}, {}}"#]];
        let pretty_ir_ty = {
            let this = ir.type_check(&db);
            let db = &db;
            match this {
                Ok(ty) => ty.pretty(db, &RcAllocator).pretty(80).to_string(),
                Err(err) => {
                    println!("{}", err.pretty(db, &RcAllocator).pretty(80));
                    panic!();
                }
            }
        };
        expect_ty.assert_eq(&pretty_ir_ty);
    }
}
