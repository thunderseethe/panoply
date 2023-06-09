use aiahr_ast::{Ast, AstModule, AstTerm, Term};
use aiahr_core::{
    id::{EffectName, EffectOpName, TermName, VarId},
    ident::Ident,
    modules::Module,
};
use aiahr_reducir::{
    Kind, MkReducIrTy, ReducIr, ReducIrKind::*, ReducIrTy, ReducIrTyKind, ReducIrVarTy, P,
};
use aiahr_tc::{EffectInfo, TypedItem};
use aiahr_ty::{InDb, Ty, TyScheme};
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};

pub(crate) mod id_converter;
use id_converter::IdConverter;
use salsa::AsId;

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
    lower_module,
    lower_item,
    lower_impl,
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
        lower_item(self.as_lower_reducir_db(), term_name)
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
fn lower_module(db: &dyn crate::Db, module: AstModule) -> ReducIrModule {
    let ast_db = db.as_ast_db();
    let items = module
        .terms(ast_db)
        .iter()
        .map(|term| lower_impl(db, *term))
        .collect();
    ReducIrModule::new(db, module.module(ast_db), items)
}

#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term_name: TermName) -> ReducIrItem {
    let module = term_name.module(db.as_core_db());
    let ir_module = db.lower_module_of(module);
    *ir_module
        .items(db)
        .iter()
        .find(|term| term.name(db) == term_name)
        .unwrap_or_else(|| {
            panic!(
                "ICE: No IrItem constructed for TermName {:?}",
                term_name.name(db.as_core_db())
            )
        })
}

#[salsa::tracked]
fn lower_impl(db: &dyn crate::Db, term: AstTerm) -> ReducIrItem {
    let ast_db = db.as_ast_db();
    let name = term.name(ast_db);
    let ast = term.data(ast_db);
    let typed_item = db.type_scheme_of(name);
    let ir = lower(db, name, typed_item, ast);
    ReducIrItem::new(db, name, ir)
}

#[salsa::tracked]
fn effect_handler_ir_ty(db: &dyn crate::Db, effect: EffectName) -> ReducIrTy {
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let bogus_item = TermName::from_id(salsa::Id::from_u32(0u32));
    // TODO: Clean this up so we can access lower ty without requiring a full LowerCtx
    // TODO: Do we need to create a new tyvar_conv per scheme we lower?
    let mut lower_ctx = LowerCtx::new(
        db.as_lower_reducir_db(),
        &mut var_conv,
        &mut tyvar_conv,
        bogus_item,
    );

    // TODO: Produce members in order so we don't have to sort or get names here.
    let mut members = db
        .effect_members(effect)
        .iter()
        .map(|op| {
            let scheme = db.effect_member_sig(*op);
            (db.effect_member_name(*op), lower_ctx.lower_scheme(&scheme))
        })
        .collect::<Vec<_>>();
    members.sort_by(|a, b| a.0.cmp(&b.0));
    let handler_ty = db.mk_prod_ty(
        members
            .into_iter()
            .map(|(_, ir_ty)| ir_ty)
            .collect::<Vec<_>>()
            .as_slice(),
    );
    db.mk_prod_ty(&[db.mk_reducir_ty(ReducIrTyKind::IntTy), handler_ty])
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

/// Lower an `Ast` into an `Ir`.
/// TODO: Real documentation.
pub fn lower(
    db: &dyn crate::Db,
    name: TermName,
    typed_item: TypedItem,
    ast: &Ast<VarId>,
) -> ReducIr {
    let tc_db = db.as_tc_db();
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let scheme = typed_item.ty_scheme(db.as_tc_db());

    let required_evidence = typed_item.required_evidence(tc_db);
    let (mut lower_ctx, ev_locals, ev_params) =
        LowerCtx::new(db, &mut var_conv, &mut tyvar_conv, name)
            .collect_evidence_params(required_evidence.iter());

    let body = lower_ctx.lower_top_level(ast, ast.tree);
    // Bind all unique solved row evidence to local variables at top of the term
    let body = ev_locals.into_iter().fold(body, |body, (ev_param, ev_ir)| {
        ReducIr::app(ReducIr::abss([ev_param], body), [ev_ir])
    });
    // Add unsolved row evidence as parameters of the term
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
    use aiahr_reducir::ReducIr;
    use expect_test::expect;
    use pretty::{BoxAllocator, BoxDoc, Doc};

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

    /// Lower a snippet and return the produced IR
    fn lower_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db ReducIr {
        let path = std::path::PathBuf::from("test.aiahr");
        let mut contents = r#"
effect State {
    put : {} -> {},
    get : {} -> {}
}

effect Reader {
    ask : {} -> {}
}

main = "#
            .to_string();
        contents.push_str(input);
        let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
        SourceFileSet::new(db, vec![file]);

        match db.lower_item_for_file_name(path, db.ident_str("main")) {
            Some(term) => term.item(db),
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }

    #[test]
    fn lower_id() {
        let db = TestDatabase::default();

        let ir = lower_snippet(&db, "|x| x");

        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();
        let expect = expect!["(forall [(T0: Type)] (fun [V0, V1] V1))"];
        expect.assert_eq(&pretty_ir);
    }

    #[test]
    fn lower_product_literal() {
        let db = TestDatabase::default();

        let ir = lower_snippet(&db, "|a| { x = a, y = a }");

        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();
        let expect = expect!["(forall [(T1: Type)] (fun [V1, V2, V0, V3] (V2[0] V3 V3)))"];
        expect.assert_eq(&pretty_ir);
    }

    #[test]
    fn lower_wand() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|m| |n| (m ,, n).x");
        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();

        let expect = expect![[r#"
            (forall
              [(T1: Type) (T2: SimpleRow) (T3: SimpleRow) (T5: SimpleRow) (T7: SimpleRow)]
              (fun [V1, V2, V3, V0, V4, V5] (V3[3][0] (V2[0] V4 V5))))"#]];
        expect.assert_eq(&pretty_ir)
    }

    #[test]
    fn lower_state_get() {
        let db = TestDatabase::default();
        let ir = lower_snippet(
            &db,
            r#"
with {
    put = |x| |k| {},
    get = |x| |k| {},
    return = |x| x,
} do State.get({})"#,
        );
        let pretty_ir =
            Doc::<BoxDoc<'_>>::pretty(&ir.pretty(&db, &BoxAllocator).into_doc(), 80).to_string();

        let expect = expect![[r#"
            (forall [(T4: Type)] (fun [V1, V2, V3, V4, V5, V6, V0]
                (let
                  (V8 ((V6[0]
                    (V4[0] (fun [V9, V10] {}) (fun [V11, V12] {}))
                    (fun [V13] V13)) @ {}))
                  (new_prompt [V7] (let (V0 (V2[0] V0 {V7, V8}))
                    (prompt V7 (((V6[3][0] V8) @ {})
                        ((fun [V15, V14] (yield V15[0] (fun [V16] (V15[1][1] V14 V16))))
                          (V3[3][0] V0)
                          {}))))))))"#]];
        expect.assert_eq(&pretty_ir);
    }
}
