use aiahr_ast::{Ast, AstModule, Term};
use aiahr_core::{
    id::{EffectName, EffectOpName, TermName, VarId},
    ident::Ident,
    modules::Module,
};
use aiahr_ir::{Ir, IrKind::*, IrTy, IrTyKind, IrVarTy, Kind, MkIrTy, P};
use aiahr_tc::EffectInfo;
use aiahr_ty::{InDb, Ty, TyScheme};
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};

pub(crate) mod id_converter;
use id_converter::IdConverter;
use salsa::AsId;

use self::lower::ItemSelector;

pub(crate) mod evidence;

mod lower;

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait IrEffectInfo: EffectInfo {
    fn effect_handler_return_index(&self, effect: EffectName) -> usize;
    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize;
    fn effect_vector_index(&self, effect: EffectName) -> usize;

    fn effect_handler_ir_ty(&self, effect: EffectName) -> IrTy;
}

#[salsa::jar(db = Db)]
pub struct Jar(
    IrModule,
    IrItem,
    lower_module,
    lower_item,
    effect_handler_ir_ty,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_tc::Db + aiahr_ir::Db {
    fn as_lower_ir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn lower_module(&self, module: AstModule) -> IrModule {
        lower_module(self.as_lower_ir_db(), module)
    }

    fn lower_item(&self, term_name: TermName) -> IrItem {
        lower_item(self.as_lower_ir_db(), term_name)
    }

    fn lower_module_of(&self, module: Module) -> IrModule {
        let ast_module = self.desugar_module_of(module);
        self.lower_module(ast_module)
    }

    fn lower_module_for_path(&self, path: std::path::PathBuf) -> IrModule {
        let module = self.root_module_for_path(path);
        let ast_module = self.desugar_module_of(module);
        self.lower_module(ast_module)
    }

    fn lower_item_for_file_name(&self, path: std::path::PathBuf, item: Ident) -> Option<IrItem> {
        let module = self.root_module_for_path(path);
        let term_name = self.id_for_name(module, item)?;
        Some(self.lower_item(term_name))
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_tc::Db + aiahr_ir::Db {}

#[salsa::tracked]
pub struct IrModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<IrItem>,
}

#[salsa::tracked]
pub struct IrItem {
    #[id]
    pub name: TermName,
    #[return_ref]
    pub item: Ir,
}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: AstModule) -> IrModule {
    let ast_db = db.as_ast_db();
    let items = module
        .terms(ast_db)
        .iter()
        .map(|term| {
            let module = module.module(ast_db);
            let name = term.name(db.as_ast_db());
            let ast = term.data(db.as_ast_db());
            let scheme = db.lookup_scheme(name);
            let ir = lower(db, module, name, &scheme, ast);
            IrItem::new(db, name, ir)
        })
        .collect();
    IrModule::new(db, module.module(ast_db), items)
}

#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term_name: TermName) -> IrItem {
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
fn effect_handler_ir_ty(db: &dyn crate::Db, effect: EffectName) -> IrTy {
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let bogus_item = TermName::from_id(salsa::Id::from_u32(u32::MAX));
    // TODO: Clean this up so we can access lower ty without requiring a full LowerCtx
    // TODO: Do we need to create a new tyvar_conv per scheme we lower?
    let mut lower_ctx = LowerCtx::new(
        db.as_lower_ir_db(),
        &mut var_conv,
        &mut tyvar_conv,
        ItemSelector {
            module: effect.module(db.as_core_db()),
            item: bogus_item,
        },
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
    db.mk_prod_ty(&[db.mk_ir_ty(IrTyKind::IntTy), handler_ty])
}

impl<DB> IrEffectInfo for DB
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

    fn effect_handler_ir_ty(&self, effect: EffectName) -> IrTy {
        effect_handler_ir_ty(self.as_lower_ir_db(), effect)
    }
}
impl<DB> ItemSchemes for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_scheme(&self, term: TermName) -> TyScheme {
        let typed_item = aiahr_tc::type_scheme_of(self.as_tc_db(), term);
        typed_item.ty_scheme(self.as_tc_db())
    }
}
impl<DB> VarTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty {
        let typed_item = aiahr_tc::type_scheme_of(self.as_tc_db(), term);
        typed_item.var_to_tys(self.as_tc_db())[&var_id]
    }
}
impl<DB> TermTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_term(&self, name: TermName, term: Idx<Term<VarId>>) -> aiahr_tc::TyChkRes<InDb> {
        let typed_item = aiahr_tc::type_scheme_of(self.as_tc_db(), name);
        typed_item.term_to_tys(self.as_tc_db())[&term]
    }
}

/// Lower an `Ast` into an `Ir`.
/// TODO: Real documentation.
pub fn lower(
    db: &dyn crate::Db,
    module: Module,
    name: TermName,
    scheme: &TyScheme,
    ast: &Ast<VarId>,
) -> Ir {
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let (mut lower_ctx, ev_locals, ev_params) = LowerCtx::new(
        db,
        &mut var_conv,
        &mut tyvar_conv,
        ItemSelector { module, item: name },
    )
    .collect_evidence_params(
        ast.view(ast.root()).row_ev_terms(ast.arena()),
        scheme.constrs.iter(),
    );

    let body = lower_ctx.lower_term(ast, ast.tree);
    // Bind all unique solved row evidence to local variables at top of the term
    let body = ev_locals.into_iter().fold(body, |body, (ev_param, ev_ir)| {
        Ir::app(Ir::abss([ev_param], body), [ev_ir])
    });
    // Add unsolved row evidence as parameters of the term
    let body = ev_params
        .into_iter()
        .rfold(body, |body, arg| Ir::new(Abs(arg, P::new(body))));

    // Finally wrap our term in any type variables it needs to bind
    scheme.bound.iter().rfold(body, |acc, ty_var| {
        Ir::new(TyAbs(
            IrVarTy {
                var: tyvar_conv.convert(*ty_var),
                kind: Kind::Type,
            },
            P::new(acc),
        ))
    })
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::Db as LowerIrDb;

    use aiahr_ast::{Direction, Term};
    use aiahr_core::{
        file::{FileId, SourceFile, SourceFileSet},
        id::{TermName, VarId},
        modules::Module,
        Db,
    };
    use aiahr_ir::{
        Db as IrDb, Ir,
        IrKind::{self, *},
        IrTyKind::*,
    };
    use aiahr_test::ast::{AstBuilder, MkTerm};
    use assert_matches::assert_matches;
    use ir_matcher::ir_matcher;
    use salsa::AsId;

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_ir::Jar,
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
    fn lower_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db Ir {
        let path = std::path::PathBuf::from("test.aiahr");
        let mut contents = "main = ".to_string();
        contents.push_str(input);
        let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
        SourceFileSet::new(db, vec![file]);

        db.lower_item_for_file_name(path, db.ident_str("main"))
            .unwrap()
            .item(db)
    }

    #[test]
    fn lower_id() {
        let db = TestDatabase::default();

        let ir = lower_snippet(&db, "|x| x");

        ir_matcher!(ir, TyAbs([ty_var], Abs([var], Var(var))) => {
            assert_eq!(var.ty.kind(db.as_ir_db()), VarTy(*ty_var));
        })
    }

    #[test]
    fn lower_product_literal() {
        let db = TestDatabase::default();

        let ir = lower_snippet(&db, "|a| { x = a, y = a }");
        ir_matcher!(ir,
            TyAbs([_ty_var],
                  App([
                      Abs([ev, a], App([FieldProj(0, Var(ev)), Var(a), Var(a)])),
                      Struct(ev_terms)])) => {
            let ir = &ev_terms[0];
            ir_matcher!(ir, Abs([m, n], Struct(splat)) => {
                assert_matches!(splat[0].deref().kind, Var(_m) => { assert_eq!(*m, _m); });
                assert_matches!(splat[1].deref().kind, Var(_n) => { assert_eq!(*n, _n); });
            });
        });
    }

    //TODO: Turn back on, once we have syntax for concat
    #[allow(dead_code)]
    fn lower_wand() {
        let db = TestDatabase::default();
        let m = VarId(0);
        let n = VarId(1);
        let ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abss(
                [m, n],
                builder.mk_unlabel(
                    "x",
                    builder.mk_project(
                        Direction::Left,
                        builder.mk_concat(Term::Variable(m), Term::Variable(n)),
                    ),
                ),
            )
        });
        let module = Module::from_id(salsa::Id::from_u32(0));
        let _name = TermName::new(&db, db.ident_str("test"), module);
        let (_var_tys, _term_tys, _scheme, _) = aiahr_tc::type_check(&db, &db, module, &ast);
    }
}
