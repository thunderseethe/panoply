use aiahr_core::{
    ast::{Ast, Term},
    id::{EffectId, EffectOpId, Id, TyVarId, VarId},
    ident::Ident,
    memory::handle::RefHandle,
    ty::row::Row,
};
use bumpalo::Bump;
use diagnostic::TypeCheckDiagnostic;
use ena::unify::{InPlaceUnificationTable, UnifyKey};
use rustc_hash::FxHashMap;
use salsa::DebugWithDb;

use aiahr_core::ty::infer::{InArena, TcUnifierVar, TyCtx};
use aiahr_core::ty::*;

mod unsolved_row {
    use std::cmp::Ordering;

    use aiahr_core::ty::infer::InArena;
    use aiahr_core::ty::row::{ClosedRow, Row};
    use aiahr_core::ty::{Evidence, FallibleTypeFold, TypeAlloc, TypeFoldable};

    impl<'ctx> From<OrderedRowXorRow<InArena<'ctx>>> for (Row<InArena<'ctx>>, Row<InArena<'ctx>>) {
        fn from(val: OrderedRowXorRow<InArena<'ctx>>) -> Self {
            match val {
                OrderedRowXorRow::ClosedOpen(row, var) => (Row::Closed(row), Row::Open(var)),
                OrderedRowXorRow::OpenOpen { min, max } => (Row::Open(min), Row::Open(max)),
            }
        }
    }
    impl<'ctx> TryFrom<(Row<InArena<'ctx>>, Row<InArena<'ctx>>)> for OrderedRowXorRow<InArena<'ctx>> {
        type Error = (ClosedRow<InArena<'ctx>>, ClosedRow<InArena<'ctx>>);

        fn try_from(value: (Row<InArena<'ctx>>, Row<InArena<'ctx>>)) -> Result<Self, Self::Error> {
            match value {
                (Row::Open(l), Row::Open(r)) => Ok(Self::with_open_open(l, r)),
                (Row::Open(tv), Row::Closed(row)) | (Row::Closed(row), Row::Open(tv)) => {
                    Ok(Self::ClosedOpen(row, tv))
                }
                (Row::Closed(l), Row::Closed(r)) => Err((l, r)),
            }
        }
    }
    impl<'ctx> From<UnsolvedRowEquation<InArena<'ctx>>> for Evidence<InArena<'ctx>> {
        fn from(eq: UnsolvedRowEquation<InArena<'ctx>>) -> Self {
            match eq {
                UnsolvedRowEquation::ClosedGoal(cand) => Evidence::Row {
                    left: Row::Open(cand.min),
                    right: Row::Open(cand.max),
                    goal: Row::Closed(cand.goal),
                },
                UnsolvedRowEquation::OpenGoal(cand) => match cand.orxr {
                    OrderedRowXorRow::ClosedOpen(closed, open) => Evidence::Row {
                        left: Row::Closed(closed),
                        right: Row::Open(open),
                        goal: Row::Open(cand.goal),
                    },
                    OrderedRowXorRow::OpenOpen { min, max } => Evidence::Row {
                        left: Row::Open(min),
                        right: Row::Open(max),
                        goal: Row::Open(cand.goal),
                    },
                },
            }
        }
    }
    /// Represents the value of a unification variable that is the component of a row combination.
    /// If this is the value in the unification table for row variable uv, then we can imagine it forms
    /// the row combination `uv * other ~ goal`
    #[derive(PartialEq, Eq, Hash, Clone)]
    pub(crate) struct CombineInto<A: TypeAlloc> {
        pub(crate) other: Row<A>,
        pub(crate) goal: Row<A>,
    }
    impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for CombineInto<A> {
        type Alloc = A;
        type Out<B: TypeAlloc> = CombineInto<B>;

        fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
            self,
            fold: &mut F,
        ) -> Result<Self::Out<F::Out>, F::Error> {
            Ok(CombineInto {
                other: self.other.try_fold_with(fold)?,
                goal: self.goal.try_fold_with(fold)?,
            })
        }
    }

    /// TODO: WIP Name
    /// What we want to capture here is two fold:
    ///  1. Row components are ordered in a standard way so comparison is easy (we don't have to check
    ///     any permutations)
    ///
    ///  2. We can store at most one closed row. Two closed rows is considered invalid, unlike if we
    ///     stored a `(Row<'ctx, TV>, Row<'ctx, TV>)`).
    #[derive(Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub enum OrderedRowXorRow<A: TypeAlloc> {
        ClosedOpen(ClosedRow<A>, A::TypeVar),
        OpenOpen { min: A::TypeVar, max: A::TypeVar },
    }
    impl<A: TypeAlloc> Copy for OrderedRowXorRow<A>
    where
        A: Clone,
        A::TypeVar: Copy,
        ClosedRow<A>: Copy,
    {
    }
    impl<A: TypeAlloc> OrderedRowXorRow<A> {
        pub(crate) fn with_open_open(l: A::TypeVar, r: A::TypeVar) -> Self
        where
            A::TypeVar: Ord,
        {
            debug_assert!(l != r, "Expected l != r in OpenOpen variant");
            if l < r {
                Self::OpenOpen { min: l, max: r }
            } else {
                Self::OpenOpen { min: r, max: l }
            }
        }
    }
    impl<'ctx, A: TypeAlloc + Clone + 'ctx> TypeFoldable<'ctx> for OrderedRowXorRow<A> {
        type Alloc = A;
        type Out<B: TypeAlloc> = (Row<B>, Row<B>);

        fn try_fold_with<F: FallibleTypeFold<'ctx, In = Self::Alloc>>(
            self,
            fold: &mut F,
        ) -> Result<Self::Out<F::Out>, F::Error> {
            Ok(match self {
                OrderedRowXorRow::ClosedOpen(row, tv) => (
                    Row::Closed(row.try_fold_with(fold)?),
                    fold.try_fold_row_var(tv)?,
                ),
                OrderedRowXorRow::OpenOpen { min, max } => {
                    (fold.try_fold_row_var(min)?, fold.try_fold_row_var(max)?)
                }
            })
        }
    }

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct ClosedGoal<A: TypeAlloc> {
        pub(crate) goal: ClosedRow<A>,
        pub(crate) min: A::TypeVar,
        pub(crate) max: A::TypeVar,
    }
    impl<A: TypeAlloc> Copy for ClosedGoal<A>
    where
        A: Clone,
        ClosedRow<A>: Copy,
        A::TypeVar: Copy,
    {
    }

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct OpenGoal<A: TypeAlloc> {
        pub(crate) goal: A::TypeVar,
        pub(crate) orxr: OrderedRowXorRow<A>,
    }
    impl<A: TypeAlloc> Copy for OpenGoal<A>
    where
        A: Clone,
        A::TypeVar: Copy,
        OrderedRowXorRow<A>: Copy,
    {
    }

    pub(crate) enum UnsolvedRowEquation<A: TypeAlloc> {
        ClosedGoal(ClosedGoal<A>),
        OpenGoal(OpenGoal<A>),
    }
    impl<A: TypeAlloc> Clone for UnsolvedRowEquation<A>
    where
        OpenGoal<A>: Clone,
        ClosedGoal<A>: Clone,
    {
        fn clone(&self) -> Self {
            match self {
                UnsolvedRowEquation::ClosedGoal(closed) => Self::ClosedGoal(closed.clone()),
                UnsolvedRowEquation::OpenGoal(open) => Self::OpenGoal(open.clone()),
            }
        }
    }
    impl<A: TypeAlloc> Copy for UnsolvedRowEquation<A>
    where
        OpenGoal<A>: Copy,
        ClosedGoal<A>: Copy,
    {
    }
    impl<A: TypeAlloc> PartialEq for UnsolvedRowEquation<A>
    where
        OpenGoal<A>: PartialEq,
        ClosedGoal<A>: PartialEq,
    {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (UnsolvedRowEquation::OpenGoal(a), UnsolvedRowEquation::OpenGoal(b)) => a == b,
                (UnsolvedRowEquation::ClosedGoal(a), UnsolvedRowEquation::ClosedGoal(b)) => a == b,
                _ => false,
            }
        }
    }
    impl<A: TypeAlloc> Eq for UnsolvedRowEquation<A>
    where
        OpenGoal<A>: Eq,
        ClosedGoal<A>: Eq,
    {
    }

    impl<A: TypeAlloc> PartialOrd for UnsolvedRowEquation<A>
    where
        OpenGoal<A>: PartialOrd,
        ClosedGoal<A>: PartialOrd,
    {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            match (self, other) {
                (UnsolvedRowEquation::ClosedGoal(left), UnsolvedRowEquation::ClosedGoal(right)) => {
                    left.partial_cmp(right)
                }
                (UnsolvedRowEquation::OpenGoal(left), UnsolvedRowEquation::OpenGoal(right)) => {
                    left.partial_cmp(right)
                }
                (UnsolvedRowEquation::ClosedGoal(_), UnsolvedRowEquation::OpenGoal(_)) => {
                    Some(Ordering::Greater)
                }
                (UnsolvedRowEquation::OpenGoal(_), UnsolvedRowEquation::ClosedGoal(_)) => {
                    Some(Ordering::Less)
                }
            }
        }
    }
    impl<A: TypeAlloc> Ord for UnsolvedRowEquation<A>
    where
        OpenGoal<A>: Ord,
        ClosedGoal<A>: Ord,
    {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self, other) {
                (UnsolvedRowEquation::ClosedGoal(left), UnsolvedRowEquation::ClosedGoal(right)) => {
                    left.cmp(right)
                }
                (UnsolvedRowEquation::OpenGoal(left), UnsolvedRowEquation::OpenGoal(right)) => {
                    left.cmp(right)
                }
                (UnsolvedRowEquation::ClosedGoal(_), UnsolvedRowEquation::OpenGoal(_)) => {
                    Ordering::Greater
                }
                (UnsolvedRowEquation::OpenGoal(_), UnsolvedRowEquation::ClosedGoal(_)) => {
                    Ordering::Less
                }
            }
        }
    }
}

use crate::infer::InferCtx;

mod diagnostic;

pub(crate) mod folds;
use folds::zonker::Zonker;

mod infer;
pub use infer::TyChkRes;

/// Information we need about effects during type checking
pub trait EffectInfo<'s, 'ctx> {
    /// Lookup the name of an effect from it's ID
    fn effect_name(&self, eff: EffectId) -> Ident;
    /// Lookup effect members from it's ID
    fn effect_members(&self, eff: EffectId) -> RefHandle<'ctx, [EffectOpId]>;

    /// Reverse index lookup, find an effect's ID from one of it's operation
    fn lookup_effect_by_member(&self, member: EffectOpId) -> EffectId;
    /// Look up an effect by the name of it's members, this may fail if an invalid list of member
    /// names is passed.
    fn lookup_effect_by_member_names(&self, members: &[Ident]) -> Option<EffectId>;
    fn lookup_effect_by_name(&self, name: Ident) -> Option<EffectId>;
    /// Lookup the type signature of an effect's member
    fn effect_member_sig(&self, eff: EffectId, member: EffectOpId) -> TyScheme<InDb>;
    /// Lookup the name of an effect's member
    fn effect_member_name(&self, eff: EffectId, member: EffectOpId) -> Ident;
}

pub fn type_check<'ty, 's, 'eff, E>(
    db: &dyn crate::Db,
    eff_info: &E,
    ast: &Ast<'ty, VarId>,
) -> (
    FxHashMap<VarId, Ty<InDb>>,
    FxHashMap<&'ty Term<'ty, VarId>, TyChkRes<InDb>>,
    TyScheme<InDb>,
    Vec<TypeCheckDiagnostic>,
)
where
    E: EffectInfo<'s, 'eff>,
{
    let arena = Bump::new();
    let infer_ctx = TyCtx::new(db.as_core_db(), &arena);
    tc_term(db, &infer_ctx, eff_info, ast)
}

fn tc_term<'ty, 'infer, 's, 'eff, II, E>(
    db: &dyn crate::Db,
    infer_ctx: &II,
    eff_info: &E,
    ast: &Ast<'ty, VarId>,
) -> (
    FxHashMap<VarId, Ty<InDb>>,
    FxHashMap<&'ty Term<'ty, VarId>, TyChkRes<InDb>>,
    TyScheme<InDb>,
    Vec<TypeCheckDiagnostic>,
)
where
    II: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
    E: EffectInfo<'s, 'eff>,
{
    let term = ast.root();
    let infer = InferCtx::new(db, infer_ctx, ast);

    // Infer types for all our variables and the root term.
    let (infer, gen_storage, result) = infer.infer(eff_info, term);

    // Solve constraints into the unifiers mapping.
    let (mut unifiers, unsolved_eqs, errors) = infer.solve(eff_info);

    //print_root_unifiers(&mut unifiers);
    // Zonk the variable -> type mapping and the root term type.
    let mut zonker = Zonker {
        ctx: db,
        unifiers: &mut unifiers,
        free_vars: vec![],
    };
    println!("Infer result: {:?}", result);
    let zonked_infer = result.try_fold_with(&mut zonker).unwrap();
    println!("Zonked result: {:?}", &zonked_infer.debug(db.as_core_db()));

    let zonked_var_tys = gen_storage
        .var_tys
        .into_iter()
        .map(|(var, ty)| (var, ty.try_fold_with(&mut zonker).unwrap()))
        .collect::<FxHashMap<_, _>>();

    let zonked_term_tys = gen_storage
        .term_tys
        .into_iter()
        .map(|(term, ty)| (term, ty.try_fold_with(&mut zonker).unwrap()))
        .collect::<FxHashMap<_, _>>();

    let constrs = unsolved_eqs
        .into_iter()
        .map(|eq| Evidence::from(eq).try_fold_with(&mut zonker).unwrap())
        .collect();

    let scheme = TyScheme {
        bound: zonker
            .free_vars
            .into_iter()
            .enumerate()
            .map(|(i, _)| TyVarId::from_raw(i))
            .collect(),
        constrs,
        eff: zonked_infer.eff,
        ty: zonked_infer.ty,
    };
    (zonked_var_tys, zonked_term_tys, scheme, errors)
}

#[salsa::jar(db = Db)]
pub struct Jar();
pub trait Db: salsa::DbWithJar<Jar> + aiahr_core::Db {}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_core::Db {}

impl<'db> AccessTy<'db, InDb> for &'db (dyn crate::Db + '_) {
    fn kind(&self, ty: &Ty<InDb>) -> &'db TypeKind<InDb> {
        self.as_core_db().kind(ty)
    }

    fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'db [row::RowLabel] {
        self.as_core_db().row_fields(row)
    }

    fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'db [Ty<InDb>] {
        self.as_core_db().row_values(row)
    }
}

#[allow(dead_code)]
fn print_root_unifiers(uni: &mut InPlaceUnificationTable<TcUnifierVar<'_>>) {
    println!("UnificationTable [");
    for uv in (0..uni.len()).map(|i| TcUnifierVar::from_index(i as u32)) {
        let root = uni.find(uv);
        if root != uv {
            continue;
        }
        if let Some(candidate) = uni.probe_value(root) {
            println!("\t{} => {:?}", uv.index(), candidate);
        }
    }
    println!("]");
}

pub mod test_utils {
    use aiahr_core::id::{EffectId, EffectOpId, TyVarId};
    use aiahr_core::ident::Ident;
    use aiahr_core::memory::handle::{self, RefHandle};

    use crate::{EffectInfo, InDb, MkTy, Row, TyScheme};

    // Utility trait to remove a lot of the intermediate allocation when creating ASTs
    // Helps make tests a little more readable

    pub struct DummyEff<'a>(pub &'a dyn aiahr_core::Db);
    impl<'a> DummyEff<'a> {
        pub const STATE_ID: EffectId = EffectId(0);
        pub const READER_ID: EffectId = EffectId(1);

        pub const GET_ID: EffectOpId = EffectOpId(0);
        pub const PUT_ID: EffectOpId = EffectOpId(1);
        pub const ASK_ID: EffectOpId = EffectOpId(2);
    }
    impl<'s, 'ctx> EffectInfo<'s, 'ctx> for DummyEff<'_> {
        fn effect_name(&self, eff: EffectId) -> Ident {
            match eff {
                DummyEff::STATE_ID => self.0.ident_str("State"),
                DummyEff::READER_ID => self.0.ident_str("Reader"),
                _ => unimplemented!(),
            }
        }

        fn effect_members(&self, eff: EffectId) -> RefHandle<'ctx, [EffectOpId]> {
            match eff {
                DummyEff::STATE_ID => handle::Handle(&[DummyEff::GET_ID, DummyEff::PUT_ID]),
                DummyEff::READER_ID => handle::Handle(&[DummyEff::ASK_ID]),
                _ => unimplemented!(),
            }
        }

        fn lookup_effect_by_member(&self, member: EffectOpId) -> EffectId {
            match member {
                DummyEff::GET_ID | DummyEff::PUT_ID => DummyEff::STATE_ID,
                DummyEff::ASK_ID => DummyEff::READER_ID,
                _ => unimplemented!(),
            }
        }

        fn lookup_effect_by_member_names<'a>(&self, members: &[Ident]) -> Option<EffectId> {
            members
                .get(0)
                .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                    "ask" => Some(DummyEff::READER_ID),
                    "get" => {
                        members
                            .get(1)
                            .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                                "put" => Some(DummyEff::STATE_ID),
                                _ => None,
                            })
                    }
                    "put" => {
                        members
                            .get(1)
                            .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                                "get" => Some(DummyEff::STATE_ID),
                                _ => None,
                            })
                    }
                    _ => None,
                })
        }

        fn lookup_effect_by_name(&self, name: Ident) -> Option<EffectId> {
            match name.text(self.0.as_core_db()).as_str() {
                "State" => Some(DummyEff::STATE_ID),
                "Reader" => Some(DummyEff::READER_ID),
                _ => None,
            }
        }

        fn effect_member_sig(&self, _eff: EffectId, member: EffectOpId) -> TyScheme<InDb> {
            use crate::TypeKind::*;
            match member {
                // get: forall 0 . {} -{0}-> Int
                DummyEff::GET_ID => TyScheme {
                    bound: vec![TyVarId(0)],
                    constrs: vec![],
                    eff: Row::Open(TyVarId(0)),
                    ty: self
                        .0
                        .mk_ty(FunTy(self.0.empty_row_ty(), self.0.mk_ty(IntTy))),
                },
                // put: forall 0 . Int -{0}-> {}
                DummyEff::PUT_ID => TyScheme {
                    bound: vec![TyVarId(0)],
                    constrs: vec![],
                    eff: Row::Open(TyVarId(0)),
                    ty: self
                        .0
                        .mk_ty(FunTy(self.0.mk_ty(IntTy), self.0.empty_row_ty())),
                },
                // ask: forall 0 1. {} -{0}-> 1
                DummyEff::ASK_ID => TyScheme {
                    bound: vec![TyVarId(0), TyVarId(1)],
                    constrs: vec![],
                    eff: Row::Open(TyVarId(0)),
                    ty: self.0.mk_ty(FunTy(
                        self.0.empty_row_ty(),
                        self.0.mk_ty(VarTy(TyVarId(1))),
                    )),
                },
                _ => unimplemented!(),
            }
        }

        fn effect_member_name(&self, _eff: EffectId, member: EffectOpId) -> Ident {
            match member {
                DummyEff::GET_ID => self.0.ident_str("get"),
                DummyEff::PUT_ID => self.0.ident_str("put"),
                DummyEff::ASK_ID => self.0.ident_str("ask"),
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use aiahr_core::{
        ast::{Direction, Term::*},
        id::{EffectId, EffectOpId, ModuleId, TyVarId, VarId},
        ty::{
            row::{ClosedRow, Row},
            AccessTy, TypeKind,
            TypeKind::*,
        },
    };
    use aiahr_test::ast::{AstBuilder, MkTerm};
    use assert_matches::assert_matches;
    use bumpalo::Bump;

    use crate::type_check;
    use crate::{diagnostic::TypeCheckDiagnostic, test_utils::DummyEff, Evidence};

    macro_rules! assert_matches_unit_ty {
        ($db:expr, $term:expr) => {
            assert_matches!($db.kind($term), TypeKind::ProdTy(Row::Closed(ClosedRow { fields, values })) => {
                assert!(fields.fields($db).is_empty());
                assert!(values.values($db).is_empty());
            });
        }
    }

    macro_rules! assert_vec_matches {
        ($vec: expr, [$($elem:pat),*]) => {{
            let mut tmp = $vec;
            tmp.sort();
            assert_matches!(tmp.as_slice(), [$($elem),*]);
        }};
        ($vec: expr, [$($elem:pat),*] => $body:expr) => {{
            let mut tmp = $vec;
            tmp.sort();
            assert_matches!(tmp.as_slice(), [$($elem),*] => $body)
        }};
    }

    #[derive(Default)]
    #[salsa::db(crate::Jar, aiahr_core::Jar)]
    pub(crate) struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    #[test]
    fn test_tc_unlabel() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abs(
                x,
                builder.mk_unlabel("start", builder.mk_label("start", Variable(x))),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        let db = &db;
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret) => {
                assert_matches!((db.kind(arg), db.kind(ret)), (VarTy(a), VarTy(b)) => {
                    assert_eq!(a, b);
                });
            }
        );
    }

    #[test]
    fn test_tc_unlabel_fails_on_wrong_label() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abs(
                x,
                builder.mk_unlabel("start", builder.mk_label("end", Variable(x))),
            )
        });

        let (_, _, _, errors) = type_check(&db, &DummyEff(&db), &untyped_ast);

        assert_matches!(
            errors[0],
            // TODO: Figure out how to check these errors
            TypeCheckDiagnostic {
                name: "Type Mismatch",
                principal: _
            }
        );
    }

    #[test]
    fn test_tc_label() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abs(x, builder.mk_label("start", Variable(x)))
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        let db = &db;
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret)
            => {
                assert_matches!((db.kind(arg), db.kind(ret)),
                    (VarTy(a), RowTy(ClosedRow { fields, values })) => {
                        assert_eq!(fields.fields(db).get(0).map(|start| start.text(db).as_str()), Some("start"));
                        assert_eq!(values.values(db).get(0).map(|val| db.kind(val)), Some(&VarTy(*a)));
                });
            }
        );
    }

    #[test]
    fn test_tc_abs() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let y = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abs(x, builder.mk_abs(y, Variable(x)))
        });

        let (var_to_tys, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        let db = &db;
        assert_matches!(
            var_to_tys.get(&VarId(0)).map(|ty| db.kind(ty)),
            Some(&VarTy(TyVarId(0)))
        );
        assert_matches!(
            var_to_tys.get(&VarId(1)).map(|ty| db.kind(ty)),
            Some(&VarTy(TyVarId(1)))
        );
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret) => {
                assert_matches!((db.kind(arg), db.kind(ret)), (VarTy(a), FunTy(arg, ret)) => {
                    assert_matches!((db.kind(arg), db.kind(ret)), (VarTy(_), VarTy(b)) => {
                        assert_eq!(a, b);
                    })
                })
            }
        );
    }

    #[test]
    fn test_tc_sum_literal() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let t = VarId(0);
        let f = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_branch(
                builder.mk_abs(t, builder.mk_unlabel("true", Variable(t))),
                builder.mk_abs(f, builder.mk_unlabel("false", Variable(f))),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            let ty_var = assert_matches!(
                db.kind(arg),
                SumTy(Row::Closed(ClosedRow { fields, values })) => {
                    assert_matches!(fields.fields(db).as_slice(), [true_, false_] => {
                        assert_eq!(false_.text(db), "false");
                        assert_eq!(true_.text(db), "true");
                    });
                    assert_matches!(values.values(db).as_slice(), [a, b] => {
                        assert_eq!(a, b);
                        a
                    })
                }
            );
            assert_eq!(ret, ty_var);
        });
    }

    #[test]
    fn test_tc_product_literal() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abs(
                x,
                builder.mk_concat(
                    builder.mk_concat(
                        builder.mk_label("a", Variable(x)),
                        builder.mk_label("b", Variable(x)),
                    ),
                    builder.mk_concat(
                        builder.mk_label("c", Variable(x)),
                        builder.mk_label("d", Variable(x)),
                    ),
                ),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            assert_matches!(db.kind(ret), ProdTy(Row::Closed(ClosedRow { fields, values })) => {
                assert_matches!(fields.fields(db).as_slice(), [a, b, c, d] => {
                    assert_eq!(a.text(db), "a");
                    assert_eq!(b.text(db), "b");
                    assert_eq!(c.text(db), "c");
                    assert_eq!(d.text(db), "d");
                });
                assert_matches!(values.values(db).as_slice(), [a, b, c, d] => {
                    assert_eq!(a, arg);
                    assert_eq!(b, arg);
                    assert_eq!(c, arg);
                    assert_eq!(d, arg);
                })
            })
        });
    }

    #[test]
    fn test_tc_product_wand() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let m = VarId(0);
        let n = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_abss(
                [m, n],
                builder.mk_unlabel(
                    "x",
                    builder.mk_project(
                        Direction::Right,
                        builder.mk_concat(Variable(m), Variable(n)),
                    ),
                ),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        let ty = assert_vec_matches!(
            scheme.constrs,
            [
                Evidence::Row {
                    left: Row::Open(_),
                    right: Row::Open(_),
                    goal: Row::Open(b)
                },
                Evidence::Row {
                    left: Row::Closed(ClosedRow { fields, values }),
                    right: Row::Open(_),
                    goal: Row::Open(a)
                }
            ] => {
                assert_eq!(a, b);
                assert_matches!(fields.fields(&db).as_slice(), [x] => {
                    assert_eq!(x.text(&db), "x");
                });
                assert_matches!(values.values(&db).as_slice(), [ty] => ty)
            }
        );
        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
            assert_matches!(db.kind(ret), FunTy(arg, ret) => {
                assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
                assert_eq!(ret, ty)
            })
        });
    }

    #[test]
    fn test_tc_applied_wand() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let m = VarId(0);
        let n = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_app(
                builder.mk_abss(
                    [m, n],
                    builder.mk_unlabel(
                        "x",
                        builder.mk_project(
                            Direction::Right,
                            builder.mk_concat(Variable(m), Variable(n)),
                        ),
                    ),
                ),
                builder.mk_label("x", Unit),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        assert_vec_matches!(
            scheme.constrs,
            [Evidence::Row {
                left: Row::Closed(ClosedRow { fields, values }),
                right: Row::Open(_),
                goal: Row::Open(_),
            }] => {
                assert_matches!(fields.fields(&db).as_slice(), [x] => {
                    assert_eq!(x.text(&db), "x");
                });
                assert_matches!(values.values(&db).as_slice(), [unit] => {
                    assert_matches_unit_ty!(&db, unit);
                });
            }
        );

        let db = &db;
        assert_matches!(db.kind(&scheme.ty), FunTy(arg, ret) => {
            assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
            assert_matches_unit_ty!(db, &ret);
        })
    }

    #[test]
    fn test_tc_eff_operation_infers_correct_effect() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_app(Operation((ModuleId(0), EffectId(0), EffectOpId(0))), Unit)
        });
        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), &untyped_ast);

        assert_matches!(scheme.eff, Row::Closed(ClosedRow { fields, values }) => {
            assert_matches!(fields.fields(&db).as_slice(), [state] => {
                assert_eq!(state.text(&db), "State");
            });
            assert_matches!(values.values(&db).as_slice(), [unit] => {
                assert_matches_unit_ty!(&db, unit);
            });
        });
        let db = &db;
        assert_matches!(db.kind(&scheme.ty), IntTy);
    }

    #[test]
    fn test_tc_eff_handler_removes_correct_effect() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let untyped_ast = AstBuilder::with_builder(&db, &arena, |builder| {
            builder.mk_handler(
                builder.mk_concat(
                    builder.mk_concat(
                        builder.mk_label(
                            "get",
                            builder.mk_abss(
                                [VarId(0), VarId(3)],
                                builder.mk_app(Variable(VarId(3)), Int(3)),
                            ),
                        ),
                        builder.mk_label(
                            "put",
                            builder.mk_abss(
                                [VarId(1), VarId(3)],
                                builder.mk_app(Variable(VarId(3)), Unit),
                            ),
                        ),
                    ),
                    builder.mk_label("return", builder.mk_abs(VarId(2), Variable(VarId(2)))),
                ),
                builder.mk_app(
                    Operation((ModuleId(0), DummyEff::STATE_ID, DummyEff::PUT_ID)),
                    builder.mk_app(
                        Operation((ModuleId(0), DummyEff::READER_ID, DummyEff::ASK_ID)),
                        Unit,
                    ),
                ),
            )
        });

        let (_, _, scheme, errors) = type_check(&db, &DummyEff(&db), &untyped_ast);

        assert_eq!(errors, vec![]);
        assert_matches!(
            scheme.eff,
            Row::Closed(ClosedRow { fields, values }/*row!([reader], [ty!(ty_pat!({}))])*/) => {
                assert_matches!(fields.fields(&db).as_slice(), [reader] => {
                    assert_eq!(reader.text(&db), "Reader");
                });
                assert_matches!(values.values(&db).as_slice(), [unit] => {
                    assert_matches_unit_ty!(&db, unit);
                });
            }
        );
        let db = &db;
        assert_matches!(db.kind(&scheme.ty), RowTy(ClosedRow { fields, values }) => {
            assert!(fields.fields(db).is_empty());
            assert!(values.values(db).is_empty());
        });
    }

    #[test]
    fn test_tc_undefined_var_fails() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let untyped_ast = AstBuilder::with_builder(&db, &arena, |_| Variable(VarId(0)));

        let (_, _, _, errors) = type_check(&db, &DummyEff(&db), &untyped_ast);

        assert_matches!(
            &errors[0],
            TypeCheckDiagnostic {
                name: "Undefined Variable",
                principal: _,
            }
        );
    }
}
