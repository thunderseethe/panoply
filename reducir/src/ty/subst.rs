use aiahr_core::pretty::{PrettyPrint, PrettyWithCtx};
use pretty::DocAllocator;

use super::{
    default_fold_tykind, FoldReducIrTy, MkReducIrTy, ReducIrRow, ReducIrTy, ReducIrTyApp,
    ReducIrTyKind,
};

#[derive(Debug, Clone)]
pub enum SubstPayload {
    Var(i32),
    Ty(ReducIrTy),
    // Only closed row
    Row(Vec<ReducIrTy>),
}

impl<DB> PrettyWithCtx<DB> for SubstPayload
where
    DB: ?Sized + crate::Db,
{
    fn pretty<'a>(
        &self,
        ctx: &DB,
        alloc: &'a pretty::RcAllocator,
    ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
        match self {
            SubstPayload::Var(v) => alloc.text("V").append(alloc.as_string(v)),
            SubstPayload::Ty(ty) => ty.pretty(ctx, alloc),
            SubstPayload::Row(row) => ReducIrRow::Closed(row.to_vec()).pretty(ctx, alloc),
        }
    }
}

pub trait IntoPayload {
    fn into_payload<DB: ?Sized + crate::Db>(self, db: &DB) -> SubstPayload;
}
impl IntoPayload for ReducIrTy {
    fn into_payload<DB: ?Sized + crate::Db>(self, db: &DB) -> SubstPayload {
        match self.kind(db.as_reducir_db()) {
            ReducIrTyKind::VarTy(var) => SubstPayload::Var(var),
            _ => SubstPayload::Ty(self),
        }
    }
}
impl IntoPayload for ReducIrRow {
    fn into_payload<DB: ?Sized + crate::Db>(self, _: &DB) -> SubstPayload {
        match self {
            ReducIrRow::Open(var) => SubstPayload::Var(var),
            ReducIrRow::Closed(row) => SubstPayload::Row(row),
        }
    }
}
impl IntoPayload for ReducIrTyApp {
    fn into_payload<DB: ?Sized + crate::Db>(self, db: &DB) -> SubstPayload {
        match self {
            ReducIrTyApp::Ty(ty) => ty.into_payload(db),
            ReducIrTyApp::DataRow(row) | ReducIrTyApp::EffRow(row) => row.into_payload(db),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Subst {
    Inc(i32),
    Ext(SubstPayload, Box<Self>),
    Compose(Box<Self>, Box<Self>),
}
impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for Subst {
    fn pretty<'a>(
        &self,
        ctx: &DB,
        alloc: &'a pretty::RcAllocator,
    ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
        match self {
            Subst::Inc(i) => alloc
                .text("inc")
                .append(alloc.space())
                .append(alloc.as_string(i)),
            Subst::Ext(payload, subst) => alloc.text("ext").append(
                payload
                    .pretty(ctx, alloc)
                    .append(",")
                    .append(alloc.softline())
                    .append(subst.pretty(ctx, alloc))
                    .parens(),
            ),
            Subst::Compose(s1, s2) => s1
                .pretty(ctx, alloc)
                .append(alloc.softline())
                .append(".")
                .append(alloc.softline())
                .append(s2.pretty(ctx, alloc)),
        }
    }
}

impl Subst {
    pub fn single(payload: SubstPayload) -> Self {
        Subst::Inc(0).cons(payload)
    }

    pub fn cons(self, payload: SubstPayload) -> Self {
        Self::Ext(payload, Box::new(self))
    }

    fn compose(s1: Self, s2: Self) -> Self {
        Self::Compose(Box::new(s1), Box::new(s2))
    }

    pub fn lift(self) -> Self {
        Self::compose(self, Self::Inc(1)).cons(SubstPayload::Var(0))
    }

    fn apply_ty(&self, db: &dyn crate::Db, var: i32) -> ReducIrTy {
        match self {
            Subst::Inc(k) => db.mk_reducir_ty(ReducIrTyKind::VarTy(var + k)),
            Subst::Ext(payload, _) if var == 0 => match payload {
                SubstPayload::Var(v) => db.mk_reducir_ty(ReducIrTyKind::VarTy(*v)),
                SubstPayload::Ty(ty) => *ty,
                SubstPayload::Row(_) => panic!("Tried to subsitute a row into a type"),
            },
            Subst::Ext(_, subst) => subst.apply_ty(db, var - 1),
            Subst::Compose(s1, s2) => {
                let ty = s1.apply_ty(db, var);
                let mut s2_subst = SubstFold {
                    db,
                    subst: (**s2).clone(),
                };
                s2_subst.fold_ty(ty)
            }
        }
    }

    fn apply_row(&self, db: &dyn crate::Db, var: i32) -> ReducIrRow {
        match self {
            Subst::Inc(k) => ReducIrRow::Open(var + k),
            Subst::Ext(payload, _) if var == 0 => match payload {
                SubstPayload::Var(v) => ReducIrRow::Open(*v),
                SubstPayload::Row(row) => ReducIrRow::Closed(row.clone()),
                SubstPayload::Ty(ty) => panic!(
                    "Tried to substitute type into a row: {}",
                    ty.pretty_with(db).pprint().pretty(80)
                ),
            },
            Subst::Ext(_, subst) => subst.apply_row(db, var - 1),
            Subst::Compose(s1, s2) => {
                let row = s1.apply_row(db, var);
                let mut s2_subst = SubstFold {
                    db,
                    subst: (**s2).clone(),
                };
                s2_subst.fold_row(row)
            }
        }
    }
}

pub struct SubstFold<'db> {
    pub db: &'db dyn crate::Db,
    pub subst: Subst,
}

impl<'db> SubstFold<'db> {
    fn fold_row(&mut self, row: ReducIrRow) -> ReducIrRow {
        match row {
            ReducIrRow::Open(var) => self.subst.apply_row(self.db, var),
            ReducIrRow::Closed(tys) => {
                ReducIrRow::Closed(tys.into_iter().map(|ty| self.fold_ty(ty)).collect())
            }
        }
    }
}

impl<'db> FoldReducIrTy<'db> for SubstFold<'db> {
    fn db(&self) -> &'db dyn crate::Db {
        self.db
    }

    fn fold_ty_var(&mut self, var: i32) -> ReducIrTy {
        self.subst.apply_ty(self.db, var)
    }

    fn fold_prod_var(&mut self, var: i32) -> ReducIrTy {
        self.subst.apply_row(self.db, var).into_prod_ty(self.db())
    }

    fn fold_coprod_var(&mut self, var: i32) -> ReducIrTy {
        self.subst.apply_row(self.db, var).into_coprod_ty(self.db())
    }

    fn fold_ty_kind(&mut self, kind: ReducIrTyKind) -> ReducIrTy {
        match kind {
            ReducIrTyKind::ForallTy(kind, ty) => {
                let ty = ty.fold(&mut Self {
                    db: self.db,
                    subst: self.subst.clone().lift(),
                });
                self.mk_ty(ReducIrTyKind::ForallTy(kind, ty))
            }
            kind => default_fold_tykind(self, kind),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ty::{Kind, MkReducIrTy, ReducIrTyKind};

    use super::{IntoPayload, Subst};

    #[derive(Default)]
    #[salsa::db(crate::Jar, aiahr_core::Jar, aiahr_ty::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    #[test]
    fn test_payload_prodvars_are_adjusted() {
        let db = TestDatabase::default();
        let haystack = db.mk_reducir_ty(ReducIrTyKind::ForallTy(
            Kind::Type,
            db.mk_reducir_ty(ReducIrTyKind::VarTy(1)),
        ));
        let needle = db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(0));

        let res = haystack.subst(&db, Subst::single(needle.into_payload(&db)));

        assert_eq!(
            res,
            db.mk_reducir_ty(ReducIrTyKind::ForallTy(
                Kind::Type,
                db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(1))
            ))
        );
    }
}
