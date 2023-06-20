use aiahr_core::id::ReducIrTyVarId;
use aiahr_ty::row::{RowSema, Scoped, Simple};
use pretty::{docs, DocAllocator, DocBuilder, Pretty};

/// The kind of a type variable
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Kind {
    Type,
    SimpleRow,
    ScopedRow,
}

impl<'a, D, A> Pretty<'a, D, A> for &Kind
where
    A: 'a,
    D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, a: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            Kind::Type => a.text("Type"),
            Kind::SimpleRow => a.text("SimpleRow"),
            Kind::ScopedRow => a.text("ScopedRow"),
        }
    }
}

pub trait RowReducIrKind: RowSema {
    fn kind() -> Kind;
}
impl RowReducIrKind for Simple {
    fn kind() -> Kind {
        Kind::SimpleRow
    }
}
impl RowReducIrKind for Scoped {
    fn kind() -> Kind {
        Kind::ScopedRow
    }
}

/// An ir type variable and it's kind
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReducIrVarTy {
    pub var: ReducIrTyVarId,
    pub kind: Kind,
}

impl<'a, D, A> Pretty<'a, D, A> for ReducIrVarTy
where
    A: 'a,
    D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, a: &'a D) -> DocBuilder<'a, D, A> {
        docs![a, "T", a.as_string(self.var.0)]
    }
}
impl ReducIrVarTy {
    pub(crate) fn pretty_with_kind<'a, D, A>(self, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
    {
        docs![a, "T", a.as_string(self.var.0), ":", a.space(), &self.kind,].parens()
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ReducIrTyKind {
    IntTy,
    NeverTy,
    VarTy(ReducIrVarTy),
    ProdVarTy(ReducIrVarTy),
    CoprodVarTy(ReducIrVarTy),
    FunTy(ReducIrTy, ReducIrTy),
    ForallTy(ReducIrVarTy, ReducIrTy),
    ProductTy(Vec<ReducIrTy>),
    CoproductTy(Vec<ReducIrTy>),
}

impl ReducIrTyKind {
    fn pretty<'a, D, DB>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DB: ?Sized + crate::Db,
    {
        match self {
            ReducIrTyKind::IntTy => a.text("Int"),
            ReducIrTyKind::NeverTy => a.text("Never"),
            ReducIrTyKind::VarTy(ty_var) => ty_var.pretty(a),
            ReducIrTyKind::ProdVarTy(ty_var) => ty_var.pretty(a).braces(),
            ReducIrTyKind::CoprodVarTy(ty_var) => ty_var.pretty(a).angles(),
            ReducIrTyKind::FunTy(arg, ret) => {
                let mut arg_doc = arg.pretty(db, a);
                if let ReducIrTyKind::FunTy(_, _) = arg.kind(db.as_ir_db()) {
                    arg_doc = arg_doc.parens();
                }
                arg_doc
                    .append(a.softline())
                    .append(a.text("->"))
                    .append(a.softline())
                    .append(ret.pretty(db, a))
            }
            ReducIrTyKind::ForallTy(ty_var, ty) => a
                .text("forall")
                .append(a.space())
                .append(ty_var.pretty(a))
                .append(a.space())
                .append(a.text("."))
                .append(a.softline())
                .append(ty.pretty(db, a)),
            ReducIrTyKind::ProductTy(tys) => a
                .intersperse(tys.into_iter().map(|ty| ty.pretty(db, a)), ",")
                .braces(),
            ReducIrTyKind::CoproductTy(tys) => a
                .intersperse(tys.into_iter().map(|ty| ty.pretty(db, a)), ",")
                .angles(),
        }
    }
}

#[salsa::interned]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ReducIrTy {
    pub kind: ReducIrTyKind,
}

impl ReducIrTy {
    /// Equate our types based on their data, this does a semantic equality check. This allows for
    /// a looser definition of equality for our types then the Id based equality we use for `Eq`.
    pub fn ty_eq(&self, db: &dyn crate::Db, rhs: &Self) -> bool {
        match (self.kind(db), rhs.kind(db)) {
            (_, ReducIrTyKind::NeverTy) => true,
            (ReducIrTyKind::NeverTy, _) => true,
            (_, _) => self == rhs,
        }
    }

    fn fold<F>(self, db: &dyn crate::Db, visit: &mut F) -> Self
    where
        F: FnMut(&ReducIrTyKind) -> Option<Self>,
    {
        let kind = self.kind(db);
        match kind {
            ReducIrTyKind::IntTy
            | ReducIrTyKind::NeverTy
            | ReducIrTyKind::VarTy(_)
            | ReducIrTyKind::ProdVarTy(_)
            | ReducIrTyKind::CoprodVarTy(_) => visit(&kind).unwrap_or(self),
            ReducIrTyKind::FunTy(arg, ret) => {
                let arg = arg.fold(db, visit);
                let ret = ret.fold(db, visit);
                visit(&ReducIrTyKind::FunTy(arg, ret))
                    .unwrap_or_else(|| db.mk_reducir_ty(ReducIrTyKind::FunTy(arg, ret)))
            }
            ReducIrTyKind::ForallTy(var, body) => {
                let k = ReducIrTyKind::ForallTy(var, body.fold(db, visit));
                visit(&k).unwrap_or_else(|| db.mk_reducir_ty(k))
            }
            ReducIrTyKind::ProductTy(elems) => {
                let elems = elems
                    .into_iter()
                    .map(|e| e.fold(db, visit))
                    .collect::<Vec<_>>();
                let prod = db.mk_prod_ty(&elems);
                visit(&ReducIrTyKind::ProductTy(elems)).unwrap_or(prod)
            }
            ReducIrTyKind::CoproductTy(elems) => {
                let elems = elems
                    .into_iter()
                    .map(|e| e.fold(db, visit))
                    .collect::<Vec<_>>();
                let coprod = db.mk_coprod_ty(&elems);
                visit(&ReducIrTyKind::CoproductTy(elems)).unwrap_or(coprod)
            }
        }
    }

    pub(crate) fn subst_ty(
        self,
        db: &dyn crate::Db,
        needle: ReducIrVarTy,
        ty: ReducIrTy,
    ) -> ReducIrTy {
        self.fold(db, &mut |kind| match kind {
            ReducIrTyKind::VarTy(var) => (*var == needle).then_some(ty),
            _ => None,
        })
    }

    pub(crate) fn subst_row(
        self,
        db: &dyn crate::Db,
        needle: ReducIrVarTy,
        row: &ReducIrRow,
    ) -> ReducIrTy {
        self.fold(db, &mut |kind| match kind {
            ReducIrTyKind::ProdVarTy(var) => (*var == needle).then(|| match &row {
                ReducIrRow::Open(row_var) => db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(*row_var)),
                ReducIrRow::Closed(tys) => db.mk_prod_ty(tys),
            }),
            ReducIrTyKind::CoprodVarTy(var) => (*var == needle).then(|| match &row {
                ReducIrRow::Open(row_var) => db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(*row_var)),
                ReducIrRow::Closed(tys) => db.mk_coprod_ty(tys),
            }),
            _ => None,
        })
    }

    //TODO: Figure out how to substitute row variables into types
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ReducIrRow {
    Open(ReducIrVarTy),
    Closed(Vec<ReducIrTy>),
}
impl ReducIrRow {
    fn pretty<'a, D, DB>(&self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            ReducIrRow::Open(var) => var.pretty(a),
            ReducIrRow::Closed(row) => a
                .intersperse(row.iter().map(|ty| ty.pretty(db, a)), ",")
                .brackets(),
        }
    }
}

// We allow Rows in type applications because they might show up in constraints.
// But we want to ensure they don't appear in our ReducIr types outside of that so we make a specific type
// for it
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ReducIrTyApp {
    Ty(ReducIrTy),
    DataRow(ReducIrRow),
    EffRow(ReducIrRow),
}
impl ReducIrTyApp {
    pub(crate) fn pretty<'a, D, DB>(&self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            ReducIrTyApp::Ty(ty) => ty.pretty(db, a),
            ReducIrTyApp::DataRow(simp) => simp.pretty(db, a),
            ReducIrTyApp::EffRow(scope) => scope.pretty(db, a),
        }
    }
}

impl ReducIrTy {
    pub(crate) fn pretty<'a, D, DB>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DB: ?Sized + crate::Db,
    {
        self.kind(db.as_ir_db()).pretty(db, a)
    }
}

pub trait MkReducIrTy {
    fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy;
    fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy;
    fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy;

    fn mk_binary_fun_ty(
        &self,
        fst_arg: impl IntoReducIrTy,
        snd_arg: impl IntoReducIrTy,
        ret: impl IntoReducIrTy,
    ) -> ReducIrTy {
        use ReducIrTyKind::*;
        self.mk_reducir_ty(FunTy(
            fst_arg.into_reducir_ty(self),
            self.mk_reducir_ty(FunTy(
                snd_arg.into_reducir_ty(self),
                ret.into_reducir_ty(self),
            )),
        ))
    }
}
pub trait IntoReducIrTy {
    fn into_reducir_ty<I: ?Sized + MkReducIrTy>(self, ctx: &I) -> ReducIrTy;
}
impl IntoReducIrTy for ReducIrTy {
    fn into_reducir_ty<I: ?Sized + MkReducIrTy>(self, _ctx: &I) -> ReducIrTy {
        self
    }
}
impl IntoReducIrTy for ReducIrTyKind {
    fn into_reducir_ty<I: ?Sized + MkReducIrTy>(self, ctx: &I) -> ReducIrTy {
        ctx.mk_reducir_ty(self)
    }
}

impl<DB> MkReducIrTy for DB
where
    DB: ?Sized + crate::Db,
{
    fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy {
        ReducIrTy::new(self.as_ir_db(), kind)
    }

    fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
        self.mk_reducir_ty(ReducIrTyKind::ProductTy(elems.to_owned()))
    }

    fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
        self.mk_reducir_ty(ReducIrTyKind::CoproductTy(elems.to_owned()))
    }
}
