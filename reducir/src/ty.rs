use aiahr_core::id::ReducIrTyVarId;
use aiahr_ty::row::{ScopedRow, SimpleRow};
use aiahr_ty::PrettyType;
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
    VarTy(ReducIrVarTy),
    FunTy(ReducIrTy, ReducIrTy),
    ForallTy(ReducIrVarTy, ReducIrTy),
    ProductTy(Vec<ReducIrTy>),
    CoproductTy(Vec<ReducIrTy>),
}

impl ReducIrTyKind {
    fn pretty<'a, D, DB, A>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DB: ?Sized + crate::Db,
    {
        match self {
            ReducIrTyKind::IntTy => a.text("Int"),
            ReducIrTyKind::VarTy(ty_var) => ty_var.pretty(a),
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

// We allow Rows in type applications because they might show up in constraints.
// But we want to ensure they don't appear in our ReducIr types outside of that so we make a specific type
// for it
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ReducIrTyApp {
    Ty(ReducIrTy),
    DataRow(SimpleRow),
    EffRow(ScopedRow),
}
impl ReducIrTyApp {
    pub(crate) fn pretty<'a, D, DB, A>(&self, db: &DB, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
        DB: ?Sized + crate::Db,
        D::Doc: pretty::Pretty<'a, D, A> + Clone,
    {
        match self {
            ReducIrTyApp::Ty(ty) => ty.pretty(db, a),
            ReducIrTyApp::DataRow(simp) => simp.pretty(a, db, &db),
            ReducIrTyApp::EffRow(scope) => scope.pretty(a, db, &db),
        }
    }
}

impl ReducIrTy {
    pub(crate) fn pretty<'a, D, DB, A>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: DocAllocator<'a, A>,
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
