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
        docs![a, "T", a.as_string(self.var.0), ":", a.space(), &self.kind].parens()
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ReducIrTyKind {
    IntTy,
    VarTy(i32),
    ProdVarTy(i32),
    CoprodVarTy(i32),
    FunTy(Box<[ReducIrTy]>, ReducIrTy),
    ForallTy(Kind, ReducIrTy),
    ProductTy(Vec<ReducIrTy>),
    CoproductTy(Vec<ReducIrTy>),
    // TODO: Figure out how to not build this in
    MarkerTy(ReducIrTy),
    /// Our delimited continuation monad type.
    /// It's specialized as a type to handle recursion without full support for recursive types.a
    ControlTy(ReducIrTy, ReducIrTy),
}

impl ReducIrTyKind {
    fn pretty<'a, D, DB>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DocBuilder<'a, D>: Clone,
        DB: ?Sized + crate::Db,
    {
        match self {
            ReducIrTyKind::IntTy => a.text("Int"),
            ReducIrTyKind::VarTy(ty_var) => a.text("T").append(a.as_string(ty_var)),
            ReducIrTyKind::ProdVarTy(ty_var) => a.as_string(ty_var).braces(),
            ReducIrTyKind::CoprodVarTy(ty_var) => a.as_string(ty_var).angles(),
            ReducIrTyKind::FunTy(args, ret) => {
                let docs = args.iter().map(|arg| {
                    let mut arg_doc = arg.pretty(db, a);
                    if let ReducIrTyKind::FunTy(_, _) = arg.kind(db.as_ir_db()) {
                        arg_doc = arg_doc.parens();
                    }
                    arg_doc
                });
                a.intersperse(docs, a.text("->").enclose(a.softline(), a.softline()))
                    .append(a.text("->").enclose(a.softline(), a.softline()))
                    .append(ret.pretty(db, a))
            }
            ReducIrTyKind::ForallTy(kind, ty) => {
                let preamble = a
                    .text("forall")
                    .append(a.space())
                    .append(kind.pretty(a))
                    .append(a.space())
                    .append(a.text("."));

                let single_line = a.space().append(ty.pretty(db, a));
                let multi_line = a.line().append(ty.pretty(db, a)).nest(2);
                preamble.append(multi_line.flat_alt(single_line).group())
            }
            ReducIrTyKind::ProductTy(tys) => {
                // I don't understand layout rules well enough to avoid this special case
                if tys.is_empty() {
                    return a.text("{}");
                }
                let single_line = a
                    .intersperse(tys.iter().map(|ty| ty.pretty(db, a)), ", ")
                    .braces();
                let multi_line = a
                    .text("{")
                    .append(a.space())
                    .append(a.intersperse(
                        tys.into_iter().map(|ty| ty.pretty(db, a)),
                        a.line().append(",").append(a.space()),
                    ))
                    .append(a.line())
                    .append("}")
                    .align();

                multi_line.flat_alt(single_line).group()
            }
            ReducIrTyKind::CoproductTy(tys) => a
                .intersperse(tys.into_iter().map(|ty| ty.pretty(db, a)), ",")
                .angles(),
            ReducIrTyKind::MarkerTy(ret) => a
                .text("Marker")
                .append(a.space())
                .append(ret.pretty(db, a))
                .parens(),
            ReducIrTyKind::ControlTy(evv, t) => a
                .text("Control")
                .append(a.space())
                .append(evv.pretty(db, a))
                .append(a.space())
                .append(t.pretty(db, a))
                .parens(),
        }
    }
}

#[salsa::interned]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ReducIrTy {
    pub kind: ReducIrTyKind,
}

pub struct UnwrapMonTy {
    pub evv_ty: ReducIrTy,
    pub a_ty: ReducIrTy,
}
impl ReducIrTy {
    /// Unwrap a monadic type into it's evv type and value type.
    pub fn try_unwrap_monadic(self, db: &dyn crate::Db) -> Result<UnwrapMonTy, Self> {
        // Monadic type is evv -> Control evv a
        // Unwrap and return (evv, a) from Control type
        match self.kind(db) {
            ReducIrTyKind::FunTy(args, ret) if args.len() == 1 => match ret.kind(db) {
                ReducIrTyKind::ControlTy(evv_ty, a_ty) if args[0] == evv_ty => {
                    Ok(UnwrapMonTy { evv_ty, a_ty })
                }
                _ => Err(self),
            },
            _ => Err(self),
        }
    }
}

struct Env<T> {
    env: Vec<(i32, Option<T>)>,
}
impl<T: Clone + std::fmt::Debug> Env<T> {
    fn nil() -> Self {
        Self { env: vec![] }
    }

    fn with_entry((i, ty): (i32, T)) -> Self {
        Self {
            env: vec![(i, Some(ty))],
        }
    }

    fn with<R>(&mut self, j: i32, body: impl FnOnce(&mut Self) -> R) -> R {
        self.env.push((j, None));
        let ret = body(self);
        self.env.pop();
        ret
    }

    fn get(&self, n: i32) -> &(i32, Option<T>) {
        // Index from the end of our vec
        // because we're using it as a stack
        let (res, overflow) = self.env.len().overflowing_sub(1);
        if overflow {
            panic!("Env length is 0");
        }
        let (indx, overflow) = res.overflowing_sub(n as usize);
        if overflow {
            panic!("env length is less than {}", n);
        }
        &self.env[indx]
    }

    fn is_empty(&self) -> bool {
        self.env.is_empty()
    }
}
impl ReducIrTy {
    /// Assume `self` is a forall and reduce it by applying `ty` as it's argument.
    /// This applies type substitution without having to create a TyApp ReducIr node.
    pub fn reduce_forall(self, db: &dyn crate::Db, ty: ReducIrTy) -> ReducIrTy {
        match self.kind(db) {
            ReducIrTyKind::ForallTy(Kind::Type, ret_ty) => ret_ty.subst_ty(db, ty),
            _ => panic!("reduce_forall called on non forall type"),
        }
    }

    pub(crate) fn subst_ty(self, db: &dyn crate::Db, ty: ReducIrTy) -> ReducIrTy {
        fn subst_aux(
            ty: ReducIrTy,
            db: &dyn crate::Db,
            i: i32,
            j: i32,
            env: &mut Env<ReducIrTy>,
        ) -> ReducIrTy {
            if i == 0 && j == 0 && env.is_empty() {
                return ty;
            }
            match ty.kind(db) {
                ReducIrTyKind::IntTy => ty,
                ReducIrTyKind::ProdVarTy(n) => {
                    if n >= i {
                        db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(n - i + j))
                    } else {
                        let (j_, _) = env.get(n);
                        db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(j - j_ - 1))
                    }
                }
                ReducIrTyKind::CoprodVarTy(n) => {
                    if n >= i {
                        db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(n - i + j))
                    } else {
                        let (j_, _) = env.get(n);
                        db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(j - j_ - 1))
                    }
                }
                ReducIrTyKind::VarTy(n) => {
                    if n >= i {
                        db.mk_reducir_ty(ReducIrTyKind::VarTy(n - i + j))
                    } else {
                        let (j_, ty) = env.get(n);
                        match ty {
                            None => db.mk_reducir_ty(ReducIrTyKind::VarTy(j - j_ - 1)),
                            Some(ty) => subst_aux(*ty, db, 0, j - j_, &mut Env::nil()),
                        }
                    }
                }
                ReducIrTyKind::FunTy(args, ret) => {
                    let ret = subst_aux(ret, db, i, j, env);
                    db.mk_fun_ty(args.iter().map(|arg| subst_aux(*arg, db, i, j, env)), ret)
                }
                ReducIrTyKind::ForallTy(kind, body) => db.mk_reducir_ty(ReducIrTyKind::ForallTy(
                    kind,
                    env.with(j, |env| subst_aux(body, db, i + 1, j + 1, env)),
                )),
                ReducIrTyKind::ProductTy(elems) => db.mk_reducir_ty(ReducIrTyKind::ProductTy(
                    elems
                        .into_iter()
                        .map(|ty| subst_aux(ty, db, i, j, env))
                        .collect(),
                )),
                ReducIrTyKind::CoproductTy(elems) => db.mk_reducir_ty(ReducIrTyKind::CoproductTy(
                    elems
                        .into_iter()
                        .map(|ty| subst_aux(ty, db, i, j, env))
                        .collect(),
                )),
                ReducIrTyKind::MarkerTy(ret) => {
                    db.mk_reducir_ty(ReducIrTyKind::MarkerTy(subst_aux(ret, db, i, j, env)))
                }
                ReducIrTyKind::ControlTy(evv, t) => db.mk_reducir_ty(ReducIrTyKind::ControlTy(
                    subst_aux(evv, db, i, j, env),
                    subst_aux(t, db, i, j, env),
                )),
            }
        }

        subst_aux(self, db, 1, 0, &mut Env::with_entry((0, ty)))
    }

    pub(crate) fn subst_row(self, db: &dyn crate::Db, row: ReducIrRow) -> ReducIrTy {
        fn subst_aux(
            ty: ReducIrTy,
            db: &dyn crate::Db,
            i: i32,
            j: i32,
            env: &mut Env<ReducIrRow>,
        ) -> ReducIrTy {
            if i == 0 && j == 0 && env.is_empty() {
                return ty;
            }
            match ty.kind(db) {
                ReducIrTyKind::IntTy => ty,
                ReducIrTyKind::ProdVarTy(n) => {
                    if n >= i {
                        db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(n - i + j))
                    } else {
                        let (j_, row) = env.get(n);
                        match row {
                            None => db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(j - j_)),
                            Some(row) => {
                                let ty = match row {
                                    ReducIrRow::Open(v) => {
                                        db.mk_reducir_ty(ReducIrTyKind::ProdVarTy(*v))
                                    }
                                    ReducIrRow::Closed(row) => db.mk_prod_ty(row.as_slice()),
                                };
                                subst_aux(ty, db, 0, j - j_, &mut Env::nil())
                            }
                        }
                    }
                }
                ReducIrTyKind::CoprodVarTy(n) => {
                    if n >= i {
                        db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(n - i + j))
                    } else {
                        let (j_, row) = env.get(n);
                        match row {
                            None => db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(j - j_)),
                            Some(row) => {
                                let ty = match row {
                                    ReducIrRow::Open(v) => {
                                        db.mk_reducir_ty(ReducIrTyKind::CoprodVarTy(*v))
                                    }
                                    ReducIrRow::Closed(row) => db.mk_coprod_ty(row.as_slice()),
                                };
                                subst_aux(ty, db, 0, j - j_, &mut Env::nil())
                            }
                        }
                    }
                }
                ReducIrTyKind::VarTy(n) => {
                    if n >= i {
                        db.mk_reducir_ty(ReducIrTyKind::VarTy(n - i + j))
                    } else {
                        let (j_, _) = env.get(n);
                        db.mk_reducir_ty(ReducIrTyKind::VarTy(j - j_))
                    }
                }
                ReducIrTyKind::FunTy(args, ret) => {
                    let ret = subst_aux(ret, db, i, j, env);
                    db.mk_fun_ty(args.iter().map(|arg| subst_aux(*arg, db, i, j, env)), ret)
                }
                ReducIrTyKind::ForallTy(kind, body) => db.mk_reducir_ty(ReducIrTyKind::ForallTy(
                    kind,
                    env.with(j, |env| subst_aux(body, db, i + 1, j + 1, env)),
                )),
                ReducIrTyKind::ProductTy(elems) => db.mk_reducir_ty(ReducIrTyKind::ProductTy(
                    elems
                        .into_iter()
                        .map(|ty| subst_aux(ty, db, i, j, env))
                        .collect(),
                )),
                ReducIrTyKind::CoproductTy(elems) => db.mk_reducir_ty(ReducIrTyKind::CoproductTy(
                    elems
                        .into_iter()
                        .map(|ty| subst_aux(ty, db, i, j, env))
                        .collect(),
                )),
                ReducIrTyKind::MarkerTy(ret) => {
                    db.mk_reducir_ty(ReducIrTyKind::MarkerTy(subst_aux(ret, db, i, j, env)))
                }
                ReducIrTyKind::ControlTy(evv, t) => db.mk_reducir_ty(ReducIrTyKind::ControlTy(
                    subst_aux(evv, db, i, j, env),
                    subst_aux(t, db, i, j, env),
                )),
            }
        }

        subst_aux(self, db, 1, 0, &mut Env::with_entry((0, row)))
    }

    /// Shift all the variables in a term by delta.
    pub fn shift(self, db: &dyn crate::Db, delta: i32) -> Self {
        fn shift_aux(ty: ReducIrTy, db: &dyn crate::Db, delta: i32, bound: i32) -> ReducIrTy {
            use ReducIrTyKind::*;
            match ty.kind(db) {
                IntTy => ty,
                VarTy(var) if var >= bound => db.mk_reducir_ty(VarTy(var + delta)),
                ProdVarTy(var) if var >= bound => db.mk_reducir_ty(ProdVarTy(var + delta)),
                CoprodVarTy(var) if var >= bound => db.mk_reducir_ty(CoprodVarTy(var + delta)),
                // If the variable is bound don't shift it
                VarTy(_) | ProdVarTy(_) | CoprodVarTy(_) => ty,
                FunTy(args, ret) => db.mk_fun_ty(
                    args.iter().map(|arg| arg.shift(db, delta)),
                    ret.shift(db, delta),
                ),
                ForallTy(kind, body) => {
                    db.mk_reducir_ty(ForallTy(kind, shift_aux(body, db, delta, bound + 1)))
                }
                ProductTy(elems) => db.mk_reducir_ty(ProductTy(
                    elems
                        .into_iter()
                        .map(|ty| shift_aux(ty, db, delta, bound))
                        .collect(),
                )),
                CoproductTy(elems) => db.mk_reducir_ty(CoproductTy(
                    elems
                        .into_iter()
                        .map(|ty| shift_aux(ty, db, delta, bound))
                        .collect(),
                )),
                MarkerTy(ret) => {
                    db.mk_reducir_ty(ReducIrTyKind::MarkerTy(shift_aux(ret, db, delta, bound)))
                }
                ControlTy(evv, t) => db.mk_reducir_ty(ReducIrTyKind::ControlTy(
                    shift_aux(evv, db, delta, bound),
                    shift_aux(t, db, delta, bound),
                )),
            }
        }

        shift_aux(self, db, delta, 0)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ReducIrRow {
    Open(i32),
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
            ReducIrRow::Open(var) => a.as_string(var),
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
    pub fn pretty<'a, D, DB>(self, db: &DB, a: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        DocBuilder<'a, D>: Clone,
        DB: ?Sized + crate::Db,
    {
        self.kind(db.as_ir_db()).pretty(db, a)
    }
}

pub trait MkReducIrTy {
    fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy;
    fn mk_fun_ty(
        &self,
        args: impl IntoIterator<Item = impl IntoReducIrTy>,
        ret: impl IntoReducIrTy,
    ) -> ReducIrTy;
    fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy;
    fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy;

    fn mk_forall_ty<I>(&self, kinds: I, ty: impl IntoReducIrTy) -> ReducIrTy
    where
        I: IntoIterator<Item = Kind>,
        I::IntoIter: DoubleEndedIterator,
    {
        kinds
            .into_iter()
            .rfold(ty.into_reducir_ty(self), |ty, kind| {
                self.mk_reducir_ty(ReducIrTyKind::ForallTy(kind, ty))
            })
    }

    fn mk_mon_ty(&self, evv_ty: impl IntoReducIrTy, a_ty: impl IntoReducIrTy) -> ReducIrTy {
        let evv_ty = evv_ty.into_reducir_ty(self);
        let a_ty = a_ty.into_reducir_ty(self);
        self.mk_fun_ty(
            [evv_ty],
            self.mk_reducir_ty(ReducIrTyKind::ControlTy(evv_ty, a_ty)),
        )
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

    fn mk_fun_ty(
        &self,
        args: impl IntoIterator<Item = impl IntoReducIrTy>,
        ret: impl IntoReducIrTy,
    ) -> ReducIrTy {
        let mut args = args
            .into_iter()
            .map(|arg| arg.into_reducir_ty(self))
            .peekable();
        let ret = ret.into_reducir_ty(self);
        // If we have no args don't output a function type
        if args.peek().is_none() {
            ret
        } else {
            match ret.kind(self.as_ir_db()) {
                ReducIrTyKind::FunTy(iargs, ret) => self.mk_reducir_ty(ReducIrTyKind::FunTy(
                    args.chain(iargs.iter().copied()).collect(),
                    ret,
                )),
                _ => self.mk_reducir_ty(ReducIrTyKind::FunTy(args.collect(), ret)),
            }
        }
    }
}
