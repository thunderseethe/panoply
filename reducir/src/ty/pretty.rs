use base::pretty::PrettyWithCtx;
use pretty::{docs, DocAllocator, DocBuilder, Pretty};

use super::{Kind, ReducIrRow, ReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrVarTy};

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

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for ReducIrTyKind {
    fn pretty<'a>(
        &self,
        db: &DB,
        a: &'a pretty::RcAllocator,
    ) -> DocBuilder<'a, pretty::RcAllocator> {
        match self {
            ReducIrTyKind::IntTy => a.text("Int"),
            ReducIrTyKind::VarTy(ty_var) => a.text("T").append(a.as_string(ty_var)),
            ReducIrTyKind::ProdVarTy(ty_var) => a.as_string(ty_var).braces(),
            ReducIrTyKind::CoprodVarTy(ty_var) => a.as_string(ty_var).angles(),
            ReducIrTyKind::FunTy(args, ret) => {
                let docs = args.iter().map(|arg| {
                    let mut arg_doc = arg.pretty_with(db).pretty(a);
                    if let ReducIrTyKind::FunTy(_, _) = arg.kind(db.as_reducir_db()) {
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
            /*ReducIrTyKind::ExistsTy(kind, ty) => {
                let preamble = a
                    .text("exists")
                    .append(a.space())
                    .append(kind.pretty(a))
                    .append(a.space())
                    .append(a.text("."));

                let single_line = a.space().append(ty.pretty(db, a));
                let multi_line = a.line().append(ty.pretty(db, a)).nest(2);
                preamble.append(multi_line.flat_alt(single_line).group())
            }*/
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
                        tys.iter().map(|ty| ty.pretty(db, a)),
                        a.line().append(",").append(a.space()),
                    ))
                    .append(a.line())
                    .append("}")
                    .align();

                multi_line.flat_alt(single_line).group()
            }
            ReducIrTyKind::CoproductTy(tys) => a
                .intersperse(tys.iter().map(|ty| ty.pretty(db, a)), ",")
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

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for ReducIrRow {
    fn pretty<'a>(
        &self,
        db: &DB,
        a: &'a pretty::RcAllocator,
    ) -> DocBuilder<'a, pretty::RcAllocator> {
        match self {
            ReducIrRow::Open(var) => a.as_string(var),
            ReducIrRow::Closed(row) => a
                .intersperse(row.iter().map(|ty| ty.pretty(db, a)), ",")
                .brackets(),
        }
    }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for ReducIrTyApp {
    fn pretty<'a>(
        &self,
        db: &DB,
        a: &'a pretty::RcAllocator,
    ) -> DocBuilder<'a, pretty::RcAllocator> {
        match self {
            ReducIrTyApp::Ty(ty) => a.text("Ty").append(ty.pretty_with(db).pretty(a).parens()),
            ReducIrTyApp::DataRow(simp) => a.text("Data").append(simp.pretty(db, a).parens()),
            ReducIrTyApp::EffRow(scope) => a.text("Eff").append(scope.pretty(db, a).parens()),
        }
    }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for ReducIrTy {
    fn pretty<'a>(
        &self,
        db: &DB,
        a: &'a pretty::RcAllocator,
    ) -> DocBuilder<'a, pretty::RcAllocator> {
        self.kind(db.as_reducir_db()).pretty(db, a)
    }
}
