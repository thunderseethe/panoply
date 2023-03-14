use aiahr_core::{
    ir::{IrTy, IrTyKind, IrTyKind::*},
    memory::intern::{Interner, InternerByRef, SyncInterner},
};
use bumpalo::Bump;

pub(crate) struct IrCtx<'ctx> {
    tys: SyncInterner<'ctx, IrTyKind<'ctx>, Bump>,
    tys_slices: SyncInterner<'ctx, [IrTy<'ctx>], Bump>,
}

impl<'ctx> IrCtx<'ctx> {
    #[allow(dead_code)]
    pub(crate) fn new(arena: &'ctx Bump) -> Self {
        Self {
            tys: SyncInterner::new(arena),
            tys_slices: SyncInterner::new(arena),
        }
    }
}

pub trait MkIrTy<'ctx> {
    fn mk_ir_ty(&self, kind: IrTyKind<'ctx>) -> IrTy<'ctx>;
    fn mk_prod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx>;
    fn mk_coprod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx>;

    fn mk_binary_fun_ty<F, S, R>(&self, fst_arg: F, snd_arg: S, ret: R) -> IrTy<'ctx>
    where
        F: IntoIrTy<'ctx>,
        S: IntoIrTy<'ctx>,
        R: IntoIrTy<'ctx>,
    {
        self.mk_ir_ty(FunTy(
            fst_arg.into_ir_ty(self),
            self.mk_ir_ty(FunTy(snd_arg.into_ir_ty(self), ret.into_ir_ty(self))),
        ))
    }
}
pub trait IntoIrTy<'ctx> {
    fn into_ir_ty<I: ?Sized + MkIrTy<'ctx>>(self, ctx: &I) -> IrTy<'ctx>;
}
impl<'ctx> IntoIrTy<'ctx> for IrTy<'ctx> {
    fn into_ir_ty<I: ?Sized + MkIrTy<'ctx>>(self, _ctx: &I) -> IrTy<'ctx> {
        self
    }
}
impl<'ctx> IntoIrTy<'ctx> for IrTyKind<'ctx> {
    fn into_ir_ty<I: ?Sized + MkIrTy<'ctx>>(self, ctx: &I) -> IrTy<'ctx> {
        ctx.mk_ir_ty(self)
    }
}

impl<'ctx> MkIrTy<'ctx> for IrCtx<'ctx> {
    fn mk_ir_ty(&self, kind: IrTyKind<'ctx>) -> IrTy<'ctx> {
        IrTy::new(self.tys.intern(kind))
    }

    fn mk_prod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx> {
        IrTy::new(
            self.tys
                .intern(IrTyKind::ProductTy(self.tys_slices.intern_by_ref(elems))),
        )
    }

    fn mk_coprod_ty(&self, elems: &[IrTy<'ctx>]) -> IrTy<'ctx> {
        IrTy::new(
            self.tys
                .intern(IrTyKind::CoproductTy(self.tys_slices.intern_by_ref(elems))),
        )
    }
}
