use super::{default_fold_tykind, FoldReducIrTy, ReducIrRow, ReducIrTy, ReducIrTyKind};

pub(super) struct Env<T> {
    env: Vec<(i32, Option<T>)>,
}

impl<T> Env<T> {
    pub(super) fn nil() -> Self {
        Self { env: vec![] }
    }

    pub(super) fn with_entry((i, ty): (i32, T)) -> Self {
        Self {
            env: vec![(i, Some(ty))],
        }
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
pub(super) struct Subst<'db, Needle> {
    pub(super) db: &'db dyn crate::Db,
    pub(super) i: i32,
    pub(super) j: i32,
    pub(super) env: Env<Needle>,
}

impl<T> Subst<'_, T> {
    fn with_forall<R>(&mut self, body: impl FnOnce(&mut Self) -> R) -> R {
        self.env.env.push((self.j, None));
        self.j += 1;
        self.i += 1;
        let out = body(self);
        self.i -= 1;
        self.j -= 1;
        self.env.env.pop();
        out
    }

    /// Adjust an unbound var based on how many foralls we've moved it under
    fn adjust_unbound_var(&self, var: i32) -> i32 {
        var - self.i + self.j
    }

    fn adjust_bound_var(&self, var: i32) -> i32 {
        self.j - var - 1
    }
    /// Get nth variable out of environment and adjust it based on how many foralls we've moved
    /// under during substitution
    fn get_bound_var(&self, nth: i32) -> i32 {
        let (bound, _) = self.env.get(nth);
        self.adjust_bound_var(*bound)
    }

    /// Adjust a bound or unbound variable based on how many foralls we've moved under and return
    /// its corresponding ReducIrTyKind
    ///
    /// This method never performs substitution
    fn adjust_var(&self, var: i32, var_kind: impl FnOnce(i32) -> ReducIrTyKind) -> ReducIrTyKind {
        let is_bound = var < self.i;
        if is_bound {
            var_kind(self.get_bound_var(var))
        } else {
            var_kind(self.adjust_unbound_var(var))
        }
    }
}
impl<'db> Subst<'db, ReducIrTy> {
    fn has_subst(&self, var: i32) -> Option<(i32, ReducIrTy)> {
        let is_bound = var < self.i;
        if is_bound {
            let (bound, ty) = self.env.get(var);
            ty.map(|ty| (*bound, ty))
        } else {
            None
        }
    }
}
impl<'db> Subst<'db, ReducIrRow> {
    fn has_subst(
        &self,
        var: i32,
        row_to_ty: impl FnOnce(&ReducIrRow) -> ReducIrTyKind,
    ) -> Option<(i32, ReducIrTyKind)> {
        let is_bound = var < self.i;
        if is_bound {
            let (bound, row) = self.env.get(var);
            row.as_ref().map(|row| (*bound, row_to_ty(row)))
        } else {
            None
        }
    }
}
impl<'db> FoldReducIrTy<'db> for Subst<'db, ReducIrTy> {
    fn db(&self) -> &'db dyn crate::Db {
        self.db
    }

    fn fold_prod_var(&mut self, var: i32) -> ReducIrTy {
        self.mk_ty(self.adjust_var(var, ReducIrTyKind::ProdVarTy))
    }

    fn fold_coprod_var(&mut self, var: i32) -> ReducIrTy {
        self.mk_ty(self.adjust_var(var, ReducIrTyKind::CoprodVarTy))
    }

    fn fold_ty_var(&mut self, var: i32) -> ReducIrTy {
        match self.has_subst(var) {
            Some((bound, ty)) => ty.fold(&mut Subst::<'db, ReducIrTy> {
                db: self.db(),
                i: 0,
                j: self.j - bound,
                env: Env::nil(),
            }),
            None => self.mk_ty(self.adjust_var(var, ReducIrTyKind::VarTy)),
        }
    }

    fn fold_ty_kind(&mut self, kind: ReducIrTyKind) -> ReducIrTy {
        match kind {
            ReducIrTyKind::ForallTy(kind, ty) => {
                let ty = self.with_forall(|this| ty.fold(this));
                self.mk_ty(ReducIrTyKind::ForallTy(kind, ty))
            }
            kind => default_fold_tykind(self, kind),
        }
    }

    fn fold_ty(&mut self, ty: ReducIrTy) -> ReducIrTy {
        if self.i == 0 && self.j == 0 && self.env.is_empty() {
            return ty;
        }
        self.fold_ty_kind(ty.kind(self.db()))
    }
}

impl<'db> FoldReducIrTy<'db> for Subst<'db, ReducIrRow> {
    fn db(&self) -> &'db dyn crate::Db {
        self.db
    }

    fn fold_ty_var(&mut self, var: i32) -> ReducIrTy {
        self.mk_ty(self.adjust_var(var, ReducIrTyKind::VarTy))
    }

    fn fold_prod_var(&mut self, var: i32) -> ReducIrTy {
        match self.has_subst(var, |row| match row {
            ReducIrRow::Open(v) => ReducIrTyKind::ProdVarTy(*v),
            ReducIrRow::Closed(row) => ReducIrTyKind::ProductTy(row.clone()),
        }) {
            Some((bound, kind)) => {
                let mut fold = Subst::<'db, ReducIrTy> {
                    db: self.db(),
                    i: 0,
                    j: self.j - bound,
                    env: Env::nil(),
                };
                fold.fold_ty_kind(kind)
            }
            None => self.mk_ty(self.adjust_var(var, ReducIrTyKind::ProdVarTy)),
        }
    }

    fn fold_coprod_var(&mut self, var: i32) -> ReducIrTy {
        match self.has_subst(var, |row| match row {
            ReducIrRow::Open(v) => ReducIrTyKind::CoprodVarTy(*v),
            ReducIrRow::Closed(row) => ReducIrTyKind::CoproductTy(row.clone()),
        }) {
            Some((bound, kind)) => {
                let mut fold = Subst::<'db, ReducIrTy> {
                    db: self.db(),
                    i: 0,
                    j: self.j - bound,
                    env: Env::nil(),
                };
                fold.fold_ty_kind(kind)
            }
            None => self.mk_ty(self.adjust_var(var, ReducIrTyKind::CoprodVarTy)),
        }
    }

    fn fold_ty_kind(&mut self, kind: ReducIrTyKind) -> ReducIrTy {
        match kind {
            ReducIrTyKind::ForallTy(kind, ty) => {
                let ty = self.with_forall(|this| ty.fold(this));
                self.mk_ty(ReducIrTyKind::ForallTy(kind, ty))
            }
            kind => default_fold_tykind(self, kind),
        }
    }

    fn fold_ty(&mut self, ty: ReducIrTy) -> ReducIrTy {
        if self.i == 0 && self.j == 0 && self.env.is_empty() {
            return ty;
        }
        self.fold_ty_kind(ty.kind(self.db()))
    }
}
