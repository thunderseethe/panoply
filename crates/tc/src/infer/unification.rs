use super::*;
/// This trait covers the bulk of our work in the constraint solving stage.
/// Unification proceeds by decomposing types and rows and unifying their composite types
/// and rows. For ease of use we capture these various possible combinations as impls of this
/// trait.
pub(super) trait Unify<'infer, L, R> {
  /// Synytactically unify L and R. Returns Ok(()) if they are unifiable, otherwise returns an error.
  fn unify(&mut self, left: L, right: R) -> Result<(), TypeCheckError<'infer>>;
}

impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, TypeK>, InferTy<'infer>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  /// Unify a variable and a type.
  /// This checks that the variable is not present in type, throwing an error if varaibles is
  /// present.
  /// If not we record that the unification variable is solved to given type.
  fn unify(
    &mut self,
    var: TcUnifierVar<'infer, TypeK>,
    ty: InferTy<'infer>,
  ) -> Result<(), TypeCheckError<'infer>> {
    let ty_ = ty
      .try_fold_with(&mut OccursCheck { ctx: self.ctx, var })
      .map_err(|var| TypeCheckError::TypeOccursCheckFailed(var, ty))?;
    log::info!("{:?} ~~ {}", var, ty.pretty_string(&(self.db, ()), 80));
    self
      .ty_unifiers
      .unify_var_value(var, Some(ty_))
      .map_err(|e| e.into())
  }
}
impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, TypeK>, TcUnifierVar<'infer, TypeK>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: TcUnifierVar<'infer, TypeK>,
    right: TcUnifierVar<'infer, TypeK>,
  ) -> Result<(), TypeCheckError<'infer>> {
    log::info!("{:?} ~~ {:?}", left, right,);
    self
      .ty_unifiers
      .unify_var_var(left, right)
      .map_err(From::from)
  }
}

impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, SimpleRowK>, TcUnifierVar<'infer, SimpleRowK>>
  for InferCtx<'_, 'infer, I, Solution>
{
  fn unify(
    &mut self,
    left: TcUnifierVar<'infer, SimpleRowK>,
    right: TcUnifierVar<'infer, SimpleRowK>,
  ) -> Result<(), TypeCheckError<'infer>> {
    log::info!("{:?} ~~ {:?}", left, right,);
    self
      .data_row_unifiers
      .unify_var_var(left, right)
      .map_err(From::from)
  }
}
impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, SimpleRowK>, SimpleClosedRow<InArena<'infer>>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    var: TcUnifierVar<'infer, SimpleRowK>,
    row: SimpleClosedRow<InArena<'infer>>,
  ) -> Result<(), TypeCheckError<'infer>> {
    let row_ = row
      .try_fold_with(&mut OccursCheck { ctx: self.ctx, var })
      .map_err(|var| TypeCheckError::DataRowOccursCheckFailed(var, row))?;
    self.dispatch_solved::<Simple>(var, row)?;
    log::info!("{:?} ~~ {}", var, row.pretty_string(&(self.db, ()), 80));
    self
      .data_row_unifiers
      .unify_var_value(var, Some(row_))
      .map_err(|err| TypeCheckError::DataRowsNotEqual(Row::Closed(err.0), Row::Closed(err.1)))
  }
}
impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, SimpleRowK>, SimpleInferRow<'infer>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: TcUnifierVar<'infer, SimpleRowK>,
    right: SimpleInferRow<'infer>,
  ) -> Result<(), TypeCheckError<'infer>> {
    match right {
      Row::Open(var) => self.unify(left, var),
      Row::Closed(row) => self.unify(left, row),
    }
  }
}
impl<'infer, I> Unify<'infer, SimpleInferRow<'infer>, SimpleClosedRow<InArena<'infer>>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: SimpleInferRow<'infer>,
    right: SimpleClosedRow<InArena<'infer>>,
  ) -> Result<(), TypeCheckError<'infer>> {
    match left {
      Row::Open(var) => self.unify(var, right),
      Row::Closed(row) => self.unify(row, right),
    }
  }
}
impl<'infer, I> Unify<'infer, SimpleClosedRow<InArena<'infer>>, SimpleClosedRow<InArena<'infer>>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: SimpleClosedRow<InArena<'infer>>,
    right: SimpleClosedRow<InArena<'infer>>,
  ) -> Result<(), TypeCheckError<'infer>> {
    if !left.is_unifiable(right) {
      return Err((left, right).into());
    }

    for (left, right) in left.values(&*self).iter().zip(right.values(&*self).iter()) {
      self.unify(*left, *right)?;
    }

    Ok(())
  }
}
impl<'infer, I> Unify<'infer, SimpleInferRow<'infer>, SimpleInferRow<'infer>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: SimpleInferRow<'infer>,
    right: SimpleInferRow<'infer>,
  ) -> Result<(), TypeCheckError<'infer>> {
    match (left, right) {
      (Row::Open(left), Row::Open(right)) => self.unify(left, right),
      (Row::Open(var), Row::Closed(row)) | (Row::Closed(row), Row::Open(var)) => {
        self.unify(var, row)
      }
      (Row::Closed(left), Row::Closed(right)) => self.unify(left, right),
    }
  }
}

impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, ScopedRowK>, TcUnifierVar<'infer, ScopedRowK>>
  for InferCtx<'_, 'infer, I, Solution>
{
  fn unify(
    &mut self,
    left: TcUnifierVar<'infer, ScopedRowK>,
    right: TcUnifierVar<'infer, ScopedRowK>,
  ) -> Result<(), TypeCheckError<'infer>> {
    log::info!("{:?} ~~ {:?}", left, right);
    self
      .eff_row_unifiers
      .unify_var_var(left, right)
      .map_err(From::from)
  }
}
impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, ScopedRowK>, ScopedClosedRow<InArena<'infer>>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    var: TcUnifierVar<'infer, ScopedRowK>,
    row: ScopedClosedRow<InArena<'infer>>,
  ) -> Result<(), TypeCheckError<'infer>> {
    let row_ = row
      .try_fold_with(&mut OccursCheck { ctx: self.ctx, var })
      .map_err(|var| TypeCheckError::EffectRowOccursCheckFailed(var, row))?;
    self.dispatch_solved::<Scoped>(var, row)?;
    log::info!("{:?} ~~ {}", var, row.pretty_string(&(self.db, ()), 80));
    self
      .eff_row_unifiers
      .unify_var_value(var, Some(row_))
      .map_err(|err| TypeCheckError::EffectRowsNotEqual(Row::Closed(err.0), Row::Closed(err.1)))
  }
}
impl<'infer, I> Unify<'infer, TcUnifierVar<'infer, ScopedRowK>, ScopedInferRow<'infer>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: TcUnifierVar<'infer, ScopedRowK>,
    right: ScopedInferRow<'infer>,
  ) -> Result<(), TypeCheckError<'infer>> {
    match right {
      Row::Open(var) => self.unify(left, var),
      Row::Closed(row) => self.unify(left, row),
    }
  }
}
impl<'infer, I> Unify<'infer, ScopedClosedRow<InArena<'infer>>, ScopedClosedRow<InArena<'infer>>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: ScopedClosedRow<InArena<'infer>>,
    right: ScopedClosedRow<InArena<'infer>>,
  ) -> Result<(), TypeCheckError<'infer>> {
    if !left.is_unifiable(right) {
      return Err((left, right).into());
    }

    for (left, right) in left.values(&*self).iter().zip(right.values(&*self).iter()) {
      self.unify(*left, *right)?;
    }

    Ok(())
  }
}
impl<'infer, I> Unify<'infer, ScopedInferRow<'infer>, ScopedClosedRow<InArena<'infer>>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: ScopedInferRow<'infer>,
    right: ScopedClosedRow<InArena<'infer>>,
  ) -> Result<(), TypeCheckError<'infer>> {
    match left {
      Row::Open(var) => self.unify(var, right),
      Row::Closed(row) => self.unify(row, right),
    }
  }
}
impl<'infer, I> Unify<'infer, ScopedInferRow<'infer>, ScopedInferRow<'infer>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: ScopedInferRow<'infer>,
    right: ScopedInferRow<'infer>,
  ) -> Result<(), TypeCheckError<'infer>> {
    match (left, right) {
      (Row::Open(left), Row::Open(right)) => self.unify(left, right),
      (Row::Open(var), Row::Closed(row)) | (Row::Closed(row), Row::Open(var)) => {
        self.unify(var, row)
      }
      (Row::Closed(left), Row::Closed(right)) => self.unify(left, right),
    }
  }
}

/// This is the main entry point of unificaiton and handles unifying two arbitrary types.
/// Each type is substituted by the current substitution to remove as many unification
/// variables as possible before unifying.
impl<'infer, I> Unify<'infer, InferTy<'infer>, InferTy<'infer>>
  for InferCtx<'_, 'infer, I, Solution>
where
  I: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
{
  fn unify(
    &mut self,
    left: InferTy<'infer>,
    right: InferTy<'infer>,
  ) -> Result<(), TypeCheckError<'infer>> {
    let left = self.normalize_ty(left);
    let right = self.normalize_ty(right);

    log::info!(
      "{} ~~ {}",
      left.pretty_string(&(self.db, ()), 80),
      right.pretty_string(&(self.db, ()), 80)
    );

    match (*left, *right) {
      // If an error appears anywhere fail unification
      (ErrorTy, _) | (_, ErrorTy) => Err((left, right).into()),

      // Special case for when two keys meet
      // Instead of unifiying either variable as a value of the other, we need to record that
      // the two key's equivalence classes must be the same.
      (VarTy(left_var), VarTy(right_var)) => self.unify(left_var, right_var),

      // If a key meets a new ty record they must be equal
      (_, VarTy(var)) => self.unify(var, left),
      (VarTy(var), _) => self.unify(var, right),

      // Coerce a product into a row
      (RowTy(left), ProdTy(right)) => self.unify(Row::Closed(left), right),
      (ProdTy(left), RowTy(right)) => self.unify(left, Row::Closed(right)),

      // Coerce a sum into a row
      (RowTy(left), SumTy(right)) => self.unify(Row::Closed(left), right),
      (SumTy(left), RowTy(right)) => self.unify(left, Row::Closed(right)),

      // Decompose compound types
      (FunTy(left_arg, left_eff, left_ret), FunTy(right_arg, right_eff, right_ret)) => {
        self.unify(left_arg, right_arg)?;
        self.unify(left_eff, right_eff)?;
        self.unify(left_ret, right_ret)
      }
      (RowTy(left_row), RowTy(right_row)) => self.unify(left_row, right_row),
      (ProdTy(left), ProdTy(right)) => self.unify(left, right),
      (SumTy(left), SumTy(right)) => self.unify(left, right),
      // Discharge equal types
      (IntTy, IntTy) => Ok(()),

      // Type mismatch
      (IntTy, FunTy(_, _, _))
      | (IntTy, RowTy(_))
      | (IntTy, ProdTy(_))
      | (IntTy, SumTy(_))
      | (FunTy(_, _, _), IntTy)
      | (FunTy(_, _, _), RowTy(_))
      | (FunTy(_, _, _), ProdTy(_))
      | (FunTy(_, _, _), SumTy(_))
      | (RowTy(_), IntTy)
      | (RowTy(_), FunTy(_, _, _))
      | (ProdTy(_), IntTy)
      | (ProdTy(_), FunTy(_, _, _))
      | (ProdTy(_), SumTy(_))
      | (SumTy(_), IntTy)
      | (SumTy(_), FunTy(_, _, _))
      | (SumTy(_), ProdTy(_)) => Err((left, right).into()),
    }
  }
}
