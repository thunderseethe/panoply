use super::*;

/// A row predicate `left ⊙ right ~ goal`
/// Read: `left` combines with `right` to equal `goal`
pub(super) struct RowCombination<Row> {
  pub(super) left: Row,
  pub(super) right: Row,
  pub(super) goal: Row,
}
pub(super) type SimpleRowCombination<'infer> = RowCombination<SimpleRow<InArena<'infer>>>;
pub(super) type ScopedRowCombination<'infer> = RowCombination<ScopedRow<InArena<'infer>>>;

pub(super) trait IntoTypeCheckerError<'ctx> {
  fn into_tychk_err<I: MkTy<InArena<'ctx>>>(self, ctx: &I) -> TypeCheckError<'ctx>;
}

impl<'ctx> IntoTypeCheckerError<'ctx> for RowsNotDisjoint<'ctx, InferTy<'ctx>> {
  fn into_tychk_err<I: MkTy<InArena<'ctx>>>(self, ctx: &I) -> TypeCheckError<'ctx> {
    let left = ctx.mk_row(self.left.0, self.left.1);
    let right = ctx.mk_row(self.right.0, self.right.1);
    TypeCheckError::RowsNotDisjoint(left, right, self.label)
  }
}

impl<'ctx> IntoTypeCheckerError<'ctx> for Infallible {
  fn into_tychk_err<I: MkTy<InArena<'ctx>>>(self, _ctx: &I) -> TypeCheckError<'ctx> {
    unreachable!()
  }
}

/// RowTheory defines how we solve rows during unification
/// All our row predicates take the form of `left ⊙ right ~ goal`.
/// To solve such predicates we need to be able to calculate 3 things about our rows:
///  * Combine left and right to infer goal (`combine`)
///  * Difference goal and left to infer right (`diff_left`)
///  * Difference goal and right to infer left (`diff_right`)
/// If we know how to do these 3 things we can solve row predicates for a given row semantics.
///
/// Our final helper method is `match_eqn` this determines if a row combination could unify against
/// an unsolved row equation. This is used to detech when we shouldn't add a new unsolved row
/// equation but simply unify against an existing one in our unsolved set.
pub(super) trait RowTheory: RowSema + Sized {
  type Error<'ctx>: IntoTypeCheckerError<'ctx>;

  fn combine<'ctx>(
    left: &Self::Closed<InArena<'ctx>>,
    right: &Self::Closed<InArena<'ctx>>,
  ) -> Result<RowInternals<InArena<'ctx>>, Self::Error<'ctx>>;

  fn diff_left<'ctx>(
    goal: &Self::Closed<InArena<'ctx>>,
    left: &Self::Closed<InArena<'ctx>>,
  ) -> RowInternals<InArena<'ctx>>;

  fn diff_right<'ctx>(
    goal: &Self::Closed<InArena<'ctx>>,
    right: &Self::Closed<InArena<'ctx>>,
  ) -> RowInternals<InArena<'ctx>>;

  fn match_eqn<'ctx>(
    eqn: &UnsolvedRowEquation<InArena<'ctx>, Self>,
    rows: RowCombination<Row<Self, InArena<'ctx>>>,
  ) -> Option<RowCombination<Row<Self, InArena<'ctx>>>>;
}

impl RowTheory for Simple {
  type Error<'ctx> = RowsNotDisjoint<'ctx, Ty<InArena<'ctx>>>;

  fn combine<'ctx>(
    left: &Self::Closed<InArena<'ctx>>,
    right: &Self::Closed<InArena<'ctx>>,
  ) -> Result<RowInternals<InArena<'ctx>>, Self::Error<'ctx>> {
    let left_fields = left.fields(());
    let left_values = left.values(());
    let right_fields = right.fields(());
    let right_values = right.values(());

    SimpleClosedRow::<InArena<'ctx>>::merge_rowlikes(
      (left_fields, left_values),
      (right_fields, right_values),
    )
  }

  fn diff_left<'ctx>(
    goal: &Self::Closed<InArena<'ctx>>,
    left: &Self::Closed<InArena<'ctx>>,
  ) -> RowInternals<InArena<'ctx>> {
    goal.difference(*left)
  }

  fn diff_right<'ctx>(
    goal: &Self::Closed<InArena<'ctx>>,
    right: &Self::Closed<InArena<'ctx>>,
  ) -> RowInternals<InArena<'ctx>> {
    goal.difference(*right)
  }

  fn match_eqn<'ctx>(
    eqn: &UnsolvedRowEquation<InArena<'ctx>, Self>,
    rows: RowCombination<Row<Self, InArena<'ctx>>>,
  ) -> Option<RowCombination<Row<Self, InArena<'ctx>>>> {
    match eqn {
      UnsolvedRowEquation::ClosedGoal(cand) => match rows {
        RowCombination {
          left: Row::Open(left),
          right: Row::Open(right),
          goal: Row::Closed(goal),
        } if (cand.goal.is_unifiable(goal) && (cand.left == left || cand.right == right))
          || (cand.left == left && cand.right == right) =>
        {
          Some(RowCombination {
            left: Row::Open(cand.left),
            right: Row::Open(cand.right),
            goal: Row::Closed(cand.goal),
          })
        }
        RowCombination {
          left: Row::Open(left),
          right: Row::Open(right),
          goal: Row::Closed(goal),
        } if (cand.goal.is_unifiable(goal) && (cand.left == right || cand.right == left))
          || (cand.left == right && cand.right == left) =>
        {
          Some(RowCombination {
            left: Row::Open(cand.right),
            right: Row::Open(cand.left),
            goal: Row::Closed(cand.goal),
          })
        }
        _ => None,
      },
      UnsolvedRowEquation::OpenGoal(OpenGoal {
        goal,
        ops: Operatives::OpenOpen { left, right },
      }) => match rows {
        RowCombination {
          left: Row::Open(left_var),
          right: Row::Open(right_var),
          goal: Row::Open(goal_var),
        } if (*goal == goal_var && (*left == left_var || *right == right_var))
          || (*left == left_var && *right == right_var) =>
        {
          Some(RowCombination {
            left: Row::Open(*left),
            right: Row::Open(*right),
            goal: Row::Open(*goal),
          })
        }
        RowCombination {
          left: Row::Open(left_var),
          right: Row::Open(right_var),
          goal: Row::Open(goal_var),
        } if (*goal == goal_var && (*left == right_var || *right == left_var))
          || (*left == right_var && *right == left_var) =>
        {
          Some(RowCombination {
            left: Row::Open(*right),
            right: Row::Open(*left),
            goal: Row::Open(*goal),
          })
        }
        _ => None,
      },
      UnsolvedRowEquation::OpenGoal(OpenGoal {
        goal,
        ops: Operatives::ClosedOpen { left, right },
      }) => {
        match rows {
          // Forward row combinations
          RowCombination {
            left: Row::Closed(left_row),
            right: Row::Open(right_var),
            goal: Row::Open(goal_var),
          } if (left.is_unifiable(left_row) && (*right == right_var || *goal == goal_var))
            || (*right == right_var && *goal == goal_var) =>
          {
            Some(RowCombination {
              left: Row::Closed(*left),
              right: Row::Open(*right),
              goal: Row::Open(*goal),
            })
          }
          // Reverse row combinations
          RowCombination {
            left: Row::Open(left_var),
            right: Row::Closed(right_row),
            goal: Row::Open(goal_var),
          } if (left.is_unifiable(right_row) && (*goal == goal_var || *right == left_var))
            || (*goal == goal_var && *right == left_var) =>
          {
            Some(RowCombination {
              left: Row::Open(*right),
              right: Row::Closed(*left),
              goal: Row::Open(*goal),
            })
          }
          // We didn't find a match
          _ => None,
        }
      }
      UnsolvedRowEquation::OpenGoal(OpenGoal {
        goal,
        ops: Operatives::OpenClosed { left, right },
      }) => {
        match rows {
          // Forward row combinations
          RowCombination {
            left: Row::Open(left_var),
            right: Row::Closed(right_row),
            goal: Row::Open(goal_var),
          } if (right.is_unifiable(right_row) && (*left == left_var || *goal == goal_var))
            || (*left == left_var && *goal == goal_var) =>
          {
            Some(RowCombination {
              left: Row::Open(*left),
              right: Row::Closed(*right),
              goal: Row::Open(*goal),
            })
          }
          // Reverse row combinations
          RowCombination {
            left: Row::Closed(left_row),
            right: Row::Open(right_var),
            goal: Row::Open(goal_var),
          } if (right.is_unifiable(left_row) && (*left == right_var || *goal == goal_var))
            || (*left == right_var && *goal == goal_var) =>
          {
            Some(RowCombination {
              left: Row::Closed(*right),
              right: Row::Open(*left),
              goal: Row::Open(*goal),
            })
          }
          _ => None,
        }
      }
    }
  }
}

impl RowTheory for Scoped {
  type Error<'ctx> = Infallible;

  fn combine<'ctx>(
    left: &Self::Closed<InArena<'ctx>>,
    right: &Self::Closed<InArena<'ctx>>,
  ) -> Result<RowInternals<InArena<'ctx>>, Self::Error<'ctx>> {
    let left_fields = left.fields(());
    let left_values = left.values(());
    let right_fields = right.fields(());
    let right_values = right.values(());

    Ok(ScopedClosedRow::<InArena<'ctx>>::merge_rowlikes(
      (left_fields, left_values),
      (right_fields, right_values),
    ))
  }

  fn diff_left<'ctx>(
    goal: &Self::Closed<InArena<'ctx>>,
    left: &Self::Closed<InArena<'ctx>>,
  ) -> RowInternals<InArena<'ctx>> {
    let goal_fields = goal.fields(());
    let goal_values = goal.values(());
    ScopedClosedRow::<InArena<'ctx>>::diff_left_rowlikes(
      (goal_fields, goal_values),
      left.fields(()),
    )
  }

  fn diff_right<'ctx>(
    goal: &Self::Closed<InArena<'ctx>>,
    right: &Self::Closed<InArena<'ctx>>,
  ) -> RowInternals<InArena<'ctx>> {
    let goal_fields = goal.fields(());
    let goal_values = goal.values(());

    ScopedClosedRow::<InArena<'ctx>>::diff_right_rowlikes(
      (goal_fields, goal_values),
      right.fields(()),
    )
  }

  fn match_eqn<'ctx>(
    eqn: &UnsolvedRowEquation<InArena<'ctx>, Self>,
    rows: RowCombination<Row<Self, InArena<'ctx>>>,
  ) -> Option<RowCombination<Row<Self, InArena<'ctx>>>> {
    match eqn {
      UnsolvedRowEquation::ClosedGoal(cand) => match rows {
        RowCombination {
          left: Row::Open(left),
          right: Row::Open(right),
          goal: Row::Closed(goal),
        } if (cand.goal.is_unifiable(goal) && (cand.left == left || cand.right == right))
          || (cand.left == left && cand.right == right) =>
        {
          Some(RowCombination {
            left: Row::Open(cand.left),
            right: Row::Open(cand.right),
            goal: Row::Closed(cand.goal),
          })
        }
        _ => None,
      },
      UnsolvedRowEquation::OpenGoal(OpenGoal { goal, ops }) => match ops {
        Operatives::OpenOpen { left, right } => match rows {
          RowCombination {
            left: Row::Open(left_var),
            right: Row::Open(right_var),
            goal: Row::Open(goal_var),
          } if (*goal == goal_var && (*left == left_var || *right == right_var))
            || (*left == left_var && *right == right_var) =>
          {
            Some(RowCombination {
              left: Row::Open(*left),
              right: Row::Open(*right),
              goal: Row::Open(*goal),
            })
          }
          _ => None,
        },
        Operatives::OpenClosed { left, right } => match rows {
          RowCombination {
            left: Row::Open(left_var),
            right: Row::Closed(right_row),
            goal: Row::Open(goal_var),
          } if (right.is_unifiable(right_row) && (*left == left_var || *goal == goal_var))
            || (*left == left_var && *goal == goal_var) =>
          {
            Some(RowCombination {
              left: Row::Open(*left),
              right: Row::Closed(*right),
              goal: Row::Open(*goal),
            })
          }
          _ => None,
        },
        Operatives::ClosedOpen { left, right } => match rows {
          RowCombination {
            left: Row::Closed(left_row),
            right: Row::Open(right_var),
            goal: Row::Open(goal_var),
          } if (left.is_unifiable(left_row) && (*right == right_var || *goal == goal_var))
            || (*right == right_var && *goal == goal_var) =>
          {
            Some(RowCombination {
              left: Row::Closed(*left),
              right: Row::Open(*right),
              goal: Row::Open(*goal),
            })
          }
          _ => None,
        },
      },
    }
  }
}

pub(super) trait RowEquationSolver<'ctx, Sema: RowSema>:
  Unify<'ctx, Sema::Open<InArena<'ctx>>, Sema::Closed<InArena<'ctx>>>
  + Unify<'ctx, Sema::Closed<InArena<'ctx>>, Sema::Closed<InArena<'ctx>>>
  + Unify<'ctx, Row<Sema, InArena<'ctx>>, Sema::Closed<InArena<'ctx>>>
  + Unify<'ctx, Row<Sema, InArena<'ctx>>, Row<Sema, InArena<'ctx>>>
{
  fn equations(&self) -> &BTreeSet<UnsolvedRowEquation<InArena<'ctx>, Sema>>;
  fn equations_mut(&mut self) -> &mut BTreeSet<UnsolvedRowEquation<InArena<'ctx>, Sema>>;

  fn with_equations<F>(&mut self, fun: F)
  where
    F: FnOnce(
      BTreeSet<UnsolvedRowEquation<InArena<'ctx>, Sema>>,
    ) -> BTreeSet<UnsolvedRowEquation<InArena<'ctx>, Sema>>;

  fn normalize_row(&mut self, row: Row<Sema, InArena<'ctx>>) -> Row<Sema, InArena<'ctx>>;
}
impl<'infer, I> RowEquationSolver<'infer, Simple> for InferCtx<'_, 'infer, I, Solution>
where
  I: AccessTy<'infer, InArena<'infer>> + MkTy<InArena<'infer>>,
{
  fn equations(&self) -> &BTreeSet<UnsolvedRowEquation<InArena<'infer>, Simple>> {
    &self.state.data_eqns
  }

  fn equations_mut(&mut self) -> &mut BTreeSet<UnsolvedRowEquation<InArena<'infer>, Simple>> {
    &mut self.state.data_eqns
  }

  fn with_equations<F>(&mut self, fun: F)
  where
    F: FnOnce(
      BTreeSet<UnsolvedRowEquation<InArena<'infer>, Simple>>,
    ) -> BTreeSet<UnsolvedRowEquation<InArena<'infer>, Simple>>,
  {
    self.state.data_eqns = fun(std::mem::take(&mut self.state.data_eqns));
  }

  fn normalize_row(&mut self, row: Row<Simple, InArena<'infer>>) -> Row<Simple, InArena<'infer>> {
    row
      .try_fold_with(&mut Normalize {
        ctx: self.ctx,
        ty_unifiers: &mut self.ty_unifiers,
        datarow_unifiers: &mut self.data_row_unifiers,
        effrow_unifiers: &mut self.eff_row_unifiers,
      })
      .unwrap()
  }
}
impl<'infer, I> RowEquationSolver<'infer, Scoped> for InferCtx<'_, 'infer, I, Solution>
where
  I: AccessTy<'infer, InArena<'infer>> + MkTy<InArena<'infer>>,
{
  fn equations(&self) -> &BTreeSet<UnsolvedRowEquation<InArena<'infer>, Scoped>> {
    &self.state.eff_eqns
  }

  fn equations_mut(&mut self) -> &mut BTreeSet<UnsolvedRowEquation<InArena<'infer>, Scoped>> {
    &mut self.state.eff_eqns
  }

  fn with_equations<F>(&mut self, fun: F)
  where
    F: FnOnce(
      BTreeSet<UnsolvedRowEquation<InArena<'infer>, Scoped>>,
    ) -> BTreeSet<UnsolvedRowEquation<InArena<'infer>, Scoped>>,
  {
    self.state.eff_eqns = fun(std::mem::take(&mut self.state.eff_eqns));
  }

  fn normalize_row(&mut self, row: Row<Scoped, InArena<'infer>>) -> Row<Scoped, InArena<'infer>> {
    row
      .try_fold_with(&mut Normalize {
        ctx: self.ctx,
        ty_unifiers: &mut self.ty_unifiers,
        datarow_unifiers: &mut self.data_row_unifiers,
        effrow_unifiers: &mut self.eff_row_unifiers,
      })
      .unwrap()
  }
}
