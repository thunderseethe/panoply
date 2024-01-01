use base::{
    id::{IdSupply, ReducIrTyVarId, ReducIrVarId, TermName},
    modules::Module,
    pretty::PrettyErrorWithDb,
};
use reducir::{
    mon::{MonReducIrItem, MonReducIrModule},
    optimized::{OptimizedReducIrItem, OptimizedReducIrModule},
    ty::{IntoPayload, Kind, MkReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrVarTy, Subst},
    zip_non_consuming::ZipNonConsuming,
    Lets, ReducIr, ReducIrFold, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrTyErr,
    ReducIrVar, TypeCheck,
};
use rustc_hash::FxHashMap;
use std::convert::Infallible;

use crate::subst::{Inline, SubstTy};

#[salsa::jar(db = Db)]
pub struct Jar(simple_reducir_item, simple_reducir_module);

pub trait Db: salsa::DbWithJar<Jar> + lower_reducir::Db {
    fn as_opt_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn simplify_reducir_for_name(&self, name: TermName) -> OptimizedReducIrItem {
        let reducir_item = self.lower_reducir_mon_item_of(name);
        self.simple_reducir_item(reducir_item)
    }

    fn simple_reducir_item(&self, item: MonReducIrItem) -> OptimizedReducIrItem {
        simple_reducir_item(self.as_opt_reducir_db(), item)
    }

    fn simple_reducir_module(&self, module: Module) -> OptimizedReducIrModule {
        let ir_module = self.lower_reducir_mon_module_of(module);
        simple_reducir_module(self.as_opt_reducir_db(), ir_module)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + lower_reducir::Db {}

#[salsa::tracked]
fn simple_reducir_item(db: &dyn crate::Db, item: MonReducIrItem) -> OptimizedReducIrItem {
    let ir = simplify(db, item);

    let term_name = item.name(db.as_reducir_db());
    OptimizedReducIrItem::new(db.as_reducir_db(), ReducIrTermName::Term(term_name), ir)
}

#[salsa::tracked]
fn simple_reducir_module(
    db: &dyn crate::Db,
    ir_module: MonReducIrModule,
) -> OptimizedReducIrModule {
    let reducir_db = db.as_reducir_db();
    OptimizedReducIrModule::new(
        db.as_reducir_db(),
        ir_module.module(reducir_db),
        ir_module
            .items(reducir_db)
            .iter()
            .map(|item| db.simple_reducir_item(*item))
            .collect(),
    )
}

mod subst {
    use std::convert::Infallible;

    use reducir::ty::{ReducIrRow, ReducIrTy, ReducIrTyApp, Subst};
    use reducir::{
        ReducIr, ReducIrEndoFold, ReducIrFold, ReducIrKind, ReducIrLocal, ReducIrVar, P,
    };
    use rustc_hash::FxHashMap;

    pub(crate) struct Inline<'a> {
        pub(crate) env: &'a FxHashMap<ReducIrLocal, ReducIr>,
    }
    impl ReducIrEndoFold for Inline<'_> {
        type Ext = Infallible;

        fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
            match kind {
                ReducIrKind::Var(v) => match self.env.get(&v.var) {
                    Some(val) => val.clone(),
                    None => ReducIr::new(kind),
                },
                _ => ReducIr::new(kind),
            }
        }
    }

    pub(crate) struct SubstTy<'a> {
        pub(crate) db: &'a dyn crate::Db,
        pub(crate) subst: Subst,
    }

    impl SubstTy<'_> {
        fn subst(&self, ty: ReducIrTy) -> ReducIrTy {
            ty.subst(self.db.as_reducir_db(), self.subst.clone())
        }
    }

    impl ReducIrEndoFold for SubstTy<'_> {
        type Ext = Infallible;

        fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
            match kind {
                ReducIrKind::TyApp(body, ty_apps) => {
                    let ty_apps = ty_apps
                        .into_iter()
                        .map(|ty_app| match ty_app {
                            ReducIrTyApp::Ty(ty) => ReducIrTyApp::Ty(self.subst(ty)),
                            // Don't do anything for these
                            ReducIrTyApp::DataRow(ReducIrRow::Open(_))
                            | ReducIrTyApp::EffRow(ReducIrRow::Open(_)) => ty_app,
                            ReducIrTyApp::EffRow(ReducIrRow::Closed(row)) => {
                                ReducIrTyApp::EffRow(ReducIrRow::Closed(
                                    row.into_iter().map(|ty| self.subst(ty)).collect(),
                                ))
                            }
                            ReducIrTyApp::DataRow(ReducIrRow::Closed(row)) => {
                                ReducIrTyApp::DataRow(ReducIrRow::Closed(
                                    row.into_iter().map(|ty| self.subst(ty)).collect(),
                                ))
                            }
                        })
                        .collect();
                    ReducIr::new(ReducIrKind::TyApp(body, ty_apps))
                }
                ReducIrKind::Var(var) => ReducIr::var(ReducIrVar {
                    var: var.var,
                    ty: self.subst(var.ty),
                }),
                ReducIrKind::Item(item, item_ty) => {
                    ReducIr::new(ReducIrKind::Item(item, self.subst(item_ty)))
                }
                ReducIrKind::Abs(vars, body) => ReducIr::new(ReducIrKind::Abs(
                    vars.iter()
                        .map(|v| ReducIrVar {
                            var: v.var,
                            ty: self.subst(v.ty),
                        })
                        .collect(),
                    body,
                )),
                ReducIrKind::Tag(ty, tag, value) => {
                    ReducIr::new(ReducIrKind::Tag(self.subst(ty), tag, value))
                }
                ReducIrKind::Case(ty, discr, branches) => {
                    ReducIr::new(ReducIrKind::Case(self.subst(ty), discr, branches))
                }
                kind => ReducIr::new(kind),
            }
        }

        fn endotraverse_ir(&mut self, ir: &ReducIr<Self::Ext>) -> ReducIr<Self::Ext> {
            use ReducIrKind::*;
            match ir.kind() {
                Abs(vars, body) => {
                    let body = body.fold(self);
                    self.fold_ir(Abs(vars.clone(), P::new(body)))
                }
                App(head, spine) => {
                    let head = head.fold(self);
                    let spine = spine.iter().map(|ir| ir.fold(self)).collect();
                    self.fold_ir(App(P::new(head), spine))
                }
                TyAbs(ty_var, body) => {
                    let subst = self.subst.clone();
                    let subst = std::mem::replace(&mut self.subst, subst.lift());
                    let body = body.fold(self);
                    self.subst = subst;
                    self.fold_ir(TyAbs(*ty_var, P::new(body)))
                }
                TyApp(body, ty_app) => {
                    let body = body.fold(self);
                    self.fold_ir(TyApp(P::new(body), ty_app.clone()))
                }
                Struct(elems) => {
                    let elems = elems.iter().map(|e| e.fold(self)).collect();
                    self.fold_ir(Struct(elems))
                }
                FieldProj(indx, body) => {
                    let body = body.fold(self);
                    self.fold_ir(FieldProj(*indx, P::new(body)))
                }
                Tag(ty, tag, body) => {
                    let body = body.fold(self);
                    self.fold_ir(Tag(*ty, *tag, P::new(body)))
                }
                Case(ty, discr, branches) => {
                    let discr = discr.fold(self);
                    let branches = branches.iter().map(|branch| branch.fold(self)).collect();
                    self.fold_ir(Case(*ty, P::new(discr), branches))
                }
                Int(i) => self.fold_ir(Int(*i)),
                Var(v) => self.fold_ir(Var(*v)),
                Item(name, ty) => self.fold_ir(Item(*name, *ty)),

                X(_) => {
                    unreachable!()
                }
            }
        }
    }
}

/// True if an ir term is a value (contains no computations), false if the term does require
/// computation that can be done
fn is_value(ir: &ReducIr) -> bool {
    match ir.kind() {
        ReducIrKind::Int(_)
        | ReducIrKind::Var(_)
        | ReducIrKind::Item(_, _)
        | ReducIrKind::Abs(_, _)
        | ReducIrKind::TyAbs(_, _)
        | ReducIrKind::X(_) => true,
        ReducIrKind::App(_, _) | ReducIrKind::Case(_, _, _) | ReducIrKind::TyApp(_, _) => false,
        ReducIrKind::Struct(elems) => elems.iter().all(is_value),
        ReducIrKind::FieldProj(_, base) => is_value(base),
        ReducIrKind::Tag(_, _, base) => is_value(base),
    }
}

struct InsertLet;
impl ReducIrFold for InsertLet {
    type InExt = Infallible;

    type OutExt = Lets;

    fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
    }

    fn fold_ir(&mut self, kind: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        match kind {
            ReducIrKind::App(head, spine) => match head.into_inner().kind {
                ReducIrKind::Abs(vars, body) => {
                    let mut vars_iter = vars.iter().copied().peekable();
                    let mut args_iter = spine.into_iter().peekable();
                    let binds = vars_iter
                        .zip_non_consuming(&mut args_iter)
                        .collect::<Vec<_>>();
                    ReducIr::app(
                        ReducIr::abss(
                            vars_iter,
                            // We want these lets inside the abs so they're not considered free.
                            ReducIr::new(ReducIrKind::X(Lets { binds, body })),
                        ),
                        args_iter,
                    )
                }
                head => ReducIr::app(ReducIr::new(head), spine),
            },
            kind => ReducIr::new(kind),
        }
    }
}

struct Simplify<'a> {
    db: &'a dyn crate::Db,
    builtin_evs: FxHashMap<ReducIrTermName, &'a ReducIr>,
}
impl ReducIrFold for Simplify<'_> {
    type InExt = Infallible;

    type OutExt = Infallible;

    fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
    }

    fn fold_ir(&mut self, ir: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        use reducir::ReducIrKind::*;
        fn apply_tyabs<'a>(
            body: &'a ReducIr,
            in_app: &mut Vec<ReducIrTyApp>,
            mut out_app: Vec<ReducIrTyApp>,
        ) -> (&'a ReducIr, Vec<ReducIrTyApp>) {
            match body.kind() {
                TyAbs(_, inner) => match in_app.pop() {
                    Some(app) => {
                        out_app.push(app);
                        apply_tyabs(inner, in_app, out_app)
                    }
                    None => (body, out_app),
                },
                _ => (body, out_app),
            }
        }
        match ir {
            TyApp(body, mut ty_app) => {
                ty_app.reverse();
                let (body, apps) = apply_tyabs(&body, &mut ty_app, vec![]);
                ty_app.reverse();
                let body = if !apps.is_empty() {
                    // only perform a substitution if we actually applied types
                    body.fold(&mut SubstTy {
                        db: self.db,
                        subst: apps.into_iter().fold(Subst::Inc(0), |subst, app| {
                            subst.cons(app.into_payload(self.db))
                        }),
                    })
                } else {
                    body.clone()
                };
                ReducIr::ty_app(body, ty_app)
            }
            FieldProj(indx, ref base) => match base.kind() {
                Struct(elems) => self.traverse_ir(&elems[indx]),
                _ => ReducIr::new(ir),
            },
            App(head, spine) => match head.kind() {
                Abs(ref args, body) => {
                    let mut remaining_args = vec![];
                    let mut remaining_apps = vec![];

                    let mut args_iter = args.iter().copied();
                    let mut spine_iter = spine.into_iter();
                    let mut env = FxHashMap::default();

                    loop {
                        match (args_iter.next(), spine_iter.next()) {
                            (Some(var), Some(arg)) => {
                                let arg = arg.fold(&mut Inline { env: &env }).fold(self);
                                if is_value(&arg) {
                                    // Later args may refer to earlier args we're about to
                                    // substitute
                                    env.insert(var.var, arg);
                                } else {
                                    remaining_args.push(var);
                                    remaining_apps.push(arg);
                                }
                            }
                            (None, Some(arg)) => {
                                remaining_apps.push(arg);
                                break;
                            }
                            (Some(var), None) => {
                                remaining_args.push(var);
                                break;
                            }
                            (None, None) => break,
                        }
                    }
                    remaining_args.extend(args_iter);
                    remaining_apps.extend(spine_iter);

                    ReducIr::app(
                        ReducIr::abss(
                            remaining_args,
                            self.traverse_ir(&body.fold(&mut Inline { env: &env })),
                        ),
                        remaining_apps,
                    )
                }
                _ => ReducIr::new(App(head, spine)),
            },
            Case(ty, discr, branches) => match discr.kind() {
                Tag(_, tag, val) => self.traverse_ir(&ReducIr::app(
                    branches[*tag].clone(),
                    [val.clone().into_inner()],
                )),
                _ => ReducIr::new(Case(ty, discr, branches)),
            },
            // Always inline row evidence
            Item(name, _) => match self.builtin_evs.remove(&name) {
                Some(builtin) => self.traverse_ir(builtin),
                None => ReducIr::new(ir),
            },
            ir => ReducIr::new(ir),
        }
    }
}

fn simplify(db: &dyn crate::Db, item: MonReducIrItem) -> ReducIr<Lets> {
    let reducir_db = db.as_reducir_db();
    let row_evs = item.row_evs(reducir_db);
    let ir = item.item(reducir_db);
    let mut builtin_evs = row_evs
        .iter()
        .flat_map(|row_ev| {
            let simple_item = row_ev.simple(reducir_db);
            let scoped_item = row_ev.scoped(reducir_db);

            let simple = (
                ReducIrTermName::Gen(simple_item.name(reducir_db)),
                simple_item.item(reducir_db),
            );
            let scoped = (
                ReducIrTermName::Gen(scoped_item.name(reducir_db)),
                scoped_item.item(reducir_db),
            );
            [simple, scoped]
        })
        .collect::<FxHashMap<_, _>>();

    let module = item.name(reducir_db).module(db.as_core_db());
    let bind_name = ReducIrTermName::gen(db, "__mon_bind", module);
    let bind = bind_term(db, bind_name);
    bind.type_check(db)
        .map_err_pretty_with(db)
        .expect("bind should type check");
    builtin_evs.insert(bind_name, &bind);

    let prompt_name = ReducIrTermName::gen(db, "__mon_prompt", module);
    let prompt = prompt_term(db, module, prompt_name);
    prompt
        .type_check(db)
        .map_err_pretty_with(db)
        .expect("prompt should type check");
    builtin_evs.insert(prompt_name, &prompt);

    let freshm_name = ReducIrTermName::gen(db, "__mon_freshm", module);
    let freshm = freshm_term(db, module, freshm_name);
    builtin_evs.insert(freshm_name, &freshm);

    ir.fold(&mut Simplify { db, builtin_evs })
        .fold(&mut InsertLet)
}

fn freshm_term(db: &dyn crate::Db, module: Module, top_level: ReducIrTermName) -> ReducIr {
    let a = ReducIrVarTy {
        var: ReducIrTyVarId(0),
        kind: Kind::Type,
    };
    let b = ReducIrVarTy {
        var: ReducIrTyVarId(1),
        kind: Kind::Type,
    };
    let var_ty0 = db.mk_reducir_ty(ReducIrTyKind::VarTy(0));
    let var_ty1 = db.mk_reducir_ty(ReducIrTyKind::VarTy(1));
    let f = ReducIrVar {
        var: ReducIrLocal {
            top_level,
            id: ReducIrVarId(0),
        },
        ty: db.mk_fun_ty(
            [db.mk_reducir_ty(ReducIrTyKind::MarkerTy(var_ty1))],
            var_ty0,
        ),
    };

    let marker = ReducIr::new(ReducIrKind::Item(
        ReducIrTermName::gen(db, "__mon_generate_marker", module),
        db.mk_forall_ty(
            [Kind::Type],
            db.mk_fun_ty(
                [db.mk_prod_ty(vec![])],
                db.mk_reducir_ty(ReducIrTyKind::MarkerTy(
                    var_ty1.shift(db.as_reducir_db(), 1),
                )),
            ),
        ),
    ));

    ReducIr::ty_abs(
        [a, b],
        ReducIr::abss(
            [f],
            ReducIr::app(
                ReducIr::var(f),
                [ReducIr::app(
                    ReducIr::ty_app(marker, [ReducIrTyApp::Ty(var_ty1)]),
                    [ReducIr::new(ReducIrKind::Struct(vec![]))],
                )],
            ),
        ),
    )
}

/// Prompt handles "installing" our prompt into the stack and running an action under an
/// updated effect row
fn prompt_term(db: &dyn crate::Db, module: Module, name: ReducIrTermName) -> ReducIr {
    use ReducIrTyKind::*;
    let m_ty = db.mk_reducir_ty(VarTy(2));
    let upd_m = db.mk_reducir_ty(VarTy(1));
    let a = db.mk_reducir_ty(VarTy(0));

    let mark_ty = db.mk_reducir_ty(MarkerTy(a));
    let upd_fun_ty = db.mk_fun_ty([m_ty], upd_m);
    let body_fun_ty = db.mk_mon_ty(upd_m, a);
    let ret_ty = db.mk_reducir_ty(ControlTy(m_ty, a));

    let prompt_type = db.mk_forall_ty(
        [Kind::Type, Kind::Type, Kind::Type],
        db.mk_fun_ty(
            [mark_ty, upd_fun_ty, body_fun_ty],
            db.mk_fun_ty([m_ty], ret_ty),
        ),
    );

    let mut var_gen = IdSupply::default();
    let mut gen_local = || ReducIrLocal {
        top_level: name,
        id: var_gen.supply_id(),
    };
    let mark = ReducIrVar {
        var: gen_local(),
        ty: mark_ty,
    };
    let upd = ReducIrVar {
        var: gen_local(),
        ty: upd_fun_ty,
    };
    let body = ReducIrVar {
        var: gen_local(),
        ty: body_fun_ty,
    };
    let evv = ReducIrVar {
        var: gen_local(),
        ty: m_ty,
    };

    let reinstall_prompt = || {
        let prompt_item = ReducIr::new(ReducIrKind::Item(name, prompt_type));
        ReducIr::app(
            ReducIr::ty_app(
                prompt_item,
                [
                    ReducIrTyApp::Ty(m_ty),
                    ReducIrTyApp::Ty(upd_m),
                    ReducIrTyApp::Ty(a),
                ],
            ),
            [ReducIr::var(mark), ReducIr::var(upd)],
        )
    };

    let exists_b = db.mk_reducir_ty(VarTy(2));
    let exists_m = db.mk_reducir_ty(VarTy(1));
    let exists_r = db.mk_reducir_ty(VarTy(0));
    let exists_mon_m_r = db.mk_mon_ty(exists_m, exists_r);
    let exists_body_fun_ty = db.mk_mon_ty(VarTy(4), VarTy(3)); //db.mk_mon_ty(upd_m, a);
    let y_var = ReducIrVar {
        var: gen_local(),
        ty: db.mk_forall_ty(
            [Kind::Type, Kind::Type, Kind::Type],
            db.mk_prod_ty(vec![
                db.mk_reducir_ty(MarkerTy(exists_r)),
                db.mk_fun_ty([db.mk_fun_ty([exists_b], exists_mon_m_r)], exists_mon_m_r),
                db.mk_fun_ty([exists_b], exists_body_fun_ty),
            ]),
        ),
    };
    let y = ReducIr::ty_app(
        ReducIr::var(y_var),
        [
            ReducIrTyApp::Ty(a),
            ReducIrTyApp::Ty(m_ty),
            ReducIrTyApp::Ty(a),
        ],
    );

    let unit_ty = db.mk_prod_ty(vec![]);
    let x = ReducIrVar {
        var: gen_local(),
        ty: a,
    };
    let unused = ReducIrVar {
        var: gen_local(),
        ty: unit_ty,
    };

    let meq = ReducIr::new(ReducIrKind::Item(
        ReducIrTermName::gen(db, "__mon_eqm", module),
        db.mk_fun_ty(
            [mark_ty, mark_ty],
            db.mk_reducir_ty(CoproductTy(vec![unit_ty, unit_ty])),
        ),
    ));

    fn compose<'a, DB: ?Sized + crate::Db>(
        db: &DB,
        mut gen_local: impl FnMut() -> ReducIrLocal,
        a_fn: ReducIr,
        b_fn: &'a ReducIr,
    ) -> Result<ReducIr, ReducIrTyErr<'a, Infallible>> {
        let b_fn_ty = b_fn.type_check(db)?;

        let b_arg = match b_fn_ty.kind(db.as_reducir_db()) {
            FunTy(args, _) => args[0],
            _ => {
                return Err(ReducIrTyErr::ExpectedFunTy {
                    ty: b_fn_ty,
                    func: b_fn,
                })
            }
        };

        let x = ReducIrVar {
            var: gen_local(),
            ty: b_arg,
        };

        Ok(ReducIr::abss(
            [x],
            ReducIr::app(a_fn, [ReducIr::app(b_fn.clone(), [ReducIr::var(x)])]),
        ))
    }

    /*
    prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
    prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
      Pure x -> Pure x
      Yield m' f k ->
        case m == m' of
          False -> Yield m' f (\x . prompt upd m h (k x))
          True -> f (\x . prompt upd m h (k x)) w
    */
    let m_ = ReducIr::field_proj(0, y.clone());
    let f = ReducIr::field_proj(1, y.clone());
    let k = ReducIr::field_proj(2, y);
    let mut tyvar_supply = IdSupply::default();
    ReducIr::ty_abs(
        [
            ReducIrVarTy {
                var: tyvar_supply.supply_id(),
                kind: Kind::Type,
            },
            ReducIrVarTy {
                var: tyvar_supply.supply_id(),
                kind: Kind::Type,
            },
            ReducIrVarTy {
                var: tyvar_supply.supply_id(),
                kind: Kind::Type,
            },
        ],
        ReducIr::abss(
            [mark, upd, body, evv],
            ReducIr::case(
                ret_ty,
                ReducIr::app(
                    ReducIr::var(body),
                    [ReducIr::app(ReducIr::var(upd), [ReducIr::var(evv)])],
                ),
                [
                    ReducIr::abss([x], ReducIr::tag(ret_ty, 0, ReducIr::var(x))),
                    ReducIr::abss(
                        [y_var],
                        ReducIr::case(
                            ret_ty,
                            ReducIr::app(meq, [ReducIr::var(mark), m_.clone()]),
                            [
                                // False branch
                                ReducIr::abss(
                                    [unused],
                                    ReducIr::tag(
                                        ret_ty,
                                        1,
                                        ReducIr::new(ReducIrKind::Struct(vec![
                                            m_,
                                            f.clone(),
                                            compose(db, &mut gen_local, reinstall_prompt(), &k)
                                                .map_err_pretty_with(db)
                                                .expect("Failed to compose"),
                                        ])),
                                    ),
                                ),
                                // True branch
                                ReducIr::abss(
                                    [unused],
                                    ReducIr::app(
                                        f,
                                        [
                                            compose(db, &mut gen_local, reinstall_prompt(), &k)
                                                .expect("Failed to compose"),
                                            ReducIr::var(evv),
                                        ],
                                    ),
                                ),
                            ],
                        ),
                    ),
                ],
            ),
        ),
    )
}

/// TODO: Return an item representing the bind implementation of our delimited continuation
/// monad
fn bind_term<DB: ?Sized + crate::Db>(db: &DB, name: ReducIrTermName) -> ReducIr {
    use ReducIrTyKind::*;
    /*
    (e: Mon m a) |> (g : a -> Mon m b) : Mon m b =
       𝜆w. case e w of
           Pure x → g x w (monadic bind)
           Yield m f k → Yield m f (𝜆x. g x |> k)
    */
    let m = db.mk_reducir_ty(VarTy(2));
    let a = db.mk_reducir_ty(VarTy(1));
    let b = db.mk_reducir_ty(VarTy(0));
    let ctl_m_b = db.mk_reducir_ty(ControlTy(m, b));
    let mon_m_b = db.mk_mon_ty(m, b);
    let bind_type = db.mk_forall_ty(
        [Kind::Type, Kind::Type, Kind::Type],
        db.mk_fun_ty([db.mk_mon_ty(m, a), db.mk_fun_ty([a], mon_m_b)], mon_m_b),
    );

    let bind_item = ReducIr::new(ReducIrKind::Item(name, bind_type));
    let mut tyvar_supply: IdSupply<ReducIrTyVarId> = IdSupply::default();

    let mut supply: IdSupply<ReducIrVarId> = IdSupply::default();
    let mut gen_local = |ty| ReducIrVar {
        var: ReducIrLocal {
            top_level: name,
            id: supply.supply_id(),
        },
        ty,
    };

    let e = gen_local(db.mk_mon_ty(m, a));
    let g = gen_local(db.mk_fun_ty([a], mon_m_b));
    let w = gen_local(m);
    let x = gen_local(a);

    let exists_b = db.mk_reducir_ty(VarTy(2));
    let exists_m = db.mk_reducir_ty(VarTy(1));
    let exists_r = db.mk_reducir_ty(VarTy(0));
    let exists_mon_m_r = db.mk_mon_ty(exists_m, exists_r);
    let exists_body_fun_ty = db.mk_mon_ty(VarTy(5), VarTy(4));
    let y_var = gen_local(db.mk_forall_ty(
        [Kind::Type, Kind::Type, Kind::Type],
        db.mk_prod_ty(vec![
            db.mk_reducir_ty(MarkerTy(exists_r)),
            db.mk_fun_ty([db.mk_fun_ty([exists_b], exists_mon_m_r)], exists_mon_m_r),
            db.mk_fun_ty([exists_b], exists_body_fun_ty),
        ]),
    ));
    let y = ReducIr::ty_app(
        ReducIr::var(y_var),
        [
            ReducIrTyApp::Ty(b),
            ReducIrTyApp::Ty(m),
            ReducIrTyApp::Ty(b),
        ],
    );

    ReducIr::ty_abs(
        [
            ReducIrVarTy {
                var: tyvar_supply.supply_id(),
                kind: Kind::Type,
            },
            ReducIrVarTy {
                var: tyvar_supply.supply_id(),
                kind: Kind::Type,
            },
            ReducIrVarTy {
                var: tyvar_supply.supply_id(),
                kind: Kind::Type,
            },
        ],
        ReducIr::abss(
            [e, g, w],
            ReducIr::case(
                ctl_m_b,
                ReducIr::app(ReducIr::var(e), [ReducIr::var(w)]),
                [
                    ReducIr::abss(
                        [x],
                        ReducIr::app(ReducIr::var(g), [ReducIr::var(x), ReducIr::var(w)]),
                    ),
                    ReducIr::abss([y_var], {
                        let mon = ReducIr::field_proj(0, y.clone());
                        let f = ReducIr::field_proj(1, y.clone());
                        let k = ReducIr::field_proj(2, y);

                        ReducIr::tag(
                            ctl_m_b,
                            1,
                            ReducIr::new(ReducIrKind::Struct(vec![
                                mon,
                                f,
                                ReducIr::abss(
                                    [x],
                                    ReducIr::app(
                                        ReducIr::ty_app(
                                            bind_item,
                                            [
                                                ReducIrTyApp::Ty(m),
                                                ReducIrTyApp::Ty(b),
                                                ReducIrTyApp::Ty(a),
                                            ],
                                        ),
                                        [ReducIr::app(ReducIr::var(g), [ReducIr::var(x)]), k],
                                    ),
                                ),
                            ])),
                        )
                    }),
                ],
            ),
        ),
    )
}

#[cfg(test)]
mod tests {
    use base::{
        file::{FileId, SourceFile, SourceFileSet},
        pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx},
        Db as BaseDb,
    };
    use expect_test::expect;
    use lower_reducir::Db as LowerDb;
    use parser::Db as ParseDb;
    use reducir::{mon::MonReducIrItem, TypeCheck};

    use crate::simplify;

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        ast::Jar,
        base::Jar,
        desugar::Jar,
        lower_reducir::Jar,
        nameres::Jar,
        parser::Jar,
        reducir::Jar,
        tc::Jar,
        ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn lower_function(db: &TestDatabase, input: &str, fn_name: &str) -> MonReducIrItem {
        let path = std::path::PathBuf::from("test");
        let mut contents = r#"
effect State {
    put : {} -> {},
    get : {} -> {}
}

effect Reader {
    ask : {} -> {}
}

"#
        .to_string();
        contents.push_str(input);
        let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
        SourceFileSet::new(db, vec![file]);

        match db.lower_reducir_mon_item_for_file_name(path, db.ident_str(fn_name)) {
            Some(term) => term,
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }

    fn lower_mon_snippet(db: &TestDatabase, input: &str) -> MonReducIrItem {
        let main = format!("f = {}", input);
        lower_function(db, &main, "f")
    }

    #[test]
    fn simplify_state_get() {
        let db = TestDatabase::default();

        let ir = lower_mon_snippet(
            &db,
            r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))({})"#,
        );
        let simple_ir = simplify(&db, ir);

        let pretty_ir = simple_ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                (let
                  [ (V0:__mon_bind (let
                    (V19:f ((__mon_generate_marker @ [Ty({} -> {{}, {}})]) {}))
                    (fun [V3]
                      (let
                        (V2:__mon_prompt ((__mon_bind @ [Ty({0}), Ty({}), Ty({} -> { {}
                                                                                   , {}
                                                                                   })])
                          (fun [V0]
                            (let (V17:f (V1[3][0] V0))
                              <1: {V17[0], (fun [V18] (V17[1][1] {} V18)), (fun [V0] V0)}>))
                          (fun [V22] (fun [V0] <0: (fun [V15] {V15, V22})>))))
                        (case (V2
                            (V1[0]
                              V3
                              {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun [V8, V9, V10]
                                (V9 V10 V10))}}))
                          (fun [V5] <0: V5>)
                          (fun [V4]
                            (case (__mon_eqm
                                V19
                                (V4 @ [Ty({} -> {{}, {}}), Ty({1}), Ty({} -> {{}, {}})])[0])
                              (fun [V6]
                                <1: {(V4 @ [Ty({} -> {{}, {}}), Ty({1}), Ty({} -> { {}
                                                                                  , {}
                                                                                  })])[0], (V4 @ [Ty({}
                                  -> {{}, {}}), Ty({1}), Ty({} -> {{}, {}})])[1], (fun [V7]
                                    ((__mon_prompt @ [Ty({1}), Ty({0}), Ty({} -> {{}, {}})])
                                      V19
                                      (fun [V0]
                                        (V1[0]
                                          V0
                                          {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun
                                            [V8
                                            ,V9
                                            ,V10] (V9 V10 V10))}}))
                                      ((V4 @ [Ty({} -> {{}, {}}), Ty({1}), Ty({} -> { {}
                                                                                    , {}
                                                                                    })])[2]
                                        V7)))}>)
                              (fun [V6]
                                ((V4 @ [Ty({} -> {{}, {}}), Ty({1}), Ty({} -> {{}, {}})])[1]
                                  (fun [V8]
                                    ((__mon_prompt @ [Ty({1}), Ty({0}), Ty({} -> {{}, {}})])
                                      V19
                                      (fun [V0]
                                        (V1[0]
                                          V0
                                          {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun
                                            [V8
                                            ,V9
                                            ,V10] (V9 V10 V10))}}))
                                      ((V4 @ [Ty({} -> {{}, {}}), Ty({1}), Ty({} -> { {}
                                                                                    , {}
                                                                                    })])[2]
                                        V8)))
                                  V3)))))))))
                  , (V2:__mon_bind V0)
                  ]
                  (case (V0 V2)
                    (fun [V3] (let (V25:f (V3 {})) <0: V25>))
                    (fun [V4]
                      <1: {(V4 @ [Ty({{}, {}}), Ty({1}), Ty({{}, {}})])[0], (V4 @ [Ty({ {}
                                                                                      , {}
                                                                                      }), Ty({1}), Ty({ {}
                                                                                                      , {}
                                                                                                      })])[1], (fun
                          [V3]
                          ((__mon_bind @ [Ty({1}), Ty({{}, {}}), Ty({} -> {{}, {}})])
                            (fun [V0] (let (V25:f (V3 {})) <0: V25>))
                            (V4 @ [Ty({{}, {}}), Ty({1}), Ty({{}, {}})])[2]))}>)))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect![[r#"
            forall ScopedRow .
              forall ScopedRow .
                { {1} -> { (Marker {} -> {{}, {}})
                         , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                           , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                           }
                         } -> {0}
                , forall Type .
                  (<2> -> T0) -> ({ (Marker {} -> {{}, {}})
                                  , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                    , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                    }
                                  } -> T0) -> <1> -> T0
                , {{0} -> {1}, <1> -> <0>}
                , { {0} -> { (Marker {} -> {{}, {}})
                           , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             }
                           }
                  , { (Marker {} -> {{}, {}})
                    , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                      , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                      }
                    } -> <0>
                  }
                } -> {1} -> (Control {1} {{}, {}})"#]];
        let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
        expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
    }
}
