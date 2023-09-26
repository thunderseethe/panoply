use aiahr_ast::{AstModule, AstTerm, Term};
use aiahr_core::{
    id::{EffectName, EffectOpName, Id, IdSupply, ReducIrVarId, TermName, TyVarId, VarId},
    id_converter::IdConverter,
    ident::Ident,
    modules::Module,
};
use aiahr_reducir::{
    mon::{MonReducIrGenItem, MonReducIrItem, MonReducIrModule, MonReducIrRowEv},
    ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyKind, ReducIrVarTy},
    GeneratedReducIrName, ReducIr, ReducIrGenItem, ReducIrItem,
    ReducIrKind::*,
    ReducIrModule, ReducIrRowEv, ReducIrTermName, P,
};
use aiahr_tc::EffectInfo;
use aiahr_ty::{
    row::{Scoped, Simple},
    InDb, MkTy, RowFields, Ty, TyScheme, Wrapper,
};
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};

use rustc_hash::FxHashMap;

use crate::lower::{LowerTyCtx, LowerTySchemeCtx};

use self::{
    lower::{ItemWrappers, RowReducrIrEvidence, RowVarConvert},
    lower_mon::LowerMonCtx,
};

pub(crate) mod evidence;

mod lower;
mod lower_mon {
    use aiahr_core::id::{IdSupply, ReducIrVarId};
    use aiahr_core::pretty::PrettyErrorWithDb;
    use aiahr_reducir::ty::{
        Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp, ReducIrTyKind, UnwrapMonTy,
    };
    use aiahr_reducir::{
        DelimCont, DelimReducIr, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar,
        TypeCheck, P,
    };

    use ReducIrKind::*;
    use ReducIrTyKind::*;

    pub(crate) struct LowerMonCtx<'a> {
        db: &'a dyn crate::Db,
        var_conv: &'a mut IdSupply<ReducIrVarId>,
        current: ReducIrTermName,
        evv_var_id: ReducIrVarId,
    }

    impl MkReducIrTy for LowerMonCtx<'_> {
        fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy {
            self.db.mk_reducir_ty(kind)
        }

        fn mk_fun_ty(
            &self,
            args: impl IntoIterator<Item = impl aiahr_reducir::ty::IntoReducIrTy>,
            ret: impl aiahr_reducir::ty::IntoReducIrTy,
        ) -> ReducIrTy {
            self.db.mk_fun_ty(args, ret)
        }

        fn mk_prod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
            self.db.mk_prod_ty(elems)
        }

        fn mk_coprod_ty(&self, elems: &[ReducIrTy]) -> ReducIrTy {
            self.db.mk_coprod_ty(elems)
        }
    }

    impl<'a> LowerMonCtx<'a> {
        pub(crate) fn new(
            db: &'a dyn crate::Db,
            var_conv: &'a mut IdSupply<ReducIrVarId>,
            current: ReducIrTermName,
            evv_var_id: ReducIrVarId,
        ) -> Self {
            Self {
                db,
                var_conv,
                current,
                evv_var_id,
            }
        }
    }

    impl LowerMonCtx<'_> {
        pub(crate) fn lower_monadic_entry(&mut self, ir: &DelimReducIr) -> ReducIr {
            match ir.kind() {
                ReducIrKind::Abs(vars, _) => {
                    match vars.iter().find(|var| var.var.id == self.evv_var_id) {
                        Some(evv_var) => self.lower_monadic(evv_var.ty, ir),
                        None => {
                            println!("{:?}", ir);
                            todo!()
                        }
                    }
                }
                ReducIrKind::TyAbs(ty_var, ir) => ReducIr::new(ReducIrKind::TyAbs(
                    *ty_var,
                    P::new(self.lower_monadic_entry(ir)),
                )),
                ReducIrKind::TyApp(ir, ty_app) => ReducIr::new(ReducIrKind::TyApp(
                    P::new(self.lower_monadic_entry(ir)),
                    ty_app.clone(),
                )),
                kind => panic!("{:?}", kind),
            }
        }

        fn generate_local(&mut self) -> ReducIrLocal {
            ReducIrLocal {
                top_level: self.current,
                id: self.var_conv.supply_id(),
            }
        }

        fn fresh_marker_item(&mut self) -> ReducIr {
            let ret_ty = self.mk_reducir_ty(VarTy(0));
            ReducIr::new(Item(
                ReducIrTermName::gen(self.db, "__mon_freshm", self.current.module(self.db)),
                self.mk_forall_ty(
                    [Kind::Type, Kind::Type],
                    self.mk_fun_ty(
                        [self.mk_fun_ty(
                            [self.mk_reducir_ty(MarkerTy(self.mk_reducir_ty(VarTy(1))))],
                            ret_ty,
                        )],
                        ret_ty,
                    ),
                ),
            ))
        }

        /// Prompt handles "installing" our prompt into the stack and running an action under an
        /// updated effect row
        fn prompt_item(&mut self) -> ReducIr {
            /*
              prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
              prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
                Pure x -> Pure x
                Yield m' f k ->
                    case m == m' of
                        False -> Yield m' f (\x . prompt upd m h (k x))
                        True -> f (\x . prompt upd m h (k x)) w
            */
            let m = self.mk_reducir_ty(VarTy(2));
            let upd_m = self.mk_reducir_ty(VarTy(1));
            let a = self.mk_reducir_ty(VarTy(0));

            let mark = self.mk_reducir_ty(MarkerTy(a));

            let prompt_type = self.mk_forall_ty(
                [Kind::Type, Kind::Type, Kind::Type],
                self.mk_fun_ty(
                    [
                        mark,
                        self.mk_fun_ty([m], upd_m),
                        self.mk_fun_ty([upd_m], self.mk_reducir_ty(ControlTy(upd_m, a))),
                    ],
                    self.mk_fun_ty([m], self.mk_reducir_ty(ControlTy(m, a))),
                ),
            );

            ReducIr::new(Item(
                ReducIrTermName::gen(self.db, "__mon_prompt", self.current.module(self.db)),
                prompt_type,
            ))
        }
        /// TODO: Return an item representing the bind implementation of our delimited continuation
        /// monad
        fn bind_item(&mut self) -> ReducIr {
            /*
            (e: Mon m a) |> (g : a -> Mon m b) : Mon m b =
               𝜆w. case e w of
                   Pure x → g x w (monadic bind)
                   Yield m f k → Yield m f (𝜆x. g x |> k)
            */
            let m = self.mk_reducir_ty(VarTy(2));
            let a = self.mk_reducir_ty(VarTy(1));
            let b = self.mk_reducir_ty(VarTy(0));
            let mon_m_b = self.mk_mon_ty(m, b);

            let bind_type = self.mk_forall_ty(
                [Kind::Type, Kind::Type, Kind::Type],
                self.mk_fun_ty(
                    [self.mk_mon_ty(m, a), self.mk_fun_ty([a], mon_m_b)],
                    mon_m_b,
                ),
            );
            ReducIr::new(Item(
                ReducIrTermName::gen(self.db, "__mon_bind", self.current.module(self.db)),
                bind_type,
            ))
        }

        fn bind(
            &mut self,
            ir: ReducIr,
            derive_out_ty: impl FnOnce(ReducIrTy) -> ReducIrTy,
            body: impl FnOnce(&mut Self, ReducIrVar) -> ReducIr,
        ) -> ReducIr {
            let ir_db = self.db.as_reducir_db();
            let ty = ir.type_check(ir_db).map_err_pretty_with(ir_db).unwrap();
            let mon_ty = ty
                .try_unwrap_monadic(ir_db)
                .map_err_pretty_with(ir_db)
                .unwrap();
            let tmp = ReducIrVar {
                var: self.generate_local(),
                ty: mon_ty.a_ty,
            };
            let bind = self.bind_item();
            ReducIr::app(
                ReducIr::ty_app(
                    bind,
                    [
                        ReducIrTyApp::Ty(mon_ty.evv_ty),
                        ReducIrTyApp::Ty(mon_ty.a_ty),
                        ReducIrTyApp::Ty(derive_out_ty(mon_ty.a_ty)),
                    ],
                ),
                [ir, ReducIr::abss([tmp], body(self, tmp))],
            )
        }

        fn lower_monadic(&mut self, evv_ty: ReducIrTy, ir: &ReducIr<DelimCont>) -> ReducIr {
            let reducir_db = self.db.as_reducir_db();
            let evv_var_id = ReducIrLocal {
                top_level: self.current,
                id: self.evv_var_id,
            };
            let pure = |ir: ReducIr| {
                let ty = ir
                    .type_check(reducir_db)
                    .expect("ICE: lower_monadic type check error");
                ReducIr::abss(
                    [ReducIrVar {
                        var: evv_var_id,
                        ty: evv_ty,
                    }],
                    ReducIr::new(ReducIrKind::Tag(
                        reducir_db.mk_reducir_ty(ControlTy(evv_ty, ty)),
                        0,
                        P::new(ir),
                    )),
                )
            };
            match ir.kind() {
                Int(i) => pure(ReducIr::new(Int(*i))),
                Var(v) => ReducIr::var(*v),
                Abs(vars, body) => {
                    let evv_ty = vars
                        .iter()
                        .find(|v| v.var.id == self.evv_var_id)
                        .map(|evv_var| evv_var.ty)
                        .unwrap_or(evv_ty);
                    ReducIr::abss(vars.iter().copied(), self.lower_monadic(evv_ty, body))
                }
                App(func, args) => {
                    let bind = self.bind_item();
                    let func_mon = self.lower_monadic(evv_ty, func);
                    let func_ty = func_mon
                        .type_check(reducir_db)
                        .map_err_pretty_with(reducir_db)
                        .unwrap();

                    let mut bind_args = vec![];
                    let mon_args = args
                        .iter()
                        .map(|arg| {
                            let mon_arg = self.lower_monadic(evv_ty, arg);
                            let mon_arg_ty = mon_arg
                                .type_check(reducir_db)
                                .map_err_pretty_with(reducir_db)
                                .unwrap();
                            match mon_arg_ty.try_unwrap_monadic(reducir_db) {
                                Ok(_) => {
                                    let arg_ty = arg
                                        .type_check(reducir_db)
                                        .map_err_pretty_with(reducir_db)
                                        .unwrap();
                                    let arg_var = ReducIrVar {
                                        var: self.generate_local(),
                                        ty: arg_ty,
                                    };
                                    bind_args.push((mon_arg, arg_var));
                                    ReducIr::var(arg_var)
                                }
                                // We don't need to do anything for a non monadic arg
                                Err(_) => mon_arg,
                            }
                        })
                        .collect::<Vec<_>>();

                    let (body, ret_ty) = match func_ty.try_fun_returns_monadic(reducir_db) {
                        // Our function might take some number of argument and then return another
                        // function wrapped in our monad:
                        //    a -> b -> {evv} -> Ctl {evv} (c -> d -> e)
                        // To handle this case we split our args based on how many args appear before
                        // the monadic type. This application is then passed to bind to produce the
                        // underlying function which is applied to any remaining args and then wrapped
                        // up in our monad again.
                        Ok((arg_count, mon)) => {
                            let mut mon_args = mon_args;
                            let post_mon_args = mon_args.split_off(arg_count);
                            let pre_mon_args = mon_args;

                            let f = ReducIrVar {
                                var: self.generate_local(),
                                ty: mon.a_ty,
                            };

                            if post_mon_args.is_empty() {
                                (ReducIr::app(func_mon, pre_mon_args), mon.a_ty)
                            } else {
                                let applied_fun_ty =
                                    mon.a_ty.drop_args(reducir_db, post_mon_args.len()).unwrap();
                                let body = ReducIr::app(
                                    ReducIr::ty_app(
                                        bind.clone(),
                                        [
                                            ReducIrTyApp::Ty(evv_ty),
                                            ReducIrTyApp::Ty(mon.a_ty),
                                            ReducIrTyApp::Ty(applied_fun_ty),
                                        ],
                                    ),
                                    [
                                        ReducIr::app(func_mon, pre_mon_args),
                                        ReducIr::abss([f], {
                                            let y = ReducIrVar {
                                                var: self.generate_local(),
                                                ty: applied_fun_ty,
                                            };
                                            ReducIr::local(
                                                y,
                                                ReducIr::app(ReducIr::var(f), post_mon_args),
                                                pure(ReducIr::var(y)),
                                            )
                                        }),
                                    ],
                                );
                                (body, applied_fun_ty)
                            }
                        }
                        Err(ty) => {
                            // If we have no monadic args and our function isn't monadic return early
                            // with normal function application
                            if bind_args.is_empty() {
                                return ReducIr::app(func_mon, mon_args);
                            }

                            let applied_fun_ty = ty.drop_args(reducir_db, mon_args.len()).unwrap();

                            // Otherwise lift our return value into our monad
                            let y = ReducIrVar {
                                var: self.generate_local(),
                                ty: applied_fun_ty,
                            };

                            let body = ReducIr::local(
                                y,
                                ReducIr::app(func_mon, mon_args),
                                pure(ReducIr::var(y)),
                            );

                            (body, applied_fun_ty)
                        }
                    };

                    bind_args.into_iter().rfold(body, |body, (arg, arg_var)| {
                        ReducIr::app(
                            ReducIr::ty_app(
                                bind.clone(),
                                [
                                    ReducIrTyApp::Ty(evv_ty),
                                    ReducIrTyApp::Ty(arg_var.ty),
                                    ReducIrTyApp::Ty(ret_ty),
                                ],
                            ),
                            [arg, ReducIr::abss([arg_var], body)],
                        )
                    })
                }
                TyAbs(tyvar, ir) => {
                    ReducIr::new(TyAbs(*tyvar, P::new(self.lower_monadic(evv_ty, ir))))
                }
                TyApp(ir, ty) => {
                    ReducIr::new(TyApp(P::new(self.lower_monadic(evv_ty, ir)), ty.clone()))
                }
                Struct(elems) => {
                    let mut binds = vec![];
                    let mut is_mon = false;
                    let vars = elems
                        .iter()
                        .map(|elem| match elem.kind() {
                            Var(v) => {
                                if v.ty.try_unwrap_monadic(reducir_db).is_ok() {
                                    is_mon = true;
                                }
                                *v
                            }
                            _ => {
                                let v = ReducIrVar {
                                    var: self.generate_local(),
                                    ty: elem
                                        .type_check(reducir_db)
                                        .map_err_pretty_with(reducir_db)
                                        .unwrap(),
                                };
                                binds.push((v, elem));
                                v
                            }
                        })
                        .collect::<Vec<_>>();
                    // If all our elements are variables we just return the pure Struct
                    if binds.is_empty() {
                        if is_mon {
                            return pure(ReducIr::new(Struct(
                                vars.into_iter().map(ReducIr::var).collect(),
                            )));
                        } else {
                            return ReducIr::new(Struct(
                                vars.into_iter().map(ReducIr::var).collect(),
                            ));
                        }
                    }
                    let anf = binds.into_iter().fold(
                        ReducIr::new(Struct(vars.into_iter().map(ReducIr::var).collect())),
                        |body, (var, defn)| ReducIr::local(var, defn.clone(), body),
                    );
                    self.lower_monadic(evv_ty, &anf)
                }
                FieldProj(indx, strukt) => {
                    let strukt = self.lower_monadic(evv_ty, strukt);
                    let strukt_ty = strukt
                        .type_check(reducir_db)
                        .map_err_pretty_with(reducir_db)
                        .unwrap();
                    match strukt_ty.try_unwrap_monadic(reducir_db) {
                        Ok(_) => self.bind(
                            strukt,
                            |ty| match ty.kind(reducir_db) {
                                ProductTy(ref elems) => elems[*indx],
                                _ => unreachable!(),
                            },
                            |_, s| pure(ReducIr::field_proj(*indx, ReducIr::var(s))),
                        ),
                        // No bind required
                        Err(_) => ReducIr::field_proj(*indx, strukt),
                    }
                }
                Tag(ty, tag, ir) => {
                    let ir = self.lower_monadic(evv_ty, ir);
                    self.bind(
                        ir,
                        |_| *ty,
                        |_, t| pure(ReducIr::new(Tag(*ty, *tag, P::new(ReducIr::var(t))))),
                    )
                }
                Case(ty, disc, branches) => {
                    let disc = self.lower_monadic(evv_ty, disc);
                    self.bind(
                        disc,
                        |_| *ty,
                        |ctx, d| {
                            ReducIr::case_on_var(
                                ctx.mk_mon_ty(evv_ty, *ty),
                                d,
                                branches.iter().map(|b| ctx.lower_monadic(evv_ty, b)),
                            )
                        },
                    )
                }
                // TODO: do we need to handle this specially? item should be lowered monadically so it
                // already returns a monad when we call it here
                Item(item, ty) => ReducIr::new(Item(*item, *ty)),
                X(DelimCont::NewPrompt(mark_var, ir)) => {
                    let x = self.lower_monadic(evv_ty, ir);
                    let a_ty = match mark_var.ty.kind(reducir_db) {
                        MarkerTy(a_ty) => a_ty,
                        _ => unreachable!(),
                    };
                    let ir = ReducIr::app(
                        ReducIr::ty_app(
                            self.fresh_marker_item(),
                            [
                                ReducIrTyApp::Ty(a_ty),
                                ReducIrTyApp::Ty(
                                    x.type_check(reducir_db)
                                        .map_err_pretty_with(reducir_db)
                                        .unwrap(),
                                ),
                            ],
                        ),
                        [ReducIr::abss([*mark_var], x)],
                    );
                    ir
                }
                X(DelimCont::Prompt(marker, upd_evv, body)) => {
                    let update_evv_fn_ty = upd_evv
                        .type_check(reducir_db)
                        .map_err_pretty_with(reducir_db)
                        .unwrap();
                    // Invariant that this is a function from evv to upd_evv type.
                    let (_, upd_evv_ty) = match update_evv_fn_ty.kind(reducir_db) {
                        FunTy(args, ret) => (args[0], ret),
                        _ => unreachable!(),
                    };
                    let mon_body = self.lower_monadic(upd_evv_ty, body);
                    let mon_marker = self.lower_monadic(evv_ty, marker);
                    let mon_body_ty = mon_body
                        .type_check(reducir_db)
                        .map_err_pretty_with(reducir_db)
                        .unwrap();
                    let UnwrapMonTy {
                        evv_ty: _,
                        a_ty: body_ty,
                    } = match mon_body_ty.try_unwrap_monadic(reducir_db) {
                        Ok(upd_mon) => upd_mon,
                        Err(_) => unreachable!(),
                    };
                    ReducIr::app(
                        ReducIr::ty_app(
                            self.prompt_item(),
                            [
                                ReducIrTyApp::Ty(evv_ty),
                                ReducIrTyApp::Ty(upd_evv_ty),
                                ReducIrTyApp::Ty(body_ty),
                            ],
                        ),
                        [mon_marker, upd_evv.assume_no_ext(), mon_body],
                    )
                }
                X(DelimCont::Yield(ty, mark, f)) => {
                    let w = ReducIrVar {
                        var: ReducIrLocal {
                            top_level: self.current,
                            id: self.evv_var_id,
                        },
                        ty: evv_ty,
                    };
                    let x = ReducIrVar {
                        var: ReducIrLocal {
                            top_level: self.current,
                            id: self.evv_var_id,
                        },
                        ty: *ty,
                    };
                    ReducIr::abss(
                        [w],
                        ReducIr::new(Tag(
                            self.mk_reducir_ty(ControlTy(evv_ty, *ty)),
                            1,
                            P::new(ReducIr::new(Struct(vec![
                                self.lower_monadic(evv_ty, mark),
                                self.lower_monadic(evv_ty, f),
                                ReducIr::abss([x], ReducIr::var(x)),
                            ]))),
                        )),
                    )
                }
            }
        }
    }
}

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait ReducIrEffectInfo: EffectInfo {
    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize;

    fn effect_handler_ir_ty(&self, effect: EffectName) -> ReducIrTy;
}

#[salsa::jar(db = Db)]
pub struct Jar(
    lower_module,
    lower_item,
    lower_row_ev,
    lower_mon_item,
    effect_handler_ir_ty,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_tc::Db + aiahr_reducir::Db {
    fn as_lower_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    /*
     * AST -> ReducIr Lowering Methods
     */

    fn lower_reducir_module(&self, module: AstModule) -> ReducIrModule {
        lower_module(self.as_lower_reducir_db(), module)
    }

    fn lower_reducir_item(&self, ast_term: AstTerm) -> ReducIrItem {
        lower_item(self.as_lower_reducir_db(), ast_term)
    }

    fn reducir_var_supply(&self, term_name: TermName) -> &IdSupply<ReducIrVarId> {
        self.lower_reducir_item_of(term_name)
            .var_supply(self.as_reducir_db())
    }

    fn lower_reducir_item_of(&self, term_name: TermName) -> ReducIrItem {
        let ast_term = self.desugar_term_of(term_name);
        self.lower_reducir_item(ast_term)
    }

    fn lower_reducir_module_of(&self, module: Module) -> ReducIrModule {
        let ast_module = self.desugar_module_of(module);
        self.lower_reducir_module(ast_module)
    }

    fn lower_reducir_module_for_path(&self, path: std::path::PathBuf) -> ReducIrModule {
        let module = self.root_module_for_path(path);
        let ast_module = self.desugar_module_of(module);
        self.lower_reducir_module(ast_module)
    }

    fn lower_reducir_item_for_file_name(
        &self,
        path: std::path::PathBuf,
        item: Ident,
    ) -> Option<ReducIrItem> {
        let module = self.root_module_for_path(path);
        let term_name = self.id_for_name(module, item)?;
        Some(self.lower_reducir_item_of(term_name))
    }

    /*
     * ReducIr -> Monadic ReducIr Lowering Methods
     */

    fn lower_reducir_mon_module(&self, module: ReducIrModule) -> MonReducIrModule {
        lower_mon_module(self.as_lower_reducir_db(), module)
    }

    fn lower_reducir_mon_module_of(&self, module: Module) -> MonReducIrModule {
        let reducir_module = self.lower_reducir_module_of(module);
        self.lower_reducir_mon_module(reducir_module)
    }

    fn lower_reducir_mon_item(&self, item: ReducIrItem) -> MonReducIrItem {
        lower_mon_item(self.as_lower_reducir_db(), item)
    }

    fn lower_reducir_mon_item_of(&self, name: TermName) -> MonReducIrItem {
        let item = self.lower_reducir_item_of(name);
        self.lower_reducir_mon_item(item)
    }

    fn lower_reducir_mon_item_for_file_name(
        &self,
        path: std::path::PathBuf,
        item: Ident,
    ) -> Option<MonReducIrItem> {
        let module = self.root_module_for_path(path);
        let term_name = self.id_for_name(module, item)?;
        Some(self.lower_reducir_mon_item_of(term_name))
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_tc::Db + aiahr_reducir::Db {}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: AstModule) -> ReducIrModule {
    let ast_db = db.as_ast_db();
    let items = module
        .terms(ast_db)
        .iter()
        .map(|term| lower_item(db, *term))
        .collect();
    ReducIrModule::new(db.as_reducir_db(), module.module(ast_db), items)
}

#[salsa::tracked]
fn lower_row_ev(
    db: &dyn crate::Db,
    module: Module,
    left: RowFields,
    right: RowFields,
    goal: RowFields,
) -> ReducIrRowEv {
    ReducIrRowEv::new(
        db.as_reducir_db(),
        lower_row_ev_item::<Simple>(db, module, "simple", left, right, goal),
        lower_row_ev_item::<Scoped>(db, module, "scoped", left, right, goal),
    )
}

fn lower_row_ev_item<Sema: RowReducrIrEvidence>(
    db: &dyn crate::Db,
    module: Module,
    mark: &str,
    left: RowFields,
    right: RowFields,
    goal: RowFields,
) -> ReducIrGenItem
where
    for<'a, 'b> LowerTyCtx<'a, 'b>: RowVarConvert<Sema>,
    Sema::Closed<InDb>: Copy,
    Sema::Open<InDb>: Copy,
{
    let ty_db = db.as_ty_db();
    let left_fields = left.fields(ty_db);
    let left_field_str: String = left_fields
        .iter()
        .map(|ident| ident.text(db.as_core_db()).as_str())
        .collect();
    let right_fields = right.fields(ty_db);
    let right_field_str: String = right_fields
        .iter()
        .map(|ident| ident.text(db.as_core_db()).as_str())
        .collect();
    let row_ev_ident = db.ident(format!(
        "_row_{}_{}_{}",
        mark, left_field_str, right_field_str
    ));
    let row_ev_name = GeneratedReducIrName::new(db.as_reducir_db(), row_ev_ident, module);

    let mut id_count: i32 = -1;
    let mut left_values = left_fields
        .iter()
        .map(|_| {
            id_count += 1;
            id_count
        })
        .map(|id| TyVarId::from_raw(id.try_into().unwrap()))
        .collect::<Vec<_>>();
    let right_values = right_fields
        .iter()
        .map(|_| {
            id_count += 1;
            id_count
        })
        .map(|id| TyVarId::from_raw(id.try_into().unwrap()))
        .collect::<Vec<_>>();

    let left_row = db.mk_row_iter::<Sema::Closed<InDb>>(
        left_fields.iter().copied(),
        left_values
            .iter()
            .map(|id| db.mk_ty(aiahr_ty::TypeKind::VarTy(*id))),
    );
    let right_row = db.mk_row_iter::<Sema::Closed<InDb>>(
        right_fields.iter().copied(),
        right_values
            .iter()
            .map(|id| db.mk_ty(aiahr_ty::TypeKind::VarTy(*id))),
    );

    let goal_row_indices = Sema::merge(db, left_row, right_row);
    let goal_values = goal_row_indices
        .iter()
        .map(|indx| match indx {
            lower::RowIndx::Left(_, ty) => *ty,
            lower::RowIndx::Right(_, ty) => *ty,
        })
        .collect::<Vec<_>>();

    let goal_row =
        db.mk_row::<Sema::Closed<InDb>>(goal.fields(ty_db).as_slice(), goal_values.as_slice());

    left_values.extend(right_values);
    let order_of_tys = left_values;
    let tyvar_env = order_of_tys
        .iter()
        .enumerate()
        .map(|(i, ty)| (*ty, i as i32))
        .collect::<FxHashMap<_, _>>();

    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let mut lower_ctx = LowerCtx::new(
        db,
        &mut var_conv,
        LowerTyCtx::new(db, &mut tyvar_conv, tyvar_env),
        ReducIrTermName::Gen(row_ev_name),
    );
    let ir = lower_ctx.row_evidence_ir::<Sema>(left_row, right_row, goal_row);
    let mon_ir = ir.assume_no_ext();

    let (ir, mon_ir) = order_of_tys
        .into_iter()
        .rfold((ir, mon_ir), |(ir, mon_ir), var| {
            let var = ReducIrVarTy {
                var: tyvar_conv.convert(var),
                kind: Kind::Type,
            };
            (
                ReducIr::new(TyAbs(var, P::new(ir))),
                ReducIr::new(TyAbs(var, P::new(mon_ir))),
            )
        });

    ReducIrGenItem::new(db.as_reducir_db(), row_ev_name, ir, mon_ir)
}

/// Lower an `Ast` into an `ReducIr`.
/// TODO: Real documentation.
#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term: AstTerm) -> ReducIrItem {
    let ast_db = db.as_ast_db();
    let name = term.name(ast_db);
    let ast = term.data(ast_db);
    let typed_item = db.type_scheme_of(name);
    let tc_db = db.as_tc_db();
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let scheme = typed_item.ty_scheme(db.as_tc_db());

    let (_, ty_ctx) = LowerTySchemeCtx::new(db, &mut tyvar_conv)
        .lower_scheme(name.module(db.as_core_db()), &scheme);

    let required_evidence = typed_item.required_evidence(tc_db);
    let (mut lower_ctx, ev_solved, ev_params, ev_row_items) =
        LowerCtx::new(db, &mut var_conv, ty_ctx, ReducIrTermName::Term(name))
            .collect_evidence_params(required_evidence.iter());

    let body = lower_ctx.lower_term(ast, ast.root());
    // TODO: Bit of a hack. Eventually we'd like to generate our solved row ev in a central location.
    // Add row evidence as parameters of the term
    let body = ev_solved
        .into_iter()
        .rfold(body, |body, (arg, term)| ReducIr::local(arg, term, body));
    // Wrap our term in any unsolved row evidence params we need
    let evv_var = lower_ctx.evv_var(ast);
    let body = ReducIr::abss(ev_params.into_iter().chain(std::iter::once(evv_var)), body);

    // Finally wrap our term in any type/row variables it needs to bind
    let body = scheme
        .bound_data_row
        .iter()
        .rfold(body, |acc, simple_row_var| {
            ReducIr::new(TyAbs(
                ReducIrVarTy {
                    var: lower_ctx.tyvar_conv().convert(*simple_row_var),
                    kind: Kind::SimpleRow,
                },
                P::new(acc),
            ))
        });
    let body = scheme
        .bound_eff_row
        .iter()
        .rfold(body, |acc, scoped_row_var| {
            ReducIr::new(TyAbs(
                ReducIrVarTy {
                    var: lower_ctx.tyvar_conv().convert(*scoped_row_var),
                    kind: Kind::ScopedRow,
                },
                P::new(acc),
            ))
        });
    let ir = scheme.bound_ty.iter().rfold(body, |acc, ty_var| {
        ReducIr::new(TyAbs(
            ReducIrVarTy {
                var: lower_ctx.tyvar_conv().convert(*ty_var),
                kind: Kind::Type,
            },
            P::new(acc),
        ))
    });
    let mon_ir = lower_ctx.lower_monadic_entry(&ir);
    ReducIrItem::new(
        db.as_reducir_db(),
        name,
        ir,
        mon_ir,
        ev_row_items,
        var_conv.into(),
        tyvar_conv.into(),
    )
}

fn lower_mon_module(db: &dyn crate::Db, module: ReducIrModule) -> MonReducIrModule {
    let reducir_db = db.as_reducir_db();
    let items = module
        .items(reducir_db)
        .iter()
        .map(|item| db.lower_reducir_mon_item(*item))
        .collect();
    MonReducIrModule::new(reducir_db, module.module(reducir_db), items)
}

#[salsa::tracked]
fn lower_mon_item(db: &dyn crate::Db, item: ReducIrItem) -> MonReducIrItem {
    let reducir_db = db.as_reducir_db();
    let name = item.name(reducir_db);
    let mut supply = IdSupply::start_from(item.var_supply(reducir_db));
    // TODO: Figure out a better way to do evv_id
    let mut ctx = LowerMonCtx::new(
        db,
        &mut supply,
        ReducIrTermName::Term(name),
        ReducIrVarId(0),
    );
    let mon_ir = ctx.lower_monadic_entry(item.item(reducir_db));
    let row_evs = item
        .row_evs(reducir_db)
        .iter()
        .map(|row| {
            let simple = row.simple(reducir_db);
            let mon_simple = MonReducIrGenItem::new(
                reducir_db,
                simple.name(reducir_db),
                simple.item(reducir_db).assume_no_ext(),
                IdSupply::start_from(item.var_supply(reducir_db)),
            );
            let scoped = row.scoped(reducir_db);
            let mon_scoped = MonReducIrGenItem::new(
                reducir_db,
                scoped.name(reducir_db),
                scoped.item(reducir_db).assume_no_ext(),
                IdSupply::start_from(item.var_supply(reducir_db)),
            );
            MonReducIrRowEv::new(reducir_db, mon_simple, mon_scoped)
        })
        .collect();
    MonReducIrItem::new(reducir_db, name, mon_ir, row_evs, supply)
}

#[salsa::tracked]
fn effect_handler_ir_ty(db: &dyn crate::Db, effect: EffectName) -> ReducIrTy {
    let mut tyvar_conv = IdConverter::new();

    let varp_ty = db.mk_reducir_ty(ReducIrTyKind::VarTy(0));
    // TODO: Produce members in order so we don't have to sort or get names here.
    let mut members = db
        .effect_members(effect)
        .iter()
        .map(|op| {
            let lower_ty_ctx = LowerTySchemeCtx::new(db.as_lower_reducir_db(), &mut tyvar_conv);
            let scheme = db.effect_member_sig(*op);
            let (ir_ty_scheme, _) =
                lower_ty_ctx.lower_scheme(effect.module(db.as_core_db()), &scheme);
            let ir_ty_scheme = match ir_ty_scheme.kind(db.as_reducir_db()) {
                ReducIrTyKind::FunTy(args, ret) => {
                    let (arg, rest) = args.split_at(1);

                    let mut ty = ret;
                    if !rest.is_empty() {
                        ty = db.mk_fun_ty(rest.iter().copied(), ret);
                    }
                    db.mk_fun_ty([arg[0], db.mk_fun_ty([ty], varp_ty)], varp_ty)
                }
                ty => panic!("{:?}", ty),
            };
            (db.effect_member_name(*op), ir_ty_scheme)
        })
        .collect::<Vec<_>>();

    members.sort_by(|a, b| a.0.cmp(&b.0));

    db.mk_reducir_ty(ReducIrTyKind::ForallTy(
        Kind::Type,
        db.mk_reducir_ty(ReducIrTyKind::ProductTy(vec![
            db.mk_reducir_ty(ReducIrTyKind::MarkerTy(varp_ty)),
            db.mk_reducir_ty(ReducIrTyKind::ProductTy(
                members.into_iter().map(|(_, ir_ty)| ir_ty).collect(),
            )),
        ])),
    ))
}

impl<DB> ReducIrEffectInfo for DB
where
    DB: ?Sized + crate::Db,
{
    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize {
        aiahr_nameres::effect_handler_op_index(self.as_nameres_db(), effect_op)
    }

    fn effect_handler_ir_ty(&self, effect: EffectName) -> ReducIrTy {
        effect_handler_ir_ty(self.as_lower_reducir_db(), effect)
    }
}
impl<DB> ItemWrappers for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_wrapper(&self, term_name: TermName, term: Idx<Term<VarId>>) -> &Wrapper {
        let typed_item = self.type_scheme_of(term_name);
        let wrappers = typed_item.item_to_wrappers(self.as_tc_db());
        &wrappers[&term]
    }
}

impl<DB> ItemSchemes for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_scheme(&self, term: TermName) -> TyScheme {
        let typed_item = self.type_scheme_of(term);
        typed_item.ty_scheme(self.as_tc_db())
    }
}
impl<DB> VarTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty {
        let typed_item = self.type_scheme_of(term);
        typed_item.var_to_tys(self.as_tc_db())[&var_id]
    }
}
impl<DB> TermTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_term(&self, name: TermName, term: Idx<Term<VarId>>) -> aiahr_tc::TyChkRes<InDb> {
        let typed_item = self.type_scheme_of(name);
        typed_item.term_to_tys(self.as_tc_db())[&term]
    }
}

#[cfg(test)]
mod tests {

    use std::path::PathBuf;

    use crate::Db as LowerIrDb;

    use aiahr_core::{
        file::{FileId, SourceFile, SourceFileSet},
        pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx},
        Db,
    };
    use aiahr_parser::Db as ParserDb;
    use aiahr_reducir::{ty::ReducIrTy, DelimReducIr, ReducIr, ReducIrTyErr, TypeCheck};
    use expect_test::expect;
    use pretty::RcAllocator;

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_reducir::Jar,
        aiahr_nameres::Jar,
        aiahr_parser::Jar,
        aiahr_tc::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn lower_function<R>(
        db: &TestDatabase,
        input: &str,
        fn_name: &str,
        op: impl FnOnce(&TestDatabase, PathBuf, &str) -> Option<R>,
    ) -> R {
        let path = std::path::PathBuf::from("test.aiahr");
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

        match op(db, path, fn_name) {
            Some(term) => term,
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }

    /// Lower a snippet and return the produced IR
    fn lower_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db DelimReducIr {
        let main = format!("f = {}", input);
        lower_function(db, &main, "f", |db, path, fn_name| {
            db.lower_reducir_item_for_file_name(path, db.ident_str(fn_name))
        })
        .item(db)
    }

    fn lower_mon_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db ReducIr {
        let main = format!("f = {}", input);
        lower_function(db, &main, "f", |db, path, fn_name| {
            db.lower_reducir_mon_item_for_file_name(path, db.ident_str(fn_name))
        })
        .item(db)
    }

    trait PrettyTyErr {
        fn to_pretty(self, db: &TestDatabase) -> String;
    }
    impl<'a, Ext: PrettyWithCtx<TestDatabase> + Clone> PrettyTyErr
        for Result<ReducIrTy, ReducIrTyErr<'a, Ext>>
    {
        fn to_pretty(self, db: &TestDatabase) -> String {
            match self {
                Ok(ty) => ty.pretty(db, &RcAllocator).pretty(80).to_string(),
                Err(err) => err.pretty(db, &RcAllocator).pretty(80).to_string(),
            }
        }
    }

    #[test]
    fn lower_id() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|x| x");
        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect!["(forall [(T1: Type) (T0: ScopedRow)] (fun [V0, V1] V1))"];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . {0} -> T1 -> T1"];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_product_literal() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|a| { x = a, y = a }");
        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: Type) (T0: ScopedRow)] (fun [V0]
                (let (V1 ((_row_simple_x_y @ T1) @ T1)) (fun [V2] (V1[0] V2 V2)))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . {0} -> T1 -> {T1, T1}"];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_wand() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|m| |n| (m ,, n).x");
        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall
              [(T5: Type) (T4: ScopedRow) (T3: SimpleRow) (T2: SimpleRow) (T1: SimpleRow) (T0: SimpleRow)]
              (fun [V1, V2, V0, V3, V4] (V2[3][0] (V1[0] V3 V4))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect![[r#"
            forall Type .
              forall ScopedRow .
                forall SimpleRow .
                  forall SimpleRow .
                    forall SimpleRow .
                      forall SimpleRow .
                        { {4} -> {3} -> {2}
                        , forall Type . (<5> -> T0) -> (<4> -> T0) -> <3> -> T0
                        , {{2} -> {4}, <4> -> <2>}
                        , {{2} -> {3}, <3> -> <2>}
                        } -> { {1} -> T5 -> {2}
                             , forall Type . (<2> -> T0) -> (T6 -> T0) -> <3> -> T0
                             , {{2} -> {1}, <1> -> <2>}
                             , {{2} -> T5, T5 -> <2>}
                             } -> {0} -> {4} -> {3} -> T5"#]];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_state_get() {
        let db = TestDatabase::default();

        let ir = lower_snippet(
            &db,
            r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))({})"#,
        );
        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                ((let
                  [ (V2 ((_row_simple_state_value @ {}) @ {}))
                  , (V3 (((_row_simple_return_putget @ {} -> ({} -> {} -> {{}, {}}) -> {} ->
                  {{}, {}}) @ {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}) @ {} -> {} ->
                  {{}, {}}))
                  , (V4 (((_row_simple_putget_return @ {} -> {} -> {{}, {}}) @ {} -> ({} ->
                  {} -> {{}, {}}) -> {} -> {{}, {}}) @ {} -> ({} -> {} -> {{}, {}}) -> {} ->
                  {{}, {}}))
                  , (V5 ((_row_simple_get_put @ {} -> ({} -> {} -> {{}, {}}) -> {} -> { {}
                                                                                      , {}
                                                                                      }) @ {}
                  -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}))
                  , (V6 (V4[0]
                    (V5[0] (fun [V7, V8, V9] (V8 V9 V9)) (fun [V10, V11, V12] (V11 {} V10)))
                    (fun [V13, V14] (V2[0] V14 V13))))
                  ]
                  (new_prompt [V18] (prompt V18 (fun [V0] (V1[0] V0 {V18, (V4[2][0] V6)}))
                    (V4[3][0]
                      V6
                      (let (V15 {})
                        (let (V16 (V1[3][0] V0))
                          (yield V16[0] (fun [V17] (V16[1][1] V15 V17))))))))) {})))"#]];
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
                } -> {1} -> {{}, {}}"#]];
        let pretty_ir_ty = {
            let this = ir.type_check(&db);
            let db = &db;
            match this {
                Ok(ty) => ty.pretty(db, &RcAllocator).pretty(80).to_string(),
                Err(err) => {
                    println!("{}", err.pretty(db, &RcAllocator).pretty(80));
                    panic!();
                }
            }
        };
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn monadic_lower_state_get() {
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

        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                ((((__mon_bind @ {1}) @ {} -> {{}, {}}) @ {{}, {}})
                  (let
                    [ (V2 ((_row_simple_state_value @ {}) @ {}))
                    , (V3 (((_row_simple_return_putget @ {} -> ({} -> {} -> {{}, {}}) -> {}
                    -> {{}, {}}) @ {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}) @ {} ->
                    {} -> {{}, {}}))
                    , (V4 (((_row_simple_putget_return @ {} -> {} -> {{}, {}}) @ {} -> ({}
                    -> {} -> {{}, {}}) -> {} -> {{}, {}}) @ {} -> ({} -> {} -> {{}, {}}) ->
                    {} -> {{}, {}}))
                    , (V5 ((_row_simple_get_put @ {} -> ({} -> {} -> {{}, {}}) -> {} -> { {}
                                                                                        , {}
                                                                                        }) @ {}
                    -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}))
                    , (V6 (V4[0]
                      (V5[0]
                        (fun [V7, V8, V9] (V8 V9 V9))
                        (fun [V10, V11, V12] (V11 {} V10)))
                      (fun [V13, V14] (V2[0] V14 V13))))
                    ]
                    (((__mon_freshm @ {} -> {{}, {}}) @ {1} -> (Control {1} {} -> {{}, {}}))
                      (fun [V18]
                        ((((__mon_prompt @ {1}) @ {0}) @ {} -> {{}, {}})
                          V18
                          (fun [V0] (V1[0] V0 {V18, (V4[2][0] V6)}))
                          ((((__mon_bind @ {0}) @ {}) @ {} -> {{}, {}})
                            (let (V15 {})
                              (let (V16 (V1[3][0] V0))
                                (fun [V0]
                                  <1: {V16[0], (fun [V17] (V16[1][1] V15 V17)), (fun [V0]
                                      V0)}>)))
                            (fun [V27]
                              (let (V28 (V4[3][0] V6 V27)) (fun [V0] <0: V28>))))))))
                  (fun [V29] (let (V30 (V29 {})) (fun [V0] <0: V30>))))))"#]];
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
                } -> {1} -> {1} -> (Control {1} {{}, {}})"#]];
        let pretty_ty = ir
            .type_check(&db)
            .map_err_pretty_with(&db)
            .unwrap()
            .pretty_with(&db)
            .pprint()
            .pretty(80)
            .to_string();
        expect_ty.assert_eq(&pretty_ty);
    }
}
