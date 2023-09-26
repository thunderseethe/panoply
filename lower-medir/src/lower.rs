use aiahr_core::id::MedIrVarId;
use aiahr_core::id_converter::IdConverter;
use aiahr_medir as medir;
use aiahr_medir::MedIrItemName;
use aiahr_reducir::{Lets, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar};
use medir::{Atom, Locals, MedIr, MedIrKind, MedIrVar};

pub(crate) struct LowerCtx<'a> {
    db: &'a dyn crate::Db,
    current: ReducIrTermName,
    var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
    pub(crate) lifts: Vec<medir::Defn>,
}

impl<'a> LowerCtx<'a> {
    pub(crate) fn new(
        db: &'a dyn crate::Db,
        current: ReducIrTermName,
        var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
    ) -> Self {
        Self {
            db,
            current,
            var_converter,
            lifts: vec![],
        }
    }

    pub(crate) fn lower_item(&mut self, reducir: &ReducIr<Lets>) -> medir::Defn {
        let mut binds = vec![];
        let (params, body) = match reducir.try_top_level_def() {
            Ok(top_level) => {
                let params = top_level
                    .vars
                    .iter()
                    .map(|var| self.var_converter.convert(var.var))
                    .map(MedIrVar::new)
                    .collect();
                (params, self.lower_binds(top_level.body, &mut binds))
            }
            Err(body) => (vec![], self.lower_binds(body, &mut binds)),
        };
        medir::Defn {
            name: MedIrItemName::new(self.current),
            params,
            body: medir::Locals { binds, body },
        }
    }

    fn mk_atom(&mut self, body: MedIr, binds: &mut Vec<(MedIrVar, MedIr)>) -> Atom {
        match body.kind {
            MedIrKind::Atom(atom) => atom,
            _ => {
                let v = MedIrVar::new(self.var_converter.generate());
                binds.push((v, body));
                Atom::Var(v)
            }
        }
    }

    fn lower_binds(&mut self, body: &ReducIr<Lets>, binds: &mut Vec<(MedIrVar, MedIr)>) -> MedIr {
        match body.kind() {
            ReducIrKind::Int(i) => MedIr::int(*i),
            ReducIrKind::Var(v) => {
                let var = self.var_converter.convert(v.var);
                MedIr::var(MedIrVar::new(var))
            }
            ReducIrKind::Item(name, _) => {
                MedIr::new(MedIrKind::Closure(MedIrItemName::new(*name), vec![]))
            }
            ReducIrKind::Abs(vars, body) => {
                let (name, free_vars) = self.closure_convert(vars, body);
                MedIr::new(MedIrKind::Closure(name, free_vars))
            }
            ReducIrKind::App(head, spine) => {
                let medir_spine = spine
                    .iter()
                    .map(|arg| {
                        let medir = self.lower_binds(arg, binds);
                        match &medir.kind {
                            MedIrKind::Atom(atom) => *atom,
                            _ => {
                                let var = MedIrVar::new(self.var_converter.generate());
                                binds.push((var, medir));
                                medir::Atom::Var(var)
                            }
                        }
                    })
                    .collect();
                let medir = self.lower_binds(head, binds);
                MedIr::new(match medir.kind {
                    MedIrKind::Atom(medir::Atom::Var(v)) => {
                        MedIrKind::Call(medir::Call::Unknown(v), medir_spine)
                    }
                    MedIrKind::Closure(item, args) => {
                        let args = args
                            .into_iter()
                            .map(medir::Atom::Var)
                            .chain(medir_spine)
                            .collect();
                        MedIrKind::Call(medir::Call::Known(item), args)
                    }
                    kind => {
                        let v = MedIrVar::new(self.var_converter.generate());
                        binds.push((v, MedIr::new(kind)));
                        MedIrKind::Call(medir::Call::Unknown(v), medir_spine)
                    }
                })
            }
            // We get rid of types in this IR
            ReducIrKind::TyAbs(_, body) | ReducIrKind::TyApp(body, _) => {
                self.lower_binds(body, binds)
            }
            ReducIrKind::Struct(elems) => {
                // TODO: Flatten?
                MedIr::new(MedIrKind::Blocks(
                    elems
                        .iter()
                        .map(|elem| {
                            let e = self.lower_binds(elem, binds);
                            self.mk_atom(e, binds)
                        })
                        .collect(),
                ))
            }
            ReducIrKind::FieldProj(indx, body) => {
                let base = self.lower_binds(body, binds);
                let base_var = match self.mk_atom(base, binds) {
                    Atom::Var(v) => v,
                    _ => unreachable!(),
                };
                MedIr::new(MedIrKind::BlockAccess(base_var, *indx))
            }
            ReducIrKind::Tag(_, tag, body) => {
                // TODO: Flatten?
                let value = self.lower_binds(body, binds);
                let value_atom = self.mk_atom(value, binds);
                MedIr::new(MedIrKind::Blocks(vec![Atom::Int(*tag), value_atom]))
            }
            ReducIrKind::Case(_, scrutinee, branches) => {
                let medir_scrutinee = self.lower_binds(scrutinee, binds);
                let scrutinee_var = match self.mk_atom(medir_scrutinee, binds) {
                    Atom::Var(v) => v,
                    Atom::Int(_) => unreachable!(),
                };
                let discr_var = MedIrVar::new(self.var_converter.generate());
                binds.push((
                    discr_var,
                    MedIr::new(MedIrKind::BlockAccess(scrutinee_var, 0)),
                ));
                let cases = branches
                    .iter()
                    .map(|branch| {
                        match branch.kind() {
                            ReducIrKind::Abs(vars, body) => {
                                let branch_var =
                                    MedIrVar::new(self.var_converter.convert(vars[0].var));
                                let scrutinee_value =
                                    MedIr::new(MedIrKind::BlockAccess(scrutinee_var, 1));
                                //TODO: Handle flattening?
                                let mut binds = vec![(branch_var, scrutinee_value)];
                                let body = self.lower_binds(body, &mut binds);
                                medir::Locals { binds, body }
                            }
                            _ => unreachable!(),
                        }
                    })
                    .collect();
                MedIr::new(MedIrKind::Switch(Atom::Var(discr_var), cases))
            }
            ReducIrKind::X(Lets {
                binds: reducir_binds,
                body,
            }) => {
                for (var, body) in reducir_binds.iter() {
                    let defn = self.lower_binds(body, binds);
                    binds.push((MedIrVar::new(self.var_converter.convert(var.var)), defn));
                }
                self.lower_binds(body, binds)
            }
        }
    }

    fn closure_convert(
        &mut self,
        vars: &[ReducIrVar],
        body: &ReducIr<Lets>,
    ) -> (MedIrItemName, Vec<MedIrVar>) {
        let mut free_vars = body
            .free_var_set()
            .into_iter()
            .filter(|var| !vars.contains(var))
            .map(|var| self.var_converter.convert(var.var))
            .map(MedIrVar::new)
            .collect::<Vec<_>>();
        free_vars.sort();

        let params = free_vars
            .iter()
            .copied()
            .chain(
                vars.iter()
                    .copied()
                    .map(|var| self.var_converter.convert(var.var))
                    .map(MedIrVar::new),
            )
            .collect();
        let mut binds = vec![];
        let body = self.lower_binds(body, &mut binds);

        let num = self.lifts.len();
        let current_name = self.current.name(self.db).text(self.db.as_core_db());
        let module = self.current.module(self.db);
        let lift_name =
            ReducIrTermName::gen(self.db, format!("{}_lam_{}", current_name, num), module);
        let name = MedIrItemName::new(lift_name);
        self.lifts.push(medir::Defn {
            name,
            params,
            body: Locals { binds, body },
        });
        (name, free_vars)
    }
}
