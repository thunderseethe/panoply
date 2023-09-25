#[salsa::jar(db = Db)]
pub struct Jar();
pub trait Db: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db {
    fn as_lower_medir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db {}

mod lower {
    use aiahr_core::id::MedIrVarId;
    use aiahr_core::id_converter::IdConverter;
    use aiahr_medir as medir;
    use aiahr_medir::MedIrItem;
    use aiahr_reducir::{Lets, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName};
    use medir::{Atom, MedIr, MedIrKind, MedIrVar};

    pub(crate) struct LowerCtx<'a> {
        db: &'a dyn crate::Db,
        var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
    }

    impl<'a> LowerCtx<'a> {
        pub(crate) fn new(
            db: &'a dyn crate::Db,
            var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
        ) -> Self {
            Self { db, var_converter }
        }

        pub(crate) fn lower_item(
            &mut self,
            name: ReducIrTermName,
            reducir: &ReducIr<Lets>,
        ) -> medir::Defn {
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
                name: MedIrItem::new(name),
                params,
                body: medir::Locals { binds, body },
            }
        }

        fn to_atom(&mut self, body: MedIr, binds: &mut Vec<(MedIrVar, MedIr)>) -> Atom {
            match body.kind {
                MedIrKind::Atom(atom) => atom,
                _ => {
                    let v = MedIrVar::new(self.var_converter.generate());
                    binds.push((v, body));
                    Atom::Var(v)
                }
            }
        }

        fn lower_binds(
            &mut self,
            body: &ReducIr<Lets>,
            binds: &mut Vec<(MedIrVar, MedIr)>,
        ) -> MedIr {
            match body.kind() {
                ReducIrKind::Int(i) => MedIr::int(*i),
                ReducIrKind::Var(v) => {
                    let var = self.var_converter.convert(v.var);
                    MedIr::var(MedIrVar::new(var))
                }
                ReducIrKind::Item(name, _) => {
                    MedIr::new(MedIrKind::Closure(MedIrItem::new(*name), vec![]))
                }
                ReducIrKind::Abs(_, _) => todo!(), // lambda lift
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
                                self.to_atom(e, binds)
                            })
                            .collect(),
                    ))
                }
                ReducIrKind::FieldProj(indx, body) => {
                    let base = self.lower_binds(body, binds);
                    let base_var = match self.to_atom(base, binds) {
                        Atom::Var(v) => v,
                        _ => unreachable!(),
                    };
                    MedIr::new(MedIrKind::BlockAccess(base_var, *indx))
                }
                ReducIrKind::Tag(_, tag, body) => {
                    // TODO: Flatten?
                    let value = self.lower_binds(body, binds);
                    let value_atom = self.to_atom(value, binds);
                    MedIr::new(MedIrKind::Blocks(vec![Atom::Int(*tag), value_atom]))
                }
                ReducIrKind::Case(_, scrutinee, branches) => {
                    let medir_scrutinee = self.lower_binds(scrutinee, binds);
                    let scrutinee_var = match self.to_atom(medir_scrutinee, binds) {
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
    }
}
