#![allow(dead_code)]

use aiahr_core::id::ReducIrVarId;
use aiahr_reducir::{ReducIr, ReducIrKind, ReducIrVar, P};
use rustc_hash::{FxHashMap, FxHashSet};

/// A Prompt that delimits the stack for delimited continuations
#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct Prompt(usize);

/// A mapping from variables to values
type Env = FxHashMap<ReducIrVarId, Value>;

/// An interpreter value.
/// This will be the result of interpretation and all intermediate computations
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(usize),
    /// A lambda (or closure). Stores any captured variables in env.
    Lam {
        env: FxHashMap<ReducIrVarId, Value>,
        arg: ReducIrVar,
        body: P<ReducIr>,
    },
    /// A tuple of values
    Tuple(Vec<Value>),
    /// A tagged value, this is used as the discriminant of case
    Tag(usize, Box<Value>),
    /// A prompt that can be used to delimit the stack
    Prompt(Prompt),
    /// A continuation is a slice of the stack reified as a value
    Cont(Stack),
    Vector(im::Vector<Value>),
}

use pretty::{docs, DocAllocator, DocBuilder, Pretty};

fn pretty_env<'a, D, A: 'a>(
    env: &FxHashMap<ReducIrVarId, Value>,
    a: &'a D,
) -> pretty::DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    DocBuilder<'a, D, A>: Clone,
{
    if env.is_empty() {
        return a.nil();
    }

    a.intersperse(
        env.iter().map(|(ir_var, _)| ir_var.pretty(a)),
        a.text(",").append(a.softline()),
    )
    .brackets()
}

impl Value {
    pub fn pretty<'a, D, DB>(&self, db: &DB, a: &'a D) -> pretty::DocBuilder<'a, D>
    where
        D: DocAllocator<'a> + 'a,
        DocBuilder<'a, D>: Clone,
        DB: ?Sized + aiahr_reducir::Db,
        D::Doc: pretty::Pretty<'a, D> + Clone,
    {
        match self {
            Value::Int(i) => a.as_string(i),
            Value::Lam { env, arg, body } => docs![
                a,
                pretty_env(env, a),
                "|",
                arg,
                "|",
                a.line(),
                body.pretty(db, a).nest(2).align(),
            ],
            Value::Tuple(vals) => a
                .intersperse(
                    vals.iter().map(|val| val.pretty(db, a)),
                    a.text(",").append(a.softline()),
                )
                .align()
                .nest(2)
                .braces(),
            Value::Tag(tag, val) => a
                .as_string(tag)
                .append(a.text(":"))
                .append(a.softline())
                .append(val.pretty(db, a))
                .group()
                .angles(),
            Value::Prompt(_) => todo!(),
            Value::Cont(_) => todo!(),
            Value::Vector(_) => todo!(),
        }
    }
}

impl Value {
    fn unwrap_prompt(self) -> Prompt {
        match self {
            Value::Prompt(prompt) => prompt,
            _ => panic!("Stuck: expected prompt value but received {:?}", self),
        }
    }

    fn unwrap_vector(self) -> im::Vector<Value> {
        match self {
            Value::Vector(evv) => evv,
            _ => panic!("Stuck: expected vector value but received {:?}", self),
        }
    }
}

/// An evaluation context
/// These store in process computation, and are pushed onto the stack as we interpret.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalCtx {
    FnApp {
        arg: Value,
    },
    ArgApp {
        func: P<ReducIr>,
    },
    PromptMarker {
        body: P<ReducIr>,
    },
    YieldMarker {
        value: P<ReducIr>,
    },
    YieldValue {
        marker: Value,
    },
    StructEval {
        vals: Vec<Value>,
        rest: Vec<P<ReducIr>>,
    },
    Index {
        index: usize,
    },
    Tag {
        tag: usize,
    },
    CaseScrutinee {
        branches: Vec<P<ReducIr>>,
    },
    VectorSet {
        evv: ReducIrVar,
        index: usize,
    },
}

/// A stack frame is either a prompt or a list of evaluation contexts.
#[derive(Debug, Clone, PartialEq)]
pub enum StackFrame {
    Prompt(Prompt),
    Eval(Env, Vec<EvalCtx>),
}

/// A stack is a sequence of stack frames
type Stack = Vec<StackFrame>;
trait StackExt
where
    Self: Sized,
{
    fn split_off_prompt(&mut self, prompt: Prompt) -> Self;
}
impl StackExt for Stack {
    fn split_off_prompt(&mut self, prompt: Prompt) -> Self {
        // rposition so we find the innermost instance of a prompt
        let idx = self
            .iter()
            .rposition(|stack_frame| match stack_frame {
                StackFrame::Prompt(p) => p == &prompt,
                StackFrame::Eval(_, _) => false,
            })
            .unwrap_or_else(|| panic!("Stuck: could not find prompt {:?} in stack", prompt));
        self.split_off(idx)
    }
}

/// Virtual Machine that interprets the
#[derive(Default)]
pub struct Machine {
    stack: Stack,
    prompt: usize,
    cur_frame: Vec<EvalCtx>,
    cur_env: Env,
}

enum InterpretResult {
    Step(P<ReducIr>),
    Done(Value),
}

impl Machine {
    fn pop_stack(&mut self, val: Value) -> InterpretResult {
        match self.stack.pop() {
            // If we found a new stack frame move it into current and unwind into it
            Some(StackFrame::Eval(env, frame)) => {
                self.cur_env = env;
                self.cur_frame = frame;
                self.unwind(val)
            }
            // Skip over any prompts we encounter during unwinding
            Some(StackFrame::Prompt(_)) => self.pop_stack(val),
            // If our stack is empty this is our final result
            None => InterpretResult::Done(val),
        }
    }

    fn unwind(&mut self, val: Value) -> InterpretResult {
        match self.cur_frame.pop() {
            None => self.pop_stack(val),
            Some(eval_ctx) => match eval_ctx {
                EvalCtx::ArgApp { func } => {
                    self.cur_frame.push(EvalCtx::FnApp { arg: val });
                    InterpretResult::Step(func)
                }
                EvalCtx::FnApp { arg } => match val {
                    Value::Lam {
                        env,
                        arg: arg_var,
                        body,
                    } => {
                        self.cur_env.extend(env);
                        self.cur_env.insert(arg_var.var, arg);
                        InterpretResult::Step(body)
                    }
                    Value::Cont(substack) => {
                        self.new_stack_frame();
                        self.stack.extend(substack);
                        self.unwind(arg)
                    }
                    _ => panic!("Stuck: called a non funciton object"),
                },
                EvalCtx::PromptMarker { body } => {
                    let prompt = val.unwrap_prompt();
                    self.new_stack_frame();
                    self.stack.push(StackFrame::Prompt(prompt));
                    InterpretResult::Step(body)
                }
                EvalCtx::YieldMarker { value } => {
                    self.cur_frame.push(EvalCtx::YieldValue { marker: val });
                    InterpretResult::Step(value)
                }
                EvalCtx::YieldValue { marker } => {
                    let prompt = marker.unwrap_prompt();
                    self.new_stack_frame();
                    let substack = self.stack.split_off_prompt(prompt);
                    match val {
                        Value::Lam { env, arg, body } => {
                            self.cur_env.extend(env);
                            self.cur_env.insert(arg.var, Value::Cont(substack));
                            InterpretResult::Step(body)
                        }
                        _ => panic!("Stuck: expected a function as Yield value"),
                    }
                }
                EvalCtx::StructEval { mut vals, mut rest } => match rest.pop() {
                    Some(ir) => {
                        vals.push(val);
                        self.cur_frame.push(EvalCtx::StructEval { vals, rest });
                        InterpretResult::Step(ir)
                    }
                    None => {
                        vals.push(val);
                        self.unwind(Value::Tuple(vals))
                    }
                },
                EvalCtx::Index { index } => match val {
                    Value::Tuple(mut elems) => self.unwind(elems.swap_remove(index)),
                    _ => panic!("Stuck: non-tuple value passed to FieldAccess"),
                },
                EvalCtx::CaseScrutinee { mut branches } => {
                    let (tag, discr) = match val {
                        Value::Tag(tag, discr) => (tag, discr),
                        _ => panic!("Stuck: non-tagged avlue passed to Case"),
                    };
                    let branch = branches.swap_remove(tag);
                    self.cur_frame.push(EvalCtx::FnApp { arg: *discr });
                    InterpretResult::Step(branch)
                }
                EvalCtx::Tag { tag } => self.unwind(Value::Tag(tag, Box::new(val))),
                EvalCtx::VectorSet { evv, index } => {
                    let vec = self.lookup_var(evv).unwrap_vector();
                    self.unwind(Value::Vector(vec.update(index, val)))
                }
            },
        }
    }

    fn new_stack_frame(&mut self) {
        let env = std::mem::take(&mut self.cur_env);
        let frame = std::mem::take(&mut self.cur_frame);

        match self.stack.last_mut() {
            Some(StackFrame::Eval(ref mut last_env, ref mut last_frame)) => {
                // If our last stack frame is an Eval we can add this frame onto and avoid a new frame
                last_env.extend(env);
                last_frame.extend(frame);
            }
            _ => {
                self.stack.push(StackFrame::Eval(env, frame));
            }
        }
    }

    fn lookup_var(&self, v: ReducIrVar) -> Value {
        self.cur_env
            .get(&v.var)
            .or_else(|| {
                self.stack.iter().rev().find_map(|frame| match frame {
                    StackFrame::Prompt(_) => None,
                    StackFrame::Eval(env, _) => env.get(&v.var),
                })
            })
            .unwrap_or_else(|| panic!("Stuck: Undefined variable {:?}", v))
            // Todo remove this clone.
            .clone()
    }

    fn step(&mut self, ir: ReducIr) -> InterpretResult {
        match ir.kind {
            // Literals
            ReducIrKind::Int(i) => {
                log::info!("Unwind: Int({})", i);
                self.unwind(Value::Int(i))
            }
            // Lambda Calculus
            ReducIrKind::Var(v) => {
                log::info!("Unwind: Var({:?})", v);
                self.unwind(self.lookup_var(v))
            }
            ReducIrKind::Abs(arg, body) => {
                log::info!("Unwind: Lam({:?}, {:?})", arg, body);
                // TODO: make this check each env scope for all variables rathan than looking up
                // all variables one by one.
                let env = body
                    .unbound_vars_with_bound(FxHashSet::from_iter(std::iter::once(arg.var)))
                    .map(|v| (v.var, self.lookup_var(v)))
                    .collect();
                self.unwind(Value::Lam { env, arg, body })
            }
            ReducIrKind::App(func, arg) => {
                log::info!("Step: App({:?}, {:?})", func, arg);
                self.cur_frame.push(EvalCtx::ArgApp { func });
                InterpretResult::Step(arg)
            }
            // Product rows
            ReducIrKind::Struct(mut elems) => {
                elems.reverse();
                match elems.pop() {
                    // if elems is empty unwind with unit value
                    None => {
                        log::info!("Unwind: {{}}");
                        self.unwind(Value::Tuple(vec![]))
                    }
                    Some(next) => {
                        log::info!("Step: Struct({:?})", elems);
                        self.cur_frame.push(EvalCtx::StructEval {
                            vals: vec![],
                            rest: elems,
                        });
                        InterpretResult::Step(next)
                    }
                }
            }
            ReducIrKind::FieldProj(index, value) => {
                log::info!("Step: FieldProj({:?}, {:?})", value, index);
                self.cur_frame.push(EvalCtx::Index { index });
                InterpretResult::Step(value)
            }
            // Sum rows
            ReducIrKind::Tag(_, tag, value) => {
                log::info!("Step: Tag({:?}, {:?})", tag, value);
                self.cur_frame.push(EvalCtx::Tag { tag });
                InterpretResult::Step(value)
            }
            ReducIrKind::Case(_, discr, branches) => {
                log::info!("Step: Case({:?}, {:?})", discr, branches);
                self.cur_frame.push(EvalCtx::CaseScrutinee { branches });
                InterpretResult::Step(discr)
            }
            // Delimited continuations
            ReducIrKind::NewPrompt(arg, body) => {
                let prompt = Prompt(self.prompt);
                self.prompt += 1;
                self.cur_env.insert(arg.var, Value::Prompt(prompt));
                InterpretResult::Step(body)
            }
            ReducIrKind::Prompt(marker, body) => {
                self.cur_frame.push(EvalCtx::PromptMarker { body });
                InterpretResult::Step(marker)
            }
            ReducIrKind::Yield(_, marker, value) => {
                self.cur_frame.push(EvalCtx::YieldMarker { value });
                InterpretResult::Step(marker)
            }
            // Type applications
            // TODO: Handle type applications but they shouldn't show up in real runtime
            ReducIrKind::TyAbs(_, body) => InterpretResult::Step(body),
            ReducIrKind::TyApp(_, _) => todo!(),
            ReducIrKind::Item(_, _) => todo!(),
        }
    }

    /// Interpret an IR term until it is a value, or diverge.
    pub fn interpret(&mut self, top: ReducIr) -> Value {
        let mut ir = top;
        loop {
            match self.step(ir) {
                InterpretResult::Step(next) => ir = next.into_inner(),
                InterpretResult::Done(value) => return value,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use aiahr_reducir::ty::{MkReducIrTy, ReducIrTyKind};

    use super::*;

    #[derive(Default)]
    #[salsa::db(
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_reducir::Jar,
        aiahr_lower_reducir::Jar,
        aiahr_nameres::Jar,
        aiahr_parser::Jar,
        aiahr_tc::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    #[test]
    fn interpret_id_fun() {
        let db = TestDatabase::default();
        let x = ReducIrVar {
            var: ReducIrVarId(0),
            ty: db.mk_reducir_ty(ReducIrTyKind::IntTy),
        };
        let ir = ReducIr::app(
            ReducIr::abss([x], ReducIr::var(x)),
            [ReducIr::new(ReducIrKind::Int(1))],
        );
        let mut interpreter = Machine::default();
        assert_eq!(interpreter.interpret(ir), Value::Int(1));
    }

    #[test]
    fn interpret_lamba_captures_env_as_expected() {
        let db = TestDatabase::default();
        let x = ReducIrVar {
            var: ReducIrVarId(0),
            ty: db.mk_reducir_ty(ReducIrTyKind::IntTy),
        };
        let y = ReducIrVar {
            var: ReducIrVarId(1),
            ty: db.mk_reducir_ty(ReducIrTyKind::IntTy),
        };
        let ir = ReducIr::app(
            ReducIr::abss([x, y], ReducIr::var(x)),
            [
                ReducIr::new(ReducIrKind::Int(2)),
                ReducIr::new(ReducIrKind::Int(0)),
            ],
        );
        let mut interpreter = Machine::default();
        assert_eq!(interpreter.interpret(ir), Value::Int(2));
    }
}
