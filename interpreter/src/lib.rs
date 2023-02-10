#![allow(dead_code)]

use aiahr_core::id::IrVarId;
use aiahr_core::ir::{Ir, IrKind, IrVar, P};
use rustc_hash::{FxHashMap, FxHashSet};

/// A Prompt that delimits the stack for delimited continuations
#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
struct Prompt(usize);

/// A mapping from variables to values
type Env<'ctx> = FxHashMap<IrVarId, Value<'ctx>>;

/// An interpreter value.
/// This will be the result of interpretation and all intermediate computations
#[derive(Debug, Clone, PartialEq)]
enum Value<'ctx> {
    Int(usize),
    /// A lambda (or closure). Stores any captured variables in env.
    Lam {
        env: FxHashMap<IrVarId, Value<'ctx>>,
        arg: IrVar<'ctx>,
        body: P<Ir<'ctx>>,
    },
    /// A tuple of values
    Tuple(Vec<Value<'ctx>>),
    /// A tagged value, this is used as the discriminant of case
    Tag(usize, Box<Value<'ctx>>),
    /// A prompt that can be used to delimit the stack
    Prompt(Prompt),
    /// A continuation is a slice of the stack reified as a value
    Cont(Stack<'ctx>),
    Vector(im::Vector<Value<'ctx>>),
}

impl<'ctx> Value<'ctx> {
    fn unwrap_prompt(self) -> Prompt {
        match self {
            Value::Prompt(prompt) => prompt,
            _ => panic!("Stuck: expected prompt value but received {:?}", self),
        }
    }

    fn unwrap_vector(self) -> im::Vector<Value<'ctx>> {
        match self {
            Value::Vector(evv) => evv,
            _ => panic!("Stuck: expected vector value but received {:?}", self),
        }
    }
}

/// An evaluation context
/// These store in process computation, and are pushed onto the stack as we interpret.
#[derive(Debug, Clone, PartialEq)]
enum EvalCtx<'ctx> {
    FnApp {
        arg: Value<'ctx>,
    },
    ArgApp {
        func: P<Ir<'ctx>>,
    },
    PromptMarker {
        body: P<Ir<'ctx>>,
    },
    YieldMarker {
        value: P<Ir<'ctx>>,
    },
    YieldValue {
        marker: Value<'ctx>,
    },
    StructEval {
        vals: Vec<Value<'ctx>>,
        rest: Vec<P<Ir<'ctx>>>,
    },
    Index {
        index: usize,
    },
    Tag {
        tag: usize,
    },
    CaseScrutinee {
        branches: Vec<P<Ir<'ctx>>>,
    },
    VectorSet {
        evv: IrVar<'ctx>,
        index: usize,
    },
}

/// A stack frame is either a prompt or a list of evaluation contexts.
#[derive(Debug, Clone, PartialEq)]
enum StackFrame<'ctx> {
    Prompt(Prompt),
    Eval(Env<'ctx>, Vec<EvalCtx<'ctx>>),
}

/// A stack is a sequence of stack frames
type Stack<'ctx> = Vec<StackFrame<'ctx>>;
trait StackExt
where
    Self: Sized,
{
    fn split_off_prompt(&mut self, prompt: Prompt) -> Self;
}
impl<'ctx> StackExt for Stack<'ctx> {
    fn split_off_prompt(&mut self, prompt: Prompt) -> Self {
        // rposition so we find the innermost instance of a prompt
        let idx = self
            .iter()
            .rposition(|stack_frame| match stack_frame {
                StackFrame::Prompt(p) => p == &prompt,
                StackFrame::Eval(_, _) => false,
            })
            .expect(&format!(
                "Stuck: could not find prompt {:?} in stack",
                prompt
            ));
        self.split_off(idx)
    }
}

/// Virtual Machine that interprets the
#[derive(Default)]
struct Machine<'ctx> {
    stack: Stack<'ctx>,
    prompt: usize,
    cur_frame: Vec<EvalCtx<'ctx>>,
    cur_env: Env<'ctx>,
}

enum InterpretResult<'ctx> {
    Step(P<Ir<'ctx>>),
    Done(Value<'ctx>),
}

impl<'ctx> Machine<'ctx> {
    fn pop_stack(&mut self, val: Value<'ctx>) -> InterpretResult<'ctx> {
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

    fn unwind(&mut self, val: Value<'ctx>) -> InterpretResult<'ctx> {
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
        let env = std::mem::replace(&mut self.cur_env, FxHashMap::default());
        let frame = std::mem::replace(&mut self.cur_frame, vec![]);

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

    fn lookup_var(&self, v: IrVar) -> Value<'ctx> {
        self.cur_env
            .get(&v.var)
            .or_else(|| {
                self.stack.iter().rev().find_map(|frame| match frame {
                    StackFrame::Prompt(_) => None,
                    StackFrame::Eval(env, _) => env.get(&v.var),
                })
            })
            .expect(&format!("Stuck: Undefined variable {:?}", v))
            // Todo remove this clone.
            .clone()
    }

    fn step(&mut self, ir: Ir<'ctx>) -> InterpretResult<'ctx> {
        match ir.kind {
            // Literals
            IrKind::Int(i) => self.unwind(Value::Int(i)),
            // Lambda Calculus
            IrKind::Var(v) => self.unwind(self.lookup_var(v)),
            IrKind::Abs(arg, body) => {
                // TODO: make this check each env scope for all variables rathan than looking up
                // all variables one by one.
                let env = body
                    .unbound_vars_with_bound(FxHashSet::from_iter(std::iter::once(arg.var)))
                    .map(|v| (v.var, self.lookup_var(v)))
                    .collect();
                self.unwind(Value::Lam { env, arg, body })
            }
            IrKind::App(func, arg) => {
                self.cur_frame.push(EvalCtx::ArgApp { func });
                InterpretResult::Step(arg)
            }
            // Product rows
            IrKind::Struct(mut elems) => {
                elems.reverse();
                match elems.pop() {
                    // if elems is empty unwind with unit value
                    None => self.unwind(Value::Tuple(vec![])),
                    Some(next) => {
                        self.cur_frame.push(EvalCtx::StructEval {
                            vals: vec![],
                            rest: elems,
                        });
                        InterpretResult::Step(next)
                    }
                }
            }
            IrKind::FieldProj(index, value) => {
                self.cur_frame.push(EvalCtx::Index { index });
                InterpretResult::Step(value)
            }
            // Sum rows
            IrKind::Tag(tag, value) => {
                self.cur_frame.push(EvalCtx::Tag { tag });
                InterpretResult::Step(value)
            }
            IrKind::Case(discr, branches) => {
                self.cur_frame.push(EvalCtx::CaseScrutinee { branches });
                InterpretResult::Step(discr)
            }
            // Delimited continuations
            IrKind::NewPrompt(arg, body) => {
                let prompt = Prompt(self.prompt);
                self.prompt += 1;
                self.cur_env.insert(arg.var, Value::Prompt(prompt));
                InterpretResult::Step(body)
            }
            IrKind::Prompt(marker, body) => {
                self.cur_frame.push(EvalCtx::PromptMarker { body });
                InterpretResult::Step(marker)
            }
            IrKind::Yield(marker, value) => {
                self.cur_frame.push(EvalCtx::YieldMarker { value });
                InterpretResult::Step(marker)
            }
            // Effect evidence vector operations
            IrKind::VectorSet(evv, index, value) => {
                self.cur_frame.push(EvalCtx::VectorSet { evv, index });
                InterpretResult::Step(value)
            }
            IrKind::VectorGet(evv, index) => {
                let vec = self.lookup_var(evv).unwrap_vector();
                let val = vec.get(index).expect(&format!(
                    "Stuck: expected index {} to exist in vector but it did not",
                    index
                ));
                // Remove this clone
                self.unwind(val.clone())
            }
            // Type applications
            IrKind::TyAbs(_, _) => todo!(),
            IrKind::TyApp(_, _) => todo!(),
        }
    }

    /// Interpret an IR term until it is a value, or diverge.
    pub fn interpret(&mut self, top: Ir<'ctx>) -> Value<'ctx> {
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
    use aiahr_core::ir::{IrTy, IrTyKind};
    use aiahr_core::memory::handle::Handle;

    use super::*;

    #[test]
    fn interpret_id_fun() {
        let x = IrVar {
            var: IrVarId(0),
            ty: IrTy(Handle(&IrTyKind::IntTy)),
        };
        let ir = Ir::app(Ir::abss([x], Ir::var(x)), [Ir::new(IrKind::Int(1))]);
        let mut interpreter = Machine::default();
        assert_eq!(interpreter.interpret(ir), Value::Int(1));
    }

    #[test]
    fn interpret_lamba_captures_env_as_expected() {
        let x = IrVar {
            var: IrVarId(0),
            ty: IrTy(Handle(&IrTyKind::IntTy)),
        };
        let y = IrVar {
            var: IrVarId(1),
            ty: IrTy(Handle(&IrTyKind::IntTy)),
        };
        let ir = Ir::app(
            Ir::abss([x, y], Ir::var(x)),
            [Ir::new(IrKind::Int(2)), Ir::new(IrKind::Int(0))],
        );
        let mut interpreter = Machine::default();
        assert_eq!(interpreter.interpret(ir), Value::Int(2));
    }
}