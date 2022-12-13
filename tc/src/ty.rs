use aiahr_core::define_ids;
use aiahr_core::id::VarId;
use bumpalo::Bump;

define_ids!(
/// A type variable.
/// These are explicity referred to by the AST and can persist through type checking.
/// They may not be modified by the type checking process, often referred to as untouchabale.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcVar;

/// A unifier variable.
/// These are produced during the type checking process and MUST NOT persist outside the type
/// checker. They may not appear in the AST once type checking is completed and are removed by
/// zonking.
/// Conversely to the untouchable TcVar, these are "touchable" and will be modified by the type
/// chcker.
#[derive(Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub TcUnifierVar;
);

/// During type checking we will refer to both type and unifier variables.
/// However once type checking completes we must remove all unifiers variables, so we use
/// UnifierTypeId to track which type variables are touch and untouchable.
/// Once type checking completes we transform from `Type<'_, UnifierTypeId>` to `Type<'_, TcVar>`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum UnifierTypeId {
    Var(TcVar),
    Unifier(TcUnifierVar),
}
impl From<&TcUnifierVar> for UnifierTypeId {
    fn from(uv: &TcUnifierVar) -> Self {
        UnifierTypeId::Unifier(*uv)
    }
}
impl From<TcUnifierVar> for UnifierTypeId {
    fn from(uv: TcUnifierVar) -> Self {
        UnifierTypeId::Unifier(uv)
    }
}

impl AsRef<TcUnifierVar> for TcUnifierVar {
    fn as_ref(&self) -> &TcUnifierVar {
        &self
    }
}

/// A monomorphic type
#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Type<'ty, TV> {
    /// Marker that signifies an operation produced an error. This exists so that we can try to
    /// gracefully recover from a type checking error and produce a list of errors at the end of type checking
    ErrorTy,
    /// Type of integers.
    //TODO: This should be part of some BaseTy or LiteralTy as opposed to a member of Type directly
    IntTy,
    /// A type variable, during type checking this may be either a unifier or a proper type variable
    VarTy(TV),
    /// A function type
    FunTy(&'ty Type<'ty, TV>, &'ty Type<'ty, TV>),
}

impl<'ty, TV> Type<'ty, TV> {
    pub fn and_then<NewTV>(
        &self,
        arena: &'ty Bump,
        op: impl Fn(&TV) -> &'ty Type<'ty, NewTV> + Clone,
    ) -> &'ty Type<'ty, NewTV> {
        match self {
            Type::ErrorTy => arena.alloc(Type::ErrorTy),
            Type::IntTy => arena.alloc(Type::IntTy),
            Type::VarTy(var) => op(var),
            Type::FunTy(arg, ret) => arena.alloc(Type::FunTy(
                arg.and_then(arena, op.clone()),
                ret.and_then(arena, op),
            )),
        }
    }
}

/// A variable marked with it's type
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TypedVarId<'ty, TV> {
    pub var: VarId,
    pub ty: &'ty Type<'ty, TV>,
}

pub type UnifierType<'ty> = Type<'ty, UnifierTypeId>;
pub type UnifierVarId<'ty> = TypedVarId<'ty, UnifierTypeId>;

impl<'ty> UnifierType<'ty> {
    /// A variant of apply that only performs one substitution.
    /// Split out as a separate method to allow for more performant implementations.
    pub(crate) fn apply_oneshot(
        &'ty self,
        ty_arena: &'ty Bump,
        key: TcUnifierVar,
        val: &'ty UnifierType<'ty>,
    ) -> &'ty UnifierType<'ty> {
        self.and_then(ty_arena, |var| match var {
            UnifierTypeId::Unifier(uni) if uni == &key => val,
            _ => self,
        })
    }
}

impl<'tv, TV> Type<'tv, TV> {
    pub fn type_vars(&self) -> impl Iterator<Item = &TV> {
        TypeVarsIter { stack: vec![self] }
    }
}

struct TypeVarsIter<'tv, TV> {
    stack: Vec<&'tv Type<'tv, TV>>,
}
impl<'tv, TV> Iterator for TypeVarsIter<'tv, TV> {
    type Item = &'tv TV;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|ty| match ty {
            Type::ErrorTy => self.next(),
            Type::IntTy => self.next(),
            Type::VarTy(tv) => Some(tv),
            Type::FunTy(arg_ty, ret_ty) => {
                self.stack.extend_from_slice(&[arg_ty, ret_ty]);
                self.next()
            }
        })
    }
}
