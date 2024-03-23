use std::convert::Infallible;
use std::fmt;

pub use pretty::{DocBuilder, RcAllocator};

pub trait PrettyWithCtx<Ctx: ?Sized> {
  fn pretty_with<'me>(&'me self, ctx: &'me Ctx) -> PrettyWith<'me, Ctx>
  where
    Self: Sized + 'me,
  {
    PrettyWith {
      value: BoxRef::R(self),
      ctx,
    }
  }

  fn pretty_string<'me>(&'me self, ctx: &'me Ctx, width: usize) -> String
  where
    Self: Sized + 'me,
  {
    PrettyWith {
      value: BoxRef::R(self),
      ctx,
    }
    .pprint()
    .pretty(width)
    .to_string()
  }

  fn into_pretty<'me>(self, ctx: &'me Ctx) -> PrettyWith<'me, Ctx>
  where
    Self: Sized + 'me,
  {
    PrettyWith {
      value: BoxRef::B(Box::new(self)),
      ctx,
    }
  }

  fn pretty<'a>(&self, ctx: &Ctx, alloc: &'a RcAllocator) -> DocBuilder<'a, RcAllocator>;
}

impl<Ctx: ?Sized> PrettyWithCtx<Ctx> for Infallible {
  fn pretty<'a>(&self, _: &Ctx, _: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    unreachable!()
  }
}

#[derive(Clone)]
enum BoxRef<'me, T: ?Sized + 'me> {
  B(Box<T>),
  R(&'me T),
}
impl<T> From<T> for BoxRef<'_, T> {
  fn from(value: T) -> Self {
    BoxRef::B(Box::new(value))
  }
}
impl<'me, T> From<&'me T> for BoxRef<'me, T> {
  fn from(value: &'me T) -> Self {
    BoxRef::R(value)
  }
}

impl<'me, T: ?Sized> AsRef<T> for BoxRef<'me, T> {
  fn as_ref(&self) -> &T {
    match self {
      BoxRef::B(me) => me,
      BoxRef::R(me) => me,
    }
  }
}

pub struct PrettyWith<'me, Ctx: ?Sized> {
  value: BoxRef<'me, dyn PrettyWithCtx<Ctx>>,
  ctx: &'me Ctx,
}
impl<'a, Ctx: ?Sized> pretty::Pretty<'a, RcAllocator> for &PrettyWith<'_, Ctx> {
  fn pretty(self, allocator: &'a RcAllocator) -> pretty::DocBuilder<'a, RcAllocator> {
    self.value.as_ref().pretty(self.ctx, allocator)
  }
}

pub trait PrettyPrint {
  fn pprint(&self) -> pretty::DocBuilder<'static, RcAllocator>;
}
impl<P> PrettyPrint for P
where
  for<'a> &'a P: pretty::Pretty<'static, RcAllocator>,
{
  fn pprint(&self) -> pretty::DocBuilder<'static, RcAllocator> {
    pretty::Pretty::pretty(self, &RcAllocator)
  }
}

pub struct PrettyDebug<'a, Ctx: ?Sized> {
  pretty: BoxRef<'a, PrettyWith<'a, Ctx>>,
  width: usize,
}
impl<'a, Ctx: ?Sized, T> From<T> for PrettyDebug<'a, Ctx>
where
  T: Into<BoxRef<'a, PrettyWith<'a, Ctx>>>,
{
  fn from(value: T) -> Self {
    Self {
      pretty: value.into(),
      width: 80,
    }
  }
}

impl<Ctx: ?Sized> fmt::Debug for PrettyDebug<'_, Ctx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.pretty.as_ref().pprint().pretty(self.width))
  }
}

pub trait PrettyErrorWithDb<'me, Ctx: ?Sized> {
  type Me<E>
  where
    E: 'me,
    Ctx: 'me;

  /// Use `PrettyWithDb` instance to format Error in Debug output.
  /// This is useful when unwraping on an error to avoid having to pretty print at each
  /// unwrap site.
  fn map_err_pretty_with(self, db: &'me Ctx) -> Self::Me<PrettyDebug<'me, Ctx>>;
}

impl<'me, T, E, Ctx> PrettyErrorWithDb<'me, Ctx> for Result<T, E>
where
  Ctx: ?Sized + 'me,
  E: PrettyWithCtx<Ctx> + 'me,
{
  type Me<Err: 'me> = Result<T, Err>;

  fn map_err_pretty_with(self, db: &'me Ctx) -> Self::Me<PrettyDebug<'me, Ctx>> {
    self.map_err(|err| PrettyDebug::from(err.into_pretty(db)))
  }
}
