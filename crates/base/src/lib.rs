use salsa::Database as Db;

pub mod diagnostic;
pub mod displayer;
pub mod file;
pub mod graph;
pub mod id;
pub mod id_converter;
pub mod indexed;
pub mod loc;
pub mod memory;
pub mod modules;
pub mod option;
pub mod pretty;
pub mod span;
pub mod spanner;

pub mod ident {
  use pretty::DocAllocator;

  use crate::pretty::PrettyWithCtx;

  /// An interned identifier.
  #[salsa::interned(debug, no_lifetime)]
  #[derive(PartialOrd, Ord)]
  pub struct Ident {
    #[returns(ref)]
    pub text: String,
  }

  impl<DB: ?Sized + salsa::Database> PrettyWithCtx<DB> for Ident {
    fn pretty<'a>(
      &self,
      db: &DB,
      alloc: &'a pretty::RcAllocator,
    ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
      alloc.text(self.text(db).to_string())
    }
  }
}
