use crate::diagnostic::{Citation, Diagnostic};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckDiagnostic {
  pub name: &'static str,
  pub principal: Citation,
}

impl Diagnostic for TypeCheckDiagnostic {
  fn name(&self) -> &'static str {
    self.name
  }

  fn principal<M>(&self, _: &M) -> Citation {
    self.principal.clone()
  }

  fn additional<M>(&self, _: &M) -> Vec<Citation> {
    // TODO: allow for additional citations
    vec![]
  }
}
