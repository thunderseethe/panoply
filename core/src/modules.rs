use crate::{file::FileId, ident::Ident};

/// A tracked module.
///
/// This stores input for a module and ties together the metadata produced by each pass for a given
/// module.
#[salsa::tracked]
#[derive(DebugWithDb, Debug)]
pub struct Module {
    pub name: Ident,
    pub uri: FileId,
}
