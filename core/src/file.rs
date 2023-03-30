use crate::id::ModuleId;

#[salsa::input]
pub struct SourceFile {
    pub module: ModuleId,
    #[return_ref]
    pub path: std::path::PathBuf,
    #[return_ref]
    pub contents: String,
}
