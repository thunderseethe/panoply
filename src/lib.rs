#[salsa::db(
    aiahr_ast::Jar,
    aiahr_core::Jar,
    aiahr_desugar::Jar,
    aiahr_ir::Jar,
    aiahr_lower_ir::Jar,
    aiahr_nameres::Jar,
    aiahr_parser::Jar,
    aiahr_tc::Jar,
    aiahr_ty::Jar
)]
#[derive(Default)]
pub struct AiahrDatabase {
    storage: salsa::Storage<Self>,
}
impl salsa::Database for AiahrDatabase {}
impl salsa::ParallelDatabase for AiahrDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
        })
    }
}