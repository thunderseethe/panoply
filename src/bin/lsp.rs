use std::ops::DerefMut;
use std::sync::{Arc, Mutex};

use aiahr::{canonicalize_path_set, create_source_file_set, AiahrDatabase};
use aiahr_core::file::FileId;
use aiahr_core::loc::Loc;
use aiahr_core::span::{Span, Spanned};
use aiahr_core::Db as CoreDb;
use aiahr_nameres::ops::IdOps;
use aiahr_nameres::Db as NameResDb;
use salsa::Durability;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
    InitializeResult, Location, Position, Range, ServerCapabilities, Url, WorkspaceFolder,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    _client: Client,
    db: Arc<Mutex<AiahrDatabase>>,
}

fn from_loc(loc: Loc) -> Position {
    Position {
        line: loc.line.try_into().unwrap(),
        character: loc.col.try_into().unwrap(),
    }
}
fn from_span(span: Span) -> Range {
    Range {
        start: from_loc(span.start),
        end: from_loc(span.end),
    }
}

fn init_lsp(db: &AiahrDatabase, folders: Vec<WorkspaceFolder>) -> eyre::Result<()> {
    let paths = folders.into_iter().flat_map(|folder| {
        folder
            .uri
            .to_file_path()
            .and_then(|dir_path| std::fs::read_dir(dir_path).map_err(|_| ()))
            .map(|read_dirs| {
                read_dirs.filter_map(|dir_entry| {
                    dir_entry
                        .and_then(|dir_entry| {
                            let file_type = dir_entry.file_type()?;
                            let path = dir_entry.path();
                            Ok((file_type.is_file()
                                && path.extension().map(|ext| ext == "aiahr").unwrap_or(false))
                            .then_some(path))
                        })
                        .unwrap_or(None)
                })
            })
            .into_iter()
            .flatten()
    });

    let uniq_paths = canonicalize_path_set(paths)?;
    let _ = create_source_file_set(db, uniq_paths)?;
    Ok(())
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, init: InitializeParams) -> Result<InitializeResult> {
        if let Some(folders) = init.workspace_folders {
            if let Err(err) = init_lsp(&self.db.lock().expect("Mutex not to be poisoned"), folders)
            {
                return Result::Err(Error::invalid_params(err.root_cause().to_string()));
            }
        }
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                definition_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
                ..Default::default()
            },
            server_info: None,
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let path_buf = params.text_document.uri.to_file_path().unwrap();
        let mut db = self.db.lock().unwrap();
        let file_id = FileId::new(db.as_core_db(), path_buf);
        let source_file = db.file_for_id(file_id);
        // File is open in editor so we expect frequent edits.
        source_file
            .set_contents(db.deref_mut())
            .with_durability(Durability::LOW)
            .to(params.text_document.text);
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let path_buf = params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
            .expect("Expected uri to be valid file path");
        let db = self.db.lock().unwrap();
        let core_db = db.as_core_db();
        let file_id = FileId::new(db.as_core_db(), path_buf);
        // TODO: Figure out how character works
        let Position { line, character } = params.text_document_position_params.position;
        log::info!(
            "GoToDefinition: {:?}:{}:{}",
            file_id.path(core_db),
            line,
            character
        );
        let resp = db
            .name_at_position(file_id, line, character)
            .map(|name| match name {
                aiahr_nameres::InScopeName::Effect(eff) => {
                    let module = eff.module(core_db);
                    let nameres_module = db.nameres_module_of(module);
                    let module_names = &nameres_module.names(db.as_nameres_db())[&module];
                    let span = module_names.get(eff).span();
                    (module.uri(core_db).path(core_db), span)
                }
                aiahr_nameres::InScopeName::EffectOp(eff_op) => {
                    let effect = eff_op.effect(core_db);
                    let module = effect.module(core_db);
                    let nameres_module = db.nameres_module_of(module);
                    let module_names = &nameres_module.names(db.as_nameres_db())[&module];
                    let span = module_names.get_effect(&effect).get(eff_op).span();
                    (module.uri(core_db).path(core_db), span)
                }
                aiahr_nameres::InScopeName::Term(term) => {
                    let module = term.module(db.as_core_db());
                    let nameres_module = db.nameres_module_of(module);
                    let module_names = &nameres_module.names(db.as_nameres_db())[&module];
                    let span = module_names.get(term).span();
                    (module.uri(core_db).path(core_db), span)
                }
                aiahr_nameres::InScopeName::TermTyVar(term, ty_var) => {
                    let module = term.module(db.as_core_db());
                    let span = db.term_defn(term).locals(db.as_nameres_db()).ty_vars[ty_var].span();
                    (module.uri(core_db).path(core_db), span)
                }
                aiahr_nameres::InScopeName::TermVar(term, var) => {
                    let module = term.module(db.as_core_db());
                    let span = db.term_defn(term).locals(db.as_nameres_db()).vars[var].span();
                    (module.uri(core_db).path(core_db), span)
                }
                aiahr_nameres::InScopeName::EffectTyVar(eff_op, ty_var) => {
                    let effect = eff_op.effect(db.as_core_db());
                    let module = effect.module(db.as_core_db());
                    let span =
                        db.effect_defn(effect).locals(db.as_nameres_db()).ty_vars[ty_var].span();
                    (module.uri(core_db).path(core_db), span)
                }
            })
            .map(|(path, span)| {
                let uri = Url::from_file_path(path)
                    .expect("ICE: All interned file paths must be valid LSP Urls");
                GotoDefinitionResponse::Scalar(Location {
                    uri,
                    range: from_span(span),
                })
            });
        Ok(resp)
    }
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let db = AiahrDatabase::default();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        _client: client,
        db: Arc::new(Mutex::new(db)),
    });

    // Serve LSP
    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}
