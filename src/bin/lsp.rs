use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use base::{
  diagnostic::Diagnostic as PanoplyDiagnostic, file::FileId, loc::Loc, span::Span, Db as CoreDb,
};
use nameres::Db as NameResDb;
use panoply::{canonicalize_path_set, create_source_file_set, PanoplyDatabase};
use parser::Db;
use salsa::{Durability, ParallelDatabase};
use tc::Db as TcDb;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
  Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
  GotoDefinitionResponse, InitializeParams, InitializeResult, Location, OneOf, Position,
  PositionEncodingKind, Range, ServerCapabilities, TextDocumentSyncCapability,
  TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
  client: Client,
  db: Arc<Mutex<PanoplyDatabase>>,
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

fn init_lsp(
  db: &PanoplyDatabase,
  paths: impl IntoIterator<Item = std::result::Result<PathBuf, ()>>,
) -> eyre::Result<()> {
  let paths = paths.into_iter().flat_map(|path| {
    path
      .and_then(|dir_path| std::fs::read_dir(dir_path).map_err(|_| ()))
      .map(|read_dirs| {
        read_dirs.filter_map(|dir_entry| {
          dir_entry
            .and_then(|dir_entry| {
              let file_type = dir_entry.file_type()?;
              let path = dir_entry.path();
              Ok(
                (file_type.is_file() && path.extension().map(|ext| ext == "pan").unwrap_or(false))
                  .then_some(path),
              )
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
      log::debug!("Init with workspace folders");
      if let Err(err) = init_lsp(
        &self.db.lock().expect("Mutex not to be poisoned"),
        folders.into_iter().map(|folder| folder.uri.to_file_path()),
      ) {
        return Result::Err(Error::invalid_params(err.root_cause().to_string()));
      }
    } else if let Some(root_uri) = init.root_uri {
      log::debug!("Init with root uri");
      if let Err(err) = init_lsp(
        &self.db.lock().expect("Mutex not to be poisoned"),
        std::iter::once(root_uri.to_file_path()),
      ) {
        return Result::Err(Error::invalid_params(err.root_cause().to_string()));
      }
    }
    Ok(InitializeResult {
      capabilities: ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        position_encoding: Some(PositionEncodingKind::UTF32),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
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
    log::debug!("did_open {}", path_buf.display());
    let mut db = self.db.lock().unwrap();
    let file_id = FileId::new(db.as_core_db(), path_buf);
    let source_file = db.file_for_id(file_id);
    // File is open in editor so we expect frequent edits.
    source_file
      .set_contents(db.deref_mut())
      .with_durability(Durability::LOW)
      .to(params.text_document.text);
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    let path_buf = params.text_document.uri.to_file_path().unwrap();
    log::debug!("did_change {}", path_buf.display());
    // Scoped mutable database access
    // We do this so we drop our lock on the database before our await point.
    let file_id = {
      let mut db = self.db.lock().unwrap();
      let file_id = FileId::new(db.as_core_db(), path_buf);
      let source_file = db.file_for_id(file_id);

      source_file
        .set_contents(db.deref_mut())
        .with_durability(Durability::LOW)
        .to(params.content_changes[0].text.clone());

      file_id
    };

    // Immutable database access
    let db = {
      let lock = self.db.lock().unwrap();
      lock.snapshot()
    };

    let diags = db
      .parse_errors(file_id)
      .into_iter()
      .chain(db.nameres_errors(file_id))
      .chain(db.type_check_errors(file_id))
      .map(|err| {
        let citation = err.principal(db.deref());
        Diagnostic::new_simple(from_span(citation.span), citation.message)
      })
      .collect();

    self
      .client
      .publish_diagnostics(
        params.text_document.uri,
        diags,
        Some(params.text_document.version),
      )
      .await;
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
    log::debug!(
      "GoToDefinition: {}:{}:{}",
      file_id.path(core_db).display(),
      line,
      character
    );
    let resp = db
      .name_at_position(file_id, line, character)
      .map(|name| {
        let core_db = db.as_core_db();
        (
          name.module(core_db).uri(core_db).path(core_db),
          name.span(db.deref()),
        )
      })
      .map(|(path, span)| {
        let uri =
          Url::from_file_path(path).expect("ICE: All interned file paths must be valid LSP Urls");
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
  fern::Dispatch::new()
    .format(|out, message, record| {
      out.finish(format_args!(
        "[{} {}] {}",
        record.level(),
        record.target(),
        message
      ))
    })
    .level(log::LevelFilter::Debug)
    .chain(std::io::stdout())
    .chain(fern::log_file("output.log")?)
    .apply()?;
  let db = PanoplyDatabase::default();

  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| Backend {
    client,
    db: Arc::new(Mutex::new(db)),
  });

  // Serve LSP
  Server::new(stdin, stdout, socket).serve(service).await;

  Ok(())
}
