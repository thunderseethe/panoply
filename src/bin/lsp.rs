use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use base::file::file_for_id;
use base::{file::FileId, loc::Loc, span::Span};
use nameres::{name_at_position, nameres_errors};
use panoply::{PanoplyDatabase, canonicalize_path_set, create_source_file_set};
use parser::{parse_errors, unlocate};
use salsa::{Durability, Setter};
use tc::type_check_errors;
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

fn from_loc(db: &dyn salsa::Database, file_id: FileId, loc: Loc) -> Option<Position> {
  let (line, character) = unlocate(db, file_id, loc)?;
  Some(Position { line, character })
}

fn from_span(db: &dyn salsa::Database, file_id: FileId, span: Span) -> Option<Range> {
  Some(Range {
    start: from_loc(db, file_id, span.start)?,
    end: from_loc(db, file_id, span.end)?,
  })
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
    let file_id = FileId::new(db.deref(), path_buf);
    let source_file = file_for_id(db.deref(), file_id);
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
      let file_id = FileId::new(db.deref(), path_buf);
      let source_file = file_for_id(db.deref(), file_id);

      source_file
        .set_contents(db.deref_mut())
        .with_durability(Durability::LOW)
        .to(params.content_changes[0].text.clone());

      file_id
    };

    // Immutable database access
    let diags = {
      let db = self.db.lock().unwrap();

      parse_errors(db.deref(), file_id)
        .into_iter()
        .map(|err| match err.error {
          base::diagnostic::parser::ParseError::WrongToken {
            span,
            got,
            want_any,
          } => Diagnostic::new_simple(
            from_span(
              db.deref(),
              file_id,
              Span {
                start: Loc { byte: span.start },
                end: Loc { byte: span.end },
              },
            )
            .expect("Invalid range in file"),
            format!("Received {} but wanted {:?}", got, want_any),
          ),
        })
        .chain(
          nameres_errors(db.deref(), file_id)
            .into_iter()
            .map(|_err| todo!()),
        )
        .chain(
          type_check_errors(db.deref(), file_id)
            .into_iter()
            .map(|_err| todo!()),
        )
        .collect()
    };

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
    let file_id = FileId::new(db.deref(), path_buf);
    // TODO: Figure out how character works
    let Position { line, character } = params.text_document_position_params.position;
    log::debug!(
      "GoToDefinition: {}:{}:{}",
      file_id.path(db.deref()).display(),
      line,
      character
    );
    let resp = name_at_position(db.deref(), file_id, line, character)
      .map(|name| {
        (
          name.module(db.deref()).uri(db.deref()).path(db.deref()),
          name.span(db.deref()),
        )
      })
      .map(|(path, span)| {
        let uri =
          Url::from_file_path(path).expect("ICE: All interned file paths must be valid LSP Urls");
        GotoDefinitionResponse::Scalar(Location {
          uri,
          range: from_span(db.deref(), file_id, span).expect("Invalid span"),
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
