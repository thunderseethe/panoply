use std::sync::{Arc, Mutex};

use aiahr::AiahrDatabase;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{InitializeParams, InitializeResult};
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    db: Arc<Mutex<AiahrDatabase>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        db: Arc::new(Mutex::new(AiahrDatabase::default())),
    });
    Server::new(stdin, stdout, socket).serve(service).await
}
