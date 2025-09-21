use zed_extension_api::{self as zed, LanguageServerId, Result, Worktree};

struct CommonLispExtension;

impl zed::Extension for CommonLispExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        _worktree: &Worktree,
    ) -> Result<zed::Command> {
        eprintln!("Executing language_server_command...");
        Ok(zed::Command {
            // command: "/home/nathan/dev/alive-lsp-wrapper/target/release/alive-lsp-wrapper"
            // command: "/home/nathan/quicklisp/local-projects/alive-lsp/alive-lsp".to_string(),
            // command: "/home/nathan/.roswell/bin/cl-lsp".to_string(),
            command: "/home/nathan/dev/cl-micros-lsp/cl-micros-lsp".to_string(),
            args: vec![
                "--stdio".to_string(),
                "--log-file".to_string(),
                "/home/nathan/cl-micros-lsp.log".to_string(),
                // "--zed-mode".to_string(),
            ],
            env: Default::default(),
        })
    }
}

zed::register_extension!(CommonLispExtension);
