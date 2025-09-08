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
            command: "/home/nathan/dev/alive-lsp-wrapper/target/release/alive-lsp-wrapper"
                .to_string(),
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(CommonLispExtension);
