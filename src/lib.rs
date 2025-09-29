use zed_extension_api::{self as zed, LanguageServerId, Result, Worktree, serde_json::json};

struct CommonLispExtension;

impl zed::Extension for CommonLispExtension {
    fn new() -> Self {
        Self
    }

    // fn language_server_initialization_options(
    //     &mut self,
    //     _language_server_id: &LanguageServerId,
    //     _worktree: &Worktree,
    // ) -> Result<Option<zed_extension_api::serde_json::Value>> {
    //     Ok(Some(json!({
    //         "alive.format.closeParenOwnLine": "never",
    //         "alive.format.indentWidth": 4,
    //         "alive.format.maxBlankLines": 2,
    //         "commonlisp.trace.server": "verbose",
    //         "editor.semanticTokenColorCustomizations": {
    //             "rules": {
    //                 "error": "#ff0000",
    //                 "symbol": "#5599aa",
    //                 "parenthesis": "#999"
    //             }
    //         },
    //         "editor.formatOnType": true
    //     })))
    // }

    fn language_server_workspace_configuration(
        &mut self,
        _language_server_id: &LanguageServerId,
        _worktree: &Worktree,
    ) -> Result<Option<zed_extension_api::serde_json::Value>> {
        // TODO: How to make this user configurable? I've been unable to
        // figure out where/how to configure this in user settings, as attempting to do this
        // in the lsp option was unsuccessful
        Ok(Some(json!({
            // These MUST respond objects with the specific paths requested by Alive LSP.
            // It requested "alive.format", but the Alive repo's .vscode config did "alive.format.closeParenOwnLine"... etc.
            // I suspect this a difference in how VSCode honors the section lookups in the workspace/configuration request
            "alive.format": {
                "closeParenOwnLine": "never",
                "indentWidth": 4,
                "maxBlankLines": 2,
            },
            "commonlisp.trace.server": "verbose",
            "editor.semanticTokenColorCustomizations": {
                "rules": {
                    "error": "#ff0000",
                    "symbol": "#5599aa",
                    "parenthesis": "#999"
                }
            },
            "editor.formatOnType": true
        })))
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<zed::Command> {
        let (os, _) = zed::current_platform();
        let nc_command = if os == zed::Os::Windows {
            worktree.which("ncat").or_else(|| worktree.which("nc"))
        } else {
            worktree.which("nc").or_else(|| worktree.which("ncat"))
        };

        let path = nc_command
            .ok_or_else(|| "nc or ncat must be installed and available on your PATH".to_string())?;

        let lsp_settings = zed::settings::LspSettings::for_worktree("commonlisp", worktree);
        let mut args = None;

        if let Ok(lsp_settings) = lsp_settings {
            if let Some(binary) = lsp_settings.binary {
                args = binary.arguments;
            }
        }

        Ok(zed::Command {
            // command: "/home/nathan/dev/alive-lsp-wrapper/target/release/alive-lsp-wrapper"
            // command: "/home/nathan/quicklisp/local-projects/alive-lsp/alive-lsp".to_string(),
            // command: "/home/nathan/.roswell/bin/cl-lsp".to_string(),
            // command: "/home/nathan/dev/cl-micros-lsp/cl-micros-lsp".to_string(),
            // TODO: Add check to ensure nc is installed.
            // See https://github.com/GDQuest/zed-gdscript/blob/main/src/gdscript.rs#L17
            command: path,
            args: args.unwrap_or(vec!["127.0.0.1".to_string(), "8006".to_string()]),
            env: Default::default(),
        })
    }
}

zed::register_extension!(CommonLispExtension);
