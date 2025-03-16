{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.aichat;

  # Determine the config path based on operating system
  configDir =
    if pkgs.stdenv.isDarwin
    then "${config.home.homeDirectory}/Library/Application Support/aichat"
    else "${config.home.homeDirectory}/.config/aichat";

  # Generate config as YAML
  configFile = ''
    # ---- behavior ----
    stream: ${
      if cfg.stream
      then "true"
      else "false"
    }
    save: ${
      if cfg.save
      then "true"
      else "false"
    }
    keybindings: ${cfg.keybindings}
    editor: ${
      if cfg.editor == null
      then "null"
      else cfg.editor
    }
    wrap: ${
      if builtins.isInt cfg.wrap
      then toString cfg.wrap
      else cfg.wrap
    }
    wrap_code: ${
      if cfg.wrap_code
      then "true"
      else "false"
    }

    # ---- session ----
    save_session: ${
      if cfg.save_session == null
      then "null"
      else if cfg.save_session
      then "true"
      else "false"
    }
    compress_threshold: ${toString cfg.compress_threshold}
    summarize_prompt: '${cfg.summarize_prompt}'
    summary_prompt: '${cfg.summary_prompt}'

    # ---- misc ----
    user_agent: ${
      if cfg.user_agent == null
      then "null"
      else cfg.user_agent
    }
    save_shell_history: ${
      if cfg.save_shell_history
      then "true"
      else "false"
    }

    clients:
    ${builtins.concatStringsSep "\n" (map (client: ''
        - type: ${client.type}
          name: ${client.name}
          api_base: ${client.api_base}
          api_key: ${client.api_key}
          models:
          ${builtins.concatStringsSep "\n" (map (model: ''
            - name: ${model.name}
              ${
              if model.max_input_tokens != null
              then "max_input_tokens: ${toString model.max_input_tokens}"
              else ""
            }
              ${
              if model.supports_function_calling
              then "supports_function_calling: true"
              else ""
            }
              ${
              if model.supports_vision
              then "supports_vision: true"
              else ""
            }
              ${
              if model.type != null
              then "type: ${model.type}"
              else ""
            }
              ${
              if model.default_chunk_size != null
              then "default_chunk_size: ${toString model.default_chunk_size}"
              else ""
            }
              ${
              if model.max_batch_size != null
              then "max_batch_size: ${toString model.max_batch_size}"
              else ""
            }
          '')
          client.models)}
      '')
      cfg.clients)}
  '';
in {
  options.programs.aichat = {
    enable = mkEnableOption "aichat command-line AI chat client";

    package = mkOption {
      type = types.package;
      default = pkgs.callPackage ../../../../pkgs/aichat {};
      description = "The aichat package to use";
    };

    # Behavior options
    stream = mkOption {
      type = types.bool;
      default = true;
      description = "Controls whether to use the stream-style API";
    };

    save = mkOption {
      type = types.bool;
      default = true;
      description = "Indicates whether to persist messages";
    };

    keybindings = mkOption {
      type = types.enum ["emacs" "vi"];
      default = "emacs";
      description = "Choose keybinding style (emacs, vi)";
    };

    editor = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Specifies the command used to edit input buffer or session. (e.g. vim, emacs, nano)";
    };

    wrap = mkOption {
      type = types.either types.int (types.enum ["no" "auto"]);
      default = "no";
      description = "Controls text wrapping (no, auto, <max-width>)";
    };

    wrap_code = mkOption {
      type = types.bool;
      default = false;
      description = "Enables or disables wrapping of code blocks";
    };

    # Session options
    save_session = mkOption {
      type = types.nullOr types.bool;
      default = null;
      description = "Controls the persistence of the session. If true, auto save; if false, not save; if null, asking the user";
    };

    compress_threshold = mkOption {
      type = types.int;
      default = 4000;
      description = "Compress session when token count reaches or exceeds this threshold";
    };

    summarize_prompt = mkOption {
      type = types.str;
      default = "Summarize the discussion briefly in 200 words or less to use as a prompt for future context.";
      description = "Text prompt used for creating a concise summary of session message";
    };

    summary_prompt = mkOption {
      type = types.str;
      default = "This is a summary of the chat history as a recap: ";
      description = "Text prompt used for including the summary of the entire session";
    };

    # Misc options
    user_agent = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Set User-Agent HTTP header, use `auto` for aichat/<current-version>";
    };

    save_shell_history = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to save shell execution command to the history file";
    };

    # Clients configuration
    clients = mkOption {
      type = types.listOf (types.submodule {
        options = {
          type = mkOption {
            type = types.str;
            default = "openai-compatible";
            description = "Type of client";
          };

          name = mkOption {
            type = types.str;
            description = "Name of the client";
          };

          api_base = mkOption {
            type = types.str;
            description = "API base URL";
          };

          api_key = mkOption {
            type = types.str;
            default = "xxx";
            description = "API key (optional)";
          };

          models = mkOption {
            type = types.listOf (types.submodule {
              options = {
                name = mkOption {
                  type = types.str;
                  description = "Model name";
                };

                max_input_tokens = mkOption {
                  type = types.nullOr types.int;
                  default = null;
                  description = "Maximum input tokens";
                };

                supports_function_calling = mkOption {
                  type = types.bool;
                  default = false;
                  description = "Whether the model supports function calling";
                };

                supports_vision = mkOption {
                  type = types.bool;
                  default = false;
                  description = "Whether the model supports vision";
                };

                type = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  description = "Type of model (e.g., 'embedding')";
                };

                default_chunk_size = mkOption {
                  type = types.nullOr types.int;
                  default = null;
                  description = "Default chunk size for embedding models";
                };

                max_batch_size = mkOption {
                  type = types.nullOr types.int;
                  default = null;
                  description = "Maximum batch size for embedding models";
                };
              };
            });
            default = [];
            description = "List of models supported by the client";
          };
        };
      });
      default = [];
      description = "AI service client configurations";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [cfg.package];

    # Create the config directory if it doesn't exist
    home.file = {
      "${configDir}/.keep".text = "";

      # Create the config file
      "${configDir}/config.yaml" = {
        text = configFile;
      };
    };
  };
}
