{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.llm;

  # Helper function to convert model to attributes
  modelToAttrs = model: {
    model_id = model.id;
    model_name = model.name;
    inherit (model) api_base;
  };
in {
  options.programs.llm = {
    enable = mkEnableOption "LLM CLI configuration";

    package = mkOption {
      type = types.package;
      default = pkgs.llm;
      defaultText = literalExpression "pkgs.llm";
      description = "The LLM package to use.";
    };

    # Support platform-specific paths
    configDir = mkOption {
      type = types.str;
      default =
        if pkgs.stdenv.isDarwin
        then "Library/Application Support/io.datasette.llm"
        else ".config/llm";
      description = "Directory for LLM configuration files (relative to home directory)";
      readOnly = true;
    };

    defaultModel = mkOption {
      type = types.str;
      inherit (config.claude-options.models) default;
      description = "Default model to use";
    };

    models = mkOption {
      type = types.listOf (types.submodule {
        options = {
          id = mkOption {
            type = types.str;
            description = "Model identifier";
            example = "bedrock-claude-sonnet";
          };

          name = mkOption {
            type = types.str;
            description = "Full model name";
            example = "bedrock/us.anthropic.claude-sonnet-4-5-20250929-v1:0";
          };

          api_base = mkOption {
            type = types.str;
            description = "API base URL";
            example = "http://localhost:4000";
          };
        };
      });
      description = "List of models to configure for LLM CLI";
    };
  };

  config = mkIf cfg.enable {
    home = {
      file = {
        "${cfg.configDir}/default_model.txt" = {
          text = cfg.defaultModel;
        };

        "${cfg.configDir}/extra-openai-models.yaml" = {
          text = builtins.toJSON (map modelToAttrs cfg.models);
        };
      };

      packages = [cfg.package];
    };
  };
}
