{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.services.litellm-proxy;

  # Whether any model uses a runtime file for its model identifier
  hasModelFiles = any (m: m.modelFile != null) cfg.models;

  # Convert model data to attributes for JSON conversion
  modelToAttrs = model: {
    model_name = model.name;
    litellm_params =
      {
        # Use a placeholder when modelFile is set; the wrapper replaces it at runtime.
        # Placeholder must not contain / to avoid breaking bash parameter substitution.
        model =
          if model.modelFile != null
          then "@MODEL_FILE_${builtins.hashString "sha256" model.modelFile}@"
          else model.model;
      }
      // optionalAttrs (model.aws_profile_name != null) {
        inherit (model) aws_profile_name;
      }
      // (
        if model.extraConfig != ""
        then builtins.fromJSON model.extraConfig
        else {}
      );
  };

  # Generate config using builtins.toJSON
  configToJson = models: builtins.toJSON {model_list = map modelToAttrs models;};

  # Config path used by litellm
  configPath = "${config.home.homeDirectory}/.config/litellm/config.yaml";

  # Runtime config path where the wrapper writes the resolved config
  runtimeConfigPath = "${config.home.homeDirectory}/.config/litellm/config.runtime.yaml";

  # Wrapper script that resolves modelFile placeholders at runtime.
  # Reads the template config, replaces @MODEL_FILE:path@ placeholders
  # with actual file contents, writes the resolved config, then execs litellm.
  litellmWrapper = let
    # Build a sequence of bash parameter substitutions, one per modelFile
    substitutions = concatMapStrings (model:
      if model.modelFile != null
      then let
        placeholder = "@MODEL_FILE_${builtins.hashString "sha256" model.modelFile}@";
        fileRef = escapeShellArg model.modelFile;
      in ''
        model_arn="bedrock/converse/$(cat ${fileRef})"
        resolved=''${resolved//${placeholder}/$model_arn}
      ''
      else "")
    cfg.models;
  in
    pkgs.writeShellApplication {
      name = "litellm-proxy-wrapper";
      runtimeInputs = [pkgs.coreutils cfg.package];
      text = ''
        resolved=$(cat ${escapeShellArg configPath})
        ${substitutions}
        printf '%s\n' "$resolved" > ${escapeShellArg runtimeConfigPath}
        exec litellm --config ${escapeShellArg runtimeConfigPath} --host ${escapeShellArg cfg.host} --port ${escapeShellArg (toString cfg.port)}
      '';
    };
in {
  options.services.litellm-proxy = {
    enable = mkEnableOption "LiteLLM proxy service";

    package = mkOption {
      type = types.package;
      default = pkgs.callPackage ../../../../pkgs/litellm {};
      description = "LiteLLM package";
    };

    aws-saml = mkOption {
      type = types.package;
      inherit (inputs.aws-saml-bitgo.packages.${pkgs.system}) default;
      description = "aws-saml package for AWS authentication";
    };

    keepAlive = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to keep the service alive";
    };

    host = mkOption {
      type = types.str;
      default = "localhost";
      description = "Host for the LiteLLM proxy service";
    };

    port = mkOption {
      type = types.port;
      default = 4000;
      description = "Port for the LiteLLM proxy service";
    };

    baseUrl = mkOption {
      type = types.str;
      default = "http://${cfg.host}:${toString cfg.port}";
      description = "Base URL for the LiteLLM proxy service";
      readOnly = true;
    };

    apiUrl = mkOption {
      type = types.str;
      default = "${cfg.baseUrl}/v1";
      description = "API URL for the LiteLLM proxy service";
      readOnly = true;
    };

    # Structured model definition, similar to llm module
    models = mkOption {
      type = types.listOf (types.submodule {
        options = {
          name = mkOption {
            type = types.str;
            description = "Model name for the litellm proxy";
            example = "bedrock-claude-sonnet";
          };

          model = mkOption {
            type = types.str;
            default = "";
            description = "Full model identifier. Ignored when modelFile is set.";
            example = "bedrock/us.anthropic.claude-sonnet-4-5-20250929-v1:0";
          };

          modelFile = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Path to a file containing the model identifier (e.g. a sops-decrypted secret). Read at service startup.";
            example = "/run/secrets/bedrock_sonnet_arn";
          };

          aws_profile_name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "AWS profile name for Bedrock models";
            example = "dev";
          };

          extraConfig = mkOption {
            type = types.lines;
            default = "";
            description = "Additional model configuration in JSON format. Will be merged with the model's litellm_params.";
          };
        };
      });
      default = [];
      description = "List of models to configure for LiteLLM proxy";
    };

    logging = {
      stdout = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/litellm-proxy.log";
        description = "Path to stdout log file";
      };

      stderr = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/litellm-proxy.error.log";
        description = "Path to stderr log file";
      };
    };
  };

  config = mkIf cfg.enable {
    # Generate config file from Nix models definition using built-in JSON converter
    home.file = {
      ".config/litellm/config.yaml" = {
        text = configToJson cfg.models;
      };
    };

    launchd-with-logs.services.litellm-proxy =
      if hasModelFiles
      then {
        command = "${litellmWrapper}/bin/litellm-proxy-wrapper";
        environment = {
          PATH = lib.makeBinPath [cfg.aws-saml];
        };
        inherit (cfg) keepAlive;
        logging = {
          inherit (cfg.logging) stdout stderr;
        };
      }
      else {
        command = "${cfg.package}/bin/litellm";
        args = [
          "--config"
          configPath
          "--host"
          cfg.host
          "--port"
          "${toString cfg.port}"
        ];
        environment = {
          PATH = lib.makeBinPath [cfg.aws-saml];
        };
        inherit (cfg) keepAlive;
        logging = {
          inherit (cfg.logging) stdout stderr;
        };
      };
  };
}
