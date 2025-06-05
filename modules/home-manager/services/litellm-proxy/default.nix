{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.litellm-proxy;

  # Helper function to generate YAML for models
  modelToYaml = model: ''
    - model_name: ${model.name}
      litellm_params:
        model: ${model.model}
        ${optionalString (model.aws_profile_name != null) "aws_profile_name: ${model.aws_profile_name}"}
        ${model.extraConfig}
  '';

  # Generate complete config YAML
  configToYaml = models: ''
    model_list:
    ${concatMapStrings modelToYaml models}
  '';
in {
  imports = [
    ../../options/services.nix
    ../../options/claude.nix
  ];

  options.services.litellm-proxy = {
    enable = mkEnableOption "LiteLLM proxy service";

    package = mkOption {
      type = types.package;
      default = pkgs.callPackage ../../../../pkgs/litellm {};
      description = "LiteLLM package";
    };

    aws-saml = mkOption {
      type = types.package;
      default = pkgs.callPackage ../../../../pkgs/aws-saml {};
      description = "aws-saml package for AWS authentication";
    };

    keepAlive = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to keep the service alive";
    };

    # Use fixed defaults, not derived from services-options
    # to break circular dependency
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
            description = "Full model identifier";
            example = "bedrock/us.anthropic.claude-3-7-sonnet-20250219-v1:0";
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
            description = "Additional model configuration in YAML format";
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
    # Set shared options for other modules to use
    services-options.litellm-proxy = {
      inherit (cfg) host port;
    };

    # Generate config file from Nix models definition
    home.file = {
      ".config/litellm/config.yaml" = {
        text = configToYaml cfg.models;
      };
    };

    launchd-with-logs.services.litellm-proxy = {
      command = "${cfg.package}/bin/litellm";
      args = [
        "--config"
        "${config.home.homeDirectory}/.config/litellm/config.yaml"
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
