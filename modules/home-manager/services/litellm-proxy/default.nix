{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.litellm-proxy;
in {
  imports = [
    ../../options/services.nix
  ];

  options.services.litellm-proxy = {
    enable = mkEnableOption "LiteLLM proxy service";

    package = mkOption {
      type = types.package;
      default = pkgs.callPackage ../../../../pkgs/litellm {};
      description = "LiteLLM package";
    };

    configFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Path to litellm config file";
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

    launchd-with-logs.services.litellm-proxy = {
      command = "${cfg.package}/bin/litellm";
      args = [
        "--config"
        (
          if cfg.configFile != null
          then "${toString cfg.configFile}"
          else "${config.home.homeDirectory}/.config/litellm/config.yaml"
        )
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
