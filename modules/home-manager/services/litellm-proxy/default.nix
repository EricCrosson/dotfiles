{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.litellm-proxy;
in {
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
    # We don't need to copy the config file anywhere - we'll use it directly

    launchd-with-logs.services.litellm-proxy = {
      command = "${cfg.package}/bin/litellm";
      args = [
        "--config"
        (
          if cfg.configFile != null
          then "${toString cfg.configFile}"
          else "${config.home.homeDirectory}/.config/litellm/config.yaml"
        )
      ];
      environment = {
        PATH = lib.makeBinPath [cfg.aws-saml];
      };
      keepAlive = cfg.keepAlive;
      logging = {
        stdout = cfg.logging.stdout;
        stderr = cfg.logging.stderr;
      };
    };
  };
}
