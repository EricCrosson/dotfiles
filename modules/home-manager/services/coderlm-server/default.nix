{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.coderlm-server;
in {
  options.services.coderlm-server = {
    enable = mkEnableOption "CodeRLM server (tree-sitter code indexer)";

    package = mkOption {
      type = types.package;
      default = pkgs.callPackage ../../../../pkgs/coderlm-server {};
      description = "coderlm-server package";
    };

    port = mkOption {
      type = types.port;
      default = 3000;
      description = "Port for the CodeRLM server";
    };

    bind = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Bind address for the CodeRLM server";
    };

    maxProjects = mkOption {
      type = types.int;
      default = 5;
      description = "Maximum number of concurrent indexed projects";
    };

    keepAlive = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to keep the service alive";
    };

    logging = {
      stdout = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/coderlm-server.log";
        description = "Path to stdout log file";
      };

      stderr = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/coderlm-server.error.log";
        description = "Path to stderr log file";
      };
    };
  };

  config = mkIf cfg.enable {
    launchd-with-logs.services.coderlm-server = {
      command = "${cfg.package}/bin/coderlm-server";
      args = [
        "serve"
        "--port"
        "${toString cfg.port}"
        "--bind"
        cfg.bind
        "--max-projects"
        "${toString cfg.maxProjects}"
      ];
      inherit (cfg) keepAlive;
      logging = {
        inherit (cfg.logging) stdout stderr;
      };
    };
  };
}
