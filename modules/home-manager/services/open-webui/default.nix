{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.open-webui;
in {
  imports = [
    ../../options
  ];

  options.services.open-webui = {
    enable = mkEnableOption "Open WebUI service";

    package = mkOption {
      type = types.package;
      default = pkgs.open-webui;
      description = "Open WebUI package";
    };

    port = mkOption {
      type = types.port;
      default = 6251;
      description = "Port to expose the Open WebUI service on";
    };

    dataDir = mkOption {
      type = types.str;
      default = "${config.home.homeDirectory}/.local/share/open-webui";
      description = "Directory to store Open WebUI data";
    };

    openai = {
      apiKey = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "OpenAI API key for OpenAI-compatible endpoints";
      };
    };

    litellmProxy = {
      enable = mkOption {
        type = types.bool;
        default = config.services-options.litellm-proxy.enabled;
        description = "Whether to connect to LiteLLM Proxy";
      };

      # Keep for backward compatibility but prefer the shared options
      url = mkOption {
        type = types.str;
        default = config.services-options.litellm-proxy.baseUrl;
        description = "URL for connecting to LiteLLM Proxy";
      };
    };

    keepAlive = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to keep the service alive";
    };

    extraServiceDependencies = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional service dependencies";
    };

    environment = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = "Additional environment variables";
    };

    extraArgs = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional command-line arguments to pass to OpenWebUI";
    };

    secretKey = mkOption {
      type = types.str;
      default = "t0p-s3cr3t";
      description = "Secret key for OpenWebUI sessions";
    };

    logging = {
      stdout = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/open-webui.log";
        description = "Path to stdout log file";
      };

      stderr = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/open-webui.error.log";
        description = "Path to stderr log file";
      };
    };
  };

  config = mkIf cfg.enable {
    # Create Open WebUI data directory structure using activation script
    home.activation.createOpenWebUIDataDirs = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run mkdir -p "${cfg.dataDir}"
      run mkdir -p "${cfg.dataDir}/logs"
      run mkdir -p "${cfg.dataDir}/user_data"
      run mkdir -p "${cfg.dataDir}/backend/data"

      run chmod 755 "${cfg.dataDir}"
      run chmod 755 "${cfg.dataDir}/logs"
      run chmod 755 "${cfg.dataDir}/user_data"
      run chmod 755 "${cfg.dataDir}/backend/data"
    '';

    # Define the Open WebUI service
    launchd-with-logs.services.open-webui = {
      command = "${cfg.package}/bin/open-webui";
      args =
        [
          "serve"
          "--port"
          "${toString cfg.port}"
          "--host"
          "0.0.0.0"
        ]
        ++ cfg.extraArgs;
      environment =
        {
          PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
          DATA_DIR = cfg.dataDir;
          WEBUI_SECRET_KEY = cfg.secretKey;
        }
        // (
          if cfg.litellmProxy.enable
          then {
            OPENAI_API_BASE_URL = "${config.services-options.litellm-proxy.apiUrl}";
            OPENAI_API_KEY = "dummy-key-for-litellm-proxy";
          }
          else {}
        )
        // cfg.environment;
      serviceDependencies =
        (
          if cfg.litellmProxy.enable
          then ["org.nixos.home-manager.litellm-proxy"]
          else []
        )
        ++ cfg.extraServiceDependencies;
      inherit (cfg) keepAlive;
      logging = {
        inherit (cfg.logging) stdout stderr;
      };
    };
  };
}
