{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.services.librechat;
in {
  options.services.librechat = {
    enable = mkEnableOption "LibreChat service";

    litellmHost = mkOption {
      type = types.str;
      default = "host.docker.internal";
      description = "Hostname for connecting to litellm service";
    };

    litellmPort = mkOption {
      type = types.port;
      default = 4000;
      description = "Port for connecting to litellm service";
    };

    configDir = mkOption {
      type = types.path;
      default = ../../../../.config/librechat;
      description = "Directory containing LibreChat configuration files";
    };
  };

  config = mkIf cfg.enable {
    home.file = {
      ".config/librechat/docker-compose.yml" = {
        source = cfg.configDir + "/docker-compose.yml";
      };

      ".config/librechat/docker-compose.override.yml" = {
        source = cfg.configDir + "/docker-compose.override.yml";
      };
    };

    # Install docker mount files as regular files
    home.activation.installLibreChatConfigs = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # Ensure librechat config directory exists
      run mkdir -p "$HOME/.config/librechat"

      # Install .env file directly as a regular file
      run install -m 644 "${cfg.configDir}/.env" "$HOME/.config/librechat/.env"

      # Create modified librechat.yaml directly as a regular file
      run cat "${cfg.configDir}/librechat.yaml" | \
        sed 's|http://host.docker.internal:4000|http://${cfg.litellmHost}:${toString cfg.litellmPort}|g' > \
        "$HOME/.config/librechat/librechat.yaml"
      run chmod 644 "$HOME/.config/librechat/librechat.yaml"
    '';

    # Restart LibreChat containers when config changes
    home.activation.restartLibreChat = lib.hm.dag.entryAfter ["installLibreChatConfigs"] ''
      if [[ -d "$HOME/.config/librechat" && -f "$HOME/.config/librechat/docker-compose.yml" ]]; then
        run echo "Restarting LibreChat containers..."
        run cd "$HOME/.config/librechat" && /opt/homebrew/bin/docker-compose -f docker-compose.yml -f docker-compose.override.yml restart
      fi
    '';

    # Define the librechat launchd service
    launchd-with-logs.services.librechat = {
      command = "/opt/homebrew/bin/docker-compose";
      args = [
        "-f"
        "docker-compose.yml"
        "-f"
        "docker-compose.override.yml"
        "up"
        "-d"
      ];
      workingDirectory = "${config.home.homeDirectory}/.config/librechat";
      environment = {
        PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
      };
      serviceDependencies = ["org.nixos.home-manager.colima"];
      logging = {
        stdout = "${config.home.homeDirectory}/Library/Logs/librechat.log";
        stderr = "${config.home.homeDirectory}/Library/Logs/librechat.error.log";
      };
    };
  };
}
