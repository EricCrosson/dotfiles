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
      default = ../../.config/librechat;
      description = "Directory containing LibreChat configuration files";
    };
  };

  config = mkIf cfg.enable {
    # Install configuration files to ~/.config/librechat
    home.file = {
      ".config/librechat/docker-compose.yml" = {
        source = cfg.configDir + "/docker-compose.yml";
      };

      ".config/librechat/.env" = {
        source = cfg.configDir + "/.env";
      };

      ".config/librechat/librechat.yaml" = {
        # Create a modified librechat.yaml to use the configured litellm host and port
        text =
          replaceStrings
          ["http://host.docker.internal:4000"]
          ["http://${cfg.litellmHost}:${toString cfg.litellmPort}"]
          (builtins.readFile (cfg.configDir + "/librechat.yaml"));
      };

      ".config/librechat/docker-compose.override.yml" = {
        source = cfg.configDir + "/docker-compose.override.yml";
      };
    };

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
