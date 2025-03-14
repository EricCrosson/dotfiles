{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.fabric;
in {
  options.programs.fabric = {
    enable = mkEnableOption "Fabric AI integration";

    package = mkOption {
      type = types.package;
      default = pkgs.fabric-ai;
      description = "The fabric-ai package to use";
      example = "pkgs.fabric-ai";
    };

    configFile = mkOption {
      type = types.path;
      default = ../../../.config/fabric/config.yaml;
      description = "Path to the fabric configuration file";
    };

    envFile = mkOption {
      type = types.path;
      default = ../../../.config/fabric/.env;
      description = "Path to the fabric environment file";
    };
  };

  config = mkIf cfg.enable {
    # Add the package to the users' packages
    home.packages = [cfg.package];

    # Configure environment files
    home.file = {
      ".config/fabric/.env".source = cfg.envFile;
    };

    # Create a fabric-config.yaml file using the provided configuration
    programs.zsh.shellAliases = {
      fabric = "fabric --config ${pkgs.writeText "fabric-config.yaml" (builtins.readFile cfg.configFile)}";
    };
  };
}
