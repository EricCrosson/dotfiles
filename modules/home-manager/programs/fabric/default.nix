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

    env = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = "Environment variables for Fabric";
      example = literalExpression ''
        {
          DEFAULT_VENDOR = "OpenAI";
          DEFAULT_MODEL = "gpt-4";
        }
      '';
    };

    defaultEnv = mkOption {
      type = types.attrsOf types.str;
      default = {
        PATTERNS_LOADER_GIT_REPO_URL = "https://github.com/danielmiessler/fabric.git";
        PATTERNS_LOADER_GIT_REPO_PATTERNS_FOLDER = "patterns";
      };
      description = "Default environment variables for Fabric that can be overridden";
      internal = true;
    };

    stream = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to stream responses from the AI";
    };

    pattern = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The pattern to use for detecting queries";
    };
  };

  config = mkIf cfg.enable {
    # Add the package to the users' packages
    home.packages = [cfg.package];

    # Configure environment file with merged defaults and user settings
    home.file.".config/fabric/.env".text = concatStringsSep "\n" (
      mapAttrsToList (name: value: "${name}=${value}") (
        # Merge defaultEnv with user-provided env, with user values taking precedence
        cfg.defaultEnv // cfg.env
      )
    );

    # Create a fabric-config.yaml file using the Nix options
    programs.zsh.shellAliases = {
      fabric = "fabric --config ${pkgs.writeText "fabric-config.yaml" (builtins.toJSON (
        lib.filterAttrs (_n: v: v != null) {
          inherit (cfg) stream pattern;
        }
      ))}";
    };
  };
}
