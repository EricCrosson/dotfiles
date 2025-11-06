{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.aider;
in {
  imports = [
    ../../options
  ];

  options.programs.aider = {
    enable = mkEnableOption "Aider AI coding assistant";

    codeTheme = mkOption {
      type = types.str;
      default = "default";
      description = "Code theme for aider";
    };

    detectUrls = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to detect URLs in code";
    };

    gitignore = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to respect .gitignore files";
    };

    subtreeOnly = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to only operate on a subtree of the repository";
    };

    watchFiles = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to watch files for changes";
    };

    read = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "List of files to read on startup";
    };

    extraConfig = mkOption {
      type = types.nullOr types.lines;
      default = null;
      description = "Extra configuration to append to the aider config file";
    };

    awsProfile = mkOption {
      type = types.str;
      default = config.claude-options.bedrock.profile;
      description = "AWS profile to use for authentication";
    };

    pythonVersion = mkOption {
      type = types.str;
      default = "3.9";
      description = "Python version to use with uvx";
    };

    repository = mkOption {
      type = types.str;
      default = "git+ssh://git@github.com/BitGo/aider";
      description = "Git repository URL for aider";
    };

    extraEnv = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = "Extra environment variables to set for aider";
    };
  };

  config = mkIf cfg.enable {
    # Generate aider config file
    home.file.".aider.conf.yml".text = ''
      code-theme: ${cfg.codeTheme}
      detect-urls: ${
        if cfg.detectUrls
        then "true"
        else "false"
      }
      gitignore: ${
        if cfg.gitignore
        then "true"
        else "false"
      }
      subtree-only: ${
        if cfg.subtreeOnly
        then "true"
        else "false"
      }
      watch-files: ${
        if cfg.watchFiles
        then "true"
        else "false"
      }
      ${optionalString (cfg.read != []) "read:${concatMapStrings (file: "\n  - ${file}") cfg.read}"}
      ${optionalString (cfg.extraConfig != null) cfg.extraConfig}
    '';

    # Create aider wrapper script as a proper package
    home.packages = [
      (pkgs.symlinkJoin {
        name = "aider-wrapped";
        paths = [];
        buildInputs = [pkgs.makeWrapper];

        postBuild = ''
          mkdir -p $out/bin

          # Create the base script
          cat > $out/bin/aider << 'EOF'
          #!/bin/sh
          exec uvx --python ${cfg.pythonVersion} --from ${cfg.repository} aider "$@"
          EOF

          chmod +x $out/bin/aider

          # Wrap the script with proper environment variables
          wrapProgram $out/bin/aider \
            --set AWS_PROFILE "${cfg.awsProfile}" \
            ${concatStringsSep " \\\n            " (mapAttrsToList (name: value: "--set ${name} \"${value}\"") cfg.extraEnv)}
        '';
      })
    ];
  };
}
