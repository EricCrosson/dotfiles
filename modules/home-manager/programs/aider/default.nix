{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.aider;
in {
  options.programs.aider = {
    enable = mkEnableOption "Aider AI coding assistant";

    configFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Path to aider configuration file";
    };

    awsProfile = mkOption {
      type = types.str;
      default = "dev";
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
    # Set up aider configuration file if provided
    home.file = mkIf (cfg.configFile != null) {
      ".aider.conf.yml".source = cfg.configFile;
    };

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
