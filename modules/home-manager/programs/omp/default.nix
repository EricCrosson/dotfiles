{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.omp;
  yamlFormat = pkgs.formats.yaml {};
  baseConfig = yamlFormat.generate "omp-base-config.yml" cfg.settings;
  ompConfigSync = pkgs.callPackage ../../../../pkgs/omp-config-sync {};
in {
  options.programs.omp = {
    enable = mkEnableOption "Oh My Pi (omp) configuration";

    settings = mkOption {
      inherit (yamlFormat) type;
      default = {};
      description = ''
        Configuration written to ~/.omp/agent/config.yml.
      '';
      example = literalExpression ''
        {
          advisor.enabled = true;
          modelProviderOrder = [ "openrouter" ];
        }
      '';
    };

    models = mkOption {
      inherit (yamlFormat) type;
      default = {};
      description = ''
        Configuration written to ~/.omp/agent/models.yml.
      '';
      example = literalExpression ''
        {
          providers.openrouter.apiKey = "!cat /path/to/key";
        }
      '';
    };

    env = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = ''
        Environment variables written to ~/.omp/agent/.env.
      '';
      example = literalExpression ''
        {
          GOOGLE_CLOUD_PROJECT = "ai-enablement-500217";
          GOOGLE_CLOUD_LOCATION = "global";
          SMART_CD_LS = "false";
          SMART_CD_GIT_STATUS = "false";
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    home = {
      activation.syncOmpConfig = config.lib.dag.entryAfter ["linkGeneration"] ''
        if [[ -v DRY_RUN ]]; then
          echo "Would synchronize writable Oh My Pi configuration"
        else
          ${ompConfigSync}/bin/omp-config-sync \
            ${baseConfig} \
            "$HOME/.omp/agent/config.yml"
        fi
      '';

      file.".omp/agent/models.yml" = mkIf (cfg.models != {}) {
        source = yamlFormat.generate "omp-models.yml" cfg.models;
      };

      file.".omp/agent/.env" = mkIf (cfg.env != {}) {
        text = concatStringsSep "\n" (mapAttrsToList (k: v: "${k}=${v}") cfg.env) + "\n";
      };
    };
  };
}
