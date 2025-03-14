{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.auto-merge-bitgo-prs;

  mkAutoMergeService = name: serviceCfg: {
    command = "${serviceCfg.package}/bin/${name}";
    environment = {
      GITHUB_TOKEN_PATH = "${config.sops.secrets.github_token_bitgo.path}";
    };
    inherit (serviceCfg) interval;
    serviceDependencies = ["sops-nix"];
    logging = {
      stdout = "${config.home.homeDirectory}/Library/Logs/${name}.log";
      stderr = "${config.home.homeDirectory}/Library/Logs/${name}.error.log";
    };
  };
in {
  options.services.auto-merge-bitgo-prs = {
    previouslyReviewedOpenapiSpecs = {
      enable = mkEnableOption "Auto-merge previously reviewed API docs PRs";

      package = mkOption {
        type = types.package;
        default = pkgs.callPackage ../../../../pkgs/auto-merge-previously-reviewed-api-docs-prs {};
        description = "Package for auto-merge-previously-reviewed-api-docs-prs";
      };

      interval = mkOption {
        type = types.int;
        default = 300; # 5 minutes
        description = "Interval in seconds between runs";
      };
    };

    trivialOpenapiSpecVersionBump = {
      enable = mkEnableOption "Auto-merge PRs that only bump OpenAPI spec version numbers";

      package = mkOption {
        type = types.package;
        default = pkgs.callPackage ../../../../pkgs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers {};
        description = "Package for auto-merge-prs-that-only-bump-openapi-spec-version-numbers";
      };

      interval = mkOption {
        type = types.int;
        default = 300; # 5 minutes
        description = "Interval in seconds between runs";
      };
    };
  };

  config = {
    # Create the services if their respective enables are set
    launchd-with-logs.services = mkMerge [
      (mkIf cfg.previouslyReviewedOpenapiSpecs.enable {
        auto-merge-previously-reviewed-api-docs-prs =
          mkAutoMergeService "auto-merge-previously-reviewed-api-docs-prs" cfg.previouslyReviewedOpenapiSpecs;
      })

      (mkIf cfg.trivialOpenapiSpecVersionBump.enable {
        auto-merge-prs-that-only-bump-openapi-spec-version-numbers =
          mkAutoMergeService "auto-merge-prs-that-only-bump-openapi-spec-version-numbers" cfg.trivialOpenapiSpecVersionBump;
      })
    ];
  };
}
