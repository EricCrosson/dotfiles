{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.cargo-sweep;
in {
  options.services.cargo-sweep = {
    enable = mkEnableOption "cargo-sweep (periodic Rust build artifact cleanup)";

    package = mkOption {
      type = types.package;
      default = pkgs.cargo-sweep;
      description = "cargo-sweep package";
    };

    paths = mkOption {
      type = types.listOf types.str;
      default = ["${config.home.homeDirectory}/workspace"];
      description = "Directories to recursively sweep for Rust projects";
    };

    maxAge = mkOption {
      type = types.int;
      default = 7;
      description = "Delete build artifacts older than this many days";
    };

    interval = mkOption {
      type = types.int;
      default = 604800; # weekly
      description = "Run interval in seconds";
    };
  };

  # TODO: Remove this module when Cargo ships built-in target directory GC.
  # Tracking issues:
  #   - https://github.com/rust-lang/cargo/issues/13136
  #   - https://github.com/rust-lang/cargo/issues/10589
  config = mkIf cfg.enable {
    home.packages = [cfg.package];

    launchd-with-logs.services.cargo-sweep = {
      command = "${cfg.package}/bin/cargo-sweep";
      args =
        ["sweep" "--recursive" "--time" "${toString cfg.maxAge}"]
        ++ cfg.paths;
      inherit (cfg) interval;
      runAtLoad = false;
      logging = {
        stderr = "${config.home.homeDirectory}/Library/Logs/cargo-sweep.error.log";
      };
    };
  };
}
