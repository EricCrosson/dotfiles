{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.colima;
in {
  options.services.colima = {
    enable = mkEnableOption "Colima Docker VM";

    cpus = mkOption {
      type = types.int;
      default = 8;
      description = "Number of CPUs to allocate";
    };

    memory = mkOption {
      type = types.int;
      default = 8;
      description = "Amount of memory in GB to allocate";
    };

    arch = mkOption {
      type = types.enum ["aarch64" "x86_64"];
      default = "aarch64";
      description = "Architecture to emulate";
    };

    vmType = mkOption {
      type = types.enum ["vz" "qemu"];
      default = "vz";
      description = "Type of VM to use";
    };

    enableRosetta = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to enable Rosetta translation";
    };

    extraArgs = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Extra arguments to pass to colima start";
    };

    logging = {
      stdout = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/colima.log";
        description = "Path to stdout log file";
      };

      stderr = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/colima.error.log";
        description = "Path to stderr log file";
      };
    };
  };

  config = mkIf cfg.enable {
    launchd-with-logs.services.colima = {
      command = "/opt/homebrew/bin/colima";
      args =
        [
          "start"
          "--cpu"
          (toString cfg.cpus)
          "--memory"
          (toString cfg.memory)
          "--arch"
          cfg.arch
          "--vm-type=${cfg.vmType}"
        ]
        ++ (optional cfg.enableRosetta "--vz-rosetta")
        ++ cfg.extraArgs;

      environment = {
        PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
      };
      keepAlive = false;
      logging = {
        inherit (cfg.logging) stdout stderr;
      };
    };
  };
}
