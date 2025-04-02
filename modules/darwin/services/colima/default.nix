{
  config,
  lib,
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
        default = "/tmp/colima.log";
        description = "Path to stdout log file";
      };

      stderr = mkOption {
        type = types.str;
        default = "/tmp/colima.error.log";
        description = "Path to stderr log file";
      };
    };

    installHomebrewPackages = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to install colima and related packages via Homebrew";
    };

    enableBuildKit = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to enable BuildKit and set up the CLI plugin";
    };

    username = mkOption {
      type = types.str;
      default = "";
      description = "Username for setting up BuildKit CLI plugin";
      example = "johndoe";
    };

    homeDirectory = mkOption {
      type = types.str;
      default = "";
      description = "Home directory for setting up BuildKit CLI plugin";
      example = "/Users/johndoe";
    };
  };

  config = mkMerge [
    # This part manages the Homebrew packages
    (mkIf (cfg.enable && cfg.installHomebrewPackages) {
      homebrew.brews = [
        "colima"
        "docker"
        "docker-buildx"
        "docker-compose"
      ];
    })

    # This part configures the launchd service
    (mkIf cfg.enable {
      # Validate that required fields are set when BuildKit is enabled
      assertions = [
        {
          assertion = !cfg.enableBuildKit || (cfg.homeDirectory != "" && cfg.username != "");
          message = "When enableBuildKit is true, you must set username and homeDirectory";
        }
      ];

      # Ensure ~/.docker/cli-plugins directory exists and set up BuildKit symlink
      system.activationScripts.postActivation.text = mkIf (cfg.enableBuildKit && cfg.username != "" && cfg.homeDirectory != "") ''
        echo "Setting up Docker BuildKit CLI plugin for user ${cfg.username}..."
        mkdir -p "${cfg.homeDirectory}/.docker/cli-plugins"
        sudo -u "${cfg.username}" ln -sfn /opt/homebrew/bin/docker-buildx "${cfg.homeDirectory}/.docker/cli-plugins/docker-buildx"
        echo "Setting up Docker BuildKit CLI plugin for user ${cfg.username}...done"
      '';

      launchd.user.agents.colima = {
        serviceConfig = {
          Label = "com.user.colima";
          ProgramArguments =
            [
              "/opt/homebrew/bin/colima"
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
          EnvironmentVariables = {
            PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
          };
          RunAtLoad = true;
          KeepAlive = false;
          StandardOutPath = cfg.logging.stdout;
          StandardErrorPath = cfg.logging.stderr;
        };
      };
    })
  ];
}
