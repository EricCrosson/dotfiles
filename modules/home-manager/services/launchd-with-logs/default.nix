{
  config,
  lib,
  ...
}:
with lib; let
  # Define the schema for log rotation options
  logRotationOptionsType = types.submodule {
    options = {
      count = mkOption {
        type = types.int;
        default = 7;
        description = "Number of rotated log files to keep";
      };

      size = mkOption {
        type = types.int;
        default = 786432; # 768 MB in kilobytes
        description = "Size in kilobytes at which log files should be rotated";
      };

      when = mkOption {
        type = types.str;
        default = "$D0";
        description = "When to rotate logs (e.g., $D0=daily at midnight, $W0D23=weekly on Sunday at 11pm, etc.)";
      };

      flags = mkOption {
        type = types.str;
        default = "C";
        description = "Log rotation flags (C=create if not exists, Z=compress)";
      };
    };
  };

  # Define the schema for a launchd service with logging
  serviceType = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable this launchd service";
      };

      command = mkOption {
        type = types.str;
        description = "Command to run";
      };

      args = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Arguments to pass to the command";
      };

      environment = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Environment variables to set for the service";
      };

      interval = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = "Run interval in seconds (null means no interval)";
      };

      keepAlive = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to keep the service alive";
      };

      runAtLoad = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to run the service when loaded";
      };

      logging = mkOption {
        type = types.submodule {
          options = {
            stdout = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Path for stdout logs, null means /dev/null";
            };

            stderr = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Path for stderr logs, null means /dev/null";
            };

            rotation = mkOption {
              type = logRotationOptionsType;
              default = {};
              description = "Log rotation settings";
            };
          };
        };
        default = {};
        description = "Logging configuration";
      };

      workingDirectory = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Working directory for the service";
      };

      serviceDependencies = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "List of services this service depends on";
      };

      watchPaths = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "List of file paths to watch for changes (triggers service run)";
      };
    };
  };
in {
  options.launchd-with-logs = {
    services = mkOption {
      type = types.attrsOf serviceType;
      default = {};
      description = "Launchd services with log rotation";
    };
  };

  config = mkIf (config.launchd-with-logs.services != {}) {
    # Convert launchd-with-logs services into regular launchd agents
    launchd.agents =
      mapAttrs (
        _name: service:
          mkIf service.enable {
            enable = true;
            config = {
              ProgramArguments =
                if service.args != []
                then [service.command] ++ service.args
                else [service.command];

              EnvironmentVariables = service.environment;

              # Set StartInterval if defined
              ${
                if service.interval != null
                then "StartInterval"
                else null
              } =
                service.interval;

              # Set KeepAlive if true
              ${
                if service.keepAlive
                then "KeepAlive"
                else null
              } =
                service.keepAlive;

              RunAtLoad = service.runAtLoad;

              # Set log paths or default to /dev/null
              StandardOutPath =
                if service.logging.stdout != null
                then service.logging.stdout
                else "/dev/null";

              StandardErrorPath =
                if service.logging.stderr != null
                then service.logging.stderr
                else "/dev/null";

              # Set working directory if defined
              ${
                if service.workingDirectory != null
                then "WorkingDirectory"
                else null
              } =
                service.workingDirectory;

              # Set service dependencies if any
              ${
                if service.serviceDependencies != []
                then "ServiceDependencies"
                else null
              } =
                service.serviceDependencies;

              # Set watch paths if any
              ${
                if service.watchPaths != []
                then "WatchPaths"
                else null
              } =
                service.watchPaths;
            };
          }
      )
      config.launchd-with-logs.services;

    # Create newsyslog configuration for log rotation in a file in ~/.config
    # that will be picked up by newsyslog
    home.file.".config/newsyslog-launchd-with-logs.conf" =
      mkIf (any (
        s:
          s.enable && (s.logging.stdout != null || s.logging.stderr != null)
      ) (attrValues config.launchd-with-logs.services)) {
        text = let
          # Get all services with logging enabled
          servicesWithLogs = filter (
            s:
              s.enable && (s.logging.stdout != null || s.logging.stderr != null)
          ) (attrValues config.launchd-with-logs.services);

          # Helper to create a log entry line
          makeLogEntry = logPath: rotation: ''
            ${logPath} ${config.home.username}:staff 644 ${toString rotation.count} ${toString rotation.size} ${rotation.when} ${rotation.flags}'';

          # Get log entries for a single service
          getServiceLogEntries = service:
            (optional (service.logging.stdout != null)
              (makeLogEntry service.logging.stdout service.logging.rotation))
            ++ (optional (service.logging.stderr != null)
              (makeLogEntry service.logging.stderr service.logging.rotation));

          # Combine all service log entries
          allEntries = concatMap getServiceLogEntries servicesWithLogs;
        in ''
          # logfilename                                 [owner:group]    mode count size when  flags
          ${concatStringsSep "\n" allEntries}
        '';
      };

    # Create directory for logs if it doesn't exist
    home.activation.ensureLogDirectory = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ ! -d "$HOME/Library/Logs" ]; then
        $DRY_RUN_CMD mkdir -p $HOME/Library/Logs
      fi
    '';
  };
}
