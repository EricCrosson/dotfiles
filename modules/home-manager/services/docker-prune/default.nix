{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.docker-prune;
  maxAgeDuration = "${toString (cfg.maxAge * 24)}h";
  pruneScript = pkgs.writeShellApplication {
    name = "docker-prune";
    runtimeInputs = [pkgs.coreutils];
    text = ''
      if ! ${cfg.dockerCommand} info >/dev/null 2>&1; then
        exit 0
      fi

      ${cfg.dockerCommand} system prune --all --force --filter "until=${maxAgeDuration}"
      ${cfg.dockerCommand} builder prune --all --force --filter "until=${maxAgeDuration}"

      cutoff="$(${pkgs.coreutils}/bin/date +%s -d "${toString (cfg.maxAge * 86400)} seconds ago")"
      ${cfg.dockerCommand} volume ls --filter dangling=true --quiet | while IFS= read -r volume; do
        [ -z "$volume" ] && continue
        created="$(${cfg.dockerCommand} volume inspect --format '{{.CreatedAt}}' "$volume")"
        created_epoch="$(${pkgs.coreutils}/bin/date +%s -d "$created" 2>/dev/null || true)"
        if [ -n "$created_epoch" ] && [ "$created_epoch" -le "$cutoff" ]; then
          ${cfg.dockerCommand} volume rm "$volume"
        fi
      done
    '';
  };
in {
  options.services.docker-prune = {
    enable = lib.mkEnableOption "periodic cleanup of unused Docker resources";

    dockerCommand = lib.mkOption {
      type = lib.types.str;
      default = "docker";
      description = "Docker CLI command used by the cleanup script.";
    };

    maxAge = lib.mkOption {
      type = lib.types.ints.positive;
      default = 14;
      description = "Delete unused Docker resources older than this many days.";
    };

    interval = lib.mkOption {
      type = lib.types.ints.positive;
      default = 86400;
      description = "Run interval in seconds.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.hostPlatform.isDarwin;
        message = "services.docker-prune is implemented via launchd and only supports Darwin.";
      }
    ];

    launchd-with-logs.services.docker-prune = {
      command = lib.getExe pruneScript;
      inherit (cfg) interval;
      runAtLoad = false;
      environment = {
        PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
      };
      logging.stderr = "${config.home.homeDirectory}/Library/Logs/docker-prune.error.log";
    };
  };
}
