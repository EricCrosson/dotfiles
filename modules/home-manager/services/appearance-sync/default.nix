{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.appearance-sync;
  globalPrefs = "${config.home.homeDirectory}/Library/Preferences/.GlobalPreferences.plist";
  stateDir = "${config.home.homeDirectory}/.local/state/appearance-sync";

  mkSyncScript = name: svc:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = [pkgs.coreutils] ++ svc.runtimeInputs;
      text = ''
        if defaults read -g AppleInterfaceStyle &>/dev/null; then
          mode="dark"
        else
          mode="light"
        fi

        state_file="${stateDir}/${name}"
        current="$(cat "$state_file" 2>/dev/null || true)"
        if [ "$current" = "$mode" ]; then
          exit 0
        fi

        if [ "$mode" = "dark" ]; then
          ${svc.onDark}
        else
          ${svc.onLight}
        fi

        mkdir -p "$(dirname "$state_file")"
        printf '%s\n' "$mode" > "$state_file"
      '';
    };
in {
  options.appearance-sync.services = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to enable this appearance-sync service.";
        };

        runtimeInputs = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          default = [];
          description = "Additional packages to add to the script's PATH (coreutils is always included).";
        };

        onLight = lib.mkOption {
          type = lib.types.lines;
          description = "Shell commands to run when system appearance is light.";
        };

        onDark = lib.mkOption {
          type = lib.types.lines;
          description = "Shell commands to run when system appearance is dark.";
        };
      };
    });
    default = {};
    description = "Services that sync application config with macOS appearance.";
  };

  config = lib.mkIf pkgs.stdenv.isDarwin {
    launchd-with-logs.services = lib.mapAttrs (name: svc:
      lib.mkIf svc.enable {
        command = lib.getExe (mkSyncScript name svc);
        runAtLoad = true;
        watchPaths = [globalPrefs];
        logging.stderr = "${config.home.homeDirectory}/Library/Logs/${name}.error.log";
      })
    cfg.services;

    home.activation = lib.mapAttrs (name: svc: let
      script = mkSyncScript name svc;
      stateFile = "${stateDir}/${name}";
    in
      lib.mkIf svc.enable (lib.hm.dag.entryAfter ["writeBoundary"] ''
        run rm -f "${stateFile}"
        run ${lib.getExe script}
      ''))
    cfg.services;
  };
}
