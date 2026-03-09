{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.helix-theme-sync;

  tomlFormat = pkgs.formats.toml {};

  lightConfig =
    tomlFormat.generate "helix-config-light"
    (cfg.settings // {theme = cfg.light-theme;});

  darkConfig =
    tomlFormat.generate "helix-config-dark"
    (cfg.settings // {theme = cfg.dark-theme;});

  helixConfigDir = "${config.home.homeDirectory}/.config/helix";

  syncScript = pkgs.writeShellApplication {
    name = "helix-theme-sync";
    runtimeInputs = with pkgs; [coreutils];
    text = ''
      # Detect macOS appearance
      if defaults read -g AppleInterfaceStyle &>/dev/null; then
        target="${darkConfig}"
      else
        target="${lightConfig}"
      fi

      config_path="${helixConfigDir}/config.toml"

      # Check if symlink already points to the correct target
      current="$(readlink "$config_path" 2>/dev/null || true)"
      if [ "$current" = "$target" ]; then
        exit 0
      fi

      # Atomically swap the symlink
      ln -sf "$target" "$config_path.tmp"
      mv "$config_path.tmp" "$config_path"

      # Signal all running Helix instances to reload config
      pkill -USR1 hx || true
    '';
  };
in {
  options.services.helix-theme-sync = {
    enable = mkEnableOption "Sync Helix theme with macOS appearance";

    settings = mkOption {
      type = types.attrs;
      default = {};
      description = "Base Helix settings to merge with each theme variant";
    };

    light-theme = mkOption {
      type = types.str;
      description = "Helix theme name for light mode";
    };

    dark-theme = mkOption {
      type = types.str;
      description = "Helix theme name for dark mode";
    };
  };

  config = mkIf cfg.enable {
    launchd-with-logs.services.helix-theme-sync = {
      command = "${syncScript}/bin/helix-theme-sync";
      runAtLoad = true;
      watchPaths = ["${config.home.homeDirectory}/Library/Preferences/.GlobalPreferences.plist"];
      logging = {
        stderr = "${config.home.homeDirectory}/Library/Logs/helix-theme-sync.error.log";
      };
    };

    home.activation.syncHelixTheme = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run ${syncScript}/bin/helix-theme-sync
    '';
  };
}
