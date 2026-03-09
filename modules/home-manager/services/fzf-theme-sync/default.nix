{
  config,
  lib,
  pkgs,
  ...
}: let
  fzfThemeFile = "${config.home.homeDirectory}/.config/fzf/theme";

  syncScript = pkgs.writeShellApplication {
    name = "fzf-theme-sync";
    runtimeInputs = with pkgs; [coreutils];
    text = ''
      if defaults read -g AppleInterfaceStyle &>/dev/null; then
        color="--color=dark"
      else
        color="--color=light"
      fi

      current="$(cat "${fzfThemeFile}" 2>/dev/null || true)"
      if [ "$current" = "$color" ]; then
        exit 0
      fi

      mkdir -p "$(dirname "${fzfThemeFile}")"
      printf '%s\n' "$color" > "${fzfThemeFile}.tmp"
      mv "${fzfThemeFile}.tmp" "${fzfThemeFile}"
    '';
  };
in
  lib.mkIf pkgs.stdenv.isDarwin {
    home.sessionVariables.FZF_DEFAULT_OPTS_FILE = fzfThemeFile;

    launchd-with-logs.services.fzf-theme-sync = {
      command = "${syncScript}/bin/fzf-theme-sync";
      runAtLoad = true;
      watchPaths = ["${config.home.homeDirectory}/Library/Preferences/.GlobalPreferences.plist"];
      logging = {
        stderr = "${config.home.homeDirectory}/Library/Logs/fzf-theme-sync.error.log";
      };
    };

    home.activation.syncFzfTheme = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run ${syncScript}/bin/fzf-theme-sync
    '';
  }
