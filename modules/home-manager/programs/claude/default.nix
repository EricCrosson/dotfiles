{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.claude;
  jsonFormat = pkgs.formats.json {};

  lightSettings = jsonFormat.generate "claude-settings-light" {theme = "light";};
  darkSettings = jsonFormat.generate "claude-settings-dark" {theme = "dark";};
  settingsPath = "${config.home.homeDirectory}/.claude/settings.local.json";

  syncScript = pkgs.writeShellApplication {
    name = "claude-theme-sync";
    runtimeInputs = with pkgs; [coreutils];
    text = ''
      if defaults read -g AppleInterfaceStyle &>/dev/null; then
        target="${darkSettings}"
      else
        target="${lightSettings}"
      fi

      current="$(readlink "${settingsPath}" 2>/dev/null || true)"
      if [ "$current" = "$target" ]; then
        exit 0
      fi

      ln -sf "$target" "${settingsPath}.tmp"
      mv "${settingsPath}.tmp" "${settingsPath}"
    '';
  };
in {
  options.programs.claude = {
    theme-sync = {
      enable = mkEnableOption "Sync Claude Code theme with macOS appearance";
    };
  };

  config = mkIf cfg.theme-sync.enable {
    launchd-with-logs.services.claude-theme-sync = {
      command = "${syncScript}/bin/claude-theme-sync";
      runAtLoad = true;
      watchPaths = [
        "${config.home.homeDirectory}/Library/Preferences/.GlobalPreferences.plist"
      ];
      logging = {
        stderr = "${config.home.homeDirectory}/Library/Logs/claude-theme-sync.error.log";
      };
    };

    home.activation.syncClaudeTheme = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run ${syncScript}/bin/claude-theme-sync
    '';
  };
}
