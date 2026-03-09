{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.claude;

  claudeJson = "${config.home.homeDirectory}/.claude.json";
  staleSymlink = "${config.home.homeDirectory}/.claude/settings.local.json";

  syncScript = pkgs.writeShellApplication {
    name = "claude-theme-sync";
    runtimeInputs = with pkgs; [coreutils jq];
    text = ''
      if defaults read -g AppleInterfaceStyle &>/dev/null; then
        desired="dark"
      else
        desired="light"
      fi

      claude_json="${claudeJson}"
      if [ ! -f "$claude_json" ]; then
        exit 0
      fi

      current=$(jq -r '.theme // empty' "$claude_json")
      if [ "$current" = "$desired" ]; then
        exit 0
      fi

      jq --arg theme "$desired" '.theme = $theme' "$claude_json" > "$claude_json.tmp"
      mv "$claude_json.tmp" "$claude_json"
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
      run rm -f "${staleSymlink}"
      run ${syncScript}/bin/claude-theme-sync
    '';
  };
}
