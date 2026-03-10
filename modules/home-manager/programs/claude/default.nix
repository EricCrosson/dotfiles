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

  patchClaudeTheme = theme: ''
    claude_json="${claudeJson}"
    if [ ! -f "$claude_json" ]; then
      exit 0
    fi
    jq --arg theme "${theme}" '.theme = $theme' "$claude_json" > "$claude_json.tmp"
    mv "$claude_json.tmp" "$claude_json"
  '';
in {
  options.programs.claude = {
    theme-sync = {
      enable = mkEnableOption "Sync Claude Code theme with macOS appearance";
    };
  };

  config = mkIf cfg.theme-sync.enable {
    appearance-sync.services.claude-theme-sync = {
      runtimeInputs = with pkgs; [jq];
      onLight = patchClaudeTheme "light";
      onDark = patchClaudeTheme "dark";
    };

    home.activation.syncClaudeTheme = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run rm -f "${staleSymlink}"
    '';
  };
}
