{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  claudeJson = "/home/testuser/.claude.json";

  drv = pkgs.writeShellApplication {
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

  script = builtins.readFile "${drv}/bin/claude-theme-sync";

  # === Contract assertions ===

  # macOS appearance detection must be present
  test-appearance-detection = assert assertContains "appearance-check" script "AppleInterfaceStyle"; true;

  # Must target ~/.claude.json
  test-claude-json = assert assertContains "claude-json-path" script ".claude.json"; true;

  # Must use jq for JSON update
  test-jq-usage = assert assertContains "jq-usage" script "jq"; true;

  # Must update the theme key
  test-theme-key = assert assertContains "theme-key" script ".theme"; true;
in
  assert test-appearance-detection;
  assert test-claude-json;
  assert test-jq-usage;
  assert test-theme-key; "all tests passed"
