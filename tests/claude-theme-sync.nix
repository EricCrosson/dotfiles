{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  jsonFormat = pkgs.formats.json {};

  lightSettings = jsonFormat.generate "claude-settings-light" {theme = "light";};
  darkSettings = jsonFormat.generate "claude-settings-dark" {theme = "dark";};
  settingsPath = "/home/testuser/.claude/settings.local.json";

  drv = pkgs.writeShellApplication {
    name = "claude-theme-sync";
    runtimeInputs = [pkgs.coreutils];
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

  script = builtins.readFile "${drv}/bin/claude-theme-sync";

  # === Contract assertions ===

  # Both theme config store paths must appear in the script (match derivation names)
  test-dark-config = assert assertContains "dark-config-ref" script "claude-settings-dark"; true;

  test-light-config = assert assertContains "light-config-ref" script "claude-settings-light"; true;

  # macOS appearance detection must be present
  test-appearance-detection = assert assertContains "appearance-check" script "AppleInterfaceStyle"; true;

  # Must target the settings.local.json path
  test-settings-target = assert assertContains "settings-path" script "settings.local.json"; true;
in
  assert test-dark-config;
  assert test-light-config;
  assert test-appearance-detection;
  assert test-settings-target; "all tests passed"
