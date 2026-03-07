{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  tomlFormat = pkgs.formats.toml {};

  lightConfig =
    tomlFormat.generate "helix-config-light"
    {theme = "catppuccin_latte";};

  darkConfig =
    tomlFormat.generate "helix-config-dark"
    {theme = "ao";};

  helixConfigDir = "/home/testuser/.config/helix";

  drv = pkgs.writeShellApplication {
    name = "helix-theme-sync";
    runtimeInputs = [pkgs.coreutils];
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

  script = builtins.readFile "${drv}/bin/helix-theme-sync";

  # === Contract assertions ===

  # Both theme config store paths must appear in the script (match derivation names)
  test-dark-config = assert assertContains "dark-config-ref" script "helix-config-dark"; true;

  test-light-config = assert assertContains "light-config-ref" script "helix-config-light"; true;

  # macOS appearance detection must be present
  test-appearance-detection = assert assertContains "appearance-check" script "AppleInterfaceStyle"; true;

  # Must target the helix config path
  test-config-target = assert assertContains "config-path" script "config/helix/config.toml"; true;

  # Must signal helix to reload
  test-reload-signal = assert assertContains "pkill-signal" script "pkill -USR1 hx"; true;
in
  assert test-dark-config;
  assert test-light-config;
  assert test-appearance-detection;
  assert test-config-target;
  assert test-reload-signal; "all tests passed"
