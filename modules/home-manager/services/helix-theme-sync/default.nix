{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.services.helix-theme-sync;

  tomlFormat = pkgs.formats.toml {};

  lightConfig =
    tomlFormat.generate "helix-config-light"
    (cfg.settings // {theme = cfg.light-theme;});

  darkConfig =
    tomlFormat.generate "helix-config-dark"
    (cfg.settings // {theme = cfg.dark-theme;});

  helixConfigDir = "${config.home.homeDirectory}/.config/helix";

  swapHelixConfig = target: ''
    ln -sf "${target}" "${helixConfigDir}/config.toml.tmp"
    mv "${helixConfigDir}/config.toml.tmp" "${helixConfigDir}/config.toml"
    pkill -USR1 hx || true
  '';
in {
  options.services.helix-theme-sync = {
    enable = lib.mkEnableOption "Sync Helix theme with macOS appearance";

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Base Helix settings to merge with each theme variant";
    };

    light-theme = lib.mkOption {
      type = lib.types.str;
      description = "Helix theme name for light mode";
    };

    dark-theme = lib.mkOption {
      type = lib.types.str;
      description = "Helix theme name for dark mode";
    };
  };

  config = lib.mkIf cfg.enable {
    appearance-sync.services.helix-theme-sync = {
      onLight = swapHelixConfig lightConfig;
      onDark = swapHelixConfig darkConfig;
    };
  };
}
