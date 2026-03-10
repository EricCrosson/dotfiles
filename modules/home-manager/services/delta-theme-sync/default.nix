{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.services.delta-theme-sync;

  gitconfigFormat = pkgs.formats.gitconfig {};

  lightConfig =
    gitconfigFormat.generate "delta-config-light"
    {delta.features = cfg.light-feature;};

  darkConfig =
    gitconfigFormat.generate "delta-config-dark"
    {delta.features = cfg.dark-feature;};

  gitConfigDir = "${config.home.homeDirectory}/.config/git";
  deltaThemeFile = "${gitConfigDir}/delta-theme.gitconfig";

  swapDeltaConfig = target: ''
    mkdir -p "${gitConfigDir}"
    ln -sf "${target}" "${deltaThemeFile}.tmp"
    mv "${deltaThemeFile}.tmp" "${deltaThemeFile}"
  '';
in {
  options.services.delta-theme-sync = {
    enable = lib.mkEnableOption "Sync delta theme with macOS appearance";

    light-feature = lib.mkOption {
      type = lib.types.str;
      description = "Delta feature name for light mode";
    };

    dark-feature = lib.mkOption {
      type = lib.types.str;
      description = "Delta feature name for dark mode";
    };
  };

  config = lib.mkIf cfg.enable {
    appearance-sync.services.delta-theme-sync = {
      onLight = swapDeltaConfig lightConfig;
      onDark = swapDeltaConfig darkConfig;
    };
  };
}
