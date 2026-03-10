{
  config,
  lib,
  ...
}: let
  cfg = config.services.fzf-theme-sync;
  fzfThemeFile = "${config.home.homeDirectory}/.config/fzf/theme";

  writeFzfTheme = theme: ''
    mkdir -p "$(dirname "${fzfThemeFile}")"
    printf '%s\n' "${theme}" > "${fzfThemeFile}.tmp"
    mv "${fzfThemeFile}.tmp" "${fzfThemeFile}"
  '';
in {
  options.services.fzf-theme-sync = {
    enable = lib.mkEnableOption "Sync fzf theme with macOS appearance";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables.FZF_DEFAULT_OPTS_FILE = fzfThemeFile;

    appearance-sync.services.fzf-theme-sync = {
      onLight = writeFzfTheme "--color=light";
      onDark = writeFzfTheme "--color=dark";
    };
  };
}
