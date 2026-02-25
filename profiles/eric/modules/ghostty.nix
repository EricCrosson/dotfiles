{pkgs, ...}: {
  programs.ghostty = {
    enable = true;
    package =
      if pkgs.stdenv.isDarwin
      then null
      else pkgs.ghostty;
    enableZshIntegration = true;
    installBatSyntax = !pkgs.stdenv.isDarwin;
    installVimSyntax = !pkgs.stdenv.isDarwin;
    # settings = {
    #   theme = "catppuccin-${pkgs.lib.strings.toLower profile.preferences.theme}";
    # };
  };
}
