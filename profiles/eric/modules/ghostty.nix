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
    settings = {
      theme = "Alabaster";
      cursor-style-blink = false;
      cursor-style = "block";
      shell-integration-features = "no-cursor";
      window-inherit-working-directory = false;
      keybind = "cmd+t=new_tab:inherit-cwd";
    };
  };
}
