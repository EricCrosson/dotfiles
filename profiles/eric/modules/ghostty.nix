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
      working-directory = "home";
      window-inherit-working-directory = false;
      keybind = [
        # Unbind default cmd+d (single-press split right) to use as leader
        "super+d=unbind"

        # Split creation (leader: cmd+d, then vim mnemonic)
        "super+d>v=new_split:right"
        "super+d>s=new_split:down"

        # Pane navigation (vim-style, direct)
        "ctrl+shift+h=goto_split:left"
        "ctrl+shift+j=goto_split:down"
        "ctrl+shift+k=goto_split:up"
        "ctrl+shift+l=goto_split:right"

        # Pane management
        "ctrl+shift+z=toggle_split_zoom"
        "ctrl+shift+w=close_surface"
        "ctrl+shift+equal=equalize_splits"
      ];
    };
  };
}
