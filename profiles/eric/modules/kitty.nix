{pkgs, ...}: let
  # Create an inline derivation for the Homebrew-installed Kitty
  homebrewKitty = pkgs.runCommand "homebrew-kitty" {} ''
    mkdir -p $out/bin
    ln -s /opt/homebrew/bin/kitty $out/bin/kitty
  '';
in {
  programs.kitty = {
    enable = true;
    package =
      if pkgs.stdenv.isDarwin
      then homebrewKitty
      else pkgs.kitty;
    font = {
      name = "JetBrains Mono";
      size = 12;
    };
    keybindings = {
      "ctrl+shift+o" = "toggle_layout stack";
    };
    settings = {
      cursor_blink_interval = 0;
      cursor_shape = "block";
      macos_option_as_alt = "yes";
      scrollback_lines = 50000;
      shell_integration = "no-cursor";
    };
  };
}
