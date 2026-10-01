{
  config,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  # Split-creation leader: cmd+d on macOS, alt+d on Linux. Keyboards and OS
  # customs differ per platform. Ghostty trigger syntax spells these "super"
  # (cmd alias) and "alt" (left Alt; there is no left/right Alt distinction).
  splitLeader =
    if isDarwin
    then "super+d"
    else "alt+d";
in {
  home.file.".config/ghostty/focus-pane.glsl".text = ''
    // Shows border on focused pane
    void mainImage(out vec4 fragColor, in vec2 fragCoord) {
      vec2 uv = fragCoord / iResolution.xy;
      vec4 terminal = texture2D(iChannel0, uv);
      vec3 color = terminal.rgb;
      if (iFocus > 0) {
        float borderSize = 2.0;
        vec2 pixelCoord = fragCoord;
        bool isBorder = pixelCoord.x < borderSize ||
          pixelCoord.x > iResolution.x - borderSize ||
          pixelCoord.y < borderSize ||
          pixelCoord.y > iResolution.y - borderSize;
        if (isBorder) {
          color = vec3(0, 0.35, 0.74) * 1.0;
        }
      }
      fragColor = vec4(color, 1.0);
    }
  '';

  programs.ghostty = {
    enable = true;
    package =
      if pkgs.stdenv.hostPlatform.isDarwin
      then null
      else pkgs.ghostty;
    enableZshIntegration = true;
    installBatSyntax = !pkgs.stdenv.hostPlatform.isDarwin;
    installVimSyntax = !pkgs.stdenv.hostPlatform.isDarwin;
    settings = {
      theme =
        if config.appearance-sync.enable
        then "light:Alabaster,dark:Kitty Default"
        else "Alabaster";
      cursor-style-blink = false;
      cursor-style = "block";
      shell-integration-features = "no-cursor";
      unfocused-split-opacity = 1;
      custom-shader = "~/.config/ghostty/focus-pane.glsl";
      mouse-hide-while-typing = true;
      selection-clear-on-typing = true;
      working-directory = "home";
      window-inherit-working-directory = false;
      keybind = [
        # Unbind default cmd+d single-press split-right so cmd+d works as the
        # sequence leader (no-op on Linux: no super+d default binding)
        "super+d=unbind"

        # Split creation (leader, then vim mnemonic)
        "${splitLeader}>v=new_split:right"
        "${splitLeader}>s=new_split:down"

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
