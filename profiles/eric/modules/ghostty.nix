{pkgs, ...}: {
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
      if pkgs.stdenv.isDarwin
      then null
      else pkgs.ghostty;
    enableZshIntegration = true;
    installBatSyntax = !pkgs.stdenv.isDarwin;
    installVimSyntax = !pkgs.stdenv.isDarwin;
    settings = {
      theme = "light:Alabaster,dark:Kitty Default";
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
