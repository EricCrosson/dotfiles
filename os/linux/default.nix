{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      alsa-utils
      caffeine-ng
      chromium
      dtrx
      evtest
      maim
      vlc
    ];

    file = {
      # DISCUSS: can we use a nix-provided path to this file?
      ".direnvrc" = {
        text = "source /run/current-system/sw/share/nix-direnv/direnvrc";
      };
    };
  };
}
