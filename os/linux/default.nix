{pkgs, ...}: {
  home = {
    packages = with pkgs;
      [
        dtrx
        evtest
        maim
      ]
      ++
      # These packages are only supported on x86_64-linux
      pkgs.lib.optionals (pkgs.system == "x86_64-linux") [simplenote];

    file = {
      # DISCUSS: can we use a nix-provided path to this file?
      ".direnvrc" = {
        text = "source /run/current-system/sw/share/nix-direnv/direnvrc";
      };
    };
  };
}
