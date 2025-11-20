{pkgs, ...}: {
  home = let
    system_specific_packages = with pkgs;
      {
        "x86_64-linux" = [
          google-chrome
        ];
        "aarch64-linux" = [];
      }
      ."${pkgs.system}";
  in {
    packages = with pkgs;
      [
        alsa-utils
        dtrx
        evtest
        maim
        rm-improved
        vlc
      ]
      ++ system_specific_packages;

    shellAliases = {
      rip = "rip --graveyard $HOME/.local/share/Trash";
    };

    file = {
      # DISCUSS: can we use a nix-provided path to this file?
      ".direnvrc" = {
        text = "source /run/current-system/sw/share/nix-direnv/direnvrc";
      };
    };
  };
}
