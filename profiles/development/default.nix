{
  pkgs,
  profile,
  inputs,
  ...
}: let
  showboat = pkgs.callPackage ../../pkgs/showboat {};
in {
  home = {
    username = "${profile.username}";
    homeDirectory = "${profile.homeDirectory}";
    stateVersion = "22.05";

    packages = with pkgs;
      [
        inputs.bash-barrier.packages.${pkgs.system}.default
        inputs.git-diff-regex.packages.${pkgs.system}.default
        inputs.git-review.packages.${pkgs.system}.default
        inputs.npm-dep-version.packages.${pkgs.system}.default

        bacon

        bc
        yazi
        showboat
      ]
      ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.libiconv
      ];

    file.".cargo/config.toml".text = ''
      [build]
      rustc-wrapper = "kache"
    '';
  };

  services.cargo-sweep.enable = true;
  services.docker-prune = {
    enable = true;
    maxAge = 14;
  };

  programs = {
    fzf = {
      tmux = {
        enableShellIntegration = true;
      };
    };
    tmux = {
      enable = true;
      clock24 = true;
      historyLimit = 10000;
    };
  };
}
