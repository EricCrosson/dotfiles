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
        inputs.batch-edit-prs.packages.${pkgs.system}.default
        inputs.git-diff-regex.packages.${pkgs.system}.default
        inputs.git-review.packages.${pkgs.system}.default
        inputs.npm-dep-version.packages.${pkgs.system}.default

        (fenix.stable.withComponents [
          "cargo"
          "clippy"
          "rustc"
          "rustfmt"
          "rust-src"
          "rust-analyzer"
        ])
        cargo-tarpaulin
        bacon

        # Allows cargo to build Rust projects that don't have a dev shell
        gcc

        bc
        vscode
        yazi
        showboat

        # TypeScript
        pnpm
        yarn
      ]
      ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.libiconv
      ];
  };

  services.cargo-sweep.enable = true;

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
