{
  pkgs,
  profile,
  inputs,
  ...
}: {
  home = {
    username = "${profile.username}";
    homeDirectory = "${profile.homeDirectory}";
    stateVersion = "22.05";

    packages = with pkgs;
      [
        inputs.bash-barrier.packages.${pkgs.system}.default
        inputs.batch-edit-prs.packages.${pkgs.system}.default
        inputs.git-diff-regex.packages.${pkgs.system}.default
        inputs.git-disjoint.packages.${pkgs.system}.default
        inputs.git-dl.packages.${pkgs.system}.default
        inputs.git-review.packages.${pkgs.system}.default
        inputs.npm-dep-version.packages.${pkgs.system}.default

        (fenix.complete.withComponents [
          "cargo"
          "clippy"
          "rustc"
          "rustfmt"
          "rust-src"
        ])
        rust-analyzer-nightly
        cargo-tarpaulin
        bacon

        # Allows cargo to build Rust projects that don't have a dev shell
        gcc

        bc
        csvq
        duckdb
        vscode

        # TypeScript
        pnpm
        yarn

        # Python
        uv

        # Nix
        nurl

        nodePackages.prettier

        # AI Tools
        (pkgs.callPackage ../../pkgs/claude-squad {})
      ]
      ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.libiconv
      ];
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
