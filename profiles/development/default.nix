{
  pkgs,
  user,
  inputs,
  ...
}: {
  home = {
    username = "${user.username}";
    homeDirectory = "${user.homeDirectory}";
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
        inputs.nurl.packages.${pkgs.system}.default

        # This isn't specific to development but it takes a system that can
        # compile Rust from source
        inputs.percentage-changed-calculator.packages.${pkgs.system}.default

        # DISCUSS: switch between nightly and precompiled releases
        (fenix.complete.withComponents [
          "cargo"
          "clippy"
          "rustc"
          "rustfmt"
          "rust-src"
        ])
        rust-analyzer-nightly
        cargo-tarpaulin
        cargo-watch

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
      ]
      ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.Security
        pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
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
