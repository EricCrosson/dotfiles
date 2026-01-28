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
        # git-disjoint-unwrapped: The actual binary for the plugin to run
        (pkgs.runCommand "git-disjoint-unwrapped" {} ''
          mkdir -p $out/bin
          ln -s ${inputs.git-disjoint.packages.${pkgs.system}.default}/bin/git-disjoint \
            $out/bin/git-disjoint-unwrapped
        '')
        # git-disjoint: Wrapper that calls through 1Password plugin
        (pkgs.writeShellScriptBin "git-disjoint" ''
          exec ${pkgs._1password-cli}/bin/op plugin run -- git-disjoint-unwrapped "$@"
        '')
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
        vscode

        # TypeScript
        pnpm
        yarn
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
