{
  description = "dotfiles dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    # deadnix: skip
    self,
    nixpkgs,
    git-hooks,
    sops-nix,
  }: let
    forEachSystem = nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-darwin"
      "aarch64-linux"
    ];
  in {
    checks = forEachSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pre-commit-check = pkgs.callPackage ./git-hooks.nix {inherit git-hooks;};
      launchd-with-logs-test =
        builtins.seq
        (import ../tests/launchd-with-logs.nix {inherit pkgs;})
        (pkgs.runCommand "launchd-with-logs-test" {} "touch $out");
      litellm-proxy-test =
        builtins.seq
        (import ../tests/litellm-proxy.nix {inherit pkgs;})
        (pkgs.runCommand "litellm-proxy-test" {} "touch $out");
      cargo-sweep-test =
        builtins.seq
        (import ../tests/cargo-sweep.nix {inherit pkgs;})
        (pkgs.runCommand "cargo-sweep-test" {} "touch $out");
      docker-prune-test =
        builtins.seq
        (import ../tests/docker-prune.nix {inherit pkgs;})
        (pkgs.runCommand "docker-prune-test" {} "touch $out");
      cargo-kache-test =
        builtins.seq
        (import ../tests/cargo-kache.nix {inherit pkgs;})
        (pkgs.runCommand "cargo-kache-test" {} "touch $out");
      appearance-sync-test =
        builtins.seq
        (import ../tests/appearance-sync.nix {inherit pkgs;})
        (pkgs.runCommand "appearance-sync-test" {} "touch $out");
      helix-theme-sync-test =
        builtins.seq
        (import ../tests/helix-theme-sync.nix {inherit pkgs;})
        (pkgs.runCommand "helix-theme-sync-test" {} "touch $out");
      delta-theme-sync-test =
        builtins.seq
        (import ../tests/delta-theme-sync.nix {inherit pkgs;})
        (pkgs.runCommand "delta-theme-sync-test" {} "touch $out");
      alabaster-tmtheme-test =
        pkgs.runCommand "alabaster-tmtheme-test" {
          nativeBuildInputs = [pkgs.libxml2];
        } ''
          xmllint --noout ${../pkgs/bat-themes/Alabaster.tmTheme}
          touch $out
        '';
      alabaster-gitconfig-test =
        pkgs.runCommand "alabaster-gitconfig-test" {
          nativeBuildInputs = [pkgs.git];
        } ''
          git config --file ${../pkgs/delta-themes/alabaster.gitconfig} --list > /dev/null
          touch $out
        '';
      chrome-devtools-mcp-test =
        builtins.seq
        (import ../tests/chrome-devtools-mcp.nix {inherit pkgs;})
        (pkgs.runCommand "chrome-devtools-mcp-test" {} "touch $out");
      mcp-remote-test =
        builtins.seq
        (import ../tests/mcp-remote.nix {inherit pkgs;})
        (pkgs.runCommand "mcp-remote-test" {} "touch $out");
      codex-config-sync-test = import ../tests/codex-config-sync.nix {inherit pkgs;};
      omp-config-sync-test = import ../tests/omp-config-sync.nix {inherit pkgs;};
    in {
      inherit
        pre-commit-check
        launchd-with-logs-test
        litellm-proxy-test
        cargo-sweep-test
        docker-prune-test
        cargo-kache-test
        appearance-sync-test
        helix-theme-sync-test
        delta-theme-sync-test
        alabaster-tmtheme-test
        alabaster-gitconfig-test
        chrome-devtools-mcp-test
        mcp-remote-test
        codex-config-sync-test
        omp-config-sync-test
        ;
    });

    formatter = forEachSystem (system: nixpkgs.legacyPackages.${system}.alejandra);

    devShells = forEachSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pre-commit-check = pkgs.callPackage ./git-hooks.nix {inherit git-hooks;};
    in {
      default = pkgs.mkShell {
        inherit (pre-commit-check) shellHook;

        # imports all files ending in .asc/.gpg
        sopsPGPKeyDirs = [
          "${toString ../.}/keys/hosts"
          "${toString ../.}/keys/users"
        ];
        nativeBuildInputs = [
          (pkgs.callPackage sops-nix {}).sops-import-keys-hook
        ];
      };
    });
  };
}
