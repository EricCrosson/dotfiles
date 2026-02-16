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
      claude-wrapper-test =
        pkgs.runCommand "claude-wrapper-test" {
          nativeBuildInputs = [pkgs.go];
          src = self.sourceInfo + "/pkgs/claude-wrapper";
        } ''
          cd $src
          export HOME=$TMPDIR
          go test -v
          touch $out
        '';
    in {
      inherit pre-commit-check claude-wrapper-test;
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
