{
  description = "dotfiles dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
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
    pre-commit-hooks,
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
      pre-commit-check = pkgs.callPackage ./pre-commit-hooks.nix {inherit pre-commit-hooks;};
    in {
      inherit pre-commit-check;
    });

    formatter = forEachSystem (system: nixpkgs.legacyPackages.${system}.alejandra);

    devShells = forEachSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pre-commit-check = pkgs.callPackage ./pre-commit-hooks.nix {inherit pre-commit-hooks;};
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
