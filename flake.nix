{
  description = "Eric's NixOS and Home-Manager flake.";

  outputs = inputs @ {
    self,
    flake-parts,
    pre-commit-hooks,
    sops-nix,
    ...
  }:
    flake-parts.lib.mkFlake {inherit (inputs) self;} {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      imports = [
        ./profiles
        ./hosts
      ];

      perSystem = {
        pkgs,
        system,
        ...
      }: let
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              actionlint.enable = true;
              alejandra.enable = true;
              deadnix.enable = true;
              prettier.enable = true;
              statix.enable = true;
              stylua.enable = true;
            };
          };
        };
      in {
        formatter = pkgs.alejandra;

        devShells.default = pkgs.mkShell {
          inherit (checks.pre-commit-check) shellHook;
          # imports all files ending in .asc/.gpg
          sopsPGPKeyDirs = [
            "${toString ./.}/keys/hosts"
            "${toString ./.}/keys/users"
          ];
          nativeBuildInputs = [
            (pkgs.callPackage sops-nix {}).sops-import-keys-hook
          ];
        };
      };
    };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    atuin = {
      url = "github:ellie/atuin";
      inputs.flake-utils.follows = "flake-utils";
      # don't follow nixpkgs until I'm using nixpkgs with Rust 1.67 or higher
      # https://github.com/ellie/atuin/issues/737
    };
    ast-grep = {
      url = "github:ericcrosson/escpkgs?dir=ast-grep";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bash-barrier = {
      url = "github:ericcrosson/bash-barrier";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bell = {
      url = "github:ericcrosson/bell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    git-diff-regex = {
      url = "github:ericcrosson/git-diff-regex";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-disjoint = {
      url = "github:ericcrosson/git-disjoint";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-dl = {
      url = "github:ericcrosson/git-dl";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-review = {
      url = "github:ericcrosson/git-review";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:helix-editor/helix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    jsonnet-language-server = {
      url = "github:grafana/jsonnet-language-server?dir=nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kmonad = {
      url = "github:kmonad/kmonad?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/nur";
    nurl = {
      url = "github:nix-community/nurl";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ouch = {
      url = "github:ericcrosson/escpkgs?dir=ouch";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    retry = {
      url = "github:ericcrosson/retry";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
