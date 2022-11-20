{
  description = "Eric's NixOS and Home-Manager flake.";

  outputs = inputs @ {
    self,
    flake-utils,
    home-manager,
    nixpkgs,
    pre-commit-hooks,
    sops-nix,
    ...
  }: let
    system = "x86_64-linux";
    user = {
      username = "eric";
      email = "eric.s.crosson@utexas.edu";
      theme = "mocha";
    };
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        inputs.nur.overlay
        (self: super: {
          # Enable Nix flakes with direnv.
          nix-direnv = super.nix-direnv.override {enableFlakes = true;};
        })
      ];
    };
    lib = nixpkgs.lib;

    specialArgs = {
      # Pass variables to configuration.nix
      inherit inputs pkgs user system;
    };

    checks = {
      pre-commit-check = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          alejandra.enable = true;
          prettier.enable = true;
        };
      };
    };
  in {
    devShells.${system}.default = pkgs.mkShell {
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
    nixosConfigurations = {
      belisaere = lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./hosts/belisaere/configuration.nix
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = specialArgs;
              users.${user.username} = {
                imports = [./profiles/eric];
              };
            };
          }
        ];
      };
    };
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ast-grep = {
      url = "github:ericcrosson/escpkgs?dir=ast-grep";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-disjoint = {
      url = "github:ericcrosson/git-disjoint";
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
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
