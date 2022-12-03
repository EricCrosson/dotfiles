{
  description = "Eric's NixOS and Home-Manager flake.";

  outputs = inputs @ {
    self,
    darwin,
    flake-utils,
    home-manager,
    nixpkgs,
    pre-commit-hooks,
    sops-nix,
    ...
  }: let
    # TODO: support darwin
    system = "x86_64-linux";
    user = {
      username = "eric";
      homeDirectory = "/home/eric";
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
    darwinConfigurations = {
      MBP-0954 = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        inherit inputs;
        modules = [
          ./hosts/MBP-0954/configuration.nix
          home-manager.darwinModules.home-manager
          {
            users.users.ericcrosson.home = "/Users/ericcrosson";
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = {
                system = "aarch64-darwin";
                inherit inputs;
                pkgs = import nixpkgs {
                  system = "aarch64-darwin";
                  config.allowUnfree = true;
                  overlays = [
                    inputs.firefox-darwin.overlay
                    inputs.nur.overlay
                    (self: super: {
                      # Enable Nix flakes with direnv.
                      nix-direnv = super.nix-direnv.override {enableFlakes = true;};
                    })
                  ];
                };
                user = {
                  username = "ericcrosson";
                  homeDirectory = "/Users/ericcrosson";
                  email = "ericcrosson@bitgo.com";
                  theme = "mocha";
                };
              };
              users.ericcrosson = {
                imports = [./profiles/eric];
              };
            };
          }
        ];
      };
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
    bash-barrier = {
      url = "github:ericcrosson/bash-barrier";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-diff-regex = {
      url = "github:ericcrosson/git-diff-regex";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
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
