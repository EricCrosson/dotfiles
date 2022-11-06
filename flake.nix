{
  description = "Eric's NixOS and Home-Manager flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
      url = "github:ericcrosson/escpkgs?dir=jsonnet-language-server";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/nur";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      user = "eric";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          inputs.nur.overlay
        ];
      };
      lib = nixpkgs.lib;

      specialArgs = {                             # Pass variables to configuration.nix
        inherit inputs pkgs user system;
      };

      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = specialArgs;
            users.${user} = {
              imports = [ ./home.nix ];
            };
          };
        }
      ];
    in
    {
      nixosConfigurations = {
        chimp = lib.nixosSystem {
          inherit system modules specialArgs;
        };
      };
    };
}
