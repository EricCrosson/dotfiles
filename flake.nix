{
  description = "Eric's NixOS and Home-Manager flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
    };
    nur.url = "github:nix-community/NUR";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, helix, nur, ... }:
    let
      system = "x86_64-linux";
      user = "eric";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;

      specialArgs = {                             # Pass variables to configuration.nix
        inherit inputs user system;
      };

      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.users.${user} = {
            imports = [ ./home.nix ];
          };
        }
      ];
    in
    {
      # nixpkgs.config.packageOverrides = pkgs: {
      #   nur = import (builtins.fetchTarball {
      #     url = "https://github.com/nix-community/NUR/archive/master.tar.gz";
      #     sha256 = "14pmj1rgcsp0w0gkzi4j6mlr85n7j26ybkrwrgnp8jfngycgx5bx";
      #   }) {
      #     inherit pkgs;
      #   };
      # };
      nixosConfigurations = {
        chimp = lib.nixosSystem {
          inherit system modules specialArgs;
        };
      };
    };
}
