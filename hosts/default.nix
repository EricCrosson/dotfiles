{
  homeImports,
  inputs,
  withSystem,
  ...
}: let
  preferences = {
    theme = "mocha";
  };
in {
  flake.nixosConfigurations = withSystem "x86_64-linux" ({system, ...}: let
    # REFACTOR: apply overlays in one spot, for entire nixos config
    pkgs = import inputs.nixpkgs {
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
    user = rec {
      inherit preferences;
      username = "eric";
      homeDirectory = "/home/${username}";
      email = "eric.s.crosson@utexas.edu";
    };
    specialArgs = {
      inherit inputs pkgs user;
    };
  in {
    belisaere = inputs.nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./belisaere/configuration.nix
        inputs.sops-nix.nixosModules.sops
        inputs.home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = specialArgs;
            users.${user.username}.imports = homeImports."eric@belisaere";
          };
        }
      ];
    };
  });

  flake.darwinConfigurations = withSystem "aarch64-darwin" ({system, ...}: let
    pkgs = import inputs.nixpkgs {
      inherit system;
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
    user = rec {
      inherit preferences;
      username = "ericcrosson";
      homeDirectory = "/Users/${username}";
      email = "ericcrosson@bitgo.com";
    };
  in {
    MBP-0954 = inputs.darwin.lib.darwinSystem {
      inherit system;
      modules = [
        inputs.home-manager.darwinModules.home-manager
        {
          users.users.${user.username}.home = user.homeDirectory;
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = {inherit inputs pkgs user;};
            users.${user.username}.imports = homeImports."eric@MBP-0954";
          };
        }
      ];
    };
  });
}
