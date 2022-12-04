{
  homeImports,
  inputs,
  withSystem,
  ...
}: {
  flake.nixosConfigurations = withSystem "x86_64-linux" ({system, ...}: let
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
    # TODO: avoid defining multiple times
    homeDir = username:
      if pkgs.stdenv.isDarwin
      then "/Users/${username}"
      else "/home/${username}";
    user = rec {
      username = "eric";
      homeDirectory = homeDir username;
      email = "eric.s.crosson@utexas.edu";
      theme = "mocha";
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
    homeDir = username:
      if pkgs.stdenv.isDarwin
      then "/Users/${username}"
      else "/home${username}";
    user = rec {
      username = "ericcrosson";
      homeDirectory = homeDir username;
      email = "ericcrosson@bitgo.com";
      theme = "mocha";
    };
  in {
    MBP-0954 = inputs.darwin.lib.darwinSystem {
      inherit system;
      modules = [
        ./MBP-0954/configuration.nix
        inputs.home-manager.darwinModules.home-manager
        {
          users.users.${user.username}.home = homeDir user.username;
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
