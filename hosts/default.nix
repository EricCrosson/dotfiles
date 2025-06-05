{inputs, ...}: {
  # Function to create a Darwin system configuration
  mkDarwinHost = {
    hostName,
    system,
    modules ? [],
  }: let
    pkgs = import inputs.nixpkgs {
      inherit system;
      config = {
        allowUnfree = true;
        allowBroken = true; # Needed for open-webui
      };
      overlays = [
        inputs.fenix.overlays.default
        (import ../overlays).combined
      ];
    };
  in
    inputs.nix-darwin.lib.darwinSystem {
      specialArgs = {
        inherit hostName inputs system;
      };
      modules =
        [
          # Import profile modules
          ../modules/profiles
          ../modules/profiles/profiles.nix

          # Standard modules
          ../modules/darwin

          # Host-specific configuration
          ./${hostName}

          # Home Manager configuration
          inputs.home-manager.darwinModules.home-manager
          ({config, ...}: {
            users.users.${config.profiles.bitgo.username}.home = config.profiles.bitgo.homeDirectory;
            home-manager = {
              extraSpecialArgs = {
                inherit pkgs inputs;
                user = config.profiles.bitgo;
              };
              sharedModules = [
                inputs.sops-nix.homeManagerModules.sops
              ];
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${config.profiles.bitgo.username}.imports = [
                ../profiles/eric
                ../profiles/bitgo
                ../profiles/development
                ../home/editor/helix
              ];
            };
          })
        ]
        ++ modules;
    };

  # Available hosts to build
  hosts = {
    "MBP-0954" = {
      system = "aarch64-darwin";
      modules = [];
    };
  };
}
