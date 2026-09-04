{inputs, ...}: {
  # Creates a Darwin system configuration for a specific host
  #
  # Arguments:
  #   hostName: The name of the host (must match directory name under hosts/)
  #   hostConfig: The complete host configuration data
  #
  # Returns: A nix-darwin system configuration
  #
  # Example:
  #   mkDarwinHost {
  #     hostName = "my-macbook";
  #     hostConfig = {
  #       system = "aarch64-darwin";
  #       profileName = "personal";
  #       homeManagerModules = [ ../modules/home-manager/personal.nix ];
  #       nixDarwinModules = [ ./modules/custom.nix ];
  #     };
  #   }
  #
  mkDarwinHost = {
    hostName,
    hostConfig,
  }: let
    inherit (hostConfig) system profileName;
    homeManagerModules = hostConfig.homeManagerModules or [];
    nixDarwinModules = hostConfig.nixDarwinModules or [];

    pkgs = import inputs.nixpkgs {
      inherit system;
      config = {
        allowUnfree = true;
        allowBroken = true; # Needed for open-webui
      };
      overlays = [
        inputs.fenix.overlays.default
        inputs.mcp-servers-nix.overlays.default
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
          ({config, ...}: let
            profile = config.profiles.${profileName};
          in {
            users.users.${profile.username} = {
              home = profile.homeDirectory;
            };

            home-manager = {
              extraSpecialArgs = {
                inherit pkgs inputs profile;
              };
              sharedModules = [
                inputs.sops-nix.homeManagerModules.sops
              ];
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${profile.username}.imports = homeManagerModules;
            };
          })
        ]
        ++ nixDarwinModules;
    };

  # Creates a NixOS system configuration for a specific host
  #
  # Arguments:
  #   hostName: The name of the host (must match directory name under hosts/)
  #   hostConfig: The complete host configuration data
  #
  # Returns: A NixOS system configuration
  mkNixosHost = {
    hostName,
    hostConfig,
  }: let
    inherit (hostConfig) system profileName;
    homeManagerModules = hostConfig.homeManagerModules or [];
    nixosModules = hostConfig.nixosModules or [];

    pkgs = import inputs.nixpkgs {
      inherit system;
      config = {
        allowUnfree = true;
        allowBroken = true; # Needed for open-webui
        nvidia.acceptLicense = true;
      };
      overlays = [
        inputs.fenix.overlays.default
        inputs.mcp-servers-nix.overlays.default
        (import ../overlays).combined
      ];
    };
  in
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit hostName inputs system;
      };
      modules =
        [
          # Import profile modules
          ../modules/profiles
          ../modules/profiles/profiles.nix

          # Disk layout
          inputs.disko.nixosModules.disko

          # Host-specific configuration
          ./${hostName}

          {nixpkgs.pkgs = pkgs;}

          # Home Manager configuration
          inputs.home-manager.nixosModules.home-manager
          ({config, ...}: let
            profile = config.profiles.${profileName};
          in {
            home-manager = {
              extraSpecialArgs = {
                inherit pkgs inputs profile;
              };
              sharedModules = [
                inputs.sops-nix.homeManagerModules.sops
              ];
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${profile.username}.imports = homeManagerModules;
            };
          })
        ]
        ++ nixosModules;
    };

  # Available host configurations
  hosts = {
    "MBP-0954" = {
      system = "aarch64-darwin";
      profileName = "bitgo";
      homeManagerModules = [
        ../profiles/omp
        ../profiles/eric
        ../profiles/bitgo
        ../profiles/development
        ../home/editor/helix
      ];
      nixDarwinModules = [];
    };
  };

  # Available NixOS host configurations
  nixosHosts = {
    "athens" = {
      system = "x86_64-linux";
      profileName = "personal";
      homeManagerModules = [
        ../profiles/omp
        ../profiles/eric
        ../profiles/development
        ../home/editor/helix
        ({lib, ...}: {services.cargo-sweep.enable = lib.mkForce false;})
        ({lib, ...}: {
          programs.omp.models.providers.litellm.baseUrl = lib.mkForce "http://mbp-proxy:4000/v1";
          programs.omp.settings.modelRoles = {
            plan = "litellm/gemini-3.8-flash:auto";
            slow = "litellm/gemini-3.8-flash:auto";
          };
        })
      ];
      nixosModules = [];
    };
  };
}
