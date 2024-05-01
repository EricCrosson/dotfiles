{pkgs, ...}: {
  config = {
    environment = {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      systemPackages = [];

      shells = [pkgs.zsh];

      variables = {
        SHELL = "${pkgs.zsh}/bin/zsh";
        LANG = "en_US.UTF-8";
      };
    };

    programs = {
      gnupg = {
        agent = {
          enable = true;
          enableSSHSupport = true;
        };
      };

      # Create /etc/zshrc that loads the nix-darwin environment.
      zsh = {
        enable = true;
      };
    };

    home-manager.sharedModules = [
      {
        home.file = {
          gpg-agent = {
            target = ".gnupg/gpg-agent.conf";
            text = ''
              pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
              default-cache-ttl 43200
              default-cache-ttl-ssh 43200
              max-cache-ttl 43200
              max-cache-ttl-ssh 43200
            '';
          };

          scdaemon = {
            target = ".gnupg/scdaemon.conf";
            text = ''
              disable-ccid
            '';
          };
        };
      }
    ];

    nix = {
      # Required to use flakes, which are an experimental module
      package = pkgs.nixUnstable;

      extraOptions = ''
        keep-derivations = true
        keep-outputs = true
        experimental-features = nix-command flakes

        min-free = ${toString (20 * 1024 * 1024 * 1024)}
        max-free = ${toString (30 * 1024 * 1024 * 1024)}
      '';

      gc = {
        user = "ericcrosson";
        automatic = true;
        interval = {
          Weekday = 0;
          Hour = 0;
          Minute = 0;
        };
        options = "--delete-older-than 7d";
      };
    };

    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
    };

    # Auto upgrade nix package and the daemon service.
    services.nix-daemon.enable = true;

    # Used for backwards compatibility, please read the changelog before changing:
    # $ darwin-rebuild changelog
    system.stateVersion = 4;
  };
}
