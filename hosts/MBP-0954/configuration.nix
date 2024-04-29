{pkgs, ...}: let
  # TODO: use ncurses-pinentry
  pinentry-program = "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac";
in {
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

    # Create /etc/zshrc that loads the nix-darwin environment.
    programs.zsh.enable = true;

    # Required to use flakes, which are an experimental module
    nix.package = pkgs.nixUnstable;

    nix.extraOptions = ''
      keep-derivations = true
      keep-outputs = true
      experimental-features = nix-command flakes

      min-free = ${toString (20 * 1024 * 1024 * 1024)}
      max-free = ${toString (30 * 1024 * 1024 * 1024)}
    '';

    nix = {
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

    programs.gnupg = {
      agent = {
        enable = true;
        enableSSHSupport = true;
      };
    };

    home-manager.sharedModules = [
      {
        home.file = {
          gpg-agent = {
            target = ".gnupg/gpg-agent.conf";
            text = ''
              pinentry-program ${pinentry-program}
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

    home-manager.useUserPackages = true;
    home-manager.useGlobalPkgs = true;

    # Auto upgrade nix package and the daemon service.
    services.nix-daemon.enable = true;

    # Used for backwards compatibility, please read the changelog before changing:
    # $ darwin-rebuild changelog
    system.stateVersion = 4;
  };
}
