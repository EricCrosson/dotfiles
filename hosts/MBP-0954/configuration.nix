{pkgs, ...}: {
  config = {
    environment = {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      # systemPackages = [];

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

      zsh = {
        enable = true;
      };
    };

    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
        keep-derivations = true
        keep-outputs = true

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

      # Required to use flakes, which are an experimental module
      package = pkgs.nixVersions.git;

      settings = {
        trusted-users = [
          "@admin"
        ];
      };
    };

    system = {
      defaults = {
        dock = {
          autohide = true;
        };
        NSGlobalDomain = {
          ApplePressAndHoldEnabled = false;
          AppleShowAllExtensions = true;
          AppleShowAllFiles = true;
          InitialKeyRepeat = 15;
          KeyRepeat = 2;
          NSAutomaticCapitalizationEnabled = false;
          NSAutomaticPeriodSubstitutionEnabled = false;
          NSAutomaticQuoteSubstitutionEnabled = false;
          NSAutomaticSpellingCorrectionEnabled = false;
        };
      };

      # Used for backwards compatibility, please read the changelog before changing:
      # $ darwin-rebuild changelog
      stateVersion = 4;
    };

    # Auto upgrade Nix and the daemon service.
    services.nix-daemon.enable = true;
  };
}
