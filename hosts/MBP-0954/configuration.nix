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

    fonts = {
      packages = [
        pkgs.hack-font
      ];
    };

    homebrew = {
      enable = true;
      onActivation.cleanup = "uninstall";

      brews = [
        "colima"
        "docker"
        "docker-buildx"
        "docker-compose"
        "ffmpeg"
        "llm"
        "md5sha1sum"
        "pass-otp"
        "xcodegen"
        "ykman"
      ];

      caskArgs = {
        no_quarantine = true;
        require_sha = true;
      };

      casks = [
        "kitty"
        "postman"
        "vlc"
      ];
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
        extra-trusted-users = ericcrosson
        keep-derivations = true
        keep-outputs = true

        min-free = ${toString (2 * 1024 * 1024 * 1024)}
        max-free = ${toString (10 * 1024 * 1024 * 1024)}
      '';

      gc = {
        automatic = true;
        interval = {
          Weekday = 0;
          Hour = 0;
          Minute = 0;
        };
        options = "--delete-older-than 7d";
      };

      optimise = {
        automatic = true;
      };

      # Required to use flakes, which are an experimental module
      package = pkgs.nixVersions.nix_2_24;

      settings = {
        trusted-users = [
          "@admin"
        ];
      };
    };

    security = {
      pam.services.sudo_local.touchIdAuth = true;
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
  };
}
